open Std
open Std.Result.Syntax

module Ast = Typ.Ast
module Env = Typ.Infer.Env
module Type = Typ.Ast.Type
module TypeScheme = Typ.Infer.TypeScheme
module SurfacePath = Typ.Model.Surface_path

let ident name =
  Syn.parse_ident name
  |> Option.map ~fn:SurfacePath.from_syn_ident
  |> Option.expect ~msg:("expected surface path test identifier " ^ name)

let nominal name = Type.Apply { ident = ident name; arguments = [] }

let int_type = fun () -> nominal "int"

let bool_type = fun () -> nominal "bool"

let scheme type_ = TypeScheme.monomorphic type_

let origin: Ast.origin = {
  span = Syn.Span.make ~start:0 ~end_:0;
  kind = Syn.SyntaxKind.TYPE_DECL;
}

let type_decl name: Ast.type_declaration = {
  Ast.origin;
  name = ident name;
  parameters = [];
  definition = { origin; kind = Ast.Abstract };
}

let core_type name: Ast.core_type = { origin; type_ = None; kind = Ast.TypeIdent (ident name) }

let record_field name type_name: Ast.record_field_declaration = {
  origin;
  name = ident name;
  mutable_ = false;
  type_annotation = core_type type_name;
}

let record_type_decl name fields: Ast.type_declaration = {
  origin;
  name = ident name;
  parameters = [];
  definition = { origin; kind = Ast.Record fields };
}

let variant_constructor name arguments: Ast.type_constructor = {
  origin;
  name = ident name;
  arguments;
  result = None;
}

let record_field_info owner field: Env.record_field_info = { owner; field }

let constructor_description name type_: Env.constructor_description = {
  name = ident name;
  scheme = scheme type_;
  result = type_;
  arguments = Env.Tuple [];
  instantiate_generics = true;
}

let inline_record_constructor_description
  name
  type_
  (owner: Ast.type_declaration)
  (constructor: Ast.type_constructor)
  (field: Ast.record_field_declaration)
  : Env.constructor_description = {
  name = ident name;
  scheme = scheme type_;
  result = type_;
  arguments =
    Env.InlineRecord {
      owner;
      constructor;
      payload_type = nominal (name ^ "_payload");
      fields = [
        ({ declaration = field; type_ = int_type () }: Env.inline_record_field);
      ];
    };
  instantiate_generics = true;
}

let constructor_scheme actual = Option.map actual ~fn:(fun description -> description.Env.scheme)

let assert_scheme_body ~expected actual =
  match actual with
  | Some actual when Type.equal actual.TypeScheme.body expected -> Ok ()
  | Some (actual: TypeScheme.t) ->
      Error ("expected " ^ Type.to_string expected ^ " but found " ^ Type.to_string actual.body)
  | None -> Error ("expected " ^ Type.to_string expected ^ " scheme")

let assert_no_scheme actual =
  match actual with
  | None -> Ok ()
  | Some (actual: TypeScheme.t) ->
      Error ("expected no scheme but found " ^ Type.to_string actual.body)

let assert_type_decl_name ~expected (actual: Ast.type_declaration option) =
  match actual with
  | Some actual when SurfacePath.equal actual.Ast.name (ident expected) -> Ok ()
  | Some actual ->
      Error ("expected type " ^ expected ^ " but found " ^ SurfacePath.to_string actual.name)
  | None -> Error ("expected type " ^ expected)

let assert_no_type_decl (actual: Ast.type_declaration option) =
  match actual with
  | None -> Ok ()
  | Some actual ->
      Error ("expected no type declaration but found " ^ SurfacePath.to_string actual.Ast.name)

let assert_record_field ~owner ~field (actual: Env.record_field_info option) =
  match actual with
  | Some actual when SurfacePath.equal actual.Env.owner.Ast.name (ident owner)
  && SurfacePath.equal actual.field.Ast.name (ident field) -> Ok ()
  | Some actual ->
      Error ("expected record field "
      ^ owner
      ^ "."
      ^ field
      ^ " but found "
      ^ SurfacePath.to_string actual.owner.Ast.name
      ^ "."
      ^ SurfacePath.to_string actual.field.Ast.name)
  | None -> Error ("expected record field " ^ owner ^ "." ^ field)

let assert_no_record_field (actual: Env.record_field_info option) =
  match actual with
  | None -> Ok ()
  | Some actual ->
      Error ("expected no record field but found "
      ^ SurfacePath.to_string actual.owner.Ast.name
      ^ "."
      ^ SurfacePath.to_string actual.field.Ast.name)

let assert_true ~msg value =
  if value then
    Ok ()
  else
    Error msg

let assert_false ~msg value = assert_true ~msg (not value)

let export_names env =
  Env.exports env
  |> Iter.Iterator.map ~fn:(fun (name, _) -> SurfacePath.to_string name)
  |> Iter.Iterator.to_list

let exported_type_names env =
  Env.exported_types env
  |> Iter.Iterator.map ~fn:(fun (name, _) -> SurfacePath.to_string name)
  |> Iter.Iterator.to_list

let assert_names ~expected actual =
  if actual = expected then
    Ok ()
  else
    Error ("expected ["
    ^ String.concat ", " expected
    ^ "] but found ["
    ^ String.concat ", " actual
    ^ "]")

let test_value_scope_shadows_and_pops _ctx =
  let name = ident "value" in
  let env = Env.create () in
  let env = Env.add_value env ~name ~scheme:(scheme (int_type ())) in
  let env = Env.push_scope env in
  let env = Env.add_value env ~name ~scheme:(scheme (bool_type ())) in
  let* () = assert_scheme_body ~expected:(bool_type ()) (Env.get_value env ~name) in
  let env = Env.pop_scope env in
  assert_scheme_body ~expected:(int_type ()) (Env.get_value env ~name)

let test_exports_ignore_local_value_scopes _ctx =
  let root_name = ident "root_value" in
  let local_name = ident "local_value" in
  let env = Env.create () in
  let env = Env.add_value env ~name:root_name ~scheme:(scheme (int_type ())) in
  let env = Env.push_scope env in
  let env = Env.add_value env ~name:local_name ~scheme:(scheme (bool_type ())) in
  let* () = assert_scheme_body ~expected:(bool_type ()) (Env.get_value env ~name:local_name) in
  assert_names ~expected:[ "root_value" ] (export_names env)

let test_types_are_current_module_not_lexical_scope _ctx =
  let name = ident "color" in
  let env = Env.create () in
  let env = Env.push_scope env in
  let env = Env.add_type env ~name ~declaration:(type_decl "color") in
  let* () = assert_type_decl_name ~expected:"color" (Env.get_type env ~name) in
  let env = Env.pop_scope env in
  let* () = assert_type_decl_name ~expected:"color" (Env.get_type env ~name) in
  assert_names ~expected:[ "color" ] (exported_type_names env)

let test_constructors_are_current_module_not_lexical_scope _ctx =
  let name = ident "Red" in
  let env = Env.create () in
  let env = Env.push_scope env in
  let env =
    Env.add_constructor
      env
      ~name
      ~description:(constructor_description "Red" (int_type ()))
  in
  let* () =
    assert_scheme_body ~expected:(int_type ()) (constructor_scheme (Env.get_constructor env ~name))
  in
  let env = Env.pop_scope env in
  assert_scheme_body ~expected:(int_type ()) (constructor_scheme (Env.get_constructor env ~name))

let test_record_fields_are_current_module_not_lexical_scope _ctx =
  let field_name = ident "x" in
  let field = record_field "x" "int" in
  let owner = record_type_decl "point" [ field ] in
  let info = record_field_info owner field in
  let env = Env.create () in
  let env = Env.push_scope env in
  let env = Env.add_record_field env ~name:field_name ~info in
  let* () =
    assert_record_field
      ~owner:"point"
      ~field:"x"
      (Env.get_record_field env ~name:field_name)
  in
  let env = Env.pop_scope env in
  assert_record_field
    ~owner:"point"
    ~field:"x"
    (Env.get_record_field env ~name:field_name)

let test_record_fields_shadow_in_declaration_order _ctx =
  let field_name = ident "hello" in
  let first_field = record_field "hello" "int" in
  let first_owner = record_type_decl "a" [ first_field ] in
  let second_field = record_field "hello" "int" in
  let second_owner = record_type_decl "b" [ second_field ] in
  let env = Env.create () in
  let env =
    Env.add_record_field
      env
      ~name:field_name
      ~info:(record_field_info first_owner first_field)
  in
  let env =
    Env.add_record_field
      env
      ~name:field_name
      ~info:(record_field_info second_owner second_field)
  in
  assert_record_field
    ~owner:"b"
    ~field:"hello"
    (Env.get_record_field env ~name:field_name)

let test_inline_record_fields_stay_on_constructor_description _ctx =
  let field_name = ident "code" in
  let constructor_name = ident "Payload" in
  let field = record_field "code" "int" in
  let owner = type_decl "t" in
  let constructor = variant_constructor "Payload" (Ast.Record [ field ]) in
  let env = Env.create () in
  let env =
    Env.add_constructor
      env
      ~name:constructor_name
      ~description:(inline_record_constructor_description
        "Payload"
        (nominal "t")
        owner
        constructor
        field)
  in
  let* () = assert_no_record_field (Env.get_record_field env ~name:field_name) in
  match Env.get_constructor env ~name:constructor_name with
  | Some { Env.arguments = Env.InlineRecord inline_record; _ } ->
      assert_true
        ~msg:"constructor description should retain inline field metadata"
        (List.exists
          (fun (field: Env.inline_record_field) ->
            SurfacePath.equal
              field.declaration.Ast.name
              field_name)
          inline_record.fields)
  | Some _ -> Error "expected inline record constructor arguments"
  | None -> Error "expected Payload constructor"

let test_module_sees_parent_types_but_exports_only_own_types _ctx =
  let parent_type = ident "a" in
  let child_type = ident "b" in
  let module_name = ident "M" in
  let env = Env.create () in
  let env = Env.add_type env ~name:parent_type ~declaration:(type_decl "a") in
  let env = Env.push_module env ~name:module_name in
  let* () = assert_type_decl_name ~expected:"a" (Env.get_type env ~name:parent_type) in
  let env = Env.add_type env ~name:child_type ~declaration:(type_decl "b") in
  let* () = assert_type_decl_name ~expected:"b" (Env.get_type env ~name:child_type) in
  let env = Env.pop_module env in
  let* () = assert_no_type_decl (Env.get_type env ~name:child_type) in
  match Env.get_module env ~name:module_name with
  | None -> Error "expected module M"
  | Some summary ->
      let* () =
        assert_false
          ~msg:"module summary should not copy parent type a"
          (Env.module_has_type summary ~name:parent_type)
      in
      assert_true
        ~msg:"module summary should export own type b"
        (Env.module_has_type summary ~name:child_type)

let test_nested_modules_resolve_upward_and_export_downward _ctx =
  let root_type = ident "a" in
  let middle_type = ident "b" in
  let inner_type = ident "c" in
  let m1_name = ident "M1" in
  let m2_name = ident "M2" in
  let env = Env.create () in
  let env = Env.add_type env ~name:root_type ~declaration:(type_decl "a") in
  let env = Env.push_module env ~name:m1_name in
  let env = Env.add_type env ~name:middle_type ~declaration:(type_decl "b") in
  let env = Env.push_module env ~name:m2_name in
  let* () = assert_type_decl_name ~expected:"a" (Env.get_type env ~name:root_type) in
  let* () = assert_type_decl_name ~expected:"b" (Env.get_type env ~name:middle_type) in
  let env = Env.add_type env ~name:inner_type ~declaration:(type_decl "c") in
  let env = Env.pop_module env in
  let* () = assert_no_type_decl (Env.get_type env ~name:inner_type) in
  let* () =
    match Env.get_module env ~name:m2_name with
    | None -> Error "expected nested module M2 while inside M1"
    | Some m2 ->
        assert_true ~msg:"M2 should export own type c" (Env.module_has_type m2 ~name:inner_type)
  in
  let env = Env.pop_module env in
  match Env.get_module env ~name:m1_name with
  | None -> Error "expected module M1"
  | Some m1 ->
      let* () =
        assert_true ~msg:"M1 should export own type b" (Env.module_has_type m1 ~name:middle_type)
      in
      (
        match Env.module_get_module m1 ~name:m2_name with
        | None -> Error "expected M1.M2"
        | Some m2 ->
            assert_true
              ~msg:"M1.M2 should export own type c"
              (Env.module_has_type m2 ~name:inner_type)
      )

let test_module_values_see_parent_values_but_do_not_leak _ctx =
  let root_value = ident "root_value" in
  let child_value = ident "child_value" in
  let module_name = ident "M" in
  let env = Env.create () in
  let env = Env.add_value env ~name:root_value ~scheme:(scheme (int_type ())) in
  let env = Env.push_module env ~name:module_name in
  let* () = assert_scheme_body ~expected:(int_type ()) (Env.get_value env ~name:root_value) in
  let env = Env.add_value env ~name:child_value ~scheme:(scheme (bool_type ())) in
  let env = Env.pop_module env in
  let* () = assert_no_scheme (Env.get_value env ~name:child_value) in
  match Env.get_module env ~name:module_name with
  | None -> Error "expected module M"
  | Some summary ->
      assert_true
        ~msg:"module summary should export own value"
        (Env.module_has_value summary ~name:child_value)

let test_module_sees_parent_record_fields_but_exports_only_own_fields _ctx =
  let parent_field_name = ident "x" in
  let child_field_name = ident "y" in
  let module_name = ident "M" in
  let parent_field = record_field "x" "int" in
  let parent_owner = record_type_decl "point" [ parent_field ] in
  let child_field = record_field "y" "bool" in
  let child_owner = record_type_decl "box" [ child_field ] in
  let env = Env.create () in
  let env =
    Env.add_record_field
      env
      ~name:parent_field_name
      ~info:(record_field_info parent_owner parent_field)
  in
  let env = Env.push_module env ~name:module_name in
  let* () =
    assert_record_field
      ~owner:"point"
      ~field:"x"
      (Env.get_record_field env ~name:parent_field_name)
  in
  let env =
    Env.add_record_field
      env
      ~name:child_field_name
      ~info:(record_field_info child_owner child_field)
  in
  let env = Env.pop_module env in
  let* () = assert_no_record_field (Env.get_record_field env ~name:child_field_name) in
  match Env.get_module env ~name:module_name with
  | None -> Error "expected module M"
  | Some summary ->
      let* () =
        assert_false
          ~msg:"module summary should not copy parent field x"
          (Env.module_has_record_field summary ~name:parent_field_name)
      in
      assert_true
        ~msg:"module summary should export own field y"
        (Env.module_has_record_field summary ~name:child_field_name)

let tests =
  Test.[
    case "infer-env: value scope shadows and pops" test_value_scope_shadows_and_pops;
    case "infer-env: exports ignore local value scopes" test_exports_ignore_local_value_scopes;
    case
      "infer-env: types are current-module not lexical-scope"
      test_types_are_current_module_not_lexical_scope;
    case
      "infer-env: constructors are current-module not lexical-scope"
      test_constructors_are_current_module_not_lexical_scope;
    case
      "infer-env: record fields are current-module not lexical-scope"
      test_record_fields_are_current_module_not_lexical_scope;
    case
      "infer-env: record fields shadow in declaration order"
      test_record_fields_shadow_in_declaration_order;
    case
      "infer-env: inline record fields stay on constructor description"
      test_inline_record_fields_stay_on_constructor_description;
    case
      "infer-env: module sees parent types but exports only own types"
      test_module_sees_parent_types_but_exports_only_own_types;
    case
      "infer-env: nested modules resolve upward and export downward"
      test_nested_modules_resolve_upward_and_export_downward;
    case
      "infer-env: module values see parent values but do not leak"
      test_module_values_see_parent_values_but_do_not_leak;
    case
      "infer-env: module sees parent record fields but exports only own fields"
      test_module_sees_parent_record_fields_but_exports_only_own_fields;
  ]

let main ~args = Test.Cli.main ~name:"typ:infer-env" ~tests ~args ()

let () = Runtime.run ~main ~args:Std.Env.args ()
