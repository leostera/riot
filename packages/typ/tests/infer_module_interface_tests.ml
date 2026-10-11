open Std
open Std.Collections
open Std.Iter
open Std.Result.Syntax

module Ast = Typ.Ast
module Env = Typ.Infer.Env
module ModuleInterface = Typ.Infer.ModuleInterface
module SurfacePath = Typ.Model.Surface_path
module Type = Typ.Ast.Type
module TypeScheme = Typ.Infer.TypeScheme

let ident name =
  Syn.parse_ident name
  |> Option.map ~fn:SurfacePath.from_syn_ident
  |> Option.expect ~msg:("expected surface path test identifier " ^ name)

let nominal name = Type.Apply { ident = ident name; arguments = [] }

let int_type = fun () -> nominal "int"

let source_slice source =
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create interface test source slice"

let bool_type = fun () -> nominal "bool"

let scheme type_ = TypeScheme.monomorphic type_

let origin: Ast.origin = {
  span = Syn.Span.make ~start:0 ~end_:0;
  kind = Syn.SyntaxKind.TYPE_DECL;
}

let type_definition kind: Ast.type_definition = { origin; kind }

let constructor name: Ast.type_constructor = {
  origin;
  name = ident name;
  arguments = Tuple [];
  result = None;
}

let abstract_type name: Ast.type_declaration = {
  origin;
  name = ident name;
  parameters = [];
  definition = type_definition Abstract;
}

let variant_type name constructors: Ast.type_declaration = {
  origin;
  name = ident name;
  parameters = [];
  definition = type_definition (Variant (List.map constructors ~fn:constructor));
}

let render_interface intf =
  Typ.SignatureGenerator.from_exports
    ~types:(ModuleInterface.types intf)
    ~values:(
      intf
      |> ModuleInterface.values
      |> Iterator.map ~fn:(fun (name, (type_scheme: TypeScheme.t)) -> (name, type_scheme.body))
    )

let names items =
  items
  |> Iterator.map ~fn:(fun (name, _) -> SurfacePath.to_string name)
  |> Iterator.to_list

let assert_equal_string ~expected actual =
  if String.equal expected actual then
    Ok ()
  else
    Error ("expected:\n" ^ expected ^ "\nbut found:\n" ^ actual)

let assert_equal_names ~expected actual =
  if actual = expected then
    Ok ()
  else
    Error ("expected ["
    ^ String.concat ", " expected
    ^ "] but found ["
    ^ String.concat ", " actual
    ^ "]")

let find_module intf name =
  ModuleInterface.modules intf
  |> Iterator.find ~fn:(fun (module_name, _) -> SurfacePath.equal module_name (ident name))
  |> Option.map ~fn:(fun (_, intf) -> intf)

let test_empty_env_renders_empty_interface _ctx =
  Env.create ()
  |> ModuleInterface.from_env
  |> render_interface
  |> assert_equal_string ~expected:""

let test_env_with_one_type_renders_type _ctx =
  let env = Env.create () in
  let env =
    Env.add_type
      env
      ~name:(ident "color")
      ~declaration:(variant_type "color" [ "Red"; "Blue" ])
  in
  env
  |> ModuleInterface.from_env
  |> render_interface
  |> assert_equal_string ~expected:"type color = Red | Blue\n"

let test_env_with_one_value_renders_value _ctx =
  let env = Env.create () in
  let env = Env.add_value env ~name:(ident "answer") ~scheme:(scheme (int_type ())) in
  env
  |> ModuleInterface.from_env
  |> render_interface
  |> assert_equal_string ~expected:"val answer : int\n"

let test_local_values_do_not_become_exports _ctx =
  let env = Env.create () in
  let env = Env.add_value env ~name:(ident "root") ~scheme:(scheme (int_type ())) in
  let env = Env.push_scope env in
  let env = Env.add_value env ~name:(ident "local") ~scheme:(scheme (bool_type ())) in
  let intf = ModuleInterface.from_env env in
  ModuleInterface.values intf
  |> names
  |> assert_equal_names ~expected:[ "root" ]

let test_multiple_modules_are_copied_recursively _ctx =
  let env = Env.create () in
  let env = Env.push_module env ~name:(ident "M") in
  let env = Env.add_value env ~name:(ident "value") ~scheme:(scheme (int_type ())) in
  let env = Env.push_module env ~name:(ident "Inner") in
  let env = Env.add_type env ~name:(ident "t") ~declaration:(abstract_type "t") in
  let env = Env.pop_module env in
  let env = Env.pop_module env in
  let env = Env.push_module env ~name:(ident "N") in
  let env = Env.add_value env ~name:(ident "flag") ~scheme:(scheme (bool_type ())) in
  let env = Env.pop_module env in
  let intf = ModuleInterface.from_env env in
  let* () =
    ModuleInterface.modules intf
    |> names
    |> assert_equal_names ~expected:[ "M"; "N" ]
  in
  match (find_module intf "M", find_module intf "N") with
  | (Some m, Some n) ->
      let* () =
        ModuleInterface.values m
        |> names
        |> assert_equal_names ~expected:[ "value" ]
      in
      let* () =
        ModuleInterface.modules m
        |> names
        |> assert_equal_names ~expected:[ "Inner" ]
      in
      ModuleInterface.values n
      |> names
      |> assert_equal_names ~expected:[ "flag" ]
  | _ -> Error "expected modules M and N"

let test_infer_signature_exports_typed_declarations _ctx =
  let source = {ocaml|
type color = Red | Blue
val choose : color -> color
val identity : 'a -> 'a
external primitive : int -> int = "primitive"
|ocaml} in
  let parse_result = Syn.parse_interface (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | _ :: _ ->
          Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
      | [] ->
          result.intf
          |> render_interface
          |> assert_equal_string
            ~expected:(
              "type color = Red | Blue\n"
              ^ "val choose : color -> color\n"
              ^ "val identity : 'a -> 'a\n"
              ^ "val primitive : int -> int\n")

let test_infer_nested_modules_and_aliases _ctx =
  let source = {ocaml|
module M = struct
  type color = Red | Blue
  let choose (value: color) = value
end
module Alias = M
let selected = Alias.choose Alias.Red
|ocaml} in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | _ :: _ ->
          Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
      | [] ->
          result.intf
          |> render_interface
          |> assert_equal_string ~expected:"val selected : M.color\n"

let test_infer_exception_declarations _ctx =
  let source = {ocaml|
exception Failure
exception Error of string
let failure = Failure
let error = Error "problem"
|ocaml} in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | _ :: _ ->
          Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
      | [] ->
          result.intf
          |> render_interface
          |> assert_equal_string ~expected:"val failure : exn\nval error : exn\n"

let test_infer_common_expression_forms _ctx =
  let source = {ocaml|
exception Failure
exception Error of string
let sum a b = a + b
let concatenate a b = a ^ b
let values = [1; 2; 3]
let total = match values with | [] -> 0 | head :: tail -> head
let loop n = for i = 0 to n do assert (i >= 0) done
let sequence = print_endline "ok"; ()
let recovered = try Error "bad" with | Failure -> Error "failed" | Error message -> Error message
let abort () = raise Failure
|ocaml} in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | _ :: _ ->
          Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
      | [] ->
          result.intf
          |> render_interface
          |> assert_equal_string
            ~expected:(
              "val sum : int -> int -> int\n"
              ^ "val concatenate : string -> string -> string\n"
              ^ "val values : int list\n"
              ^ "val total : int\n"
              ^ "val loop : int -> unit\n"
              ^ "val sequence : unit\n"
              ^ "val recovered : exn\n"
              ^ "val abort : unit -> 'a\n")

let test_infix_mismatch_has_operand_span _ctx =
  let source = "let _ = true + 1\n" in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | [ Typ.Diagnostics.Diagnostic.TypeMismatch { span; expected; actual } ]
        when span.start = 8 && span.end_ = 12 && String.equal expected "int" && String.equal actual "bool" ->
          Ok ()
      | _ ->
          Error
            (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
            |> String.concat "\n")

let test_raise_rejects_non_exception_values _ctx =
  let source = "let _ = raise 1\n" in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | [ Typ.Diagnostics.Diagnostic.TypeMismatch { span; expected; actual } ]
        when span.start = 14 && span.end_ = 15 && String.equal expected "exn" && String.equal actual "int" ->
          Ok ()
      | _ ->
          Error
            (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
            |> String.concat "\n")

let test_bidirectional_annotations_check_introductions _ctx =
  let source = {ocaml|
let identity : int -> int = fun value -> value
let pair : int * bool = (1, true)
let choose : bool -> int = fun condition -> if condition then 1 else 0
|ocaml} in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | [] ->
          render_interface result.intf
          |> assert_equal_string
            ~expected:(
              "val identity : int -> int\n"
              ^ "val pair : int * bool\n"
              ^ "val choose : bool -> int\n")
      | _ ->
          Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")

let test_bidirectional_function_body_mismatch_is_localized _ctx =
  let source = "let bad : int -> int = fun value -> true\n" in
  let span_start = 36 in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | [ Typ.Diagnostics.Diagnostic.TypeMismatch { span; expected; actual } ]
        when span.start = span_start
          && String.equal expected "int"
          && String.equal actual "bool" -> Ok ()
      | _ ->
          Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")

let test_value_restriction_keeps_reference_contents_monomorphic _ctx =
  let source =
    "let reference = ref (fun x -> x)\n"
    ^ "let _ = reference := (fun x -> x + 1)\n"
    ^ "let bad = (!reference) true\n"
  in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | [ Typ.Diagnostics.Diagnostic.TypeMismatch { span; _ } ]
        when span.start = 94 && span.end_ = 98 -> Ok ()
      | _ ->
          Error
            (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
            |> String.concat "\n")

let infer_source_diagnostics source =
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      Ok (result.diagnostics.items |> Vector.iter |> Iterator.to_list)

let infer_interface_diagnostics source =
  let parse_result = Syn.parse_interface (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      Ok (result.diagnostics.items |> Vector.iter |> Iterator.to_list)

let has_unresolved_name diagnostics ~namespace ~name =
  List.any diagnostics ~fn:(function
    | Typ.Diagnostics.Diagnostic.UnresolvedName
        { namespace = found_namespace; name = found_name; span } ->
        String.equal namespace found_namespace
        && String.equal name found_name
        && Int.(span.start < span.end_)
    | _ -> false)

let diagnostics_are_empty source =
  match infer_source_diagnostics source with
  | Error message -> Error message
  | Ok [] -> Ok ()
  | Ok diagnostics ->
      Error
        (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
        |> String.concat "\n")

let test_open_module_resolves_exported_members _ctx =
  diagnostics_are_empty {ocaml|
module Packet = struct
  type t = Item of int
  let count = 2
end
open Packet
let chosen : t = Item count
module Previous = struct let selected = 1 end
module Latest = struct let selected = true end
open Previous
open Latest
let shadowed : bool = selected
module OpenedAfterBinding = struct let precedence = true end
let precedence = 1
open OpenedAfterBinding
let opened_wins : bool = precedence
module OpenedBeforeBinding = struct let precedence = false end
open OpenedBeforeBinding
let precedence = 2
let local_wins : int = precedence
|ocaml}

let test_open_precedence_applies_to_type_and_constructor_namespaces _ctx =
  diagnostics_are_empty {ocaml|
type local = Pick of int
type t = Root
module M = struct type t = Pick of bool end
open M
let selected : t = Pick true
|ocaml}

let test_nested_module_signatures_and_aliases_typecheck _ctx =
  let source = {ocaml|
type outer = Outer
module Packed : sig
  type t = Local
  val value : t
end
module Alias = Packed
module TypeOf : module type of Packed
val selected : Alias.t
val also_selected : TypeOf.t
|ocaml} in
  match infer_interface_diagnostics source with
  | Error message -> Error message
  | Ok [] -> Ok ()
  | Ok diagnostics ->
      Error
        (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
        |> String.concat "\n")

let test_include_module_reexports_members _ctx =
  let* () =
    diagnostics_are_empty {ocaml|
module Source = struct
  type t = Item
  let value = Item
end
include Source
let selected : Source.t = value
|ocaml}
  in
  match infer_interface_diagnostics {ocaml|
module Source : sig
  type t = Item
  val value : t
end
include module type of Source
val selected : t
|ocaml} with
  | Error message -> Error message
  | Ok [] -> Ok ()
  | Ok diagnostics ->
      Error
        (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
        |> String.concat "\n")

let test_record_field_puns_use_the_last_identifier_segment _ctx =
  let* () =
    diagnostics_are_empty {ocaml|
module MapIter = struct
  type state = { items : int }
end
let items = 2
let qualified : MapIter.state = { MapIter.items }
|ocaml}
  in
  diagnostics_are_empty {ocaml|
module MapIter = struct
  type state = { items : int }
end
let items = 2
let unqualified : MapIter.state = { items }
|ocaml}

let test_record_update_checks_its_base_type _ctx =
  let source = "type point = { x : int }\nlet invalid : point = { 1 with x = 1 }\n" in
  match infer_source_diagnostics source with
  | Error message -> Error message
  | Ok [ Typ.Diagnostics.Diagnostic.TypeMismatch { span; _ } ]
    when Int.(span.start < span.end_) -> Ok ()
  | Ok diagnostics ->
      Error
        (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
        |> String.concat "\n")

let test_character_interval_patterns_check_character_inputs _ctx =
  let* () =
    diagnostics_are_empty
      "let is_lower = function 'a' .. 'z' -> true | _ -> false\n"
  in
  match infer_source_diagnostics
    "let invalid = match 1 with 'a' .. 'z' -> true | _ -> false\n"
  with
  | Error message -> Error message
  | Ok [ Typ.Diagnostics.Diagnostic.TypeMismatch { span; _ } ]
    when Int.(span.start < span.end_) -> Ok ()
  | Ok diagnostics ->
      Error
        (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
        |> String.concat "\\n")

let test_open_module_scope_does_not_export_imports _ctx =
  let source = {ocaml|
module Packet = struct
  let hidden = 1
end
module Use = struct
  open Packet
  let visible = hidden
end
let missing = hidden
|ocaml} in
  match infer_source_diagnostics source with
  | Error message -> Error message
  | Ok diagnostics ->
      if has_unresolved_name diagnostics ~namespace:"value" ~name:"hidden" then Ok ()
      else
        Error
          (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
          |> String.concat "\n")

let interface_has_type_arity_error source ~expected ~actual =
  let parse_result = Syn.parse_interface (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics = result.diagnostics.items |> Vector.iter |> Iterator.to_list in
      match diagnostics with
      | [ Typ.Diagnostics.Diagnostic.TypeConstructorArityMismatch
            { name; expected = found_expected; actual = found_actual; span } ]
        when String.equal name "wrapper"
          && Int.equal found_expected expected
          && Int.equal found_actual actual
          && Int.(span.start < span.end_) -> Ok ()
      | _ ->
          Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\n")

let test_type_constructor_arity_is_checked _ctx =
  let* () =
    interface_has_type_arity_error
      "type 'a wrapper = Wrap of 'a\nval value : (int, bool) wrapper\n"
      ~expected:1
      ~actual:2
  in
  interface_has_type_arity_error
    "type ('a, 'b) wrapper = Wrap of 'a * 'b\nval value : int wrapper\n"
    ~expected:2
    ~actual:1

let test_unresolved_names_are_errors_in_their_namespaces _ctx =
  let check source ~namespace ~name =
    match infer_source_diagnostics source with
    | Error message -> Error message
    | Ok diagnostics ->
        if has_unresolved_name diagnostics ~namespace ~name then
          Ok ()
        else
          Error
            (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
            |> String.concat "\n")
  in
  let* () = check "let value = missing_value\n" ~namespace:"value" ~name:"missing_value" in
  let* () = check "let value = Missing 1\n" ~namespace:"constructor" ~name:"Missing" in
  let* () = check "let value : missing_type = 1\n" ~namespace:"type" ~name:"missing_type" in
  let* () = check "open Missing\nlet value = 1\n" ~namespace:"module" ~name:"Missing" in
  check
    "type point = { x : int }\nlet value : point = { y = 1 }\n"
    ~namespace:"field"
    ~name:"y"

let test_generalization_does_not_quantify_an_environment_variable _ctx =
  let source = "let bad x = let id = x in (id 1, id true)\n" in
  let parse_result = Syn.parse_implementation (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error diagnostics ->
      Error (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string |> String.concat "\\n")
  | Ok ast ->
      let result = Typ.Infer.check ast in
      let diagnostics =
        result.diagnostics.items
        |> Vector.iter
        |> Iterator.to_list
      in
      match diagnostics with
      | [ Typ.Diagnostics.Diagnostic.TypeMismatch { span; _ } ]
        when span.start = 36 && span.end_ = 40 -> Ok ()
      | _ ->
          Error
            (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
            |> String.concat "\\n")

let tests =
  Test.[
    case "empty env renders empty interface" test_empty_env_renders_empty_interface;
    case "one type renders type" test_env_with_one_type_renders_type;
    case "one value renders value" test_env_with_one_value_renders_value;
    case "local values do not become exports" test_local_values_do_not_become_exports;
    case "multiple modules are copied recursively" test_multiple_modules_are_copied_recursively;
    case "signature exports typed declarations" test_infer_signature_exports_typed_declarations;
    case "nested modules and aliases expose typed members" test_infer_nested_modules_and_aliases;
    case "exception declarations provide constructor types" test_infer_exception_declarations;
    case "common expression forms infer their types" test_infer_common_expression_forms;
    case "infix mismatch points to the invalid operand" test_infix_mismatch_has_operand_span;
    case "raise rejects non-exception values" test_raise_rejects_non_exception_values;
    case "generalization preserves environment variables" test_generalization_does_not_quantify_an_environment_variable;
    case "unresolved names are errors by namespace" test_unresolved_names_are_errors_in_their_namespaces;
    case "open modules resolve exported members" test_open_module_resolves_exported_members;
    case "nested module signatures and aliases typecheck" test_nested_module_signatures_and_aliases_typecheck;
    case "include re-exports module members" test_include_module_reexports_members;
    case "open precedence applies to type and constructor namespaces" test_open_precedence_applies_to_type_and_constructor_namespaces;
    case "record field puns lower their value identifier" test_record_field_puns_use_the_last_identifier_segment;
    case "record update checks its base type" test_record_update_checks_its_base_type;
    case "character intervals check character inputs" test_character_interval_patterns_check_character_inputs;
    case "open imports are not module exports" test_open_module_scope_does_not_export_imports;
    case "type constructor arity is checked" test_type_constructor_arity_is_checked;
    case "value restriction keeps reference contents monomorphic" test_value_restriction_keeps_reference_contents_monomorphic;
    case "bidirectional annotations check introduction forms" test_bidirectional_annotations_check_introductions;
    case "bidirectional function mismatch is source-located" test_bidirectional_function_body_mismatch_is_localized;
  ]

let main ~args = Test.Cli.main ~name:"typ:infer-module-interface" ~tests ~args ()

let () = Runtime.run ~main ~args:Std.Env.args ()
