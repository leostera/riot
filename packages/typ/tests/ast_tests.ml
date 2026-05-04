open Std

module Ast = Typ.Ast
module Type = Typ.Ast.Type
module TypeVar = Typ.Ast.TypeVar
module SurfacePath = Typ.Model.Surface_path

let mk_path name =
  Syn.parse_ident name
  |> Option.map ~fn:SurfacePath.from_syn_ident
  |> Option.expect ~msg:("expected surface path test identifier " ^ name)

let int_type = fun () -> Type.Apply { ident = mk_path "int"; arguments = [] }

let bool_type = fun () -> Type.Apply { ident = mk_path "bool"; arguments = [] }

let list_type argument = Type.Apply { ident = mk_path "list"; arguments = [ argument ] }

let arrow ?(label = Type.Label.NoLabel) parameter result = Type.Arrow { label; parameter; result }

let assert_equal_type left right =
  if Type.equal left right then
    Ok ()
  else
    Error ("expected " ^ Type.to_string left ^ " to equal " ^ Type.to_string right)

let assert_not_equal_type left right =
  if Type.equal left right then
    Error ("expected " ^ Type.to_string left ^ " not to equal " ^ Type.to_string right)
  else
    Ok ()

let variable ?link id = Type.Var { id; link }

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create typ ast test source slice"

let parse_typ_ast = fun source ->
  let parse_result = Syn.parse ~filename:(Path.v "test.ml") (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  Ast.from_parse_result ~source:model_source parse_result
  |> Result.expect ~msg:"expected typ ast build"

let assert_path_string ~expected actual =
  Test.assert_equal ~expected ~actual:(SurfacePath.to_string actual);
  Ok ()

let test_type_to_string_preserves_nested_tuple_grouping _ctx =
  let type_ = Type.Tuple [ Type.Tuple [ int_type (); int_type () ]; bool_type () ] in
  Test.assert_equal ~expected:"(int * int) * bool" ~actual:(Type.to_string type_);
  Ok ()

let test_type_printer_reuses_variable_names_across_calls _ctx =
  let printer = Type.Printer.create () in
  let first = variable TypeVar.first in
  let second = variable (TypeVar.next TypeVar.first) in
  Test.assert_equal ~expected:"'a" ~actual:(Type.Printer.to_string printer first);
  Test.assert_equal ~expected:"'b" ~actual:(Type.Printer.to_string printer second);
  Test.assert_equal ~expected:"'a" ~actual:(Type.Printer.to_string printer first);
  Ok ()

let test_equal_follows_linked_variables _ctx =
  let linked = variable ~link:(int_type ()) TypeVar.first in
  assert_equal_type linked (int_type ())

let test_equal_compares_unlinked_variable_ids _ctx =
  let first = variable TypeVar.first in
  let same_first = variable TypeVar.first in
  let second = variable (TypeVar.next TypeVar.first) in
  match assert_equal_type first same_first with
  | Error _ as error -> error
  | Ok () -> assert_not_equal_type first second

let test_equal_tuple_arity_mismatch_is_false _ctx =
  let one = Type.Tuple [ int_type () ] in
  let two = Type.Tuple [ int_type (); bool_type () ] in
  assert_not_equal_type one two

let test_equal_application_arguments_are_structural _ctx =
  let int_list = list_type (int_type ()) in
  let same_int_list = list_type (int_type ()) in
  let bool_list = list_type (bool_type ()) in
  match assert_equal_type int_list same_int_list with
  | Error _ as error -> error
  | Ok () -> assert_not_equal_type int_list bool_list

let test_equal_arrows_compare_labels_and_children _ctx =
  let unlabeled = arrow (int_type ()) (bool_type ()) in
  let same_unlabeled = arrow (int_type ()) (bool_type ()) in
  let labeled = arrow ~label:(Type.Label.Labelled "value") (int_type ()) (bool_type ()) in
  match assert_equal_type unlabeled same_unlabeled with
  | Error _ as error -> error
  | Ok () -> assert_not_equal_type unlabeled labeled

let test_from_syn_keeps_constructor_patterns _ctx =
  let ast = parse_typ_ast {ocaml|let value = function
  | Some x -> x
  | None -> 0
|ocaml}
  in
  match ast with
  | Implementation {
      items = [
          {
            kind =
              Let {
                bindings = [
                    {
                      expr = {
                        kind = Function { body = Cases [ some_case; none_case ]; _ };
                        _;
                      };
                      _;
                    };
                ];
                _;
              };
            _;
          };
      ];
      _;
    } ->
      let some_result =
        match some_case.pattern.kind with
        | Constructor { ident = constructor; payload = Some { kind = Bind binding; _ } } -> (
            match assert_path_string ~expected:"Some" constructor with
            | Error _ as error -> error
            | Ok () -> assert_path_string ~expected:"x" binding
          )
        | _ -> Error "expected Some x to lower as constructor pattern with payload"
      in
      (
        match some_result with
        | Error _ as error -> error
        | Ok () -> (
            match none_case.pattern.kind with
            | Constructor { ident; payload = None } -> assert_path_string ~expected:"None" ident
            | _ -> Error "expected None to lower as constructor pattern"
          )
      )
  | _ -> Error "expected function with Some and None cases"

let test_from_syn_keeps_constructor_expression_payloads _ctx =
  let ast = parse_typ_ast {ocaml|let value = Some 1|ocaml} in
  match ast with
  | Implementation {
      items = [
          {
            kind =
              Let {
                bindings = [
                    {
                      expr = {
                        kind = Constructor { ident; payload = Some { kind = Literal Int; _ } };
                        _;
                      };
                      _;
                    };
                ];
                _;
              };
            _;
          };
      ];
      _;
    } ->
      assert_path_string ~expected:"Some" ident
  | _ -> Error "expected Some 1 to lower as constructor expression with payload"

let test_fuzz_parse_lower_and_infer = fun _ctx source ->
  let parse_result = Syn.parse ~filename:(Path.v "fuzz.ml") (source_slice source) in
  let model_source = Typ.Model.Source.make ~text:source in
  match Ast.from_parse_result ~source:model_source parse_result with
  | Error _diagnostics -> Ok ()
  | Ok ast ->
      let _infer_result = Typ.Infer.check ast in
      Ok ()

let record_pattern_empty_ident_recovery_seed =
  "let project value =\n" ^ "  match value with\n" ^ "  | { x; _\030 } -> x\n"

let path_expression_empty_ident_recovery_seed = "let value = (->)\n"

let first_class_module_empty_type_recovery_seed = "let packed = (module Seed : )\n"

let first_class_module_empty_name_recovery_seed = "let packed = (module : BOX)\n"

let package_constraint_empty_name_recovery_seed =
  "let packed = (module Seed : BOX with type = char)\n"

let value_declaration_empty_name_recovery_seed =
  "module type BOX = sig\n" ^ "  val begin : t\n" ^ "end\n"

let typ_fuzz_fixture_root = Path.v "packages/typ/tests/fixtures/corpus"

let typ_source_dictionary = [
  "let";
  "let rec";
  "and";
  "in";
  "fun";
  "function";
  "match";
  "with";
  "when";
  "if";
  "then";
  "else";
  "type";
  "of";
  "as";
  "module";
  "module type";
  "struct";
  "sig";
  "end";
  "open";
  "include";
  "val";
  "external";
  "exception";
  "try";
  "raise";
  "while";
  "for";
  "to";
  "downto";
  "do";
  "done";
  "begin";
  "true";
  "false";
  "None";
  "Some";
  "Ok";
  "Error";
  "int";
  "bool";
  "string";
  "char";
  "float";
  "unit";
  "list";
  "option";
  "result";
  "array";
  "ref";
  "->";
  "=>";
  "|";
  "::";
  ":";
  ":>";
  ":=";
  "=";
  "==";
  "<>";
  "+";
  "-";
  "*";
  "/";
  "&&";
  "||";
  "(";
  ")";
  "[";
  "]";
  "[|";
  "|]";
  "{";
  "}";
  "`";
  "'";
  "\"";
  ";;";
  ".";
  ",";
  ";";
  "_";
  "?";
  "~";
  "@";
  "let value = 1\n";
  "let id x = x\n";
  "type t = A | B\n";
  "type 'a box = Box of 'a\n";
  "let value : int option = Some 1\n";
  "let choose ~left ?right () = left\n";
  "module M = struct let value = true end\n";
  "module type S = sig val value : int end\n";
  "match value with | Some x -> x | None -> 0\n";
]

let typ_source_mutator =
  Test.Fuzz.Mutator.(text
  |> with_dictionary typ_source_dictionary
  |> with_max_len 8_192)

let tests =
  Test.[
    case
      "type to string preserves nested tuple grouping"
      test_type_to_string_preserves_nested_tuple_grouping;
    case
      "type printer reuses variable names across calls"
      test_type_printer_reuses_variable_names_across_calls;
    case "type equal follows linked variables" test_equal_follows_linked_variables;
    case "type equal compares unlinked variable ids" test_equal_compares_unlinked_variable_ids;
    case "type equal tuple arity mismatch is false" test_equal_tuple_arity_mismatch_is_false;
    case
      "type equal application arguments are structural"
      test_equal_application_arguments_are_structural;
    case
      "type equal arrows compare labels and children"
      test_equal_arrows_compare_labels_and_children;
    case "from syn keeps constructor patterns" test_from_syn_keeps_constructor_patterns;
    case
      "from syn keeps constructor expression payloads"
      test_from_syn_keeps_constructor_expression_payloads;
    fuzz
      "parse lower and infer arbitrary implementation input"
      ~seeds:[
        "";
        "let value = 1\n";
        "let id x = x\n";
        "type t = A | B\nlet value = A\n";
        "module M = struct let value = true end\n";
        record_pattern_empty_ident_recovery_seed;
        path_expression_empty_ident_recovery_seed;
        first_class_module_empty_type_recovery_seed;
        first_class_module_empty_name_recovery_seed;
        package_constraint_empty_name_recovery_seed;
        value_declaration_empty_name_recovery_seed;
      ]
      ~corpus:(Test.Fuzz.Corpus.dir typ_fuzz_fixture_root ~extensions:[ ".ml"; ".mli"; ])
      ~mutator:typ_source_mutator
      test_fuzz_parse_lower_and_infer;
  ]

let main ~args = Test.Cli.main ~name:"typ:ast" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
