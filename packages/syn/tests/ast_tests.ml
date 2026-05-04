open Std
open Std.Collections
open Syn

module Ast = Syn.Ast
module Iterator = Iter.Iterator

let ( let* ) = fun result fn -> Result.and_then result ~fn

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create source slice: " ^ IO.IoVec.error_message error)

let diagnostics_to_string = fun diagnostics ->
  let items = ref [] in
  Vector.iter diagnostics
  |> Iterator.for_each ~fn:(fun diagnostic -> items := Diagnostic.to_string diagnostic :: !items);
  List.reverse !items
  |> String.concat "\n"

let expect_some = fun value ~msg ->
  match value with
  | Some value -> Ok value
  | None -> Error msg

let require_some = fun value ~msg ->
  expect_some value ~msg
  |> Result.expect ~msg

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Ast.Continue ())

let parse_root = fun ~filename source ->
  let source = source_slice source in
  let parse_result = Syn.parse ~filename source in
  if Vector.length parse_result.Parser.diagnostics > 0 then
    Error ("unexpected parse diagnostics:\n" ^ diagnostics_to_string parse_result.Parser.diagnostics)
  else
    Ok (Ast.SourceFile.make parse_result.Parser.tree)

let parse_tree = fun ~filename source ->
  let source = source_slice source in
  let parse_result = Syn.parse ~filename source in
  if Vector.length parse_result.Parser.diagnostics > 0 then
    Error ("unexpected parse diagnostics:\n" ^ diagnostics_to_string parse_result.Parser.diagnostics)
  else
    Ok parse_result.Parser.tree

let parse_ml = parse_root ~filename:(Path.v "sample.ml")

let parse_mli = parse_root ~filename:(Path.v "sample.mli")

let parse_ml_tree = parse_tree ~filename:(Path.v "sample.ml")

let test_class_subset_words_are_not_keywords = fun _ctx ->
  let words = [ "class"; "object"; "method"; "new"; "virtual"; "inherit"; "initializer" ] in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | word :: rest -> (
        match Syn.Keyword.from_string word with
        | None -> loop rest
        | Some _ -> Error (word ^ " should lex as an identifier")
      )
  in
  loop words

let test_empty_source_files_have_file_kind = fun _ctx ->
  let* implementation = parse_ml "" in
  let* interface = parse_mli "" in
  (
    match Ast.SourceFile.view implementation with
    | Ast.SourceFile.Implementation _ -> ()
    | Ast.SourceFile.Interface _ -> panic "expected empty implementation source"
  );
  match Ast.SourceFile.view interface with
  | Ast.SourceFile.Interface _ -> Ok ()
  | Ast.SourceFile.Implementation _ -> Error "expected empty interface source"

let assert_parse_ident_text = fun source expected ->
  match Syn.parse_ident source with
  | Some ident when String.equal (Ast.Ident.text ident) expected -> Ok ()
  | Some ident -> Error ("expected ident " ^ expected ^ " but found " ^ Ast.Ident.text ident)
  | None -> Error ("expected ident " ^ expected)

let assert_parse_ident_none = fun source ->
  match Syn.parse_ident source with
  | None -> Ok ()
  | Some ident -> Error ("expected no ident but found " ^ Ast.Ident.text ident)

let test_parse_ident = fun _ctx ->
  let* () = assert_parse_ident_text "value" "value" in
  let* () = assert_parse_ident_text "Some" "Some" in
  let* () = assert_parse_ident_text "int" "int" in
  let* () = assert_parse_ident_text "bool" "bool" in
  let* () = assert_parse_ident_text "float" "float" in
  let* () = assert_parse_ident_text "char" "char" in
  let* () = assert_parse_ident_text "string" "string" in
  let* () = assert_parse_ident_text "list" "list" in
  let* () = assert_parse_ident_text "unit" "unit" in
  let* () = assert_parse_ident_none "Some 1" in
  assert_parse_ident_none "let value = 1"

let nth_structure_item = fun (root: Ast.source_file) target ->
  let (found, _) =
    Ast.SourceFile.fold_structure_item
      root
      ~init:(None, 0)
      ~fn:(fun item (found, seen) ->
        match found with
        | Some _ -> Ast.Return (found, seen)
        | None ->
            if Int.equal seen target then
              Ast.Return (Some item, seen)
            else
              Ast.Continue (None, seen + 1))
  in
  found

let nth_signature_item = fun (root: Ast.source_file) target ->
  let (found, _) =
    Ast.SourceFile.fold_signature_item
      root
      ~init:(None, 0)
      ~fn:(fun item (found, seen) ->
        match found with
        | Some _ -> Ast.Return (found, seen)
        | None ->
            if Int.equal seen target then
              Ast.Return (Some item, seen)
            else
              Ast.Continue (None, seen + 1))
  in
  found

let binding_of_structure_item = fun item ->
  match Ast.StructureItem.view item with
  | Ast.StructureItem.Let decl ->
      Ast.LetDeclaration.first_binding decl
      |> expect_some ~msg:"expected first let binding"
  | _ -> Error "expected let structure item"

let body_of_binding = fun binding ->
  Ast.LetBinding.body binding
  |> expect_some ~msg:"expected let binding body"

let pattern_of_binding = fun binding ->
  Ast.LetBinding.pattern binding
  |> expect_some ~msg:"expected let binding pattern"

let assert_last_ident_text = fun ident expected ->
  let token =
    Ast.Ident.last_segment ident
    |> require_some ~msg:"expected last ident segment"
  in
  Test.assert_equal ~expected ~actual:(Ast.Token.text token)

let assert_type_ident_last_segment = fun type_expr expected ->
  match Ast.TypeExpr.view type_expr with
  | Ast.TypeExpr.Ident { ident } -> assert_last_ident_text ident expected
  | _ -> panic "expected ident type"

let vector_to_list = fun vector ->
  Vector.to_array vector
  |> Array.to_list

let first_syntax_node = fun tree kind ->
  let found = ref None in
  let rec visit node =
    match !found with
    | Some _ -> ()
    | None ->
        if SyntaxKind.is node.SyntaxTree.kind kind then
          found := Some node
        else
          SyntaxTree.for_each_child
            tree
            node
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | SyntaxTree.Node id -> visit (SyntaxTree.node tree id)
              | SyntaxTree.Token _
              | SyntaxTree.Missing _ -> ())
  in
  visit (SyntaxTree.root tree);
  !found

let direct_syntax_child_node_kinds = fun tree node ->
  let kinds = Vector.with_capacity ~size:node.SyntaxTree.child_count in
  SyntaxTree.for_each_child
    tree
    node
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | SyntaxTree.Node id ->
          let child = SyntaxTree.node tree id in
          Vector.push kinds ~value:child.SyntaxTree.kind
      | SyntaxTree.Token _
      | SyntaxTree.Missing _ -> ());
  vector_to_list kinds

let vector_first = fun vector ~msg ->
  if Int.equal (Vector.length vector) 0 then
    panic msg
  else
    Vector.get_unchecked vector ~at:0

let vector_second = fun vector ~msg ->
  if Vector.length vector < 2 then
    panic msg
  else
    Vector.get_unchecked vector ~at:1

let first_child_expr = fun node ->
  Ast.Node.fold_child_node
    node
    ~init:None
    ~fn:(fun child _ ->
      match Ast.cast_result_to_option (Ast.Expr.cast child) with
      | Some expr -> Ast.Return (Some expr)
      | None -> Ast.Continue None)

let assert_labeled_argument = fun arg expected ->
  let arg_node = Ast.Expr.as_node arg in
  if not (SyntaxKind.is (Ast.Node.kind arg_node) SyntaxKind.LABELED_ARG) then
    panic ("expected labeled argument " ^ expected);
  let label =
    Ast.Node.first_child_token arg_node ~kind:SyntaxKind.IDENT
    |> require_some ~msg:"expected labeled argument label"
  in
  Test.assert_equal ~expected ~actual:(Ast.Token.text label);
  first_child_expr arg_node

type visitor_capture = {
  let_names: string Vector.t;
  token_texts: string Vector.t;
  leave_count: int;
}

let test_source_file_and_let_binding_views = fun _ctx ->
  let root =
    parse_ml "let x = 1\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  (
    match Ast.SourceFile.view root with
    | Ast.SourceFile.Implementation _ -> ()
    | _ -> panic "expected implementation root"
  );
  let item =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
  in
  let binding =
    binding_of_structure_item item
    |> Result.expect ~msg:"expected let binding"
  in
  let pattern =
    pattern_of_binding binding
    |> Result.expect ~msg:"expected binding pattern"
  in
  (
    match Ast.Pattern.view pattern with
    | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident "x"
    | _ -> panic "expected ident pattern"
  );
  let body =
    body_of_binding binding
    |> Result.expect ~msg:"expected binding body"
  in
  (
    match Ast.Expr.view body with
    | Ast.Expr.Literal { token } ->
        Test.assert_equal ~expected:"1" ~actual:(Ast.Token.text token);
        Ok ()
    | _ -> Error "expected literal expression body"
  )

let test_local_let_type_identifier_after_typed_rec_binding = fun _ctx ->
  let root =
    parse_ml
      {ocaml|let rec type_expression (state: State.t) (expr: expression) =
let type_ = ok in 1
|ocaml}
    |> Result.expect ~msg:"expected typed recursive binding with local type_ let to parse"
  in
  let outer_binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected outer let item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected outer let binding"
  in
  let body =
    body_of_binding outer_binding
    |> Result.expect ~msg:"expected outer let body"
  in
  match Ast.Expr.view body with
  | Ast.Expr.Let { first_binding; body } ->
      let inner_pattern =
        pattern_of_binding first_binding
        |> Result.expect ~msg:"expected inner let pattern"
      in
      (
        match Ast.Pattern.view inner_pattern with
        | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident "type_"
        | _ -> panic "expected inner let identifier pattern"
      );
      (
        match Ast.Expr.view body with
        | Ast.Expr.Literal { token } ->
            Test.assert_equal ~expected:"1" ~actual:(Ast.Token.text token);
            Ok ()
        | _ -> Error "expected inner let body literal"
      )
  | _ -> Error "expected outer binding body to be local let expression"

let test_token_leading_docstring_trivia_parts = fun _ctx ->
  let root =
    parse_mli {ocaml|(** hello *)
val x:int
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let item =
    nth_signature_item root 0
    |> require_some ~msg:"expected value item"
  in
  let token =
    Ast.Node.first_descendant_token (Ast.SignatureItem.as_node item)
    |> require_some ~msg:"expected value token"
  in
  let docstring = ref None in
  iter_fold
    Ast.Token.fold_leading_trivia_item
    token
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Ast.Token.Docstring doc -> docstring := Some doc
      | Ast.Token.Comment _
      | Ast.Token.Whitespace -> ());
  match !docstring with
  | Some doc ->
      Test.assert_equal ~expected:"(** hello *)" ~actual:doc.text;
      Test.assert_equal ~expected:"(**" ~actual:doc.opening;
      Test.assert_equal ~expected:" hello " ~actual:doc.content;
      Test.assert_equal ~expected:(Some "*)") ~actual:doc.closing;
      Ok ()
  | None -> Error "expected leading docstring trivia"

let test_node_span_excludes_leading_trivia = fun _ctx ->
  let source = {ocaml|

let x = 1
  type t = int
|ocaml}
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let item =
    nth_structure_item root 1
    |> require_some ~msg:"expected second structure item"
  in
  let span = Ast.Node.span (Ast.StructureItem.as_node item) in
  let start = span.start in
  let end_ = span.end_ in
  let text = String.sub source ~offset:start ~len:(end_ - start) in
  Test.assert_equal ~expected:"type t = int" ~actual:text;
  Ok ()

let test_expression_views = fun _ctx ->
  let source = "let x = if ready then 1 else 2\nlet y = match x with | 0 -> 1 | _ -> 2\n" in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let if_item =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
  in
  let if_body =
    if_item
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected first let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected if body"
  in
  (
    match Ast.Expr.view if_body with
    | Ast.Expr.If { condition; then_branch; else_branch } ->
        ignore condition;
        ignore then_branch;
        ignore
          (
            else_branch
            |> expect_some ~msg:"expected else branch"
            |> Result.expect ~msg:"else branch"
          )
    | _ -> panic "expected if expression"
  );
  let match_item =
    nth_structure_item root 1
    |> require_some ~msg:"expected second structure item"
  in
  let match_body =
    match_item
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected second let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected match body"
  in
  match Ast.Expr.view match_body with
  | Ast.Expr.Match { scrutinee; first_case } ->
      ignore scrutinee;
      (
        match Ast.MatchCase.view first_case with
        | Ast.MatchCase.Case { pattern; guard = None; body } ->
            ignore pattern;
            ignore body;
            Ok ()
        | Ast.MatchCase.Case { guard = Some _; _ } ->
            Error "expected first match case without guard"
        | Ast.MatchCase.Unknown _ -> Error "expected complete first match case"
      )
  | _ -> Error "expected match expression"

let test_expression_constructor_views = fun _ctx ->
  let root =
    parse_ml
      {ocaml|let none = None
let some = Some 1
let ok = Result.Ok 1
let hello = Hello ("world", "yeah!")
let goodbye = Goodbye { moon = "men" }
let lower = value
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let body index =
    nth_structure_item root index
    |> require_some ~msg:"expected structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected let binding body"
  in
  (
    match Ast.Expr.view (body 0) with
    | Ast.Expr.Constructor { constructor; payload = None } ->
        assert_last_ident_text constructor "None"
    | _ -> panic "expected None constructor expression"
  );
  (
    match Ast.Expr.view (body 1) with
    | Ast.Expr.Constructor { constructor; payload = Some payload } ->
        assert_last_ident_text constructor "Some";
        (
          match Ast.Expr.view payload with
          | Ast.Expr.Literal { token } ->
              Test.assert_equal ~expected:"1" ~actual:(Ast.Token.text token)
          | _ -> panic "expected Some argument literal"
        )
    | _ -> panic "expected Some constructor application"
  );
  (
    match Ast.Expr.view (body 2) with
    | Ast.Expr.Constructor { constructor; payload = Some _ } ->
        assert_last_ident_text constructor "Ok"
    | _ -> panic "expected qualified constructor application"
  );
  (
    match Ast.Expr.view (body 3) with
    | Ast.Expr.Constructor { constructor; payload = Some payload } ->
        assert_last_ident_text constructor "Hello";
        (
          match Ast.Expr.view payload with
          | Ast.Expr.Tuple { items } -> Test.assert_equal ~expected:2 ~actual:(Vector.length items)
          | _ -> panic "expected tuple constructor payload"
        )
    | _ -> panic "expected tuple-payload constructor application"
  );
  (
    match Ast.Expr.view (body 4) with
    | Ast.Expr.Constructor { constructor; payload = Some payload } ->
        assert_last_ident_text constructor "Goodbye";
        (
          match Ast.Expr.view payload with
          | Ast.Expr.Record { base = None; fields } -> (
              Test.assert_equal ~expected:1 ~actual:(Vector.length fields);
              match Vector.get_unchecked fields ~at:0 with
              | Ast.RecordExprField { ident; value = Some value; _ } ->
                  assert_last_ident_text ident "moon";
                  (
                    match Ast.Expr.view value with
                    | Ast.Expr.Literal { token } ->
                        Test.assert_equal ~expected:"\"men\"" ~actual:(Ast.Token.text token)
                    | _ -> panic "expected record constructor payload literal"
                  )
              | Ast.RecordExprField { value = None; _ } ->
                  panic "expected record constructor payload field value"
              | Ast.UnknownRecordExprField _ ->
                  panic "expected known record constructor payload field"
            )
          | _ -> panic "expected record constructor payload"
        )
    | _ -> panic "expected record-payload constructor application"
  );
  match Ast.Expr.view (body 5) with
  | Ast.Expr.Ident { ident } ->
      assert_last_ident_text ident "value";
      Ok ()
  | _ -> Error "expected lowercase identifier expression"

let test_expression_field_access_splits_value_paths = fun _ctx ->
  let root =
    parse_ml
      {ocaml|let module_value = Std.List.map
let module_value_field = Hello.record.field
let field_chain = obj.field1.field2
let qualified_field = point.Point.y
let mixed_field = hello.Module.field
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let body index =
    nth_structure_item root index
    |> require_some ~msg:"expected structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected let binding body"
  in
  (
    match Ast.Expr.view (body 0) with
    | Ast.Expr.Ident { ident } ->
        Test.assert_equal ~expected:"Std.List.map" ~actual:(Ast.Ident.text ident)
    | _ -> panic "expected qualified value path identifier"
  );
  (
    match Ast.Expr.view (body 1) with
    | Ast.Expr.FieldAccess { target; field } ->
        Test.assert_equal ~expected:"field" ~actual:(Ast.Ident.text field);
        (
          match Ast.Expr.view target with
          | Ast.Expr.Ident { ident } ->
              Test.assert_equal ~expected:"Hello.record" ~actual:(Ast.Ident.text ident)
          | _ -> panic "expected qualified target value path"
        )
    | _ -> panic "expected qualified value field access"
  );
  match Ast.Expr.view (body 2) with
  | Ast.Expr.FieldAccess { target; field } ->
      Test.assert_equal ~expected:"field2" ~actual:(Ast.Ident.text field);
      (
        match Ast.Expr.view target with
        | Ast.Expr.FieldAccess { target; field } ->
            Test.assert_equal ~expected:"field1" ~actual:(Ast.Ident.text field);
            (
              match Ast.Expr.view target with
              | Ast.Expr.Ident { ident } ->
                  Test.assert_equal ~expected:"obj" ~actual:(Ast.Ident.text ident)
              | _ -> panic "expected field-chain root identifier"
            )
        | _ -> panic "expected nested field access target"
      );
      (
        match Ast.Expr.view (body 3) with
        | Ast.Expr.FieldAccess { target; field } ->
            Test.assert_equal ~expected:"Point.y" ~actual:(Ast.Ident.text field);
            (
              match Ast.Expr.view target with
              | Ast.Expr.Ident { ident } ->
                  Test.assert_equal ~expected:"point" ~actual:(Ast.Ident.text ident)
              | _ -> panic "expected qualified field root identifier"
            )
        | _ -> panic "expected qualified record field access"
      );
      (
        match Ast.Expr.view (body 4) with
        | Ast.Expr.FieldAccess { target; field } ->
            Test.assert_equal ~expected:"Module.field" ~actual:(Ast.Ident.text field);
            (
              match Ast.Expr.view target with
              | Ast.Expr.Ident { ident } ->
                  Test.assert_equal ~expected:"hello" ~actual:(Ast.Ident.text ident)
              | _ -> panic "expected mixed field root identifier"
            )
        | _ -> panic "expected mixed qualified record field access"
      );
      Ok ()
  | _ -> Error "expected lowercase field access chain"

let test_expression_poly_variant_payload_keeps_field_access = fun _ctx ->
  let root =
    parse_ml {ocaml|let event = `Paste state.paste_buffer
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let body =
    nth_structure_item root 0
    |> require_some ~msg:"expected structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected let binding body"
  in
  match Ast.Expr.view body with
  | Ast.Expr.PolyVariant { tag; payload = Some payload } ->
      Test.assert_equal ~expected:"Paste" ~actual:(Ast.Token.text tag);
      (
        match Ast.Expr.view payload with
        | Ast.Expr.FieldAccess { target; field } ->
            Test.assert_equal ~expected:"paste_buffer" ~actual:(Ast.Ident.text field);
            (
              match Ast.Expr.view target with
              | Ast.Expr.Ident { ident } ->
                  Test.assert_equal ~expected:"state" ~actual:(Ast.Ident.text ident)
              | _ -> panic "expected field payload target identifier"
            )
        | _ -> panic "expected polymorphic variant payload field access"
      );
      Ok ()
  | _ -> Error "expected polymorphic variant payload"

let test_assert_and_lazy_parse_as_regular_applications = fun _ctx ->
  let root =
    parse_ml {ocaml|let checked = assert ready
let delayed = lazy compute
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let body index =
    nth_structure_item root index
    |> require_some ~msg:"expected structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected let binding body"
  in
  let assert_application index ~callee_text ~argument_text =
    match Ast.Expr.view (body index) with
    | Ast.Expr.Apply { callee; argument } ->
        (
          match Ast.Expr.view callee with
          | Ast.Expr.Ident { ident } ->
              Test.assert_equal ~expected:callee_text ~actual:(Ast.Ident.text ident)
          | _ -> panic "expected keyword-form callee identifier"
        );
        (
          match Ast.Expr.view argument with
          | Ast.Expr.Ident { ident } ->
              Test.assert_equal ~expected:argument_text ~actual:(Ast.Ident.text ident)
          | _ -> panic "expected keyword-form operand identifier"
        )
    | _ -> panic "expected keyword form to use regular apply view"
  in
  assert_application 0 ~callee_text:"assert" ~argument_text:"ready";
  assert_application 1 ~callee_text:"lazy" ~argument_text:"compute";
  Ok ()

let test_assignment_operator_views = fun _ctx ->
  let root =
    parse_ml "let update state remaining = state.buffer <- remaining\nlet assign r = r := 1\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let assignment_body index =
    nth_structure_item root index
    |> require_some ~msg:"expected assignment structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected assignment binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected assignment body"
  in
  let assert_assignment_operator body expected =
    match Ast.Expr.view body with
    | Ast.Expr.Assign { operator; _ } ->
        Test.assert_equal ~expected ~actual:(Ast.Token.text operator)
    | _ -> panic ("expected assignment operator " ^ expected)
  in
  assert_assignment_operator (assignment_body 0) "<-";
  assert_assignment_operator (assignment_body 1) ":=";
  Ok ()

let test_labeled_application_after_poly_variant_argument = fun _ctx ->
  let source =
    "let parse_interface ~source tokens = parse ~cst_kind:`Interface \
     ~parse_item:parse_signature_item ~source ~tokens\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let item =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
  in
  let body =
    item
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected binding body"
  in
  let rec application_parts expr args =
    match Ast.Expr.view expr with
    | Ast.Expr.Apply { callee; argument } -> application_parts callee (argument :: args)
    | _ -> (expr, args)
  in
  let (callee, arguments) = application_parts body [] in
  (
    match Ast.Expr.view callee with
    | Ast.Expr.Ident { ident } -> assert_last_ident_text ident "parse"
    | _ -> panic "expected parse callee"
  );
  match arguments with
  | [ cst_kind; parse_item; source; tokens ] ->
      let cst_kind_value = assert_labeled_argument cst_kind "cst_kind" in
      (
        match cst_kind_value
        |> require_some ~msg:"expected cst_kind value"
        |> Ast.Expr.view with
        | Ast.Expr.PolyVariant { payload = None } -> ()
        | _ -> panic "expected cst_kind value to be a bare polymorphic variant"
      );
      ignore (assert_labeled_argument parse_item "parse_item");
      ignore (assert_labeled_argument source "source");
      ignore (assert_labeled_argument tokens "tokens");
      Ok ()
  | _ -> Error "expected parse application to have four labeled arguments"

let test_pattern_views = fun _ctx ->
  let source = "let (a, b) = xs\nlet h :: t = xs\n" in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let tuple_pattern =
    nth_structure_item root 0
    |> require_some ~msg:"expected tuple pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected tuple pattern binding"
    |> pattern_of_binding
    |> Result.expect ~msg:"expected tuple pattern"
  in
  (
    match Ast.Pattern.view tuple_pattern with
    | Ast.Pattern.Tuple { parts } -> Test.assert_equal ~expected:2 ~actual:(Vector.length parts)
    | _ -> panic "expected tuple pattern"
  );
  let cons_pattern =
    nth_structure_item root 1
    |> require_some ~msg:"expected cons pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected cons pattern binding"
    |> pattern_of_binding
    |> Result.expect ~msg:"expected cons pattern"
  in
  match Ast.Pattern.view cons_pattern with
  | Ast.Pattern.Cons { head; tail } ->
      ignore head;
      ignore tail;
      Ok ()
  | _ -> Error "expected cons pattern"

let test_poly_variant_tuple_pattern_boundary = fun _ctx ->
  let source =
    {ocaml|let equal_constraint = fun left right ->
  match left, right with
  | `Decided left, `Decided right -> left
|ocaml}
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let item =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
  in
  let body =
    item
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected binding body"
  in
  let match_body =
    match Ast.Expr.view body with
    | Ast.Expr.Fun { body = Ast.Expr.Body_expr body; _ } -> body
    | _ -> panic "expected function body"
  in
  match Ast.Expr.view match_body with
  | Ast.Expr.Match { first_case; _ } ->
      let pattern =
        match Ast.MatchCase.view first_case with
        | Ast.MatchCase.Case { pattern; _ } -> pattern
        | Ast.MatchCase.Unknown _ -> panic "expected first match case pattern"
      in
      (
        match Ast.Pattern.view pattern with
        | Ast.Pattern.Tuple { parts } -> Test.assert_equal ~expected:2 ~actual:(Vector.length parts)
        | _ -> panic "expected polyvariant pair case to parse as a tuple pattern"
      );
      let children = Vector.with_capacity ~size:(Ast.Pattern.child_pattern_count pattern) in
      iter_fold
        Ast.Pattern.fold_child_pattern
        pattern
        ~fn:(fun child -> Vector.push children ~value:child);
      Test.assert_equal ~expected:2 ~actual:(Vector.length children);
      (
        match (
          Ast.Pattern.view (Vector.get_unchecked children ~at:0),
          Ast.Pattern.view (Vector.get_unchecked children ~at:1)
        ) with
        | (Ast.Pattern.PolyVariant _, Ast.Pattern.PolyVariant _) -> Ok ()
        | _ -> Error "expected both tuple items to be polymorphic variant patterns"
      )
  | _ -> Error "expected match expression"

let test_signature_and_type_views = fun _ctx ->
  let root =
    parse_mli "val x : int -> string\ntype t = int\nmodule M : sig end\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  (
    match Ast.SourceFile.view root with
    | Ast.SourceFile.Interface _ -> ()
    | _ -> panic "expected interface root"
  );
  let value_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected value signature item"
  in
  (
    match Ast.SignatureItem.view value_item with
    | Ast.SignatureItem.Value decl ->
        let (name, annotation) =
          match Ast.ValueDeclaration.view decl with
          | Ast.ValueDeclaration.Value { name; colon_token; annotation } ->
              Test.assert_equal ~expected:":" ~actual:(Ast.Token.text colon_token);
              (name, annotation)
          | Ast.ValueDeclaration.Unknown _ -> panic "expected value declaration view"
        in
        Test.assert_equal ~expected:"x" ~actual:(Ast.Ident.text name);
        (
          match Ast.TypeExpr.view annotation with
          | Ast.TypeExpr.Arrow { arg; ret; _ } ->
              assert_type_ident_last_segment arg "int";
              assert_type_ident_last_segment ret "string";
              Ok ()
          | _ -> Error "expected arrow value type"
        )
    | _ -> Error "expected value declaration"
  )
  |> Result.expect ~msg:"expected value signature";
  let type_item =
    nth_signature_item root 1
    |> require_some ~msg:"expected type signature item"
  in
  (
    match Ast.SignatureItem.view type_item with
    | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) ->
        let name =
          Ast.TypeDeclaration.name decl
          |> require_some ~msg:"expected type name"
        in
        Test.assert_equal ~expected:"t" ~actual:(Ast.Ident.text name);
        let manifest =
          Ast.TypeDeclaration.manifest decl
          |> require_some ~msg:"expected type manifest"
        in
        assert_type_ident_last_segment manifest "int"
    | _ -> panic "expected type declaration"
  );
  let module_item =
    nth_signature_item root 2
    |> require_some ~msg:"expected module signature item"
  in
  match Ast.SignatureItem.view module_item with
  | Ast.SignatureItem.Module decl ->
      let name =
        Ast.ModuleDeclaration.name decl
        |> require_some ~msg:"expected module name"
      in
      Test.assert_equal ~expected:"M" ~actual:(Ast.Ident.text name);
      Ok ()
  | _ -> Error "expected module declaration"

let test_package_type_value_annotation_views = fun _ctx ->
  let root =
    parse_mli "val get: (module ConfigSpec with type t = 'a) -> ('a, error) result\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let value_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected value signature item"
  in
  match Ast.SignatureItem.view value_item with
  | Ast.SignatureItem.Value decl ->
      let annotation =
        Ast.ValueDeclaration.type_annotation decl
        |> require_some ~msg:"expected value type annotation"
      in
      (
        match Ast.TypeExpr.view annotation with
        | Ast.TypeExpr.Arrow { arg; ret = _; _ } -> (
            match Ast.TypeExpr.view arg with
            | Ast.TypeExpr.Unknown _ -> Ok ()
            | _ -> Error "expected package type annotation to lower as unknown type"
          )
        | _ -> Error "expected arrow value type"
      )
  | _ -> Error "expected value declaration"

let test_type_expression_views = fun _ctx ->
  let root =
    parse_mli
      {ocaml|val xs : int list
external id : 'a -> 'a = "%identity"
val done_: unit
val scoped_unit: M.unit
|ocaml}
    |> Result.expect ~msg:"expected parse interface"
  in
  let value_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected value signature item"
  in
  (
    match Ast.SignatureItem.view value_item with
    | Ast.SignatureItem.Value decl ->
        let annotation =
          Ast.ValueDeclaration.type_annotation decl
          |> require_some ~msg:"expected value type annotation"
        in
        (
          match Ast.TypeExpr.view annotation with
          | Ast.TypeExpr.Apply { ident; args } ->
              assert_last_ident_text ident "list";
              let argument = vector_first args ~msg:"expected list type argument" in
              assert_type_ident_last_segment argument "int";
              Ok ()
          | _ -> Error "expected type application"
        )
    | _ -> Error "expected value declaration"
  )
  |> Result.expect ~msg:"expected value type application";
  let external_item =
    nth_signature_item root 1
    |> require_some ~msg:"expected external signature item"
  in
  (
    match Ast.SignatureItem.view external_item with
    | Ast.SignatureItem.External decl ->
        let annotation =
          Ast.ExternalDeclaration.type_annotation decl
          |> require_some ~msg:"expected external type annotation"
        in
        (
          match Ast.TypeExpr.view annotation with
          | Ast.TypeExpr.Arrow { arg; ret; _ } -> (
              match (Ast.TypeExpr.view arg, Ast.TypeExpr.view ret) with
              | (Ast.TypeExpr.Var { name = left_name }, Ast.TypeExpr.Var { name = right_name }) ->
                  Test.assert_equal ~expected:"a" ~actual:(Ast.Token.text left_name);
                  Test.assert_equal ~expected:"a" ~actual:(Ast.Token.text right_name);
                  Ok ()
              | _ -> Error "expected type variables"
            )
          | _ -> Error "expected external arrow type"
        )
    | _ -> Error "expected external declaration"
  )
  |> Result.expect ~msg:"expected external declaration";
  let unit_item =
    nth_signature_item root 2
    |> require_some ~msg:"expected unit value signature item"
  in
  (
    match Ast.SignatureItem.view unit_item with
    | Ast.SignatureItem.Value decl ->
        let annotation =
          Ast.ValueDeclaration.type_annotation decl
          |> require_some ~msg:"expected unit value type annotation"
        in
        (
          match Ast.TypeExpr.view annotation with
          | Ast.TypeExpr.Apply { ident; args } ->
              assert_last_ident_text ident "unit";
              Test.assert_equal ~expected:0 ~actual:(Vector.length args);
              Ok ()
          | _ -> Error "expected unit to be an empty type application"
        )
    | _ -> Error "expected unit value declaration"
  )
  |> Result.expect ~msg:"expected builtin unit type application";
  let scoped_unit_item =
    nth_signature_item root 3
    |> require_some ~msg:"expected scoped unit value signature item"
  in
  match Ast.SignatureItem.view scoped_unit_item with
  | Ast.SignatureItem.Value decl ->
      let annotation =
        Ast.ValueDeclaration.type_annotation decl
        |> require_some ~msg:"expected scoped unit value type annotation"
      in
      (
        match Ast.TypeExpr.view annotation with
        | Ast.TypeExpr.Ident { ident } ->
            let first =
              Ast.Ident.first_segment ident
              |> require_some ~msg:"expected first scoped unit ident segment"
            in
            let last =
              Ast.Ident.last_segment ident
              |> require_some ~msg:"expected last scoped unit ident segment"
            in
            Test.assert_equal ~expected:"M" ~actual:(Ast.Token.text first);
            Test.assert_equal ~expected:"unit" ~actual:(Ast.Token.text last);
            Ok ()
        | _ -> Error "expected scoped unit to remain an ident type"
      )
  | _ -> Error "expected scoped unit value declaration"

let test_type_expression_alias_view = fun _ctx ->
  let root =
    parse_mli {ocaml|val aliased: (int as 'a)
|ocaml}
    |> Result.expect ~msg:"expected parse interface"
  in
  let value_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected value signature item"
  in
  match Ast.SignatureItem.view value_item with
  | Ast.SignatureItem.Value decl ->
      let annotation =
        Ast.ValueDeclaration.type_annotation decl
        |> require_some ~msg:"expected aliased type annotation"
      in
      (
        match Ast.TypeExpr.view annotation with
        | Ast.TypeExpr.Alias { typ; name } ->
            assert_type_ident_last_segment typ "int";
            Test.assert_equal ~expected:"a" ~actual:(Ast.Token.text name);
            Ok ()
        | _ -> Error "expected type alias annotation"
      )
  | _ -> Error "expected value declaration"

let test_type_tuple_separator_views = fun _ctx ->
  let root =
    parse_mli "type ('a, 'e) result_like = ('a, 'e) result\ntype pair = int * string\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let result_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected result-like type item"
  in
  (
    match Ast.SignatureItem.view result_item with
    | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) ->
        let manifest =
          Ast.TypeDeclaration.manifest decl
          |> require_some ~msg:"expected result-like manifest"
        in
        (
          match Ast.TypeExpr.view manifest with
          | Ast.TypeExpr.Apply { ident; args } ->
              assert_last_ident_text ident "result";
              Test.assert_equal ~expected:2 ~actual:(Vector.length args);
              let arg = vector_first args ~msg:"expected first result argument" in
              let ret = vector_second args ~msg:"expected second result argument" in
              (
                match (Ast.TypeExpr.view arg, Ast.TypeExpr.view ret) with
                | (Ast.TypeExpr.Var { name = left_name }, Ast.TypeExpr.Var { name = right_name }) ->
                    Test.assert_equal ~expected:"a" ~actual:(Ast.Token.text left_name);
                    Test.assert_equal ~expected:"e" ~actual:(Ast.Token.text right_name);
                    Ok ()
                | _ -> Error "expected result type variables"
              )
          | _ -> Error "expected type constructor application"
        )
    | _ -> Error "expected result-like type declaration"
  )
  |> Result.expect ~msg:"expected comma type tuple";
  let pair_item =
    nth_signature_item root 1
    |> require_some ~msg:"expected pair type item"
  in
  match Ast.SignatureItem.view pair_item with
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) ->
      let manifest =
        Ast.TypeDeclaration.manifest decl
        |> require_some ~msg:"expected pair manifest"
      in
      (
        match Ast.TypeExpr.view manifest with
        | Ast.TypeExpr.Tuple { parts } ->
            Test.assert_equal ~expected:2 ~actual:(Vector.length parts);
            let arg = vector_first parts ~msg:"expected first tuple type" in
            let ret = vector_second parts ~msg:"expected second tuple type" in
            assert_type_ident_last_segment arg "int";
            assert_type_ident_last_segment ret "string";
            Ok ()
        | _ -> Error "expected star tuple type"
      )
  | _ -> Error "expected pair type declaration"

let test_poly_labeled_and_signed_views = fun _ctx ->
  let source =
    "let make:\n  type socket err. reader:(socket, err) reader -> t = fun ~reader -> value\n\
                let f = function | -1 -> true | +2 -> false\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let make_binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected make item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected make binding"
  in
  let annotation =
    Ast.LetBinding.type_annotation make_binding
    |> require_some ~msg:"expected make type annotation"
  in
  (
    match Ast.TypeExpr.view annotation with
    | Ast.TypeExpr.Forall { names; body } ->
        Test.assert_equal
          ~expected:[ "socket"; "err" ]
          ~actual:(List.map (vector_to_list names) ~fn:Ast.Token.text);
        (
          match Ast.TypeExpr.view body with
          | Ast.TypeExpr.Arrow { label = Some { name = Some label; _ }; arg = _; ret = _ } ->
              Test.assert_equal ~expected:"reader" ~actual:(Ast.Token.text label)
          | _ -> panic "expected poly type arrow body"
        )
    | _ -> panic "expected poly type annotation"
  );
  let function_body =
    nth_structure_item root 1
    |> require_some ~msg:"expected function item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected function binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected function body"
  in
  match Ast.Expr.view function_body with
  | Ast.Expr.Fun { body = Ast.Expr.Body_cases { first_case }; _ } -> (
      let pattern =
        match Ast.MatchCase.view first_case with
        | Ast.MatchCase.Case { pattern; _ } -> pattern
        | Ast.MatchCase.Unknown _ -> panic "expected first function case pattern"
      in
      match Ast.Pattern.view pattern with
      | Ast.Pattern.Literal { token } ->
          let sign =
            Ast.Pattern.literal_sign_token pattern
            |> require_some ~msg:"expected signed literal sign"
          in
          Test.assert_equal ~expected:"-" ~actual:(Ast.Token.text sign);
          Test.assert_equal ~expected:"1" ~actual:(Ast.Token.text token);
          Ok ()
      | _ -> Error "expected signed literal pattern"
    )
  | _ -> Error "expected function expression"

let test_quoted_poly_let_annotation_views = fun _ctx ->
  let source =
    "let rec record_mut_backend:\n\
    \  'field 'builder 'value. state ->\n\
    \  fields:'field De.Fields.t ->\n\
    \  create:(unit -> 'builder) ->\n\
    \  step:('builder -> 'field option -> unit) ->\n\
    \  finish:('builder -> 'value) ->\n\
    \  'value = fun state ~fields ~create ~step ~finish -> finish (create ())\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
  in
  let annotation =
    Ast.LetBinding.type_annotation binding
    |> require_some ~msg:"expected let binding type annotation"
  in
  (
    match Ast.TypeExpr.view annotation with
    | Ast.TypeExpr.Forall { names; body } ->
        Test.assert_equal
          ~expected:[ "field"; "builder"; "value" ]
          ~actual:(List.map (vector_to_list names) ~fn:Ast.Token.text);
        (
          match Ast.TypeExpr.view body with
          | Ast.TypeExpr.Arrow { arg; ret; _ } ->
              (
                match Ast.TypeExpr.view arg with
                | Ast.TypeExpr.Ident { ident } -> assert_last_ident_text ident "state"
                | _ -> panic "expected state ident type"
              );
              (
                match Ast.TypeExpr.view ret with
                | Ast.TypeExpr.Arrow {
                    label = Some { name = Some label; _ };
                    arg = labeled_annotation;
                    ret = _;
                  } ->
                    Test.assert_equal ~expected:"fields" ~actual:(Ast.Token.text label);
                    (
                      match Ast.TypeExpr.view labeled_annotation with
                      | Ast.TypeExpr.Apply { ident; _ } ->
                          assert_last_ident_text ident "t";
                          Ok ()
                      | _ -> Error "expected labeled fields apply type"
                    )
                | _ -> Error "expected arrow chain after state"
              )
          | _ -> Error "expected quoted poly type arrow body"
        )
    | _ -> Error "expected quoted poly type annotation"
  )

let assert_type_manifest_is_none = fun source ->
  let root =
    parse_mli source
    |> Result.expect ~msg:"expected parse interface"
  in
  let type_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected type signature item"
  in
  match Ast.SignatureItem.view type_item with
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) -> (
      match Ast.TypeDeclaration.manifest decl with
      | None -> Ok ()
      | Some _ -> Error "expected type declaration without manifest view"
    )
  | _ -> Error "expected type declaration"

let test_non_manifest_type_declaration_bodies = fun _ctx ->
  match assert_type_manifest_is_none "type color = Red | Blue\n" with
  | Error _ as error -> error
  | Ok () -> assert_type_manifest_is_none "type point = { x : int }\n"

let test_type_declaration_parameters = fun _ctx ->
  let root =
    parse_mli "type (+'a, _) box = 'a list\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let type_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected type signature item"
  in
  match Ast.SignatureItem.view type_item with
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) ->
      let name =
        Ast.TypeDeclaration.name decl
        |> require_some ~msg:"expected type name"
      in
      Test.assert_equal ~expected:"box" ~actual:(Ast.Ident.text name);
      let named = ref None in
      let wildcard_param = ref None in
      iter_fold
        Ast.TypeDeclaration.fold_parameter
        decl
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Ast.TypeDeclaration.Named { name; variance; _ } ->
              named := Some (Ast.Token.text name, Option.map variance ~fn:Ast.Token.text)
          | Ast.TypeDeclaration.Wildcard { wildcard; _ } ->
              wildcard_param := Some (Ast.Token.text wildcard));
      (
        match !named with
        | Some ("a", Some "+") -> ()
        | _ -> panic "expected covariant named type parameter"
      );
      (
        match !wildcard_param with
        | Some "_" -> Ok ()
        | _ -> Error "expected wildcard type parameter"
      )
  | _ -> Error "expected type declaration"

let test_type_declaration_member_views = fun _ctx ->
  let root =
    parse_mli "type 'a box = 'a list and color = Red | Blue and point = { x : int }\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let type_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected type signature item"
  in
  match Ast.SignatureItem.view type_item with
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) ->
      let child_kinds = ref [] in
      iter_fold
        Ast.Node.fold_child_node
        (Ast.TypeDeclaration.as_node decl)
        ~fn:(fun child -> child_kinds := Ast.Node.kind child :: !child_kinds);
      Test.assert_equal
        ~expected:[
          SyntaxKind.TYPE_DECL_MEMBER;
          SyntaxKind.TYPE_DECL_MEMBER;
          SyntaxKind.TYPE_DECL_MEMBER;
        ]
        ~actual:(List.reverse !child_kinds);
      let count = ref 0 in
      let names = ref [] in
      let shells = ref [] in
      let parameter_counts = ref [] in
      let manifest_shapes = ref [] in
      iter_fold
        Ast.TypeDeclaration.fold_member
        decl
        ~fn:(fun member ->
          count := !count + 1;
          let name =
            Ast.TypeDeclaration.Member.name member
            |> require_some ~msg:"expected type member name"
          in
          let shell =
            Ast.TypeDeclaration.Member.shell_token member
            |> require_some ~msg:"expected type member shell"
          in
          let parameters = ref 0 in
          iter_fold
            Ast.TypeDeclaration.Member.fold_parameter
            member
            ~fn:(fun _ -> parameters := !parameters + 1);
          let has_manifest =
            match Ast.TypeDeclaration.Member.manifest member with
            | Some _ -> true
            | None -> false
          in
          names := Ast.Ident.text name :: !names;
          shells := Ast.Token.text shell :: !shells;
          parameter_counts := !parameters :: !parameter_counts;
          manifest_shapes := has_manifest :: !manifest_shapes);
      Test.assert_equal ~expected:3 ~actual:!count;
      Test.assert_equal ~expected:[ "box"; "color"; "point" ] ~actual:(List.reverse !names);
      Test.assert_equal ~expected:[ "type"; "and"; "and" ] ~actual:(List.reverse !shells);
      Test.assert_equal ~expected:[ 1; 0; 0 ] ~actual:(List.reverse !parameter_counts);
      Test.assert_equal ~expected:[ true; false; false ] ~actual:(List.reverse !manifest_shapes);
      Ok ()
  | _ -> Error "expected type declaration"

let test_type_declaration_body_group_views = fun _ctx ->
  let root =
    parse_mli
      "type color = Red | Blue of int | Pair of int * string\n\
     type point = private { mutable x : int; y : string }\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let color_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected color type item"
  in
  (
    match Ast.SignatureItem.view color_item with
    | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) ->
        let member =
          Ast.TypeDeclaration.fold_members
            decl
            None
            (fun acc member ->
              match acc with
              | Some _ -> acc
              | None -> Some member)
          |> require_some ~msg:"expected color type member"
        in
        let variant =
          Ast.TypeDeclaration.Member.variant_type member
          |> require_some ~msg:"expected variant type body"
        in
        let names = Vector.with_capacity ~size:3 in
        let pipe_flags = Vector.with_capacity ~size:3 in
        let payload_shapes = Vector.with_capacity ~size:3 in
        iter_fold
          Ast.VariantType.fold_constructor
          variant
          ~fn:(fun constructor ->
            let (name, pipe_token, rhs) =
              match Ast.VariantConstructor.view constructor with
              | Ast.VariantConstructor.Constructor { pipe_token; name; rhs } -> (
                name,
                pipe_token,
                rhs
              )
              | Ast.VariantConstructor.Unknown _ -> panic "expected constructor view"
            in
            Vector.push names ~value:(Ast.Ident.text name);
            Vector.push pipe_flags ~value:(Option.is_some pipe_token);
            let payload_shape =
              match rhs with
              | Ast.VariantConstructor.Plain -> "none"
              | Ast.VariantConstructor.Payload {
                  payload = Ast.VariantConstructor.TypeExpr payload;
                  _;
                } ->
                  (
                      match Ast.TypeExpr.view payload with
                      | Ast.TypeExpr.Ident _ -> "ident"
                      | Ast.TypeExpr.Tuple _ -> "tuple"
                      | _ -> "other"
                    )
              | Ast.VariantConstructor.Payload { payload = Ast.VariantConstructor.Record _; _ }
              | Ast.VariantConstructor.Gadt _ -> "other"
            in
            Vector.push payload_shapes ~value:payload_shape);
        Test.assert_equal ~expected:[ "Red"; "Blue"; "Pair" ] ~actual:(vector_to_list names);
        Test.assert_equal ~expected:[ false; true; true ] ~actual:(vector_to_list pipe_flags);
        Test.assert_equal
          ~expected:[ "none"; "ident"; "tuple" ]
          ~actual:(vector_to_list payload_shapes);
        Ok ()
    | _ -> Error "expected color type declaration"
  )
  |> Result.expect ~msg:"expected variant type body";
  let point_item =
    nth_signature_item root 1
    |> require_some ~msg:"expected point type item"
  in
  match Ast.SignatureItem.view point_item with
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) ->
      let member =
        Ast.TypeDeclaration.fold_members
          decl
          None
          (fun acc member ->
            match acc with
            | Some _ -> acc
            | None -> Some member)
        |> require_some ~msg:"expected point type member"
      in
      let record =
        Ast.TypeDeclaration.Member.record_type member
        |> require_some ~msg:"expected record type body"
      in
      Test.assert_true (Option.is_some (Ast.RecordType.private_token record));
      let names = Vector.with_capacity ~size:2 in
      let mutable_flags = Vector.with_capacity ~size:2 in
      let field_types = Vector.with_capacity ~size:2 in
      iter_fold
        Ast.RecordType.fold_field
        record
        ~fn:(fun field ->
          match Ast.RecordField.view field with
          | Ast.RecordField.Field {
              mutable_token;
              name;
              colon_token;
              annotation;
            } ->
              Test.assert_equal ~expected:":" ~actual:(Ast.Token.text colon_token);
              Vector.push names ~value:(Ast.Ident.text name);
              Vector.push mutable_flags ~value:(Option.is_some mutable_token);
              (
                match Ast.TypeExpr.view annotation with
                | Ast.TypeExpr.Ident { ident } ->
                    let last =
                      Ast.Ident.last_segment ident
                      |> require_some ~msg:"expected field type ident"
                    in
                    Vector.push field_types ~value:(Ast.Token.text last)
                | _ -> panic "expected field type ident"
              )
          | Ast.RecordField.Unknown _ -> panic "expected record field view");
      Test.assert_equal ~expected:[ "x"; "y" ] ~actual:(vector_to_list names);
      Test.assert_equal ~expected:[ true; false ] ~actual:(vector_to_list mutable_flags);
      Test.assert_equal ~expected:[ "int"; "string" ] ~actual:(vector_to_list field_types);
      Ok ()
  | _ -> Error "expected point type declaration"

let test_type_alias_record_representation_views = fun _ctx ->
  let root =
    parse_mli "type point=Base.point=private{x:int;y:string}\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let type_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected type item"
  in
  match Ast.SignatureItem.view type_item with
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem decl) ->
      let member =
        Ast.TypeDeclaration.fold_members
          decl
          None
          (fun acc member ->
            match acc with
            | Some _ -> acc
            | None -> Some member)
        |> require_some ~msg:"expected type member"
      in
      let manifest =
        Ast.TypeDeclaration.Member.manifest member
        |> require_some ~msg:"expected manifest alias"
      in
      assert_type_ident_last_segment manifest "point";
      let record =
        Ast.TypeDeclaration.Member.record_type member
        |> require_some ~msg:"expected record representation"
      in
      Test.assert_true (Option.is_some (Ast.RecordType.private_token record));
      let field_names = Vector.with_capacity ~size:2 in
      iter_fold
        Ast.RecordType.fold_field
        record
        ~fn:(fun field ->
          match Ast.RecordField.view field with
          | Ast.RecordField.Field { name; _ } ->
              Vector.push field_names ~value:(Ast.Ident.text name)
          | Ast.RecordField.Unknown _ -> panic "expected field view");
      Test.assert_equal ~expected:2 ~actual:(Vector.length field_names);
      Test.assert_equal ~expected:"x" ~actual:(Vector.get_unchecked field_names ~at:0);
      Test.assert_equal ~expected:"y" ~actual:(Vector.get_unchecked field_names ~at:1);
      Ok ()
  | _ -> Error "expected type declaration"

let test_abstract_type_attribute_boundary_views = fun _ctx ->
  let source = "type ('a, 'b) stack [@@immediate]\nlet next = 1\n" in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let next_item =
    nth_structure_item root 1
    |> require_some ~msg:"expected trailing let item"
  in
  match Ast.StructureItem.view next_item with
  | Ast.StructureItem.Let _ -> Ok ()
  | _ -> Error "expected trailing let declaration after abstract type attribute"

let test_manifest_type_attribute_suffix_views = fun _ctx ->
  let root =
    parse_ml "type t = int [@@immediate]\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let type_item =
    nth_structure_item root 0
    |> require_some ~msg:"expected type item"
  in
  match Ast.StructureItem.view type_item with
  | Ast.StructureItem.Type (Ast.TypeDeclarationItem decl) ->
      let manifest =
        Ast.TypeDeclaration.manifest decl
        |> require_some ~msg:"expected type manifest"
      in
      let inner =
        Ast.TypeExpr.inner_without_attribute_suffix manifest
        |> require_some ~msg:"expected manifest inner type"
      in
      assert_type_ident_last_segment inner "int";
      Test.assert_equal ~expected:4 ~actual:(Ast.TypeExpr.attribute_suffix_token_count manifest);
      Ok ()
  | _ -> Error "expected type declaration"

let test_open_declaration_ident_tokens = fun _ctx ->
  let root =
    parse_ml "open Foo.Bar\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let item =
    nth_structure_item root 0
    |> require_some ~msg:"expected open structure item"
  in
  match Ast.StructureItem.view item with
  | Ast.StructureItem.Open decl ->
      let ident =
        Ast.OpenDeclaration.ident decl
        |> require_some ~msg:"expected open ident"
      in
      let first =
        Ast.Ident.first_segment ident
        |> require_some ~msg:"expected first open ident segment"
      in
      let last =
        Ast.Ident.last_segment ident
        |> require_some ~msg:"expected last open ident segment"
      in
      Test.assert_equal ~expected:"Foo" ~actual:(Ast.Token.text first);
      Test.assert_equal ~expected:"Bar" ~actual:(Ast.Token.text last);
      Test.assert_equal ~expected:2 ~actual:(Ast.Ident.segment_count ident);
      Ok ()
  | _ -> Error "expected open declaration"

let test_simple_declaration_token_views = fun _ctx ->
  let source =
    "include Foo.Bar\nexternal id : 'a -> 'a = \"%identity\" \"caml_id\"\nexception Boom\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let include_item =
    nth_structure_item root 0
    |> require_some ~msg:"expected include structure item"
  in
  (
    match Ast.StructureItem.view include_item with
    | Ast.StructureItem.Include decl ->
        let ident =
          Ast.IncludeDeclaration.body_ident decl
          |> require_some ~msg:"expected include ident"
        in
        let first =
          Ast.Ident.first_segment ident
          |> require_some ~msg:"expected first include ident segment"
        in
        let last =
          Ast.Ident.last_segment ident
          |> require_some ~msg:"expected last include ident segment"
        in
        Test.assert_equal ~expected:"Foo" ~actual:(Ast.Token.text first);
        Test.assert_equal ~expected:"Bar" ~actual:(Ast.Token.text last);
        Test.assert_equal ~expected:2 ~actual:(Ast.Ident.segment_count ident)
    | _ -> panic "expected include declaration"
  );
  let external_item =
    nth_structure_item root 1
    |> require_some ~msg:"expected external structure item"
  in
  (
    match Ast.StructureItem.view external_item with
    | Ast.StructureItem.External decl ->
        let (name, primitives) =
          match Ast.ExternalDeclaration.view decl with
          | Ast.ExternalDeclaration.External {
              name;
              colon_token;
              equals_token;
              primitives;
              _;
            } ->
              Test.assert_equal ~expected:":" ~actual:(Ast.Token.text colon_token);
              Test.assert_equal ~expected:"=" ~actual:(Ast.Token.text equals_token);
              (name, primitives)
          | Ast.ExternalDeclaration.Unknown _ -> panic "expected external declaration view"
        in
        Test.assert_equal ~expected:"id" ~actual:(Ast.Ident.text name);
        Test.assert_equal
          ~expected:[ "\"%identity\""; "\"caml_id\"" ]
          ~actual:(
            primitives
            |> Vector.to_array
            |> Array.map ~fn:Ast.Token.text
            |> Array.to_list
          )
    | _ -> panic "expected external declaration"
  );
  let exception_item =
    nth_structure_item root 2
    |> require_some ~msg:"expected exception structure item"
  in
  match Ast.StructureItem.view exception_item with
  | Ast.StructureItem.Exception decl ->
      let child_kinds = ref [] in
      let name =
        Ast.ExceptionDeclaration.name decl
        |> require_some ~msg:"expected exception name"
      in
      iter_fold
        Ast.Node.fold_child_node
        (Ast.ExceptionDeclaration.as_node decl)
        ~fn:(fun child -> child_kinds := Ast.Node.kind child :: !child_kinds);
      Test.assert_equal ~expected:"Boom" ~actual:(Ast.Ident.text name);
      Test.assert_equal
        ~expected:[ SyntaxKind.EXCEPTION_DECL_HEAD ]
        ~actual:(List.reverse !child_kinds);
      Ok ()
  | _ -> Error "expected exception declaration"

let test_type_extension_and_exception_views = fun _ctx ->
  let source =
    "type 'a box += | More of 'a\nexception Parse_error of string\nexception Nested = Std.Result.Error\n"
  in
  let root =
    parse_mli source
    |> Result.expect ~msg:"expected parse interface"
  in
  let type_extension_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected type extension item"
  in
  (
    match Ast.SignatureItem.view type_extension_item with
    | Ast.SignatureItem.Type (Ast.TypeExtensionItem decl) ->
        let child_kinds = ref [] in
        iter_fold
          Ast.Node.fold_child_node
          (Ast.TypeExtensionDeclaration.as_node decl)
          ~fn:(fun child -> child_kinds := Ast.Node.kind child :: !child_kinds);
        let name =
          Ast.TypeExtensionDeclaration.name decl
          |> require_some ~msg:"expected type extension name"
        in
        Test.assert_equal ~expected:"box" ~actual:(Ast.Ident.text name);
        Test.assert_equal
          ~expected:[ SyntaxKind.TYPE_EXTENSION_DECL_HEAD; SyntaxKind.TYPE_EXTENSION_DECL_BODY ]
          ~actual:(List.reverse !child_kinds);
        let parameter_count = ref 0 in
        iter_fold
          Ast.TypeExtensionDeclaration.fold_parameter
          decl
          ~fn:(fun _ -> parameter_count := !parameter_count + 1);
        Test.assert_equal ~expected:1 ~actual:!parameter_count;
        let variant =
          Ast.TypeExtensionDeclaration.variant_type decl
          |> require_some ~msg:"expected type extension body"
        in
        let constructor = ref None in
        iter_fold
          Ast.VariantType.fold_constructor
          variant
          ~fn:(fun current ->
            match !constructor with
            | Some _ -> ()
            | None -> constructor := Some current);
        let constructor =
          !constructor
          |> require_some ~msg:"expected type extension constructor"
        in
        let (constructor_name, rhs) =
          match Ast.VariantConstructor.view constructor with
          | Ast.VariantConstructor.Constructor { name; rhs; _ } -> (name, rhs)
          | Ast.VariantConstructor.Unknown _ -> panic "expected type extension constructor view"
        in
        Test.assert_equal ~expected:"More" ~actual:(Ast.Ident.text constructor_name);
        (
          match rhs with
          | Ast.VariantConstructor.Payload { payload = Ast.VariantConstructor.TypeExpr payload; _ } -> (
              match Ast.TypeExpr.view payload with
              | Ast.TypeExpr.Var { name = payload_name } ->
                  Test.assert_equal ~expected:"a" ~actual:(Ast.Token.text payload_name)
              | _ -> panic "expected type extension payload type variable"
            )
          | _ -> panic "expected type extension payload"
        );
        Ok ()
    | _ -> Error "expected type extension declaration"
  )
  |> Result.expect ~msg:"expected type extension view";
  let payload_item =
    nth_signature_item root 1
    |> require_some ~msg:"expected exception payload item"
  in
  (
    match Ast.SignatureItem.view payload_item with
    | Ast.SignatureItem.Exception decl ->
        let name =
          Ast.ExceptionDeclaration.name decl
          |> require_some ~msg:"expected exception payload name"
        in
        Test.assert_equal ~expected:"Parse_error" ~actual:(Ast.Ident.text name);
        (
          match Ast.ExceptionDeclaration.view decl with
          | Ast.ExceptionDeclaration.Payload {
              of_token;
              payload = Ast.ExceptionDeclaration.TypeExpr payload;
            } ->
              Test.assert_equal ~expected:"of" ~actual:(Ast.Token.text of_token);
              assert_type_ident_last_segment payload "string";
              Ok ()
          | _ -> Error "expected exception payload view"
        )
    | _ -> Error "expected exception declaration"
  )
  |> Result.expect ~msg:"expected exception payload view";
  let alias_item =
    nth_signature_item root 2
    |> require_some ~msg:"expected exception alias item"
  in
  match Ast.SignatureItem.view alias_item with
  | Ast.SignatureItem.Exception decl ->
      let name =
        Ast.ExceptionDeclaration.name decl
        |> require_some ~msg:"expected exception alias name"
      in
      Test.assert_equal ~expected:"Nested" ~actual:(Ast.Ident.text name);
      (
        match Ast.ExceptionDeclaration.view decl with
        | Ast.ExceptionDeclaration.Alias { equals_token; ident } ->
            Test.assert_equal ~expected:"=" ~actual:(Ast.Token.text equals_token);
            assert_last_ident_text ident "Error";
            Ok ()
        | _ -> Error "expected exception alias view"
      )
  | _ -> Error "expected exception declaration"

let test_exception_after_function_binding_views = fun _ctx ->
  let source =
    "let error_to_string = function\n  | Error message -> message\n\nexception Parse_exception of string\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let exception_item =
    nth_structure_item root 1
    |> require_some ~msg:"expected exception item after function binding"
  in
  (
    match Ast.StructureItem.view exception_item with
    | Ast.StructureItem.Exception decl ->
        let name =
          Ast.ExceptionDeclaration.name decl
          |> require_some ~msg:"expected exception name"
        in
        Test.assert_equal ~expected:"Parse_exception" ~actual:(Ast.Ident.text name);
        (
          match Ast.ExceptionDeclaration.view decl with
          | Ast.ExceptionDeclaration.Payload {
              of_token;
              payload = Ast.ExceptionDeclaration.TypeExpr payload;
            } ->
              Test.assert_equal ~expected:"of" ~actual:(Ast.Token.text of_token);
              assert_type_ident_last_segment payload "string";
              Ok ()
          | _ -> Error "expected exception payload view"
        )
    | _ -> Error "expected exception declaration"
  )
  |> Result.expect ~msg:"expected exception declaration after function binding";
  Ok ()

let test_nested_match_guard_case_boundaries = fun _ctx ->
  let source =
    "let classify = function\n  | Some x when match read x with | Some 0 | Some 1 -> false | _ -> true -> 1\n  | _ -> 0\nlet next = 1\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let next_item =
    nth_structure_item root 1
    |> require_some ~msg:"expected trailing let item after guarded match"
  in
  match Ast.StructureItem.view next_item with
  | Ast.StructureItem.Let _ -> Ok ()
  | _ -> Error "expected trailing let declaration after guarded nested match"

let test_module_declaration_tokens = fun _ctx ->
  let root =
    parse_ml "module rec M = struct end\nmodule _ = struct end\nmodule Alias = Foo.Bar\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let first_item =
    nth_structure_item root 0
    |> require_some ~msg:"expected first module item"
  in
  let second_item =
    nth_structure_item root 1
    |> require_some ~msg:"expected second module item"
  in
  let third_item =
    nth_structure_item root 2
    |> require_some ~msg:"expected third module item"
  in
  (
    match Ast.StructureItem.view first_item with
    | Ast.StructureItem.Module decl ->
        let rec_token =
          Ast.ModuleDeclaration.rec_token decl
          |> require_some ~msg:"expected rec token"
        in
        let name =
          Ast.ModuleDeclaration.name decl
          |> require_some ~msg:"expected module name"
        in
        Test.assert_equal ~expected:"rec" ~actual:(Ast.Token.text rec_token);
        Test.assert_equal ~expected:"M" ~actual:(Ast.Ident.text name);
        (
          match Ast.ModuleDeclaration.body decl with
          | Ast.ModuleDeclaration.Expr { body } -> (
              match Ast.ModuleExpr.view body with
              | Ast.ModuleExpr.Structure { body } ->
                  Test.assert_equal
                    ~expected:SyntaxKind.STRUCT_MODULE_EXPR
                    ~actual:(Ast.Node.kind body)
              | _ -> panic "expected struct module declaration body"
            )
          | _ -> panic "expected struct module declaration body"
        )
    | _ -> panic "expected first module declaration"
  );
  (
    match Ast.StructureItem.view second_item with
    | Ast.StructureItem.Module decl ->
        let name =
          Ast.ModuleDeclaration.name decl
          |> require_some ~msg:"expected module wildcard name"
        in
        Test.assert_equal ~expected:"_" ~actual:(Ast.Ident.text name)
    | _ -> panic "expected second module declaration"
  );
  (
    match Ast.StructureItem.view third_item with
    | Ast.StructureItem.Module decl ->
        let separator =
          Ast.ModuleDeclaration.separator_token decl
          |> require_some ~msg:"expected module separator"
        in
        let ident =
          Ast.ModuleDeclaration.body_ident decl
          |> require_some ~msg:"expected module body ident"
        in
        let segments =
          Ast.Ident.fold_segment
            ident
            ~init:[]
            ~fn:(fun token segments -> Ast.Continue (Ast.Token.text token :: segments))
          |> List.reverse
        in
        Test.assert_equal ~expected:"=" ~actual:(Ast.Token.text separator);
        (
          match Ast.ModuleDeclaration.body decl with
          | Ast.ModuleDeclaration.Expr { body } -> (
              match Ast.ModuleExpr.view body with
              | Ast.ModuleExpr.Ident { ident } ->
                  Test.assert_equal ~expected:SyntaxKind.IDENT ~actual:(Ast.Ident.kind ident)
              | _ -> panic "expected ident module declaration body"
            )
          | _ -> panic "expected ident module declaration body"
        );
        Test.assert_equal ~expected:[ "Foo"; "Bar" ] ~actual:segments
    | _ -> panic "expected third module declaration"
  );
  Ok ()

let test_module_declaration_application_body_view = fun _ctx ->
  let root =
    parse_ml
      {ocaml|module Int_list_show = Make (Int_show)
module Int_pair_show = Make_pair (Int_show) (Int_show)
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let assert_module_apply index expected_name =
    let item =
      nth_structure_item root index
      |> require_some ~msg:"expected module item"
    in
    match Ast.StructureItem.view item with
    | Ast.StructureItem.Module decl ->
        let name =
          Ast.ModuleDeclaration.name decl
          |> require_some ~msg:"expected module name"
        in
        Test.assert_equal ~expected:expected_name ~actual:(Ast.Ident.text name);
        (
          match Ast.ModuleDeclaration.body decl with
          | Ast.ModuleDeclaration.Expr { body } -> (
              match Ast.ModuleExpr.view body with
              | Ast.ModuleExpr.Apply { callee = Some _; argument = Some _; _ } -> ()
              | Ast.ModuleExpr.Apply _ -> panic "expected complete applied module body"
              | _ -> panic "expected applied module body"
            )
          | _ -> panic "expected module expression body"
        );
        Ok ()
    | _ -> Error "expected module declaration"
  in
  let* () = assert_module_apply 0 "Int_list_show" in
  assert_module_apply 1 "Int_pair_show"

let test_trailing_sequence_before_and_views = fun _ctx ->
  let source = "let rec f () = log \"f\";\nand g () = log \"g\";\n" in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let item =
    nth_structure_item root 0
    |> require_some ~msg:"expected let declaration"
  in
  let decl =
    match Ast.StructureItem.view item with
    | Ast.StructureItem.Let decl -> decl
    | _ -> panic "expected let declaration"
  in
  let bindings = ref [] in
  iter_fold
    Ast.LetDeclaration.fold_binding
    decl
    ~fn:(fun binding -> bindings := binding :: !bindings);
  match List.reverse !bindings with
  | [ f_binding; g_binding ] ->
      let assert_trailing_sequence binding expected_name =
        let pattern =
          pattern_of_binding binding
          |> Result.expect ~msg:"expected binding pattern"
        in
        (
          match Ast.Pattern.view pattern with
          | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident expected_name
          | _ -> panic "expected binding head ident pattern"
        );
        let parameters = ref [] in
        iter_fold
          Ast.LetBinding.fold_parameter
          binding
          ~fn:(fun parameter -> parameters := parameter :: !parameters);
        if List.is_empty !parameters then
          panic "expected function binding parameters";
        let body =
          body_of_binding binding
          |> Result.expect ~msg:"expected binding body"
        in
        match Ast.Expr.view body with
        | Ast.Expr.Sequence { left = _; right = None } -> Ok ()
        | _ -> Error "expected trailing sequence expression body"
      in
      let* () = assert_trailing_sequence f_binding "f" in
      assert_trailing_sequence g_binding "g"
  | _ -> Error "expected two recursive bindings"

let test_module_declaration_member_views = fun _ctx ->
  let root =
    parse_ml "module rec M : S = A and N : T = B\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let item =
    nth_structure_item root 0
    |> require_some ~msg:"expected module item"
  in
  match Ast.StructureItem.view item with
  | Ast.StructureItem.Module decl ->
      Test.assert_equal ~expected:true ~actual:(Ast.ModuleDeclaration.is_recursive decl);
      let count = ref 0 in
      let names = ref [] in
      let shells = ref [] in
      let body_shapes = ref [] in
      iter_fold
        Ast.ModuleDeclaration.fold_member
        decl
        ~fn:(fun member ->
          count := !count + 1;
          let name =
            Ast.ModuleDeclaration.Member.name member
            |> require_some ~msg:"expected member name"
          in
          let shell =
            Ast.ModuleDeclaration.Member.child_token_at member 0
            |> require_some ~msg:"expected member shell token"
          in
          let has_module_type =
            match Ast.ModuleDeclaration.Member.module_type member with
            | Some _ -> true
            | None -> false
          in
          let has_module_expr =
            match Ast.ModuleDeclaration.Member.module_expr member with
            | Some _ -> true
            | None -> false
          in
          names := Ast.Ident.text name :: !names;
          shells := Ast.Token.text shell :: !shells;
          body_shapes := (has_module_type, has_module_expr) :: !body_shapes);
      Test.assert_equal ~expected:2 ~actual:!count;
      Test.assert_equal ~expected:[ "M"; "N" ] ~actual:(List.reverse !names);
      Test.assert_equal ~expected:[ "module"; "and" ] ~actual:(List.reverse !shells);
      Test.assert_equal
        ~expected:[ (true, true); (true, true); ]
        ~actual:(List.reverse !body_shapes);
      Ok ()
  | _ -> Error "expected module declaration"

let test_signature_module_typeof_declaration = fun _ctx ->
  let root =
    parse_mli "module Http1 : module type of Foo.Bar\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let item =
    nth_signature_item root 0
    |> require_some ~msg:"expected module item"
  in
  match Ast.SignatureItem.view item with
  | Ast.SignatureItem.Module decl ->
      let member =
        Ast.ModuleDeclaration.fold_members
          decl
          None
          (fun acc member ->
            match acc with
            | Some _ -> acc
            | None -> Some member)
        |> require_some ~msg:"expected module member"
      in
      let separator =
        Ast.ModuleDeclaration.separator_token decl
        |> require_some ~msg:"expected module separator"
      in
      let module_type =
        Ast.ModuleDeclaration.Member.module_type member
        |> require_some ~msg:"expected module type body"
      in
      Test.assert_equal ~expected:":" ~actual:(Ast.Token.text separator);
      Test.assert_equal ~expected:SyntaxKind.TYPEOF_MODULE_TYPE ~actual:(Ast.Node.kind module_type);
      (
        match Ast.ModuleDeclaration.body decl with
        | Ast.ModuleDeclaration.Type { body } -> (
            match Ast.ModuleTypeExpr.view body with
            | Ast.ModuleTypeExpr.Typeof _ -> ()
            | _ -> panic "expected module type of declaration body"
          )
        | _ -> panic "expected module type of declaration body"
      );
      Ok ()
  | _ -> Error "expected module declaration"

let test_module_type_declaration_tokens = fun _ctx ->
  let root =
    parse_mli "module type S = Foo.S\nmodule type Empty = sig end\nmodule type Abstract\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let first_item =
    nth_signature_item root 0
    |> require_some ~msg:"expected first module type item"
  in
  let second_item =
    nth_signature_item root 1
    |> require_some ~msg:"expected second module type item"
  in
  let third_item =
    nth_signature_item root 2
    |> require_some ~msg:"expected third module type item"
  in
  (
    match Ast.SignatureItem.view first_item with
    | Ast.SignatureItem.ModuleType decl ->
        let name =
          Ast.ModuleTypeDeclaration.name decl
          |> require_some ~msg:"expected module type name"
        in
        let equals =
          Ast.ModuleTypeDeclaration.equals_token decl
          |> require_some ~msg:"expected module type equals token"
        in
        let child_kinds = ref [] in
        let ident =
          Ast.ModuleTypeDeclaration.body_ident decl
          |> require_some ~msg:"expected module type body ident"
        in
        let segments =
          Ast.Ident.fold_segment
            ident
            ~init:[]
            ~fn:(fun token segments -> Ast.Continue (Ast.Token.text token :: segments))
          |> List.reverse
        in
        iter_fold
          Ast.Node.fold_child_node
          (Ast.ModuleTypeDeclaration.as_node decl)
          ~fn:(fun child -> child_kinds := Ast.Node.kind child :: !child_kinds);
        Test.assert_equal ~expected:"S" ~actual:(Ast.Ident.text name);
        Test.assert_equal ~expected:"=" ~actual:(Ast.Token.text equals);
        Test.assert_equal
          ~expected:[ SyntaxKind.MODULE_TYPE_DECL_HEAD; SyntaxKind.MODULE_TYPE_DECL_BODY ]
          ~actual:(List.reverse !child_kinds);
        (
          match Ast.ModuleTypeDeclaration.body decl with
          | Ast.ModuleTypeDeclaration.Manifest { body } -> (
              match Ast.ModuleTypeExpr.view body with
              | Ast.ModuleTypeExpr.Ident { ident } ->
                  Test.assert_equal ~expected:SyntaxKind.IDENT ~actual:(Ast.Ident.kind ident)
              | _ -> panic "expected ident module type body"
            )
          | _ -> panic "expected ident module type body"
        );
        Test.assert_equal ~expected:[ "Foo"; "S" ] ~actual:segments
    | _ -> panic "expected first module type declaration"
  );
  (
    match Ast.SignatureItem.view second_item with
    | Ast.SignatureItem.ModuleType decl -> (
        match Ast.ModuleTypeDeclaration.body decl with
        | Ast.ModuleTypeDeclaration.Manifest { body } -> (
            match Ast.ModuleTypeExpr.view body with
            | Ast.ModuleTypeExpr.Signature { body } ->
                Test.assert_equal
                  ~expected:SyntaxKind.SIGNATURE_MODULE_TYPE
                  ~actual:(Ast.Node.kind body)
            | _ -> panic "expected signature module type body"
          )
        | _ -> panic "expected signature module type body"
      )
    | _ -> panic "expected second module type declaration"
  );
  (
    match Ast.SignatureItem.view third_item with
    | Ast.SignatureItem.ModuleType decl ->
        Test.assert_equal
          ~expected:Ast.ModuleTypeDeclaration.Abstract
          ~actual:(Ast.ModuleTypeDeclaration.body decl)
    | _ -> panic "expected third module type declaration"
  );
  Ok ()

let test_module_type_with_constraint_views = fun _ctx ->
  let root =
    parse_mli "module type S = Driver with type config = int and module Nested = Impl\n"
    |> Result.expect ~msg:"expected parse interface"
  in
  let item =
    nth_signature_item root 0
    |> require_some ~msg:"expected module type item"
  in
  match Ast.SignatureItem.view item with
  | Ast.SignatureItem.ModuleType decl ->
      (
        match Ast.ModuleTypeDeclaration.body decl with
        | Ast.ModuleTypeDeclaration.Manifest { body } -> (
            match Ast.ModuleTypeExpr.view body with
            | Ast.ModuleTypeExpr.With { body; constraints; _ } ->
                Test.assert_equal ~expected:SyntaxKind.WITH_MODULE_TYPE ~actual:(Ast.Node.kind body);
                Test.assert_equal ~expected:2 ~actual:(Vector.length constraints)
            | _ -> panic "expected constrained module type body"
          )
        | _ -> panic "expected constrained module type body"
      );
      (
        match Ast.ModuleTypeDeclaration.base_module_type decl with
        | Some base ->
            let ident =
              Ast.cast_result_to_option (Ast.Ident.cast base)
              |> require_some ~msg:"expected constrained base ident"
            in
            let name =
              Ast.Ident.last_segment ident
              |> require_some ~msg:"expected constrained base name"
            in
            Test.assert_equal ~expected:"Driver" ~actual:(Ast.Token.text name)
        | None -> panic "expected constrained base module type"
      );
      let seen = ref 0 in
      iter_fold
        Ast.ModuleTypeDeclaration.fold_constraint
        decl
        ~fn:(fun constraint_ ->
          let index = !seen in
          seen := !seen + 1;
          match Ast.ModuleTypeConstraint.view constraint_ with
          | Ast.ModuleTypeConstraint.Type { ident; operator; body } when Int.equal index 0 ->
              let ident_name =
                Ast.Ident.last_segment ident
                |> require_some ~msg:"expected type ident name"
              in
              let body_token =
                Ast.Node.first_descendant_token (Ast.TypeExpr.as_node body)
                |> require_some ~msg:"expected type constraint body token"
              in
              Test.assert_equal ~expected:"config" ~actual:(Ast.Token.text ident_name);
              Test.assert_equal ~expected:"=" ~actual:(Ast.Token.text operator);
              Test.assert_equal ~expected:"int" ~actual:(Ast.Token.text body_token)
          | Ast.ModuleTypeConstraint.Module { ident; operator; body } when Int.equal index 1 ->
              let ident_name =
                Ast.Ident.last_segment ident
                |> require_some ~msg:"expected module ident name"
              in
              let body_token =
                Ast.Node.first_descendant_token body
                |> require_some ~msg:"expected module constraint body token"
              in
              Test.assert_equal ~expected:"Nested" ~actual:(Ast.Token.text ident_name);
              Test.assert_equal ~expected:"=" ~actual:(Ast.Token.text operator);
              Test.assert_equal ~expected:"Impl" ~actual:(Ast.Token.text body_token)
          | Ast.ModuleTypeConstraint.Unknown _ -> panic "unexpected module type constraint shape"
          | _ -> panic "unexpected module type constraint ordering");
      Test.assert_equal ~expected:2 ~actual:!seen;
      Ok ()
  | _ -> Error "expected module type declaration"

let test_binding_type_annotation_view = fun _ctx ->
  let root =
    parse_ml "let x : int = 1\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
  in
  let annotation =
    Ast.LetBinding.type_annotation binding
    |> require_some ~msg:"expected binding type annotation"
  in
  match Ast.TypeExpr.view annotation with
  | Ast.TypeExpr.Ident { ident } ->
      assert_last_ident_text ident "int";
      Ok ()
  | _ -> Error "expected binding ident type annotation"

let test_function_binding_return_annotation_view = fun _ctx ->
  let root =
    parse_ml "let map (type a b) (iter: a t) ~(fn:a -> b) : b t = failwith \"todo\"\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
  in
  let annotation =
    Ast.LetBinding.type_annotation binding
    |> require_some ~msg:"expected function binding return annotation"
  in
  match Ast.TypeExpr.view annotation with
  | Ast.TypeExpr.Apply { ident; args } ->
      let argument = vector_first args ~msg:"expected function return type argument" in
      assert_type_ident_last_segment argument "b";
      assert_last_ident_text ident "t";
      Ok ()
  | _ -> Error "expected binding return apply type annotation"

let test_unit_parameter_binding_return_annotation_view = fun _ctx ->
  let root =
    parse_ml {ocaml|let create_counters (): runtime_counters = { steals = Atomic.make 0 }
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
  in
  let annotation =
    Ast.LetBinding.return_type_annotation binding
    |> require_some ~msg:"expected function binding return annotation"
  in
  match Ast.TypeExpr.view annotation with
  | Ast.TypeExpr.Ident { ident } ->
      assert_last_ident_text ident "runtime_counters";
      Ok ()
  | _ -> Error "expected binding return ident type annotation"

let test_labeled_parameter_binding_return_annotation_view = fun _ctx ->
  let root =
    parse_ml {ocaml|let make ~start ~finish ~steps : color array = steps
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
  in
  let parameter_count = Ast.LetBinding.parameter_count binding in
  if parameter_count != 3 then
    Error ("expected three labeled parameters, got " ^ Int.to_string parameter_count)
  else
    let annotation =
      Ast.LetBinding.return_type_annotation binding
      |> require_some ~msg:"expected labeled function binding return annotation"
    in
    match Ast.TypeExpr.view annotation with
    | Ast.TypeExpr.Apply { ident; args } ->
        let argument = vector_first args ~msg:"expected return type argument" in
        assert_type_ident_last_segment argument "color";
        assert_last_ident_text ident "array";
        Ok ()
    | _ -> Error "expected labeled binding return apply type annotation"

let test_parenthesized_parameter_annotation_is_not_return_annotation = fun _ctx ->
  let root =
    parse_ml {ocaml|let keep_pattern (x : int) = x
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected first structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected let binding"
  in
  match Ast.LetBinding.type_annotation binding with
  | None -> Ok ()
  | Some _ -> Error "expected parameter annotation not to become binding return annotation"

let test_ast_views_normalize_redundant_parentheses = fun _ctx ->
  let root =
    parse_ml
      {ocaml|let call = g (((f 1)))
let typed: (((int))) list = x
let render = function | (((Some (((item)))))) -> item
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let call_binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected call item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected call binding"
  in
  let call_body =
    body_of_binding call_binding
    |> Result.expect ~msg:"expected call body"
  in
  let* () =
    match Ast.Expr.view call_body with
    | Ast.Expr.Apply { argument; _ } ->
        Test.assert_equal
          ~expected:SyntaxKind.APPLY_EXPR
          ~actual:(Ast.Node.kind (Ast.Expr.as_node argument));
        Ok ()
    | _ -> Error "expected outer call expression"
  in
  let typed_binding =
    nth_structure_item root 1
    |> require_some ~msg:"expected typed item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected typed binding"
  in
  let annotation =
    Ast.LetBinding.type_annotation typed_binding
    |> require_some ~msg:"expected typed binding annotation"
  in
  let* () =
    match Ast.TypeExpr.view annotation with
    | Ast.TypeExpr.Apply { args; _ } ->
        let arg = vector_first args ~msg:"expected applied type argument" in
        Test.assert_equal
          ~expected:SyntaxKind.PATH_TYPE
          ~actual:(Ast.Node.kind (Ast.TypeExpr.as_node arg));
        Ok ()
    | _ -> Error "expected applied type annotation"
  in
  let render_binding =
    nth_structure_item root 2
    |> require_some ~msg:"expected render item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected render binding"
  in
  let render_body =
    body_of_binding render_binding
    |> Result.expect ~msg:"expected render body"
  in
  match Ast.Expr.view render_body with
  | Ast.Expr.Fun { body = Ast.Expr.Body_cases { first_case }; _ } -> (
      match Ast.MatchCase.view first_case with
      | Ast.MatchCase.Case { pattern; _ } -> (
          match Ast.Pattern.view pattern with
          | Ast.Pattern.Constructor { payload = Some payload; _ } ->
              Test.assert_equal
                ~expected:SyntaxKind.PATH_PATTERN
                ~actual:(Ast.Node.kind (Ast.Pattern.as_node payload));
              Ok ()
          | _ -> Error "expected constructor pattern"
        )
      | Ast.MatchCase.Unknown _ -> Error "expected function case pattern"
    )
  | _ -> Error "expected function expression"

let last_ident_text = fun ident ->
  let token =
    Ast.Ident.last_segment ident
    |> require_some ~msg:"expected ident segment"
  in
  Ast.Token.text token

let test_record_views = fun _ctx ->
  let source =
    {ocaml|let record = { x = 1; y }
let updated = { base with x = 2; y }
let scoped = Lockfile.{ name = package.name; version = None }
let { x; y = z; _ } = record
|ocaml}
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let record_expr =
    nth_structure_item root 0
    |> require_some ~msg:"expected record item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected record binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected record expression"
  in
  let record_view =
    Ast.cast_result_to_option (Ast.RecordExpr.cast record_expr)
    |> require_some ~msg:"expected record expression view"
  in
  let record_fields = Vector.with_capacity ~size:(Ast.RecordExpr.field_count record_view) in
  iter_fold
    Ast.RecordExpr.fold_field
    record_view
    ~fn:(fun field ->
      match field with
      | Ast.RecordExprField { ident; value; _ } ->
          Vector.push record_fields ~value:(last_ident_text ident, Option.is_some value)
      | Ast.UnknownRecordExprField _ -> panic "expected record expr field");
  Test.assert_equal ~expected:[ ("x", true); ("y", false); ] ~actual:(vector_to_list record_fields);
  let update_expr =
    nth_structure_item root 1
    |> require_some ~msg:"expected update item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected update binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected update expression"
  in
  let update_view =
    Ast.cast_result_to_option (Ast.RecordExpr.cast update_expr)
    |> require_some ~msg:"expected record update view"
  in
  (
    match Ast.RecordExpr.base update_view with
    | Some base -> (
        match Ast.Expr.view base with
        | Ast.Expr.Ident { ident } ->
            Test.assert_equal ~expected:"base" ~actual:(last_ident_text ident)
        | _ -> panic "expected record update base ident"
      )
    | None -> panic "expected record update base"
  );
  let update_fields = Vector.with_capacity ~size:(Ast.RecordExpr.field_count update_view) in
  iter_fold
    Ast.RecordExpr.fold_field
    update_view
    ~fn:(fun field ->
      match field with
      | Ast.RecordExprField { ident; value; _ } ->
          Vector.push update_fields ~value:(last_ident_text ident, Option.is_some value)
      | Ast.UnknownRecordExprField _ -> panic "expected update field");
  Test.assert_equal ~expected:[ ("x", true); ("y", false); ] ~actual:(vector_to_list update_fields);
  let scoped_expr =
    nth_structure_item root 2
    |> require_some ~msg:"expected scoped record item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected scoped record binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected scoped record expression"
  in
  let scoped_local_open =
    Ast.cast_result_to_option (Ast.LocalOpenExpr.cast scoped_expr)
    |> require_some ~msg:"expected local open record expression"
  in
  (
    match Ast.LocalOpenExpr.view scoped_local_open with
    | Ast.LocalOpenExpr.Delimited { module_ident; body; _ } ->
        Test.assert_equal ~expected:"Lockfile" ~actual:(last_ident_text module_ident);
        let scoped_record =
          Ast.cast_result_to_option (Ast.RecordExpr.cast body)
          |> require_some ~msg:"expected scoped record body"
        in
        let scoped_fields = Vector.with_capacity ~size:(Ast.RecordExpr.field_count scoped_record) in
        iter_fold
          Ast.RecordExpr.fold_field
          scoped_record
          ~fn:(fun field ->
            match field with
            | Ast.RecordExprField { ident; value; _ } ->
                Vector.push scoped_fields ~value:(last_ident_text ident, Option.is_some value)
            | Ast.UnknownRecordExprField _ -> panic "expected scoped field");
        Test.assert_equal
          ~expected:[ ("name", true); ("version", true); ]
          ~actual:(vector_to_list scoped_fields)
    | _ -> panic "expected delimited local open record expression"
  );
  let record_pattern =
    nth_structure_item root 3
    |> require_some ~msg:"expected record pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected record pattern binding"
    |> pattern_of_binding
    |> Result.expect ~msg:"expected record pattern"
  in
  let pattern_view =
    Ast.cast_result_to_option (Ast.RecordPattern.cast record_pattern)
    |> require_some ~msg:"expected record pattern view"
  in
  let pattern_fields = Vector.with_capacity ~size:(Ast.RecordPattern.field_count pattern_view) in
  iter_fold
    Ast.RecordPattern.fold_field
    pattern_view
    ~fn:(fun field ->
      match field with
      | Ast.RecordPatternField { ident; pattern; _ } ->
          Vector.push pattern_fields ~value:(last_ident_text ident, Option.is_some pattern)
      | Ast.UnknownRecordPatternField _ -> panic "expected record pattern field");
  Test.assert_equal ~expected:[ ("x", false); ("y", true); ] ~actual:(vector_to_list pattern_fields);
  let wildcard =
    Ast.RecordPattern.open_wildcard pattern_view
    |> require_some ~msg:"expected open record wildcard"
  in
  Test.assert_equal ~expected:"_" ~actual:(Ast.Token.text wildcard);
  Ok ()

let test_record_field_special_form_boundaries = fun _ctx ->
  let source =
    "let generator = {\n  run =\n    fun rnd size ->\n      let rec try_gen () =\n        match build rnd size with\n        | Some value -> value\n        | None -> try_gen ()\n      in\n      try_gen ();\n}\nlet next = 1\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let trailing_item =
    nth_structure_item root 1
    |> require_some ~msg:"expected trailing let item"
  in
  match Ast.StructureItem.view trailing_item with
  | Ast.StructureItem.Let _ -> Ok ()
  | _ -> Error "expected trailing let declaration after record field fun body"

let binding_pattern_text = fun binding ->
  let pattern =
    pattern_of_binding binding
    |> Result.expect ~msg:"expected binding pattern"
  in
  match Ast.Pattern.view pattern with
  | Ast.Pattern.Ident { ident } -> last_ident_text ident
  | _ -> panic "expected ident binding pattern"

let binding_body_ident_text = fun binding ->
  let body =
    body_of_binding binding
    |> Result.expect ~msg:"expected binding body"
  in
  match Ast.Expr.view body with
  | Ast.Expr.Ident { ident } -> last_ident_text ident
  | _ -> panic "expected ident binding body"

let test_binding_operator_views = fun _ctx ->
  let root =
    parse_ml "let both = let+ x = a and+ y = b in pair x y\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let expr =
    nth_structure_item root 0
    |> require_some ~msg:"expected binding operator item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected outer let binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected binding operator expression"
  in
  let binding_operator =
    Ast.cast_result_to_option (Ast.BindingOperatorExpr.cast expr)
    |> require_some ~msg:"expected binding operator view"
  in
  let clauses = ref [] in
  iter_fold
    Ast.BindingOperatorExpr.fold_clause
    binding_operator
    ~fn:(fun clause ->
      let keyword =
        clause.Ast.BindingOperatorExpr.keyword
        |> require_some ~msg:"expected binding operator keyword"
        |> Ast.Token.text
      in
      let operator =
        clause.Ast.BindingOperatorExpr.operator
        |> require_some ~msg:"expected binding operator suffix"
        |> Ast.Token.text
      in
      clauses := (
        keyword,
        operator,
        binding_pattern_text clause.Ast.BindingOperatorExpr.binding,
        binding_body_ident_text clause.Ast.BindingOperatorExpr.binding
      )
      :: !clauses);
  Test.assert_equal
    ~expected:[ ("let", "+", "x", "a"); ("and", "+", "y", "b"); ]
    ~actual:(List.reverse !clauses);
  let in_token =
    Ast.BindingOperatorExpr.in_token binding_operator
    |> require_some ~msg:"expected binding operator in token"
  in
  Test.assert_equal ~expected:"in" ~actual:(Ast.Token.text in_token);
  (
    match Ast.BindingOperatorExpr.body binding_operator with
    | Some body -> (
        match Ast.Expr.view body with
        | Ast.Expr.Apply _ -> Ok ()
        | _ -> Error "expected binding operator body application"
      )
    | None -> Error "expected binding operator body"
  )

let local_open_pattern_ident_text = fun pattern ->
  match Ast.LocalOpenPattern.view pattern with
  | Ast.LocalOpenPattern.Delimited { module_ident; _ } -> Ast.Ident.text module_ident
  | Ast.LocalOpenPattern.Unknown _ -> panic "expected local-open pattern view"

let first_class_module_ident_text = fun expr ->
  Ast.FirstClassModuleExpr.module_ident expr
  |> require_some ~msg:"expected first-class module ident"
  |> Ast.Ident.text

let first_class_module_ascription_text = fun expr ->
  Ast.FirstClassModuleExpr.ascription_ident expr
  |> require_some ~msg:"expected first-class module ascription ident"
  |> Ast.Ident.text

let first_class_module_pattern_ascription_text = fun pattern ->
  Ast.FirstClassModulePattern.ascription_ident pattern
  |> require_some ~msg:"expected first-class module pattern ascription ident"
  |> Ast.Ident.text

let let_module_body_ident_text = fun expr ->
  Ast.LetModuleExpr.module_body_ident expr
  |> require_some ~msg:"expected let module body ident"
  |> Ast.Ident.text

let let_exception_payload_tokens = fun expr ->
  let tokens = ref [] in
  iter_fold
    Ast.LetExceptionExpr.fold_payload_token
    expr
    ~fn:(fun token -> tokens := Ast.Token.text token :: !tokens);
  List.reverse !tokens

let test_local_open_views = fun _ctx ->
  let source =
    "let value = let open Foo.Bar in result\nlet Foo.Bar.(x) = value\nlet Frame.{ payload } = frame\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let local_open_expr =
    nth_structure_item root 0
    |> require_some ~msg:"expected local open expression item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected local open binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected local open body"
  in
  let local_open =
    Ast.cast_result_to_option (Ast.LocalOpenExpr.cast local_open_expr)
    |> require_some ~msg:"expected local open expression view"
  in
  (
    match Ast.LocalOpenExpr.view local_open with
    | Ast.LocalOpenExpr.LetOpen {
        let_token;
        open_token;
        module_ident;
        in_token;
        body;
        _;
      } ->
        Test.assert_equal ~expected:"let" ~actual:(Ast.Token.text let_token);
        Test.assert_equal ~expected:"open" ~actual:(Ast.Token.text open_token);
        Test.assert_equal ~expected:"Bar" ~actual:(last_ident_text module_ident);
        Test.assert_equal ~expected:"in" ~actual:(Ast.Token.text in_token);
        (
          match Ast.Expr.view body with
          | Ast.Expr.Ident { ident } ->
              Test.assert_equal ~expected:"result" ~actual:(last_ident_text ident)
          | _ -> panic "expected local open body ident"
        )
    | _ -> panic "expected complete let open expression"
  );
  let local_open_pattern =
    nth_structure_item root 1
    |> require_some ~msg:"expected local open pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected local open pattern binding"
    |> pattern_of_binding
    |> Result.expect ~msg:"expected local open pattern"
  in
  let local_open_pattern =
    Ast.cast_result_to_option (Ast.LocalOpenPattern.cast local_open_pattern)
    |> require_some ~msg:"expected local open pattern view"
  in
  let* () =
    match Ast.LocalOpenPattern.view local_open_pattern with
    | Ast.LocalOpenPattern.Delimited { dot_token; pattern; _ } ->
        Test.assert_equal
          ~expected:"Foo.Bar"
          ~actual:(local_open_pattern_ident_text local_open_pattern);
        Test.assert_equal ~expected:"." ~actual:(Ast.Token.text dot_token);
        (
          match Ast.Pattern.view pattern with
          | Ast.Pattern.Ident { ident } ->
              Test.assert_equal ~expected:"x" ~actual:(last_ident_text ident);
              Ok ()
          | _ -> Error "expected local open inner ident pattern"
        )
    | Ast.LocalOpenPattern.Unknown _ -> Error "expected local-open pattern view"
  in
  let local_open_record_pattern =
    nth_structure_item root 2
    |> require_some ~msg:"expected local open record pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected local open record pattern binding"
    |> pattern_of_binding
    |> Result.expect ~msg:"expected local open record pattern"
  in
  let local_open_record_pattern =
    Ast.cast_result_to_option (Ast.LocalOpenPattern.cast local_open_record_pattern)
    |> require_some ~msg:"expected local open record pattern view"
  in
  Test.assert_equal
    ~expected:"Frame"
    ~actual:(local_open_pattern_ident_text local_open_record_pattern);
  match Ast.LocalOpenPattern.view local_open_record_pattern with
  | Ast.LocalOpenPattern.Delimited { opening_token; closing_token; pattern; _ } -> (
      Test.assert_equal ~expected:"{" ~actual:(Ast.Token.text opening_token);
      Test.assert_equal ~expected:"}" ~actual:(Ast.Token.text closing_token);
      match Ast.Pattern.view pattern with
      | Ast.Pattern.Record { fields; open_wildcard } ->
          Test.assert_equal ~expected:1 ~actual:(Vector.length fields);
          Test.assert_equal ~expected:false ~actual:(Option.is_some open_wildcard);
          let field = vector_first fields ~msg:"expected record pattern field" in
          (
            match field with
            | Ast.RecordPatternField { ident; _ } ->
                Test.assert_equal ~expected:"payload" ~actual:(last_ident_text ident);
                Ok ()
            | Ast.UnknownRecordPatternField _ -> Error "expected record pattern field ident"
          )
      | _ -> Error "expected local open record inner pattern"
    )
  | Ast.LocalOpenPattern.Unknown _ -> Error "expected local open record inner pattern"

let test_local_open_argument_views = fun _ctx ->
  let source =
    "let value = send pid Server.(Telemetry (Stop { reply_to = self (); request_id }))\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let body =
    nth_structure_item root 0
    |> require_some ~msg:"expected local open application item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected local open application binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected local open application body"
  in
  let rec application_parts expr args =
    match Ast.Expr.view expr with
    | Ast.Expr.Apply { callee; argument } -> application_parts callee (argument :: args)
    | _ -> (expr, args)
  in
  let (callee, arguments) = application_parts body [] in
  (
    match Ast.Expr.view callee with
    | Ast.Expr.Ident { ident } -> assert_last_ident_text ident "send"
    | _ -> panic "expected send callee"
  );
  match arguments with
  | [ pid_arg; local_open_arg ] ->
      (
        match Ast.Expr.view pid_arg with
        | Ast.Expr.Ident { ident } -> assert_last_ident_text ident "pid"
        | _ -> panic "expected pid argument"
      );
      let local_open =
        Ast.cast_result_to_option (Ast.LocalOpenExpr.cast local_open_arg)
        |> require_some ~msg:"expected local open argument"
      in
      (
        match Ast.LocalOpenExpr.view local_open with
        | Ast.LocalOpenExpr.Delimited {
            module_ident;
            opening_token;
            body = inner_body;
            closing_token;
            _;
          } ->
            Test.assert_equal ~expected:"Server" ~actual:(last_ident_text module_ident);
            Test.assert_equal ~expected:"(" ~actual:(Ast.Token.text opening_token);
            Test.assert_equal ~expected:")" ~actual:(Ast.Token.text closing_token);
            (
              match Ast.Expr.view inner_body with
              | Ast.Expr.Constructor { payload = Some _; _ } -> Ok ()
              | _ -> Error "expected local open inner constructor payload"
            )
        | _ -> Error "expected complete delimited local open argument"
      )
  | _ -> Error "expected send application arguments"

let test_local_open_labeled_argument_views = fun _ctx ->
  let source =
    "let store = Contentstore.create ~root:Path.(tmpdir / Path.v \"cache\") ~ns:(namespace parts)\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let body =
    nth_structure_item root 0
    |> require_some ~msg:"expected labeled local open item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected labeled local open binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected labeled local open body"
  in
  let rec application_parts expr args =
    match Ast.Expr.view expr with
    | Ast.Expr.Apply { callee; argument } -> application_parts callee (argument :: args)
    | _ -> (expr, args)
  in
  let (callee, arguments) = application_parts body [] in
  (
    match Ast.Expr.view callee with
    | Ast.Expr.Ident { ident } -> assert_last_ident_text ident "create"
    | _ -> panic "expected create callee"
  );
  match arguments with
  | [ root_arg; ns_arg ] ->
      let* () =
        match assert_labeled_argument root_arg "root" with
        | Some value ->
            let local_open =
              Ast.cast_result_to_option (Ast.LocalOpenExpr.cast value)
              |> require_some ~msg:"expected local open root value"
            in
            (
              match Ast.LocalOpenExpr.view local_open with
              | Ast.LocalOpenExpr.Delimited { module_ident; body = inner_body; _ } ->
                  Test.assert_equal ~expected:"Path" ~actual:(last_ident_text module_ident);
                  (
                    match Ast.Expr.view inner_body with
                    | Ast.Expr.Infix _ -> Ok ()
                    | _ -> Error "expected infix local open body"
                  )
              | _ -> Error "expected complete labeled local open"
            )
        | None -> Error "expected labeled root argument value"
      in
      (
        match assert_labeled_argument ns_arg "ns" with
        | Some _ -> Ok ()
        | None -> Error "expected labeled ns argument value"
      )
  | _ -> Error "expected labeled application arguments"

let test_first_class_module_views = fun _ctx ->
  let source =
    "let packed = (module Foo.Bar)\nlet typed = (module Foo : S.T)\nlet advanced = (module Foo : S with type t = item)\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let packed =
    nth_structure_item root 0
    |> require_some ~msg:"expected packed module item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected packed module binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected packed module body"
  in
  let packed =
    Ast.cast_result_to_option (Ast.FirstClassModuleExpr.cast packed)
    |> require_some ~msg:"expected first-class module view"
  in
  Test.assert_equal
    ~expected:true
    ~actual:(Option.is_some (Ast.FirstClassModuleExpr.module_ident packed));
  Test.assert_equal
    ~expected:Ast.FirstClassModuleExpr.NoAscription
    ~actual:(Ast.FirstClassModuleExpr.ascription packed);
  Test.assert_equal ~expected:"Foo.Bar" ~actual:(first_class_module_ident_text packed);
  let typed =
    nth_structure_item root 1
    |> require_some ~msg:"expected typed module item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected typed module binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected typed module body"
  in
  let typed =
    Ast.cast_result_to_option (Ast.FirstClassModuleExpr.cast typed)
    |> require_some ~msg:"expected typed first-class module view"
  in
  Test.assert_equal
    ~expected:true
    ~actual:(Option.is_some (Ast.FirstClassModuleExpr.module_ident typed));
  Test.assert_equal
    ~expected:Ast.FirstClassModuleExpr.IdentAscription
    ~actual:(Ast.FirstClassModuleExpr.ascription typed);
  Test.assert_equal ~expected:"Foo" ~actual:(first_class_module_ident_text typed);
  Test.assert_equal ~expected:"S.T" ~actual:(first_class_module_ascription_text typed);
  let colon =
    Ast.FirstClassModuleExpr.colon_token typed
    |> require_some ~msg:"expected first-class module colon"
  in
  Test.assert_equal ~expected:":" ~actual:(Ast.Token.text colon);
  let advanced =
    nth_structure_item root 2
    |> require_some ~msg:"expected advanced module item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected advanced module binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected advanced module body"
  in
  let advanced =
    Ast.cast_result_to_option (Ast.FirstClassModuleExpr.cast advanced)
    |> require_some ~msg:"expected advanced first-class module view"
  in
  Test.assert_equal
    ~expected:true
    ~actual:(Option.is_some (Ast.FirstClassModuleExpr.module_ident advanced));
  Test.assert_equal
    ~expected:Ast.FirstClassModuleExpr.UnsupportedAscription
    ~actual:(Ast.FirstClassModuleExpr.ascription advanced);
  Ok ()

let test_let_module_expression_views = fun _ctx ->
  let source =
    "let value = let module M = Foo.Bar in result\nlet empty = let module Empty = struct end in done_\nlet nested = let module ByteIter = struct\n  let next = fun state ->\n    let scratch = state in\n    scratch\nend in consume\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let value_expr =
    nth_structure_item root 0
    |> require_some ~msg:"expected let module item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected outer binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected let module body"
  in
  let module_expr =
    Ast.cast_result_to_option (Ast.LetModuleExpr.cast value_expr)
    |> require_some ~msg:"expected let module expression view"
  in
  let module_token =
    Ast.LetModuleExpr.module_token module_expr
    |> require_some ~msg:"expected module token"
  in
  let name =
    Ast.LetModuleExpr.name module_expr
    |> require_some ~msg:"expected module name"
  in
  let equals =
    Ast.LetModuleExpr.equals_token module_expr
    |> require_some ~msg:"expected let module equals"
  in
  let in_token =
    Ast.LetModuleExpr.in_token module_expr
    |> require_some ~msg:"expected let module in"
  in
  Test.assert_equal ~expected:"module" ~actual:(Ast.Token.text module_token);
  Test.assert_equal ~expected:"M" ~actual:(Ast.Ident.text name);
  Test.assert_equal ~expected:"=" ~actual:(Ast.Token.text equals);
  Test.assert_equal ~expected:"in" ~actual:(Ast.Token.text in_token);
  Test.assert_equal
    ~expected:Ast.LetModuleExpr.Ident
    ~actual:(Ast.LetModuleExpr.module_body module_expr);
  Test.assert_equal ~expected:"Foo.Bar" ~actual:(let_module_body_ident_text module_expr);
  (
    match Ast.LetModuleExpr.body module_expr with
    | Some body -> (
        match Ast.Expr.view body with
        | Ast.Expr.Ident { ident } ->
            Test.assert_equal ~expected:"result" ~actual:(last_ident_text ident)
        | _ -> panic "expected let module body ident"
      )
    | None -> panic "expected let module expression body"
  );
  let empty_expr =
    nth_structure_item root 1
    |> require_some ~msg:"expected empty let module item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected empty outer binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected empty let module body"
  in
  let empty_module =
    Ast.cast_result_to_option (Ast.LetModuleExpr.cast empty_expr)
    |> require_some ~msg:"expected empty let module view"
  in
  Test.assert_equal
    ~expected:Ast.LetModuleExpr.Struct
    ~actual:(Ast.LetModuleExpr.module_body empty_module);
  let nested_expr =
    nth_structure_item root 2
    |> require_some ~msg:"expected nested let module item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected nested outer binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected nested let module body"
  in
  let nested_module =
    Ast.cast_result_to_option (Ast.LetModuleExpr.cast nested_expr)
    |> require_some ~msg:"expected nested let module view"
  in
  let nested_body =
    Ast.LetModuleExpr.module_body_node nested_module
    |> require_some ~msg:"expected nested module body node"
  in
  Test.assert_equal ~expected:SyntaxKind.STRUCT_MODULE_EXPR ~actual:(Ast.Node.kind nested_body);
  Ok ()

let test_let_exception_expression_views = fun _ctx ->
  let source =
    "let value = let exception Local of int * Foo.t in result\nlet bare = let exception Done in done_\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let value_expr =
    nth_structure_item root 0
    |> require_some ~msg:"expected let exception item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected outer binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected let exception body"
  in
  let exception_expr =
    Ast.cast_result_to_option (Ast.LetExceptionExpr.cast value_expr)
    |> require_some ~msg:"expected let exception expression view"
  in
  let exception_token =
    Ast.LetExceptionExpr.exception_token exception_expr
    |> require_some ~msg:"expected exception token"
  in
  let name =
    Ast.LetExceptionExpr.name exception_expr
    |> require_some ~msg:"expected exception name"
  in
  let of_token =
    Ast.LetExceptionExpr.of_token exception_expr
    |> require_some ~msg:"expected of token"
  in
  let in_token =
    Ast.LetExceptionExpr.in_token exception_expr
    |> require_some ~msg:"expected in token"
  in
  Test.assert_equal ~expected:"exception" ~actual:(Ast.Token.text exception_token);
  Test.assert_equal ~expected:"Local" ~actual:(Ast.Ident.text name);
  Test.assert_equal ~expected:"of" ~actual:(Ast.Token.text of_token);
  Test.assert_equal ~expected:"in" ~actual:(Ast.Token.text in_token);
  Test.assert_equal
    ~expected:[ "int"; "*"; "Foo"; "."; "t"; ]
    ~actual:(let_exception_payload_tokens exception_expr);
  (
    match Ast.LetExceptionExpr.body exception_expr with
    | Some body -> (
        match Ast.Expr.view body with
        | Ast.Expr.Ident { ident } ->
            Test.assert_equal ~expected:"result" ~actual:(last_ident_text ident)
        | _ -> panic "expected let exception body ident"
      )
    | None -> panic "expected let exception expression body"
  );
  let bare_expr =
    nth_structure_item root 1
    |> require_some ~msg:"expected bare let exception item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected bare outer binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected bare let exception body"
  in
  let bare_exception =
    Ast.cast_result_to_option (Ast.LetExceptionExpr.cast bare_expr)
    |> require_some ~msg:"expected bare let exception view"
  in
  (
    match Ast.LetExceptionExpr.of_token bare_exception with
    | None -> Ok ()
    | Some _ -> Error "expected bare let exception without payload"
  )

let test_unreachable_expression_views = fun _ctx ->
  let source = "let value = match maybe with | Some value -> value | None -> .\n" in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let match_expr =
    nth_structure_item root 0
    |> require_some ~msg:"expected let item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected outer binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected match body"
  in
  let unreachable = ref None in
  iter_fold
    Ast.Expr.fold_match_case
    match_expr
    ~fn:(fun match_case ->
      match Ast.MatchCase.view match_case with
      | Ast.MatchCase.Case { body; _ } -> (
          match Ast.cast_result_to_option (Ast.UnreachableExpr.cast body) with
          | Some _ -> unreachable := Some body
          | None -> ()
        )
      | Ast.MatchCase.Unknown _ -> ());
  let unreachable =
    !unreachable
    |> require_some ~msg:"expected unreachable expression"
  in
  let unreachable =
    Ast.cast_result_to_option (Ast.UnreachableExpr.cast unreachable)
    |> require_some ~msg:"expected unreachable expression view"
  in
  let dot =
    Ast.UnreachableExpr.dot_token unreachable
    |> require_some ~msg:"expected dot token"
  in
  Test.assert_equal ~expected:"." ~actual:(Ast.Token.text dot);
  Ok ()

let attribute_shell_text = fun ~fold_shell_token ->
  let text = ref "" in
  let first = ref true in
  fold_shell_token
    ~init:()
    ~fn:(fun token () ->
      if !first then (
        first := false;
        text := !text ^ Ast.Token.text token
      ) else
        text := !text ^ Ast.Token.full_text token;
      Ast.Continue ());
  !text

let test_attribute_views = fun _ctx ->
  let source = "let value = target [@inline always]\nlet (x [@foo]) = value\n" in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let attribute_expr =
    nth_structure_item root 0
    |> require_some ~msg:"expected attribute expression item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected attribute expression binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected attribute expression body"
  in
  (
    match Ast.Expr.view attribute_expr with
    | Ast.Expr.Ident { ident } -> assert_last_ident_text ident "target"
    | _ -> panic "expected attributed expression to unwrap to its inner ident"
  );
  let attribute_expr =
    Ast.cast_result_to_option (Ast.AttributeExpr.cast attribute_expr)
    |> require_some ~msg:"expected attribute expression view"
  in
  Test.assert_equal
    ~expected:"[@inline always]"
    ~actual:(attribute_shell_text
      ~fold_shell_token:(Ast.AttributeExpr.fold_shell_token attribute_expr));
  let attribute_pattern =
    nth_structure_item root 1
    |> require_some ~msg:"expected attribute pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected attribute pattern binding"
    |> pattern_of_binding
    |> Result.expect ~msg:"expected attribute pattern"
  in
  let attribute_pattern =
    Ast.Node.first_child_node
      (Ast.Pattern.as_node attribute_pattern)
      ~kind:SyntaxKind.ATTRIBUTE_PATTERN
    |> Option.and_then ~fn:(fun node -> Ast.cast_result_to_option (Ast.Pattern.cast node))
    |> require_some ~msg:"expected parenthesized attribute pattern"
  in
  let attribute_pattern =
    Ast.cast_result_to_option (Ast.AttributePattern.cast attribute_pattern)
    |> require_some ~msg:"expected attribute pattern view"
  in
  (
    match Ast.AttributePattern.inner attribute_pattern with
    | Some inner -> (
        match Ast.Pattern.view inner with
        | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident "x"
        | _ -> panic "expected attributed pattern inner ident"
      )
    | None -> panic "expected attribute pattern inner"
  );
  Test.assert_equal
    ~expected:"[@foo]"
    ~actual:(attribute_shell_text
      ~fold_shell_token:(Ast.AttributePattern.fold_shell_token attribute_pattern));
  Ok ()

let test_extension_views = fun _ctx ->
  let source =
    "let value = [%expr payload]\nlet [%pat payload] = value\n[%%item payload]\n[@@@warning \"-32\"]\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let extension_expr =
    nth_structure_item root 0
    |> require_some ~msg:"expected extension expression item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected extension expression binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected extension expression body"
  in
  (
    match Ast.Expr.view extension_expr with
    | Ast.Expr.Unknown _ -> ()
    | _ -> panic "expected extension expression to lower as unknown"
  );
  let extension_expr =
    Ast.cast_result_to_option (Ast.ExtensionExpr.cast extension_expr)
    |> require_some ~msg:"expected extension expression view"
  in
  Test.assert_equal
    ~expected:"[%expr payload]"
    ~actual:(attribute_shell_text
      ~fold_shell_token:(Ast.ExtensionExpr.fold_shell_token extension_expr));
  let extension_pattern =
    nth_structure_item root 1
    |> require_some ~msg:"expected extension pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected extension pattern binding"
    |> pattern_of_binding
    |> Result.expect ~msg:"expected extension pattern"
  in
  (
    match Ast.Pattern.view extension_pattern with
    | Ast.Pattern.Unknown _ -> ()
    | _ -> panic "expected extension pattern to lower as unknown"
  );
  let extension_pattern =
    Ast.cast_result_to_option (Ast.ExtensionPattern.cast extension_pattern)
    |> require_some ~msg:"expected extension pattern view"
  in
  Test.assert_equal
    ~expected:"[%pat payload]"
    ~actual:(attribute_shell_text
      ~fold_shell_token:(Ast.ExtensionPattern.fold_shell_token extension_pattern));
  let extension_item =
    nth_structure_item root 2
    |> require_some ~msg:"expected extension item"
  in
  (
    match Ast.StructureItem.view extension_item with
    | Ast.StructureItem.Extension item ->
        Test.assert_equal
          ~expected:"[%%item payload]"
          ~actual:(attribute_shell_text ~fold_shell_token:(Ast.ExtensionItem.fold_shell_token item))
    | _ -> panic "expected extension structure item"
  );
  let attribute_item =
    nth_structure_item root 3
    |> require_some ~msg:"expected attribute item"
  in
  (
    match Ast.StructureItem.view attribute_item with
    | Ast.StructureItem.Attribute item ->
        Test.assert_equal
          ~expected:"[@@@warning \"-32\"]"
          ~actual:(attribute_shell_text ~fold_shell_token:(Ast.AttributeItem.fold_shell_token item))
    | _ -> panic "expected attribute structure item"
  );
  Ok ()

let test_special_pattern_views = fun _ctx ->
  let source =
    "let f (type a b) (module M : S.T) = value\nlet h (module N : S with type t = item) = value\n"
  in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected special pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected special pattern binding"
  in
  let parameters = ref [] in
  iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun pattern -> parameters := pattern :: !parameters);
  match List.reverse !parameters with
  | [ locally_abstract; first_class_module ] ->
      let locally_abstract =
        Ast.Parameter.pattern locally_abstract
        |> require_some ~msg:"expected locally abstract type parameter pattern"
      in
      let first_class_module =
        Ast.Parameter.pattern first_class_module
        |> require_some ~msg:"expected first-class module parameter pattern"
      in
      let locally_abstract =
        Ast.cast_result_to_option (Ast.LocallyAbstractTypePattern.cast locally_abstract)
        |> require_some ~msg:"expected locally abstract type pattern view"
      in
      let type_names = ref [] in
      iter_fold
        Ast.LocallyAbstractTypePattern.fold_type_ident
        locally_abstract
        ~fn:(fun ident -> type_names := Ast.Ident.text ident :: !type_names);
      Test.assert_equal ~expected:[ "a"; "b" ] ~actual:(List.reverse !type_names);
      (
        match Ast.Pattern.view first_class_module with
        | Ast.Pattern.FirstClassModule { binder; ascription; ascription_ident } ->
            let ascription_ident =
              ascription_ident
              |> require_some ~msg:"expected first-class module pattern ascription"
            in
            Test.assert_equal ~expected:"M" ~actual:(Ast.Ident.text binder);
            Test.assert_equal ~expected:Ast.IdentAscription ~actual:ascription;
            Test.assert_equal ~expected:2 ~actual:(Ast.Ident.segment_count ascription_ident)
        | _ -> panic "expected first-class module pattern"
      );
      let first_class_module =
        Ast.cast_result_to_option (Ast.FirstClassModulePattern.cast first_class_module)
        |> require_some ~msg:"expected first-class module pattern view"
      in
      let binder =
        Ast.FirstClassModulePattern.binder first_class_module
        |> require_some ~msg:"expected first-class module binder"
      in
      let colon =
        Ast.FirstClassModulePattern.colon_token first_class_module
        |> require_some ~msg:"expected first-class module colon"
      in
      Test.assert_equal ~expected:"M" ~actual:(Ast.Ident.text binder);
      Test.assert_equal ~expected:":" ~actual:(Ast.Token.text colon);
      Test.assert_equal
        ~expected:Ast.FirstClassModulePattern.IdentAscription
        ~actual:(Ast.FirstClassModulePattern.ascription first_class_module);
      Test.assert_equal
        ~expected:"S.T"
        ~actual:(first_class_module_pattern_ascription_text first_class_module);
      Ok ()
  | _ -> Error "expected two special-pattern parameters"

let test_first_class_module_pattern_with_constraints_view = fun _ctx ->
  let root =
    parse_ml "let h (module N : S with type t = item) = value\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected constrained module pattern item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected constrained module pattern binding"
  in
  let parameters = ref [] in
  iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun pattern -> parameters := pattern :: !parameters);
  match List.reverse !parameters with
  | [ first_class_module ] -> (
      let first_class_module =
        Ast.Parameter.pattern first_class_module
        |> require_some ~msg:"expected first-class module parameter pattern"
      in
      match Ast.Pattern.view first_class_module with
      | Ast.Pattern.FirstClassModule { ascription; _ } ->
          let first_class_module =
            Ast.cast_result_to_option (Ast.FirstClassModulePattern.cast first_class_module)
            |> require_some ~msg:"expected constrained first-class module pattern view"
          in
          Test.assert_equal ~expected:Ast.UnsupportedAscription ~actual:ascription;
          Test.assert_equal
            ~expected:Ast.FirstClassModulePattern.UnsupportedAscription
            ~actual:(Ast.FirstClassModulePattern.ascription first_class_module);
          Ok ()
      | _ -> Error "expected first-class module pattern parameter"
    )
  | _ -> Error "expected one constrained first-class module parameter"

let test_typed_labeled_parameter_view = fun _ctx ->
  let source = "let map (type a b) (iter : a t) ~(fn : a -> b) = ()\n" in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected typed labeled parameter item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected typed labeled parameter binding"
  in
  let parameters = ref [] in
  iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun pattern -> parameters := pattern :: !parameters);
  match List.reverse !parameters with
  | [ _locally_abstract; _iter; labeled ] -> (
      match Ast.Parameter.view labeled with
      | Ast.Parameter.Param {
          label = Ast.Parameter.Labeled { name = Some label };
          pattern = Some pattern;
        } ->
          Test.assert_equal ~expected:"fn" ~actual:(Ast.Token.text label);
          (
            match Ast.Pattern.view pattern with
            | Ast.Pattern.Constraint { pattern = binding; annotation } ->
                (
                  match Ast.Pattern.view binding with
                  | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident "fn"
                  | _ -> panic "expected labeled parameter binding ident"
                );
                (
                  match Ast.TypeExpr.view annotation with
                  | Ast.TypeExpr.Arrow { arg = _; ret = _ } -> Ok ()
                  | _ -> Error "expected labeled parameter arrow annotation"
                )
            | _ -> Error "expected typed labeled parameter pattern"
          )
      | _ -> Error "expected labeled parameter view with label and typed pattern"
    )
  | _ -> Error "expected locally abstract, positional, and labeled parameters"

let test_optional_default_labeled_parameter_view = fun _ctx ->
  let source = "let middleware ?config:(cfg = default_config) types = cfg\n" in
  let root =
    parse_ml source
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected optional default binding item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected optional default binding"
  in
  let parameters = ref [] in
  iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun pattern -> parameters := pattern :: !parameters);
  match List.reverse !parameters with
  | [ optional; _types ] -> (
      match Ast.Parameter.view optional with
      | Ast.Parameter.Param {
          label = Ast.Parameter.Optional { name = Some label; default = Some default };
          pattern = Some pattern;
        } ->
          Test.assert_equal ~expected:"config" ~actual:(Ast.Token.text label);
          (
            match Ast.Pattern.view pattern with
            | Ast.Pattern.Ident { ident } ->
                Test.assert_equal ~expected:"cfg" ~actual:(last_ident_text ident)
            | _ -> panic "expected optional default binding ident"
          );
          (
            match Ast.Expr.view default with
            | Ast.Expr.Ident { ident } ->
                Test.assert_equal ~expected:"default_config" ~actual:(last_ident_text ident);
                Ok ()
            | _ -> Error "expected optional default expression ident"
          )
      | _ -> Error "expected optional default parameter view"
    )
  | _ -> Error "expected optional and positional parameters"

let test_let_binding_parameters_are_parameter_views = fun _ctx ->
  let root =
    parse_ml {ocaml|let build name ~(mode : mode) ?config:(cfg = default_config) = cfg
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding =
    nth_structure_item root 0
    |> require_some ~msg:"expected parameter-view item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected parameter-view binding"
  in
  let parameters = Vector.with_capacity ~size:3 in
  iter_fold
    Ast.LetBinding.fold_parameter
    binding
    ~fn:(fun parameter -> Vector.push parameters ~value:parameter);
  Test.assert_equal ~expected:3 ~actual:(Vector.length parameters);
  (
    match Ast.Parameter.view (Vector.get_unchecked parameters ~at:0) with
    | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; pattern = Some pattern } -> (
        match Ast.Pattern.view pattern with
        | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident "name"
        | _ -> panic "expected positional parameter ident"
      )
    | _ -> panic "expected positional parameter view"
  );
  (
    match Ast.Parameter.view (Vector.get_unchecked parameters ~at:1) with
    | Ast.Parameter.Param {
        label = Ast.Parameter.Labeled { name = Some label };
        pattern = Some pattern;
      } ->
        Test.assert_equal ~expected:"mode" ~actual:(Ast.Token.text label);
        (
          match Ast.Pattern.view pattern with
          | Ast.Pattern.Constraint { annotation; _ } ->
              assert_type_ident_last_segment annotation "mode"
          | _ -> panic "expected labeled parameter type constraint"
        )
    | _ -> panic "expected labeled parameter view"
  );
  match Ast.Parameter.view (Vector.get_unchecked parameters ~at:2) with
  | Ast.Parameter.Param {
      label = Ast.Parameter.Optional { name = Some label; default = Some default };
      pattern = Some pattern;
    } ->
      Test.assert_equal ~expected:"config" ~actual:(Ast.Token.text label);
      (
        match Ast.Pattern.view pattern with
        | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident "cfg"
        | _ -> panic "expected optional-default parameter pattern"
      );
      (
        match Ast.Expr.view default with
        | Ast.Expr.Ident { ident } -> assert_last_ident_text ident "default_config"
        | _ -> panic "expected optional-default parameter expression"
      );
      Ok ()
  | _ -> Error "expected optional-default parameter view"

let test_fun_parameters_parse_as_syntax_tree_parameter_children = fun _ctx ->
  let assert_fun_children source expected =
    let tree =
      parse_ml_tree source
      |> Result.expect ~msg:"expected parse tree"
    in
    let fun_node =
      first_syntax_node tree SyntaxKind.FUN_EXPR
      |> require_some ~msg:"expected fun syntax node"
    in
    Test.assert_equal ~expected ~actual:(direct_syntax_child_node_kinds tree fun_node)
  in
  assert_fun_children
    {ocaml|let f = fun Some x -> x
|ocaml}
    SyntaxKind.[ PATH_PATTERN; PATH_PATTERN; PATH_EXPR ];
  assert_fun_children
    {ocaml|let f = fun (Some x) -> x
|ocaml}
    SyntaxKind.[ PAREN_PATTERN; PATH_EXPR ];
  assert_fun_children
    {ocaml|let make_release_source = fun ?(files = []) ~package_name ~version manifest_toml: Pkgs_ml.Registry.release_source -> manifest_toml
|ocaml}
    SyntaxKind.[
      OPTIONAL_PARAM_DEFAULT;
      LABELED_PARAM;
      LABELED_PARAM;
      PATH_PATTERN;
      TYPE_EXPR;
      PATH_EXPR;
    ];
  assert_fun_children
    {ocaml|let found_text = fun ~found:token ~text ~span -> text
|ocaml}
    SyntaxKind.[ LABELED_PARAM; LABELED_PARAM; LABELED_PARAM; PATH_EXPR ];
  Ok ()

let test_fun_expression_view_preserves_labeled_parameters_after_renamed_label = fun _ctx ->
  let root =
    parse_ml {ocaml|let found_text = fun ~found:token ~text ~span -> text
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let body =
    nth_structure_item root 0
    |> require_some ~msg:"expected function item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected function binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected function body"
  in
  match Ast.Expr.view body with
  | Ast.Expr.Fun { parameters; _ } ->
      Test.assert_equal ~expected:3 ~actual:(Vector.length parameters);
      let assert_labeled_param index expected_label expected_pattern =
        match Ast.Parameter.view (Vector.get_unchecked parameters ~at:index) with
        | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; pattern } ->
            Test.assert_equal ~expected:expected_label ~actual:(Ast.Token.text label);
            (
              match (pattern, expected_pattern) with
              | (Some pattern, Some expected) -> (
                  match Ast.Pattern.view pattern with
                  | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident expected
                  | _ -> panic "expected labeled parameter pattern ident"
                )
              | (None, None) -> ()
              | _ -> panic "unexpected labeled parameter payload"
            )
        | _ -> panic "expected labeled parameter"
      in
      assert_labeled_param 0 "found" (Some "token");
      assert_labeled_param 1 "text" None;
      assert_labeled_param 2 "span" None;
      Ok ()
  | _ -> Error "expected fun expression view"

let test_fun_expression_view_exposes_parameters_and_return_annotation = fun _ctx ->
  let root =
    parse_ml
      {ocaml|let make_release_source = fun ?(files = []) ~package_name ~version manifest_toml: Pkgs_ml.Registry.release_source -> manifest_toml
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let body =
    nth_structure_item root 0
    |> require_some ~msg:"expected function item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected function binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected function body"
  in
  match Ast.Expr.view body with
  | Ast.Expr.Fun {
      parameters;
      return_annotation = Some return_annotation;
      body = Ast.Expr.Body_expr expr_body;
    } ->
      Test.assert_equal ~expected:4 ~actual:(Vector.length parameters);
      (
        match Ast.Parameter.view (Vector.get_unchecked parameters ~at:0) with
        | Ast.Parameter.Param {
            label = Ast.Parameter.Optional { name = Some label; default = Some default };
            pattern = Some pattern;
          } ->
            Test.assert_equal ~expected:"files" ~actual:(Ast.Token.text label);
            (
              match Ast.Pattern.view pattern with
              | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident "files"
              | _ -> panic "expected files binding pattern"
            );
            (
              match Ast.Expr.view default with
              | Ast.Expr.List { items } ->
                  Test.assert_equal ~expected:0 ~actual:(Vector.length items)
              | _ -> panic "expected empty-list default"
            )
        | _ -> panic "expected optional files parameter"
      );
      (
        match Ast.Parameter.view (Vector.get_unchecked parameters ~at:1) with
        | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; _ } ->
            Test.assert_equal ~expected:"package_name" ~actual:(Ast.Token.text label)
        | _ -> panic "expected package_name labeled parameter"
      );
      (
        match Ast.Parameter.view (Vector.get_unchecked parameters ~at:2) with
        | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; _ } ->
            Test.assert_equal ~expected:"version" ~actual:(Ast.Token.text label)
        | _ -> panic "expected version labeled parameter"
      );
      (
        match Ast.Parameter.view (Vector.get_unchecked parameters ~at:3) with
        | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; pattern = Some pattern } -> (
            match Ast.Pattern.view pattern with
            | Ast.Pattern.Ident { ident } -> assert_last_ident_text ident "manifest_toml"
            | _ -> panic "expected manifest_toml positional parameter"
          )
        | _ -> panic "expected manifest_toml positional parameter"
      );
      assert_type_ident_last_segment return_annotation "release_source";
      (
        match Ast.Expr.view expr_body with
        | Ast.Expr.Ident { ident } ->
            assert_last_ident_text ident "manifest_toml";
            Ok ()
        | _ -> Error "expected body ident"
      )
  | _ -> Error "expected fun expression view"

let test_fun_expression_parameters_distinguish_parenthesized_constructor_patterns = fun _ctx ->
  let root =
    parse_ml
      {ocaml|let unparenthesized = fun Some x -> x
let parenthesized = fun (Some x) -> x
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let fun_body index =
    nth_structure_item root index
    |> require_some ~msg:"expected function item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected function binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected function body"
  in
  let unparenthesized = fun_body 0 in
  let parenthesized = fun_body 1 in
  (
    match Ast.Expr.view unparenthesized with
    | Ast.Expr.Fun { parameters; _ } ->
        Test.assert_equal ~expected:2 ~actual:(Vector.length parameters);
        Test.assert_equal
          ~expected:SyntaxKind.PATH_PATTERN
          ~actual:(Ast.Parameter.kind (Vector.get_unchecked parameters ~at:0));
        Test.assert_equal
          ~expected:SyntaxKind.PATH_PATTERN
          ~actual:(Ast.Parameter.kind (Vector.get_unchecked parameters ~at:1))
    | _ -> panic "expected unparenthesized fun"
  );
  match Ast.Expr.view parenthesized with
  | Ast.Expr.Fun { parameters; _ } ->
      Test.assert_equal ~expected:1 ~actual:(Vector.length parameters);
      Test.assert_equal
        ~expected:SyntaxKind.PAREN_PATTERN
        ~actual:(Ast.Parameter.kind (Vector.get_unchecked parameters ~at:0));
      (
        match Ast.Parameter.pattern (Vector.get_unchecked parameters ~at:0) with
        | Some pattern -> (
            match Ast.Pattern.view pattern with
            | Ast.Pattern.Constructor { payload = Some _; _ } -> Ok ()
            | _ -> Error "expected parenthesized constructor pattern payload"
          )
        | None -> Error "expected parenthesized parameter pattern"
      )
  | _ -> Error "expected parenthesized fun"

let test_if_then_branch_sequence_boundaries = fun _ctx ->
  let root =
    parse_ml
      "let with_else ok = if ok then log (); next () else done_ ()\nlet without_else ok = if ok then log (); next ()\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let with_else_body =
    nth_structure_item root 0
    |> require_some ~msg:"expected with_else structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected with_else binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected with_else body"
  in
  (
    match Ast.Expr.view with_else_body with
    | Ast.Expr.If { then_branch; else_branch = Some _; _ } -> (
        match Ast.Expr.view then_branch with
        | Ast.Expr.Sequence { left = _; right = Some _ } -> ()
        | _ -> panic "expected then branch sequence to stay inside if with else"
      )
    | _ -> panic "expected with_else body to be an if expression"
  );
  let without_else_body =
    nth_structure_item root 1
    |> require_some ~msg:"expected without_else structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected without_else binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected without_else body"
  in
  match Ast.Expr.view without_else_body with
  | Ast.Expr.Sequence { left; right = Some _ } -> (
      match Ast.Expr.view left with
      | Ast.Expr.If { else_branch = None; _ } -> Ok ()
      | _ -> Error "expected outer sequence to keep no-else if on the left"
    )
  | _ -> Error "expected without_else body to stay a top-level sequence"

let test_if_then_match_case_sequence_boundaries = fun _ctx ->
  let root =
    parse_ml
      "let classify input =\n\
    \  if ready then\n\
    \    match input with\n\
    \    | '!' | '^' ->\n\
    \        bump ();\n\
    \        true\n\
    \    | _ -> false\n\
    \  else\n\
    \    false\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let body =
    nth_structure_item root 0
    |> require_some ~msg:"expected classify structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected classify binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected classify body"
  in
  match Ast.Expr.view body with
  | Ast.Expr.If { then_branch; _ } -> (
      match Ast.Expr.view then_branch with
      | Ast.Expr.Match { first_case; _ } -> (
          match Ast.MatchCase.view first_case with
          | Ast.MatchCase.Case { body = case_body; _ } -> (
              match Ast.Expr.view case_body with
              | Ast.Expr.Sequence { left = _; right = Some _ } -> Ok ()
              | _ -> Error "expected first match case body sequence to stay inside the case"
            )
          | Ast.MatchCase.Unknown _ -> Error "expected first match case body"
        )
      | _ -> Error "expected if then branch to remain a match expression"
    )
  | _ -> Error "expected classify body to be an if expression"

let test_loop_body_sequence_boundaries = fun _ctx ->
  let root =
    parse_ml
      "let poll ready =\n\
    \  while ready do\n\
    \    step ();\n\
    \    next ()\n\
    \  done\n\
    let count n =\n\
    \  for i = 0 to n do\n\
    \    tick i;\n\
    \    total := !total + i\n\
    \  done\n"
    |> Result.expect ~msg:"expected parse source file"
  in
  let poll_body =
    nth_structure_item root 0
    |> require_some ~msg:"expected poll structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected poll binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected poll body"
  in
  let count_body =
    nth_structure_item root 1
    |> require_some ~msg:"expected count structure item"
    |> binding_of_structure_item
    |> Result.expect ~msg:"expected count binding"
    |> body_of_binding
    |> Result.expect ~msg:"expected count body"
  in
  (
    match Ast.Expr.view poll_body with
    | Ast.Expr.While { body; _ } -> (
        match Ast.Expr.view body with
        | Ast.Expr.Sequence { left = _; right = Some _ } -> ()
        | _ -> panic "expected while body sequence to stay inside the loop body"
      )
    | _ -> panic "expected poll body to be a while expression"
  );
  match Ast.Expr.view count_body with
  | Ast.Expr.For { body; _ } -> (
      match Ast.Expr.view body with
      | Ast.Expr.Sequence { left = _; right = Some _ } -> Ok ()
      | _ -> Error "expected for body sequence to stay inside the loop body"
    )
  | _ -> Error "expected count body to be a for expression"

let test_ast_visitor_threads_state_and_skips_subtrees = fun _ctx ->
  let root =
    parse_ml {ocaml|let x = if ready then 1 else 2
let y = 3
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let initial = {
    let_names = Vector.with_capacity ~size:2;
    token_texts = Vector.with_capacity ~size:16;
    leave_count = 0;
  }
  in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_token =
      Some (fun visitor token ->
        Vector.push (Syn.Visitor.ctx visitor).token_texts ~value:(Ast.Token.text token);
        visitor);
    enter_let_binding =
      Some (fun visitor binding ->
        (
          match Ast.LetBinding.pattern binding with
          | Some pattern -> (
              match Ast.Node.first_descendant_token (Ast.Pattern.as_node pattern) with
              | Some name ->
                  Vector.push (Syn.Visitor.ctx visitor).let_names ~value:(Ast.Token.text name)
              | None -> ()
            )
          | None -> ()
        );
        (visitor, Syn.Visitor.Continue));
    enter_expr =
      Some (fun visitor expr ->
        match Ast.Node.kind (Ast.Expr.as_node expr) with
        | Syn.SyntaxKind.IF_EXPR -> (visitor, Syn.Visitor.Skip_subtree)
        | _ -> (visitor, Syn.Visitor.Continue));
    leave_node =
      Some (fun visitor _node ->
        let ctx = Syn.Visitor.ctx visitor in
        Syn.Visitor.with_ctx visitor { ctx with leave_count = ctx.leave_count + 1 });
  }
  in
  let visitor = Syn.Visitor.make ~ctx:initial ~hooks in
  let visitor = Syn.Visitor.visit_source_file visitor root in
  let ctx = Syn.Visitor.ctx visitor in
  Test.assert_equal ~expected:[ "x"; "y" ] ~actual:(vector_to_list ctx.let_names);
  let token_texts = vector_to_list ctx.token_texts in
  if List.contains token_texts ~value:"ready" then
    Error "expected skipped if-expression subtree not to visit condition tokens"
  else if ctx.leave_count <= 0 then
    Error "expected leave_node hook to thread updated context"
  else
    Ok ()

let test_ast_controlled_folds_return_early = fun _ctx ->
  let root =
    parse_ml {ocaml|let x = 1
let y = 2
let z = 3
|ocaml}
    |> Result.expect ~msg:"expected parse source file"
  in
  let binding_name = fun item ->
    let binding =
      binding_of_structure_item item
      |> Result.expect ~msg:"expected let binding"
    in
    let pattern =
      pattern_of_binding binding
      |> Result.expect ~msg:"expected let binding pattern"
    in
    match Ast.Pattern.view pattern with
    | Ast.Pattern.Ident { ident } ->
        let name =
          Ast.Ident.last_segment ident
          |> require_some ~msg:"expected let binding name"
        in
        Ast.Token.text name
    | _ -> panic "expected identifier let binding pattern"
  in
  let (names, count) =
    Ast.SourceFile.fold_structure_item
      root
      ~init:([], 0)
      ~fn:(fun item (names, count) ->
        let name = binding_name item in
        let next = (name :: names, count + 1) in
        if String.equal name "y" then
          Ast.Return next
        else
          Ast.Continue next)
  in
  Test.assert_equal ~expected:2 ~actual:count;
  Test.assert_equal ~expected:[ "x"; "y" ] ~actual:(List.reverse names);
  Ok ()

let tests =
  Test.[
    case
      "ast leaves class subset words out of the keyword table"
      test_class_subset_words_are_not_keywords;
    case "ast keeps empty source files typed by file kind" test_empty_source_files_have_file_kind;
    case "parse_ident accepts only single identifier expressions" test_parse_ident;
    case "ast exposes source file and let binding views" test_source_file_and_let_binding_views;
    case
      "ast parses local let type identifier after typed rec binding"
      test_local_let_type_identifier_after_typed_rec_binding;
    case "ast exposes separated docstring trivia parts" test_token_leading_docstring_trivia_parts;
    case "ast node spans exclude leading trivia" test_node_span_excludes_leading_trivia;
    case "ast exposes if and match expression views" test_expression_views;
    case
      "ast parses assert and lazy as regular applications"
      test_assert_and_lazy_parse_as_regular_applications;
    case "ast exposes assignment operator tokens" test_assignment_operator_views;
    case
      "ast preserves trailing sequence bodies before and-bindings"
      test_trailing_sequence_before_and_views;
    case "ast lifts constructor expressions out of identifiers" test_expression_constructor_views;
    case
      "ast parses field access separately from qualified value paths"
      test_expression_field_access_splits_value_paths;
    case
      "ast keeps field access inside polymorphic variant payloads"
      test_expression_poly_variant_payload_keeps_field_access;
    case
      "ast keeps labels after polymorphic variant arguments as application arguments"
      test_labeled_application_after_poly_variant_argument;
    case "ast exposes tuple and cons pattern views" test_pattern_views;
    case
      "ast keeps comma outside polymorphic variant pattern payloads"
      test_poly_variant_tuple_pattern_boundary;
    case "ast exposes signature declaration views" test_signature_and_type_views;
    case "ast parses package type value annotations" test_package_type_value_annotation_views;
    case "ast exposes type expression views" test_type_expression_views;
    case "ast exposes type expression aliases" test_type_expression_alias_view;
    case "ast exposes type tuple separators" test_type_tuple_separator_views;
    case
      "ast exposes poly labeled types and signed literal patterns"
      test_poly_labeled_and_signed_views;
    case "ast exposes quoted poly let annotations" test_quoted_poly_let_annotation_views;
    case
      "ast keeps non-manifest type bodies out of manifest views"
      test_non_manifest_type_declaration_bodies;
    case "ast exposes type declaration parameters" test_type_declaration_parameters;
    case "ast exposes type declaration member views" test_type_declaration_member_views;
    case "ast exposes type declaration body group views" test_type_declaration_body_group_views;
    case "ast exposes type alias record representations" test_type_alias_record_representation_views;
    case
      "ast preserves abstract type attributes before later structure items"
      test_abstract_type_attribute_boundary_views;
    case
      "ast exposes manifest type attribute suffix views"
      test_manifest_type_attribute_suffix_views;
    case
      "ast exposes type extensions and structured exception views"
      test_type_extension_and_exception_views;
    case
      "ast preserves exception declarations after function bindings"
      test_exception_after_function_binding_views;
    case
      "ast keeps nested match guards from stealing outer case arrows"
      test_nested_match_guard_case_boundaries;
    case "ast exposes open declaration ident tokens" test_open_declaration_ident_tokens;
    case "ast exposes simple declaration token views" test_simple_declaration_token_views;
    case "ast exposes module declaration tokens" test_module_declaration_tokens;
    case
      "ast exposes applied module declaration body views"
      test_module_declaration_application_body_view;
    case "ast exposes module declaration member views" test_module_declaration_member_views;
    case
      "ast preserves signature module declarations with module type of bodies"
      test_signature_module_typeof_declaration;
    case "ast exposes module type declaration tokens" test_module_type_declaration_tokens;
    case "ast exposes module type with-constraint views" test_module_type_with_constraint_views;
    case "ast exposes let binding type annotation views" test_binding_type_annotation_view;
    case
      "ast exposes function binding return annotation views"
      test_function_binding_return_annotation_view;
    case
      "ast exposes unit parameter binding return annotation views"
      test_unit_parameter_binding_return_annotation_view;
    case
      "ast exposes labeled parameter binding return annotation views"
      test_labeled_parameter_binding_return_annotation_view;
    case
      "ast keeps parenthesized parameter annotations out of return annotations"
      test_parenthesized_parameter_annotation_is_not_return_annotation;
    case "ast views normalize redundant parentheses" test_ast_views_normalize_redundant_parentheses;
    case "ast exposes record expression and pattern views" test_record_views;
    case
      "ast keeps record field special-form bodies within the field boundary"
      test_record_field_special_form_boundaries;
    case "ast exposes binding operator expression views" test_binding_operator_views;
    case "ast exposes local open expression and pattern views" test_local_open_views;
    case
      "ast preserves local open expressions as application arguments"
      test_local_open_argument_views;
    case
      "ast preserves local open expressions inside labeled arguments"
      test_local_open_labeled_argument_views;
    case "ast exposes first-class module expression views" test_first_class_module_views;
    case "ast exposes let module expression views" test_let_module_expression_views;
    case "ast exposes let exception expression views" test_let_exception_expression_views;
    case "ast exposes unreachable expression views" test_unreachable_expression_views;
    case "ast exposes attribute expression and pattern views" test_attribute_views;
    case "ast exposes extension expression pattern and item views" test_extension_views;
    case
      "ast exposes locally abstract and first-class module pattern views"
      test_special_pattern_views;
    case
      "ast marks constrained first-class module pattern ascriptions unsupported"
      test_first_class_module_pattern_with_constraints_view;
    case "ast exposes typed labeled parameter views" test_typed_labeled_parameter_view;
    case
      "ast exposes renamed optional parameters with defaults"
      test_optional_default_labeled_parameter_view;
    case
      "ast exposes let binding parameters as parameter views"
      test_let_binding_parameters_are_parameter_views;
    case
      "ast parses fun parameters as syntax tree parameter children"
      test_fun_parameters_parse_as_syntax_tree_parameter_children;
    case
      "ast preserves labeled parameters after renamed labels in fun expressions"
      test_fun_expression_view_preserves_labeled_parameters_after_renamed_label;
    case
      "ast exposes fun expression parameters and return annotations"
      test_fun_expression_view_exposes_parameters_and_return_annotation;
    case
      "ast distinguishes unparenthesized fun parameters from parenthesized constructor patterns"
      test_fun_expression_parameters_distinguish_parenthesized_constructor_patterns;
    case
      "ast keeps if then-branch sequences inside explicit else branches"
      test_if_then_branch_sequence_boundaries;
    case
      "ast keeps match-case sequences inside if then-branch matches"
      test_if_then_match_case_sequence_boundaries;
    case
      "ast keeps while and for body sequences inside done boundaries"
      test_loop_body_sequence_boundaries;
    case
      "ast visitor threads state and skips subtrees"
      test_ast_visitor_threads_state_and_skips_subtrees;
    case "ast controlled folds carry state and return early" test_ast_controlled_folds_return_early;
  ]

let main ~args = Test.Cli.main ~name:"syn-ast" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Env.args ()
