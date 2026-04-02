open Std
open Std.Collections
module Api = Fixme

let package_name = "std"

let package_rule_id = package_name ^ ":upgrade-test-ctx-callbacks"

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "Std.Test callbacks should accept a ctx argument instead of unit.";
    body =
      {|
`Std.Test` callbacks now receive a context record instead of being called with
`()`.

Examples:

Instead of:

```ocaml
Test.case "example" (fun () -> Ok ())

let test_case () = Ok ()
let tests = [ Test.case "example" test_case ]
```

write:

```ocaml
Test.case "example" (fun _ctx -> Ok ())

let test_case _ctx = Ok ()
let tests = [ Test.case "example" test_case ]
```

The first migration pass only rewrites callbacks that are clearly used as
`Std.Test`/`Test` cases in test source files. Ambiguous helper functions are
left alone for manual review.
|};
  }

let explanations = fun () -> [ explanation ]

let is_test_source_path = fun path ->
  let basename =
    match path |> String.split_on_char '/' |> List.rev with
    | basename :: _ -> basename
    | [] -> path
  in
  (String.contains path "/tests/" || String.starts_with ~prefix:"tests/" path)
  && (
    String.ends_with ~suffix:"_tests.ml" basename
    || String.ends_with ~suffix:"_test.ml" basename
    || String.starts_with ~prefix:"test_" basename
  )

let rec unwrap_expression = fun expr ->
  match expr with
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> unwrap_expression inner
  | _ -> expr

let rec unwrap_pattern = fun pattern ->
  match pattern with
  | Syn.Cst.Pattern.Parenthesized { inner; _ } -> unwrap_pattern inner
  | Syn.Cst.Pattern.Typed { pattern; _ } -> unwrap_pattern pattern
  | _ -> pattern

let rec expression_name = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Path { path; _ } ->
      Syn.Cst.Ident.segments path
      |> List.map Syn.Cst.Token.text
      |> String.concat "."
      |> fun name -> Some name
  | Syn.Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match expression_name receiver with
      | Some receiver_name -> Some (receiver_name ^ "." ^ Syn.Cst.Token.text field_name)
      | None -> None
    )
  | _ ->
      None

let simple_path_name = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Path { path; _ } when List.length (Syn.Cst.Ident.segments path) = 1 ->
      Syn.Cst.Ident.name path
  | _ ->
      None

let rec flatten_apply = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      let head, arguments = flatten_apply callee in
      (head, arguments @ [ argument ])
  | _ ->
      (unwrap_expression expr, [])

let positional_arguments = fun arguments ->
  arguments |> List.filter_map
    (
      function
      | Syn.Cst.Positional expr -> Some expr
      | Syn.Cst.Labeled _
      | Syn.Cst.Optional _ -> None
    )

let is_test_callback_callee = function
  | Some "case"
  | Some "property"
  | Some "skip"
  | Some "Test.case"
  | Some "Test.property"
  | Some "Test.skip"
  | Some "Std.Test.case"
  | Some "Std.Test.property"
  | Some "Std.Test.skip" -> true
  | _ -> false

let callback_argument_of_call = fun expr ->
  let head, arguments = flatten_apply expr in
  if not (is_test_callback_callee (expression_name head)) then
    None
  else
    match List.rev (positional_arguments arguments) with
    | callback :: _ -> Some callback
    | [] -> None

let is_partial_test_invocation = fun expr ->
  let head, arguments = flatten_apply expr in
  if not (is_test_callback_callee (expression_name head)) then
    false
  else
    Int.equal (List.length (positional_arguments arguments)) 1

let callback_argument = fun expr ->
  match callback_argument_of_call expr with
  | Some callback -> Some callback
  | None -> (
      match unwrap_expression expr with
      | Syn.Cst.Expression.Infix infix when String.equal (Syn.Cst.InfixExpression.operator infix) "@@" ->
          if is_partial_test_invocation infix.left then
            Some infix.right
          else
            None
      | _ ->
          None
    )

let is_unit_parameter = fun parameter ->
  match parameter with
  | Syn.Cst.Parameter.Positional positional -> (
      match unwrap_pattern positional.pattern with
      | Syn.Cst.Pattern.Literal { literal = Syn.Cst.PatternLiteral.Unit _; _ } -> true
      | _ -> false
    )
  | Syn.Cst.Parameter.Labeled _
  | Syn.Cst.Parameter.Optional _
  | Syn.Cst.Parameter.LocallyAbstract _ -> false

let parameter_fix = fun parameter ->
  let target = Syn.Cst.Parameter.syntax_node parameter in
  Api.Fix.make
    ~title:"Replace unit test callback parameter with _ctx"
    ~operations:[ Api.Fix.replace_node_with_text ~target ~text:" _ctx"; ]

let make_diagnostic = fun parameter ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message
    })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Parameter.syntax_node parameter))
    ~suggestion:"Change the callback parameter from () to _ctx."
    ~fix:(parameter_fix parameter)
    ()

type callback_reference =
  | Inline of Syn.Cst.Parameter.t
  | Named of string

let callback_reference = fun expr ->
  match callback_argument expr |> Option.map unwrap_expression with
  | Some (Syn.Cst.Expression.Fun { parameters = [ parameter ]; _ }) when is_unit_parameter parameter ->
      Some (Inline parameter)
  | Some callback ->
      simple_path_name callback |> Option.map (fun name -> Named name)
  | None ->
      None

let increment_count = fun counts key ->
  let next =
    match HashMap.get counts key with
    | Some count -> count + 1
    | None -> 1
  in
  ignore (HashMap.insert counts key next)

let binding_parameter = fun binding ->
  match Syn.Cst.LetBinding.parameters binding with
  | [ parameter ] when is_unit_parameter parameter -> Some parameter
  | [] -> (
      match unwrap_expression (Syn.Cst.LetBinding.value binding) with
      | Syn.Cst.Expression.Fun { parameters = [ parameter ]; _ } when is_unit_parameter parameter ->
          Some parameter
      | _ ->
          None
    )
  | _ ->
      None

let check_tree = fun (ctx: Api.Rule.context) _red_root ->
  if not (is_test_source_path ctx.file_path) then
    []
  else
    let expressions = Tusk_fix.Rule_query.expressions ctx in
    let inline_diagnostics = expressions |> List.filter_map
      (fun expr ->
        match callback_reference expr with
        | Some (Inline parameter) -> Some (make_diagnostic parameter)
        | Some (Named _)
        | None -> None)
    in
    let named_callback_counts = HashMap.create () in
    expressions |> List.iter
      (fun expr ->
        match callback_reference expr with
        | Some (Named name) -> increment_count named_callback_counts name
        | Some (Inline _)
        | None -> ());
    let path_use_counts = HashMap.create () in
    expressions |> List.iter
      (fun expr ->
        match simple_path_name expr with
        | Some name -> increment_count path_use_counts name
        | None -> ());
    let named_diagnostics =
      Tusk_fix.Rule_query.let_bindings ctx |> List.filter_map
        (fun binding ->
          match Syn.Cst.LetBinding.binding_name_token binding with
          | None -> None
          | Some name_token ->
              let name = Syn.Cst.Token.text name_token in
              let callback_uses =
                HashMap.get named_callback_counts name |> Option.unwrap_or ~default:0
              in
              let total_uses =
                HashMap.get path_use_counts name |> Option.unwrap_or ~default:0
              in
              if callback_uses = 0 || total_uses != callback_uses then
                None
              else
                binding_parameter binding |> Option.map make_diagnostic)
    in
    inline_diagnostics @ named_diagnostics

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:explanation.message
    ~explain:explanation.body
    ~run:check_tree
    ()
