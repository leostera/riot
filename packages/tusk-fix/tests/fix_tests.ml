open Std

let synthetic_token = fun ~span ~text ->
  let green = Syn.Ceibo.Green.make_token
    ~leading_trivia:[]
    ~kind:Syn.SyntaxKind.WHITESPACE
    ~text
    ~width:(String.length text) in
  Syn.Ceibo.Red.new_token green span

let find_token_by_text = fun red_root text ->
  Syn.Ceibo.Red.SyntaxNode.tokens red_root |> List.find_opt
    (fun token ->
      String.equal (Syn.Ceibo.Red.SyntaxToken.text token) text)

let warning_diagnostic = fun ~rule_id ~message ~token ~fix ->
  Tusk_fix.Diagnostic.make
    ~severity:Warning
    ~kind:(Tusk_fix.Diagnostic.Known { rule_id; message })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~fix
    ()

let replace_token_rule = fun ~rule_id ~needle ~replacement ->
  let message = "Replace " ^ needle ^ " with " ^ replacement in
  Tusk_fix.Rule.make ~id:rule_id ~description:message ~explain:message
    ~run:(fun _ctx red_root ->
      match find_token_by_text red_root needle with
      | None -> []
      | Some token ->
          let fix = Tusk_fix.Fix.make
            ~title:message
            ~operations:[ Tusk_fix.Fix.replace_token_with_text ~target:token ~text:replacement ] in
          [ warning_diagnostic ~rule_id ~message ~token ~fix ])
    ()

let overlapping_replace_rule = fun ~rule_id ~needle ~replacement ~overlap_text ->
  let message = "Apply an overlapping replacement to " ^ needle in
  Tusk_fix.Rule.make ~id:rule_id ~description:message ~explain:message
    ~run:(fun _ctx red_root ->
      match find_token_by_text red_root needle with
      | None -> []
      | Some token ->
          let span = Syn.Ceibo.Red.SyntaxToken.span token in
          let overlap_span = Syn.Ceibo.Span.make ~start:((span.start + 1)) ~end_:span.end_ in
          let overlap_token = synthetic_token ~span:overlap_span ~text:overlap_text in
          let fix = Tusk_fix.Fix.make
            ~title:message
            ~operations:[
              Tusk_fix.Fix.replace_token_with_text ~target:overlap_token ~text:replacement
            ] in
          [ warning_diagnostic ~rule_id ~message ~token:overlap_token ~fix ])
    ()

let tests = [ Test.case "apply single operation"
    (fun _ctx ->
      let source = "open Stdlib\n" in
      let span = Syn.Ceibo.Span.make ~start:5 ~end_:11 in
      let token = synthetic_token ~span ~text:"Stdlib" in
      let actual = Tusk_fix.Fix.apply_operation
        ~source
        (Tusk_fix.Fix.replace_token_with_text ~target:token ~text:"Std")
      |> Result.expect ~msg:"apply single operation failed" in
      Test.assert_equal ~expected:"open Std\n" ~actual;
      Ok ()); Test.case "apply multiple operations in descending span order"
    (fun _ctx ->
      let source = "open Stdlib\nlet q : int Queue.t = Queue.create ()\n" in
      let stdlib_token = synthetic_token ~span:(Syn.Ceibo.Span.make ~start:5 ~end_:11) ~text:"Stdlib" in
      let queue_type_token = synthetic_token ~span:(Syn.Ceibo.Span.make ~start:24 ~end_:29) ~text:"Queue" in
      let queue_value_token = synthetic_token ~span:(Syn.Ceibo.Span.make ~start:34 ~end_:39) ~text:"Queue" in
      let fixes = [
        Tusk_fix.Fix.make
          ~title:"replace Queue"
          ~operations:[
            Tusk_fix.Fix.replace_token_with_text ~target:queue_type_token ~text:"Std.Collections.Queue";
            Tusk_fix.Fix.replace_token_with_text ~target:queue_value_token ~text:"Std.Collections.Queue"
          ];
        Tusk_fix.Fix.make
          ~title:"replace Stdlib"
          ~operations:[ Tusk_fix.Fix.replace_token_with_text ~target:stdlib_token ~text:"Std" ]
      ] in
      let actual = Tusk_fix.Fix.apply_fixes ~source fixes |> Result.expect ~msg:"apply multiple edits failed" in
      let expected = "open Std\nlet q : int Std.Collections.Queue.t = \
           Std.Collections.Queue.create ()\n"
      in
      Test.assert_equal ~expected ~actual;
      Ok ()); Test.case "reject overlapping operations"
    (fun _ctx ->
      let source = "open Stdlib\n" in
      let left = synthetic_token ~span:(Syn.Ceibo.Span.make ~start:5 ~end_:8) ~text:"Std" in
      let right = synthetic_token ~span:(Syn.Ceibo.Span.make ~start:7 ~end_:11) ~text:"lib" in
      let fix = Tusk_fix.Fix.make
        ~title:"bad overlap"
        ~operations:[
          Tusk_fix.Fix.replace_token_with_text ~target:left ~text:"Std";
          Tusk_fix.Fix.replace_token_with_text ~target:right ~text:"Std"
        ] in
      Test.assert_error (Tusk_fix.Fix.apply_fix ~source fix);
      Ok ()); Test.case "source runner skips linting when parsing fails"
    (fun _ctx ->
      let source = "let =" in
      let result = Tusk_fix.Source_runner.run_rule
        ~rule:(replace_token_rule ~rule_id:"test:broken-source" ~needle:"let" ~replacement:"and")
        source in
      Test.assert_true (List.length result.parse_diagnostics > 0);
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ()); Test.case "rule-test applies snake-case type fixes and reruns cleanly"
    (fun _ctx ->
      let source = "type userProfile = { name : string }\n" in
      let result = Tusk_fix.Rule_test.run_rule ~rule:(Tusk_fix.Rules.Snake_case_type_names.make ()) source
      |> Result.expect ~msg:"expected snake-case type rule to apply" in
      Test.assert_equal ~expected:1 ~actual:(List.length result.applied_fixes);
      Test.assert_equal ~expected:(Some "type user_profile = { name : string }\n") ~actual:result.fixed_source;
      let remaining =
        match result.after with
        | Some after -> after.diagnostics
        | None -> []
      in
      Test.assert_equal ~expected:[] ~actual:remaining;
      Ok ()); Test.case "rule-test applies multiple non-overlapping fixes"
    (fun _ctx ->
      let source = "type userProfile = int\nlet old_value = userProfile\n" in
      let result = Tusk_fix.Rule_test.run
        ~rules:[
          Tusk_fix.Rules.Snake_case_type_names.make ();
          replace_token_rule ~rule_id:"test:rename-binding" ~needle:"old_value" ~replacement:"current_value"
        ]
        source
      |> Result.expect ~msg:"expected multiple safe fixes to apply" in
      Test.assert_equal ~expected:2 ~actual:(List.length result.initial.diagnostics);
      Test.assert_equal ~expected:2 ~actual:(List.length result.applied_fixes);
      Test.assert_equal
        ~expected:(Some "type user_profile = int\nlet current_value = userProfile\n")
        ~actual:result.fixed_source;
      let remaining =
        match result.after with
        | Some after -> after.diagnostics
        | None -> []
      in
      Test.assert_equal ~expected:[] ~actual:remaining;
      Ok ()); Test.case "rule-test rejects overlapping fixes from multiple rules"
    (fun _ctx ->
      let source = "let old_value = 1\n" in
      let actual = Tusk_fix.Rule_test.run
        ~rules:[
          replace_token_rule ~rule_id:"test:replace-whole-token" ~needle:"old_value" ~replacement:"value";
          overlapping_replace_rule
            ~rule_id:"test:replace-tail"
            ~needle:"old_value"
            ~replacement:"tail"
            ~overlap_text:"ld_value"
        ]
        source in
      Test.assert_error actual;
      Ok ()) ]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"tusk-fix:fix" ~tests ~args:Env.args)
    ~args:Env.args
    ()
