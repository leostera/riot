open Std

let write_file = fun path content -> Fs.write content path |> Result.expect ~msg:"failed to write test fixture"

let read_file = fun path -> Fs.read path |> Result.expect ~msg:"failed to read test fixture"

let run_cli = fun argv ->
  match ArgParser.get_matches Tusk_fix.Cli.command ("fix" :: argv) with
  | Error err -> Error (Failure (ArgParser.error_message err))
  | Ok matches -> Tusk_fix.Cli.run matches

let with_cwd = fun path fn ->
  let original = Env.current_dir () |> Result.expect ~msg:"failed to get cwd" in
  Env.set_current_dir path |> Result.expect ~msg:"failed to chdir into test dir";
  try
    let result = fn () in
    Env.set_current_dir original |> Result.expect ~msg:"failed to restore cwd";
    result
  with
  | exn ->
      Env.set_current_dir original |> Result.expect ~msg:"failed to restore cwd after exception";
      raise exn

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let diagnostic_rule_ids = fun diagnostics ->
  diagnostics |> List.map Tusk_fix.Diagnostic.rule_id |> List.sort String.compare

let assert_explanation_contains = fun ~rule_id ~snippet ->
  match Tusk_fix.Explanations.explain rule_id with
  | None -> Error ("Expected explanation for " ^ rule_id)
  | Some entry ->
      Test.assert_equal ~expected:rule_id ~actual:entry.Tusk_fix.Explanation.rule_id;
      let body = String.trim entry.Tusk_fix.Explanation.body in
      Test.assert_true (String.length body > 80);
      Test.assert_true (not (String.contains body "Avoid:"));
      Test.assert_true (not (String.contains body "Better:"));
      Test.assert_true (not (String.contains body "Why this rule exists"));
      Test.assert_true (not (String.contains body "What to do instead"));
      ignore snippet;
      Ok ()

let tests = [
  Test.case "snake-case-type-names exposes safe fixes"
    (fun _ctx ->
      let source = "type userProfile = { name : string }\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let fixes = List.filter_map Tusk_fix.Diagnostic.fix result.diagnostics in
      Test.assert_equal ~expected:1 ~actual:(List.length fixes);
      Ok ());
  Test.case "snake-case-type-names keeps compliant type names clean"
    (fun _ctx ->
      let source = "type user_profile = { name : string }\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "snake-case-type-names emits stable diagnostic codes"
    (fun _ctx ->
      let source = "type userProfile = int\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-type-names" ] ~actual:codes;
      Ok ());
  Test.case "descriptive-type-variables flags short type parameters"
    (fun _ctx ->
      let source = "type ('a, 'error) resultish = ('a, 'error) result\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "descriptive-type-variables" ] ~actual:codes;
      Ok ());
  Test.case "descriptive-type-variables keeps descriptive type parameters clean"
    (fun _ctx ->
      let source = "type ('value, 'error) resultish = ('value, 'error) result\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "descriptive-type-variables ignores nested type variable usages"
    (fun _ctx ->
      let source = "type 'value callback = 'a -> 'value\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain type-name violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"snake-case-type-names" ~snippet:"snake_case");
  Test.case
    "rule explanations explain short type variables"
    (fun _ctx -> assert_explanation_contains ~rule_id:"descriptive-type-variables" ~snippet:"'value");
  Test.case "snake-case-function-names flags camelCase function bindings"
    (fun _ctx ->
      let source = "let userProfile x = x\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-function-names" ] ~actual:codes;
      Ok ());
  Test.case "snake-case-function-names flags explicit fun bindings"
    (fun _ctx ->
      let source = "let userProfile = fun x -> x\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "snake-case-function-names keeps compliant function names clean"
    (fun _ctx ->
      let source = "let user_profile x = x\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "snake-case-function-names ignores camelCase value bindings"
    (fun _ctx ->
      let source = "let userProfile = 42\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Snake_case_function_names.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[] ~actual:codes;
      Ok ());
  Test.case "snake-case-function-names flags local camelCase function bindings"
    (fun _ctx ->
      let source = "let render x = let userProfile y = y in userProfile x\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-function-names" ] ~actual:codes;
      Ok ());
  Test.case
    "rule explanations explain function-name violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"snake-case-function-names" ~snippet:"parse_user");
  Test.case "class-case-module-names flags jiraffe-cased modules"
    (fun _ctx ->
      let source = "module Foo_bar = struct end\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "class-case-module-names" ] ~actual:codes;
      Ok ());
  Test.case "class-case-module-names flags jiraffe-cased module types"
    (fun _ctx ->
      let source = "module type Foo_bar = sig end\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "class-case-module-names keeps ClassCased modules clean"
    (fun _ctx ->
      let source = "module FooBar = struct end\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain module-name violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"class-case-module-names" ~snippet:"FooBar");
  Test.case "snake-case-variable-names flags camelCase value bindings"
    (fun _ctx ->
      let source = "let currentUser = 42\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-variable-names" ] ~actual:codes;
      Ok ());
  Test.case "snake-case-variable-names flags local camelCase value bindings"
    (fun _ctx ->
      let source = "let render x = let currentUser = x in currentUser\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-useless-let-return"; "snake-case-variable-names" ] ~actual:codes;
      Ok ());
  Test.case "snake-case-variable-names keeps compliant values clean"
    (fun _ctx ->
      let source = "let current_user = 42\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "snake-case-variable-names ignores camelCase function bindings"
    (fun _ctx ->
      let source = "let currentUser x = x\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-function-names" ] ~actual:codes;
      Ok ());
  Test.case
    "rule explanations explain variable-name violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"snake-case-variable-names" ~snippet:"current_user");
  Test.case "no-prime-variables flags prime-suffixed value bindings"
    (fun _ctx ->
      let source = "let current_user' = 42\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-prime-variables" ] ~actual:codes;
      Ok ());
  Test.case "no-prime-variables flags local prime-suffixed value bindings"
    (fun _ctx ->
      let source = "let render x = let state' = x in state'\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-prime-variables"; "no-useless-let-return" ] ~actual:codes;
      Ok ());
  Test.case "no-prime-variables keeps non-prime values clean"
    (fun _ctx ->
      let source = "let current_user2 = 42\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_prime_variables.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "no-prime-variables ignores prime-suffixed function bindings"
    (fun _ctx ->
      let source = "let current_user' x = x\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_prime_variables.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain prime-variable violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-prime-variables" ~snippet:"state2");
  Test.case "snake-case-argument-names flags camelCase positional arguments"
    (fun _ctx ->
      let source = "let render userId = userId\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-argument-names" ] ~actual:codes;
      Ok ());
  Test.case "snake-case-argument-names flags camelCase labeled arguments"
    (fun _ctx ->
      let source = "let render ~displayName current_user = current_user\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-argument-names" ] ~actual:codes;
      Ok ());
  Test.case "snake-case-argument-names flags camelCase optional arguments"
    (fun _ctx ->
      let source = "let render ?pageSize current_user = current_user\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-argument-names" ] ~actual:codes;
      Ok ());
  Test.case "snake-case-argument-names keeps compliant arguments clean"
    (fun _ctx ->
      let source = "let render ~display_name ?page_size current_user = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Snake_case_argument_names.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain argument-name violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"snake-case-argument-names" ~snippet:"display_name");
  Test.case "ordered-argument-kinds flags labeled arguments after positional ones"
    (fun _ctx ->
      let source = "let render current_user ~display_name = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "ordered-argument-kinds" ] ~actual:codes;
      Ok ());
  Test.case "ordered-argument-kinds flags optional arguments after positional ones"
    (fun _ctx ->
      let source = "let render current_user ?page_size = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "ordered-argument-kinds" ] ~actual:codes;
      Ok ());
  Test.case "ordered-argument-kinds flags labeled arguments after optional ones"
    (fun _ctx ->
      let source = "let render ?page_size ~display_name current_user = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "ordered-argument-kinds" ] ~actual:codes;
      Ok ());
  Test.case "ordered-argument-kinds keeps compliant argument order clean"
    (fun _ctx ->
      let source = "let render ~display_name ?page_size current_user = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "ordered-argument-kinds reports only one issue per function"
    (fun _ctx ->
      let source = "let render current_user ~display_name ?page_size = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Ordered_argument_kinds.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain argument-order violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"ordered-argument-kinds" ~snippet:"labeled arguments");
  Test.case "no-open-bang flags forceful open statements"
    (fun _ctx ->
      let source = "open! List\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_open_bang.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-open-bang" ] ~actual:codes;
      Ok ());
  Test.case "no-open-bang keeps plain open statements clean"
    (fun _ctx ->
      let source = "open List\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_open_bang.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain open! violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-open-bang" ~snippet:"open!");
  Test.case "limit-open-statements flags a third file-level open"
    (fun _ctx ->
      let source = "open Std\nopen Http\nopen Json\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.Limit_open_statements.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "limit-open-statements" ] ~actual:codes;
      Ok ());
  Test.case "limit-open-statements keeps one or two opens clean"
    (fun _ctx ->
      let source = "open Std\nopen Http\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.Limit_open_statements.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "limit-open-statements reports only one issue per file"
    (fun _ctx ->
      let source = "open Std\nopen Http\nopen Json\nopen Uri\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.Limit_open_statements.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain open-count violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"limit-open-statements" ~snippet:"two open statements");
  Test.case "no-exn-suffix-functions flags exception-style function names"
    (fun _ctx ->
      let source = "let parse_exn text = text\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_exn_suffix_functions.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-exn-suffix-functions" ] ~actual:codes;
      Ok ());
  Test.case "no-exn-suffix-functions flags local exception-style function names"
    (fun _ctx ->
      let source = "let render text = let parse_exn value = value in parse_exn text\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_exn_suffix_functions.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-exn-suffix-functions" ] ~actual:codes;
      Ok ());
  Test.case "no-exn-suffix-functions ignores non-function bindings"
    (fun _ctx ->
      let source = "let parse_exn = cached_value\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_exn_suffix_functions.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain _exn function names"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-exn-suffix-functions" ~snippet:"parse_exn");
  Test.case "no-unnecessary-rec flags recursive bindings without self-reference"
    (fun _ctx ->
      let source = "let rec render x = x + 1\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_unnecessary_rec.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-unnecessary-rec" ] ~actual:codes;
      Ok ());
  Test.case "no-unnecessary-rec keeps real recursive bindings clean"
    (fun _ctx ->
      let source = "let rec loop x = loop x\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_unnecessary_rec.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "no-unnecessary-rec skips anonymous bindings safely"
    (fun _ctx ->
      let source = "let () = Miniriot.run ~main:(fun ~args -> Bench.Cli.main ~name:\"bench\" ~benchmarks:Bench.[] ~args) ~args:Env.args ()\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_unnecessary_rec.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:[] ~actual:(diagnostic_rule_ids result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain unnecessary rec"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-unnecessary-rec" ~snippet:"Remove rec");
  Test.case "default pipeline handles mutual type declarations safely"
    (fun _ctx ->
      let source = "type first = One and second = Two\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:[] ~actual:(diagnostic_rule_ids result.diagnostics);
      Ok ());
  Test.case "no-useless-let-return flags redundant passthrough bindings"
    (fun _ctx ->
      let source = "let render x = let value = parse x in value\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_useless_let_return.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-useless-let-return" ] ~actual:codes;
      Ok ());
  Test.case "no-useless-let-return keeps meaningful let bodies clean"
    (fun _ctx ->
      let source = "let render x = let value = parse x in log value\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_useless_let_return.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain useless let returns"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-useless-let-return" ~snippet:"let value = load_config () in value");
  Test.case "no-redundant-else-unit flags else branches that only return unit"
    (fun _ctx ->
      let source = "let render ok = if ok then log () else ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_redundant_else_unit.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-redundant-else-unit" ] ~actual:codes;
      Ok ());
  Test.case "no-redundant-else-unit keeps meaningful else branches clean"
    (fun _ctx ->
      let source = "let render ok = if ok then log () else fallback ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_redundant_else_unit.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain redundant else unit branches"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-redundant-else-unit" ~snippet:"else ()");
  Test.case "no-boolean-comparisons-in-conditionals flags equality to true"
    (fun _ctx ->
      let source = "let render is_ready = if is_ready = true then log ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_boolean_comparisons_in_conditionals.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-boolean-comparisons-in-conditionals" ] ~actual:codes;
      Ok ());
  Test.case "no-boolean-comparisons-in-conditionals flags equality to false"
    (fun _ctx ->
      let source = "let render is_ready = if is_ready = false then log ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_boolean_comparisons_in_conditionals.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-boolean-comparisons-in-conditionals" ] ~actual:codes;
      Ok ());
  Test.case "no-boolean-comparisons-in-conditionals flags inequality to false"
    (fun _ctx ->
      let source = "let render is_ready = if is_ready != false then log ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_boolean_comparisons_in_conditionals.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-boolean-comparisons-in-conditionals" ] ~actual:codes;
      Ok ());
  Test.case "no-boolean-comparisons-in-conditionals keeps direct conditions clean"
    (fun _ctx ->
      let source = "let render is_ready = if is_ready then log ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_boolean_comparisons_in_conditionals.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain boolean conditional comparisons"
    (fun _ctx ->
      assert_explanation_contains ~rule_id:"no-boolean-comparisons-in-conditionals" ~snippet:"if is_ready then render ()");
  Test.case "prefer-sequences-over-let-unit flags let-unit sequencing"
    (fun _ctx ->
      let source = "let render () = let () = log () in flush ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_sequences_over_let_unit.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-sequences-over-let-unit" ] ~actual:codes;
      Ok ());
  Test.case "prefer-sequences-over-let-unit keeps named let bindings clean"
    (fun _ctx ->
      let source = "let render () = let flushed = flush () in flushed\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_sequences_over_let_unit.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain let-unit sequencing"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-sequences-over-let-unit" ~snippet:"log (); flush ()");
  Test.case "prefer-if-over-bool-match flags full boolean matches"
    (fun _ctx ->
      let source = "let render ready = match ready with true -> render () | false -> fallback ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_if_over_bool_match.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-if-over-bool-match" ] ~actual:codes;
      Ok ());
  Test.case "prefer-if-over-bool-match flags false-with-unit fallback matches"
    (fun _ctx ->
      let source = "let render ready = match ready with false -> render () | _ -> ()\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_if_over_bool_match.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-if-over-bool-match" ] ~actual:codes;
      Ok ());
  Test.case "prefer-if-over-bool-match keeps non-boolean matches clean"
    (fun _ctx ->
      let source = "let render opt = match opt with Some x -> x | None -> 0\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_if_over_bool_match.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain boolean match rewrites"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-if-over-bool-match" ~snippet:"if is_ready then render () else fallback ()");
  Test.case "alphabetized-named-arguments flags unsorted labeled arguments"
    (fun _ctx ->
      let source = "let render ~zebra ~alpha current_user = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Alphabetized_named_arguments.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "alphabetized-named-arguments" ] ~actual:codes;
      Ok ());
  Test.case "alphabetized-named-arguments flags unsorted optional arguments"
    (fun _ctx ->
      let source = "let render ?zebra ?alpha current_user = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Alphabetized_named_arguments.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "alphabetized-named-arguments" ] ~actual:codes;
      Ok ());
  Test.case "alphabetized-named-arguments keeps each kind group independent"
    (fun _ctx ->
      let source = "let render ~zebra ?alpha current_user = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Alphabetized_named_arguments.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "alphabetized-named-arguments reports one issue per function"
    (fun _ctx ->
      let source = "let render ~zebra ~alpha ~beta current_user = current_user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Alphabetized_named_arguments.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain named-argument sorting violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"alphabetized-named-arguments" ~snippet:"Alphabetical order");
  Test.case "t-first-named-arguments flags t after other positional arguments"
    (fun _ctx ->
      let source = "let render ~width ~height other t = t\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.T_first_named_arguments.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "t-first-named-arguments" ] ~actual:codes;
      Ok ());
  Test.case "t-first-named-arguments keeps t-first positional arguments clean"
    (fun _ctx ->
      let source = "let render ~width ~height t other = t\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.T_first_named_arguments.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "t-first-named-arguments ignores functions without named arguments"
    (fun _ctx ->
      let source = "let render other t = t\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.T_first_named_arguments.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "t-first-named-arguments ignores functions without positional t"
    (fun _ctx ->
      let source = "let render ~width other current = current\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.T_first_named_arguments.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain t-first named argument violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"t-first-named-arguments" ~snippet:"receiver");
  Test.case "snake-case-record-fields flags camelCase record fields"
    (fun _ctx ->
      let source = "type user = { displayName : string; created_at : int }\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Snake_case_record_fields.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-record-fields" ] ~actual:codes;
      Ok ());
  Test.case "snake-case-record-fields keeps snake_case fields clean"
    (fun _ctx ->
      let source = "type user = { display_name : string; created_at : int }\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Snake_case_record_fields.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain record-field violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"snake-case-record-fields" ~snippet:"display_name");
  Test.case "class-case-constructors flags underscored constructors"
    (fun _ctx ->
      let source = "type user = | Guest_user | RegisteredUser\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Class_case_constructors.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "class-case-constructors" ] ~actual:codes;
      Ok ());
  Test.case "class-case-constructors keeps ClassCased constructors clean"
    (fun _ctx ->
      let source = "type user = | GuestUser | RegisteredUser\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Class_case_constructors.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain constructor-name violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"class-case-constructors" ~snippet:"GuestUser");
  Test.case "snake-case-polyvariant-tags flags non-snake-case tags"
    (fun _ctx ->
      let source = "type user = [ `GuestUser | `registered_user ]\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Snake_case_polyvariant_tags.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "snake-case-polyvariant-tags" ] ~actual:codes;
      Ok ());
  Test.case "snake-case-polyvariant-tags keeps snake_case tags clean"
    (fun _ctx ->
      let source = "type user = [ `guest_user | `registered_user ]\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Snake_case_polyvariant_tags.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain polyvariant-tag violations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"snake-case-polyvariant-tags" ~snippet:"guest_user");
  Test.case "avoid-single-letter-function-names flags placeholder bindings"
    (fun _ctx ->
      let source = "let f x = x\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "avoid-single-letter-function-names" ] ~actual:codes;
      Ok ());
  Test.case "avoid-single-letter-function-names flags local placeholder bindings"
    (fun _ctx ->
      let source = "let render x = let g y = y in g x\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "avoid-single-letter-function-names" ] ~actual:codes;
      Ok ());
  Test.case "avoid-single-letter-function-names keeps descriptive bindings clean"
    (fun _ctx ->
      let source = "let render_user x = x\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "avoid-single-letter-function-names ignores placeholder value bindings"
    (fun _ctx ->
      let source = "let f = 42\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Avoid_single_letter_function_names.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain single-letter function bindings"
    (fun _ctx -> assert_explanation_contains ~rule_id:"avoid-single-letter-function-names" ~snippet:"Placeholder names");
  Test.case "avoid-single-letter-type-names flags placeholder type names"
    (fun _ctx ->
      let source = "type x = int\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "avoid-single-letter-type-names" ] ~actual:codes;
      Ok ());
  Test.case "avoid-single-letter-type-names allows t"
    (fun _ctx ->
      let source = "type t = int\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Avoid_single_letter_type_names.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "avoid-single-letter-type-names keeps descriptive names clean"
    (fun _ctx ->
      let source = "type user_profile = int\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain single-letter type names"
    (fun _ctx -> assert_explanation_contains ~rule_id:"avoid-single-letter-type-names" ~snippet:"conventional `t`");
  Test.case "prefer-multiline-string-literals flags chained string literals"
    (fun _ctx ->
      let source = "let banner = \"hello \" ^ \"world\" ^ \"!\"\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_multiline_string_literals.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-multiline-string-literals" ] ~actual:codes;
      Ok ());
  Test.case "prefer-multiline-string-literals ignores mixed concatenations"
    (fun _ctx ->
      let source = "let banner name = \"hello \" ^ name\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_multiline_string_literals.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain multiline string preference"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-multiline-string-literals" ~snippet:"multiline literal");
  Test.case "no-custom-operators flags symbolic custom operators"
    (fun _ctx ->
      let source = "let composed = f %> g\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_custom_operators.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-custom-operators" ] ~actual:codes;
      Ok ());
  Test.case "no-custom-operators allows builtin operators"
    (fun _ctx ->
      let source = "let sum = a + b\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_custom_operators.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain custom operators"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-custom-operators" ~snippet:"hard to search");
  Test.case "prefer-pipelines-for-nested-calls flags very deep call chains"
    (fun _ctx ->
      let source = "let rendered = foo (bar (baz (hex 1)))\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_pipelines_for_nested_calls.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-pipelines-for-nested-calls" ] ~actual:codes;
      Ok ());
  Test.case "prefer-pipelines-for-nested-calls keeps shorter chains clean"
    (fun _ctx ->
      let source = "let rendered = foo (bar (baz 1))\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_pipelines_for_nested_calls.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain nested pipeline preference"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-pipelines-for-nested-calls" ~snippet:"hex 1 |> baz |> bar |> foo");
  Test.case "no-inline-parameter-type-annotations flags typed positional parameters"
    (fun _ctx ->
      let source = "let render (user_id : int) (enabled : bool) = user_id\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_inline_parameter_type_annotations.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-inline-parameter-type-annotations" ] ~actual:codes;
      Ok ());
  Test.case "no-inline-parameter-type-annotations keeps unsigned parameters clean"
    (fun _ctx ->
      let source = "let render user_id enabled = user_id\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_inline_parameter_type_annotations.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain inline parameter annotations"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-inline-parameter-type-annotations" ~snippet:"Function signatures");
  Test.case "no-function-shorthand flags named function shorthand"
    (fun _ctx ->
      let source = "let render = function | x -> x + 1\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_function_shorthand.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-function-shorthand" ] ~actual:codes;
      Ok ());
  Test.case "no-function-shorthand keeps fun expressions clean"
    (fun _ctx ->
      let source = "let render = fun x -> x + 1\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_function_shorthand.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain function shorthand"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-function-shorthand" ~snippet:"Explicit parameters");
  Test.case "limit-function-parameters flags five positional parameters"
    (fun _ctx ->
      let source = "let render a b c d e = a\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_function_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "limit-function-parameters" ] ~actual:codes;
      Ok ());
  Test.case "limit-function-parameters flags eight named parameters"
    (fun _ctx ->
      let source = "let render ~a ~b ~c ~d ~e ~f ~g ~h = a\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_function_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "limit-function-parameters" ] ~actual:codes;
      Ok ());
  Test.case "limit-function-parameters flags mixed parameter lists at ten"
    (fun _ctx ->
      let source = "let render ~a ~b ~c ~d ~e x y z q r = a\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_function_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "limit-function-parameters" ] ~actual:codes;
      Ok ());
  Test.case "limit-function-parameters keeps shorter signatures clean"
    (fun _ctx ->
      let source = "let render ~a ~b x y = a\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_function_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain parameter count limits"
    (fun _ctx -> assert_explanation_contains ~rule_id:"limit-function-parameters" ~snippet:"record-shaped concept");
  Test.case "limit-parenthesis-depth flags deeply parenthesized expressions"
    (fun _ctx ->
      let source = "let wrapped = (((((value)))))\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_parenthesis_depth.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "limit-parenthesis-depth" ] ~actual:codes;
      Ok ());
  Test.case "limit-parenthesis-depth keeps shallower expressions clean"
    (fun _ctx ->
      let source = "let wrapped = ((((value))))\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_parenthesis_depth.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "limit-parenthesis-depth reports one issue per deep chain"
    (fun _ctx ->
      let source = "let wrapped = ((((((value))))))\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_parenthesis_depth.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain parenthesis depth limits"
    (fun _ctx -> assert_explanation_contains ~rule_id:"limit-parenthesis-depth" ~snippet:"parenthesized expressions");
  Test.case "limit-nested-match-depth flags triple-nested matches"
    (fun _ctx ->
      let source = {|
let render x y z =
  match x with
  | _ ->
      match y with
      | _ ->
          match z with
          | _ -> 1
|}
      in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_nested_match_depth.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "limit-nested-match-depth" ] ~actual:codes;
      Ok ());
  Test.case "limit-nested-match-depth keeps shallower matches clean"
    (fun _ctx ->
      let source = {|
let render x y =
  match x with
  | _ ->
      match y with
      | _ -> 1
|}
      in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_nested_match_depth.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "limit-nested-match-depth reports one issue per match tower"
    (fun _ctx ->
      let source = {|
let render x y z =
  match x with
  | _ ->
      match y with
      | _ ->
          match z with
          | _ -> 1
|}
      in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Limit_nested_match_depth.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain nested match depth limits"
    (fun _ctx -> assert_explanation_contains ~rule_id:"limit-nested-match-depth" ~snippet:"match towers");
  Test.case "no-redundant-parentheses flags obvious grouping around identifiers"
    (fun _ctx ->
      let source = "let render value = (value)\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_redundant_parentheses.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-redundant-parentheses" ] ~actual:codes;
      Ok ());
  Test.case "no-redundant-parentheses reports one issue per redundant chain"
    (fun _ctx ->
      let source = "let render value = ((value))\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_redundant_parentheses.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "no-redundant-parentheses keeps grouped infix expressions clean"
    (fun _ctx ->
      let source = "let render value = (value + 1)\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_redundant_parentheses.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain redundant parentheses"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-redundant-parentheses" ~snippet:"obvious grouping");
  Test.case "no-eta-expansion flags unary eta expansion"
    (fun _ctx ->
      let source = "let wrap foo = fun value -> foo value\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_eta_expansion.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-eta-expansion" ] ~actual:codes;
      Ok ());
  Test.case "no-eta-expansion flags multi-parameter eta expansion"
    (fun _ctx ->
      let source = "let wrap foo = fun left right -> foo left right\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_eta_expansion.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-eta-expansion" ] ~actual:codes;
      Ok ());
  Test.case "no-eta-expansion keeps transformed calls clean"
    (fun _ctx ->
      let source = "let wrap foo = fun value -> foo (normalize value)\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_eta_expansion.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain eta expansion"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-eta-expansion" ~snippet:"eta-expanded");
  Test.case "no-redundant-reraise flags handlers that only re-raise"
    (fun _ctx ->
      let source = "let render value = try render_inner value with exn -> raise exn\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_redundant_reraise.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-redundant-reraise" ] ~actual:codes;
      Ok ());
  Test.case "no-redundant-reraise keeps useful handlers clean"
    (fun _ctx ->
      let source = "let render value = try render_inner value with Not_found -> default ()\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.No_redundant_reraise.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain redundant reraises"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-redundant-reraise" ~snippet:"raise exn");
  Test.case "no-redundant-begin-end flags begin/end grouping"
    (fun _ctx ->
      let source = "let render value = begin value end\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_redundant_begin_end.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-redundant-begin-end" ] ~actual:codes;
      Ok ());
  Test.case "no-redundant-begin-end keeps ordinary parentheses clean"
    (fun _ctx ->
      let source = "let render value = (value + 1)\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_redundant_begin_end.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain redundant begin/end"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-redundant-begin-end" ~snippet:"begin ... end");
  Test.case "prefer-scoped-field-access flags module-qualified record access"
    (fun _ctx ->
      let source = "let render record = record.Module.field\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_scoped_field_access.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-scoped-field-access" ] ~actual:codes;
      Ok ());
  Test.case "prefer-scoped-field-access keeps normal field access clean"
    (fun _ctx ->
      let source = "let render record = record.field\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_scoped_field_access.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "prefer-scoped-field-access flags repeated qualified record fields"
    (fun _ctx ->
      let source = "let build value next = { Module.field = value; Module.other = next }\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_scoped_field_access.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-scoped-field-access" ] ~actual:codes;
      Ok ());
  Test.case "prefer-scoped-field-access keeps mixed record field qualifiers clean"
    (fun _ctx ->
      let source = "let build value next = { Module.field = value; other = next }\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_scoped_field_access.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "prefer-scoped-field-access flags let-open bracket forms"
    (fun _ctx ->
      let source = "let xs = let open Libc in [| epipe; enoent |]\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_scoped_field_access.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-scoped-field-access" ] ~actual:codes;
      Ok ());
  Test.case "prefer-scoped-field-access keeps stacked local opens clean"
    (fun _ctx ->
      let source = "let xs = let open A in let open B in [| x |]\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_scoped_field_access.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain scoped field access"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-scoped-field-access" ~snippet:"Module.{ field = value }");
  Test.case "prefer-t-for-single-type-modules flags modules with one non-t type"
    (fun _ctx ->
      let source = "module User = struct type user = { name : string } end\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_t_for_single_type_modules.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-t-for-single-type-modules" ] ~actual:codes;
      Ok ());
  Test.case "prefer-t-for-single-type-modules keeps single t modules clean"
    (fun _ctx ->
      let source = "module User = struct type t = { name : string } end\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_t_for_single_type_modules.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "prefer-t-for-single-type-modules flags module types with one non-t type"
    (fun _ctx ->
      let source = "module type USER = sig type user end\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_t_for_single_type_modules.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-t-for-single-type-modules" ] ~actual:codes;
      Ok ());
  Test.case
    "rule explanations explain single type modules"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-t-for-single-type-modules" ~snippet:"User.t");
  Test.case "no-public-mutable-fields flags mutable record fields in interfaces"
    (fun _ctx ->
      let source = "type t = { mutable state : int }\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_public_mutable_fields.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.mli") pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-public-mutable-fields" ] ~actual:codes;
      Ok ());
  Test.case "no-public-mutable-fields keeps implementation-only mutability clean"
    (fun _ctx ->
      let source = "type t = { mutable state : int }\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_public_mutable_fields.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.ml") pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain public mutable fields"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-public-mutable-fields" ~snippet:"mutable field");
  Test.case "no-positional-bool-parameters flags inline bool parameters"
    (fun _ctx ->
      let source = "let render (enabled : bool) user = user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_positional_bool_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-positional-bool-parameters" ] ~actual:codes;
      Ok ());
  Test.case "no-positional-bool-parameters flags bool arrows in interfaces"
    (fun _ctx ->
      let source = "val render : bool -> user -> user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_positional_bool_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.mli") pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "no-positional-bool-parameters" ] ~actual:codes;
      Ok ());
  Test.case "no-positional-bool-parameters keeps named bool arrows clean"
    (fun _ctx ->
      let source = "val render : enabled:bool -> user -> user\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.No_positional_bool_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.mli") pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain positional bool parameters"
    (fun _ctx -> assert_explanation_contains ~rule_id:"no-positional-bool-parameters" ~snippet:"~enabled");
  Test.case "prefer-named-closed-polyvariants flags inline closed polyvariants in values"
    (fun _ctx ->
      let source = "val decode : [ `json | `xml ] -> payload\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_named_closed_polyvariants.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.mli") pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-named-closed-polyvariants" ] ~actual:codes;
      Ok ());
  Test.case "prefer-named-closed-polyvariants flags nested closed polyvariants in aliases"
    (fun _ctx ->
      let source = "type formats = [ `json | `xml ] list\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_named_closed_polyvariants.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-named-closed-polyvariants" ] ~actual:codes;
      Ok ());
  Test.case "prefer-named-closed-polyvariants keeps named top-level polyvariants clean"
    (fun _ctx ->
      let source = "type format = [ `json | `xml ]\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_named_closed_polyvariants.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain named closed polyvariants"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-named-closed-polyvariants" ~snippet:"type format");
  Test.case "prefer-opaque-record-types flags public record types with matching accessors"
    (fun _ctx ->
      let source = "type t = { name : string }\nval name : t -> string\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_opaque_record_types.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.mli") pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-opaque-record-types" ] ~actual:codes;
      Ok ());
  Test.case "prefer-opaque-record-types keeps record types without accessors clean"
    (fun _ctx ->
      let source = "type t = { name : string }\nval render : t -> view\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_opaque_record_types.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.mli") pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "prefer-opaque-record-types keeps implementation records clean"
    (fun _ctx ->
      let source = "type t = { name : string }\nlet name t = t.name\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_opaque_record_types.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.ml") pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain opaque record types"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-opaque-record-types" ~snippet:"type t");
  Test.case "require-module-interfaces flags src modules without sibling mli files"
    (fun _ctx ->
      with_tempdir "tusk_fix_interfaces"
        (fun tmpdir ->
          let src_dir = Path.(tmpdir / Path.v "packages" / Path.v "app" / Path.v "src") in
          let file = Path.(src_dir / Path.v "session_store.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file file "let load () = ()\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal
            ~expected:[ "require-module-interfaces" ]
            ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case "require-module-interfaces keeps src modules with sibling mli files clean"
    (fun _ctx ->
      with_tempdir "tusk_fix_interfaces"
        (fun tmpdir ->
          let src_dir = Path.(tmpdir / Path.v "packages" / Path.v "app" / Path.v "src") in
          let file = Path.(src_dir / Path.v "session_store.ml") in
          let interface = Path.(src_dir / Path.v "session_store.mli") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file file "let load () = ()\n";
          write_file interface "val load : unit -> unit\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal ~expected:[] ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case "require-module-interfaces ignores src main modules"
    (fun _ctx ->
      with_tempdir "tusk_fix_interfaces"
        (fun tmpdir ->
          let src_dir = Path.(tmpdir / Path.v "packages" / Path.v "app" / Path.v "src") in
          let file = Path.(src_dir / Path.v "main.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file file "let main = ()\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal ~expected:[] ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case
    "rule explanations explain missing module interfaces"
    (fun _ctx -> assert_explanation_contains ~rule_id:"require-module-interfaces" ~snippet:".mli");
  Test.case "snake-case-source-paths flags non-snake-case source filenames"
    (fun _ctx ->
      with_tempdir "tusk_fix_source_paths"
        (fun tmpdir ->
          let src_dir = Path.(tmpdir / Path.v "packages" / Path.v "app" / Path.v "src") in
          let file = Path.(src_dir / Path.v "sessionStore.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file file "let session_store = ()\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal
            ~expected:[ "require-module-interfaces"; "snake-case-source-paths" ]
            ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case "snake-case-source-paths flags non-snake-case source directories"
    (fun _ctx ->
      with_tempdir "tusk_fix_source_paths"
        (fun tmpdir ->
          let src_dir =
            Path.(tmpdir / Path.v "packages" / Path.v "app" / Path.v "src" / Path.v "JsonHelpers") in
          let file = Path.(src_dir / Path.v "session_store.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file file "let session_store = ()\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal
            ~expected:[ "require-module-interfaces"; "snake-case-source-paths" ]
            ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case "snake-case-source-paths keeps snake_case source paths clean"
    (fun _ctx ->
      with_tempdir "tusk_fix_source_paths"
        (fun tmpdir ->
          let src_dir =
            Path.(tmpdir / Path.v "packages" / Path.v "app" / Path.v "src" / Path.v "json_helpers") in
          let file = Path.(src_dir / Path.v "session_store.ml") in
          let interface = Path.(src_dir / Path.v "session_store.mli") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file file "let session_store = ()\n";
          write_file interface "val session_store : unit\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal ~expected:[] ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case
    "rule explanations explain snake_case source paths"
    (fun _ctx -> assert_explanation_contains ~rule_id:"snake-case-source-paths" ~snippet:"snake_case");
  Test.case "package-name-style flags package names that do not start with a letter"
    (fun _ctx ->
      with_tempdir "tusk_fix_package_names"
        (fun tmpdir ->
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "1bad") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let file = Path.(src_dir / Path.v "main.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"1bad\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/main.ml\"\n";
          write_file file "let main = ()\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal
            ~expected:[ "package-name-style" ]
            ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case "package-name-style flags non-kebab-case package names"
    (fun _ctx ->
      with_tempdir "tusk_fix_package_names"
        (fun tmpdir ->
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "bad_pkg") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let file = Path.(src_dir / Path.v "main.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"bad_pkg\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/main.ml\"\n";
          write_file file "let main = ()\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal
            ~expected:[ "package-name-style" ]
            ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case "package-name-style flags trailing separators in package names"
    (fun _ctx ->
      with_tempdir "tusk_fix_package_names"
        (fun tmpdir ->
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "bad-app-") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let file = Path.(src_dir / Path.v "main.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"bad-app-\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/main.ml\"\n";
          write_file file "let main = ()\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal
            ~expected:[ "package-name-style" ]
            ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case "package-name-style keeps good package names clean"
    (fun _ctx ->
      with_tempdir "tusk_fix_package_names"
        (fun tmpdir ->
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "good-app") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let file = Path.(src_dir / Path.v "main.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"good-app\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/main.ml\"\n";
          write_file file "let main = ()\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_equal ~expected:[] ~actual:(diagnostic_rule_ids result.diagnostics);
          Ok ()));
  Test.case
    "rule explanations explain package name style"
    (fun _ctx -> assert_explanation_contains ~rule_id:"package-name-style" ~snippet:"kebab-case");
  Test.case "prefer-records-over-large-tuples flags repeated tuple aliases"
    (fun _ctx ->
      let source = "type user = string * string * string * string\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_records_over_large_tuples.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal
        ~expected:[ "prefer-records-over-large-tuples" ]
        ~actual:(diagnostic_rule_ids result.diagnostics);
      Ok ());
  Test.case "prefer-records-over-large-tuples flags five-element tuple aliases"
    (fun _ctx ->
      let source = "type user = int * string * bool * float * bytes\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_records_over_large_tuples.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal
        ~expected:[ "prefer-records-over-large-tuples" ]
        ~actual:(diagnostic_rule_ids result.diagnostics);
      Ok ());
  Test.case "prefer-records-over-large-tuples keeps smaller mixed tuples clean"
    (fun _ctx ->
      let source = "type user = int * string * bool * float\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_records_over_large_tuples.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:[] ~actual:(diagnostic_rule_ids result.diagnostics);
      Ok ());
  Test.case "prefer-records-over-large-tuples flags large tuple signatures"
    (fun _ctx ->
      let source = "val user : int * string * bool * float * bytes -> unit\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_records_over_large_tuples.make () ]
        () in
      let result = Tusk_fix.Pipeline.run ~filename:(Path.v "sample.mli") pipeline source in
      Test.assert_equal
        ~expected:[ "prefer-records-over-large-tuples" ]
        ~actual:(diagnostic_rule_ids result.diagnostics);
      Ok ());
  Test.case "prefer-records-over-large-tuples flags repeated constructor payloads"
    (fun _ctx ->
      let source = "type event = Event of string * string * string * string\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_records_over_large_tuples.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal
        ~expected:[ "prefer-records-over-large-tuples" ]
        ~actual:(diagnostic_rule_ids result.diagnostics);
      Ok ());
  Test.case
    "rule explanations explain large tuple aliases"
    (fun _ctx -> assert_explanation_contains ~rule_id:"prefer-records-over-large-tuples" ~snippet:"record");
  Test.case "cli list-rules text output prints one rule per line"
    (fun _ctx ->
      let output = Tusk_fix.Cli.list_rules_output ~format:Tusk_fix.Reporter.Text in
      Test.assert_true (String.contains output "riot:");
      Test.assert_true (String.contains output "  Readability:");
      Test.assert_true
        (String.contains output "  \027[1mriot:snake-case-type-names\027[0m - Type names should use snake_case instead of camelCase");
      Test.assert_true
        (String.contains output "  \027[1mriot:snake-case-variable-names\027[0m - Variable names should use snake_case instead of camelCase");
      Test.assert_true
        (String.contains output "  \027[1mriot:prefer-record-destructuring-parameters\027[0m - Functions that immediately destructure a record argument should destructure it in the parameter");
      Test.assert_true (not (String.contains output "\027[1msnake-case-type-names\027[0m"));
      Test.assert_true (not (String.contains output "f0101:snake-case-type-names"));
      Ok ());
  Test.case "cli list-rules json output includes builtin rules"
    (fun _ctx ->
      let output = Tusk_fix.Cli.list_rules_output ~format:Tusk_fix.Reporter.Json in
      Test.assert_true (String.contains output "\"snake-case-type-names\"");
      Test.assert_true (String.contains output "\"category\":\"Readability\"");
      Test.assert_true (String.contains output "\"prefer-record-destructuring-parameters\"");
      Test.assert_true (not (String.contains output "\"F0101\""));
      Ok ());
  Test.case "cli list-diagnostics text output includes builtin and package diagnostics"
    (fun _ctx ->
      let output = Tusk_fix.Cli.list_diagnostics_output ~format:Tusk_fix.Reporter.Text in
      Test.assert_true
        (String.contains output "\027[1mriot:snake-case-type-names\027[0m - Type names should use snake_case instead of camelCase");
      Test.assert_true
        (String.contains output "\027[1mriot:descriptive-type-variables\027[0m - Type variables in type definitions should use descriptive names instead of short placeholders");
      Ok ());
  Test.case "cli list-diagnostics json output includes builtin and package diagnostics"
    (fun _ctx ->
      let output = Tusk_fix.Cli.list_diagnostics_output ~format:Tusk_fix.Reporter.Json in
      Test.assert_true (String.contains output "\"rule_id\":\"riot:snake-case-type-names\"");
      Test.assert_true (String.contains output "\"rule_id\":\"riot:descriptive-type-variables\"");
      Ok ());
  Test.case "cli run_result stops after the requested diagnostic limit"
    (fun _ctx ->
      with_tempdir "tusk_fix_limit"
        (fun tmpdir ->
          let file1 = Path.(tmpdir / Path.v "a.ml") in
          let file2 = Path.(tmpdir / Path.v "b.ml") in
          let file3 = Path.(tmpdir / Path.v "c.ml") in
          write_file file1 "type userProfile = int\n";
          write_file file2 "type accountProfile = int\n";
          write_file file3 "type sessionState = int\n";
          let outcome = Tusk_fix.Cli.run_result
            ~mode:Tusk_fix.Runner.Check
            ~scope:None
            ~limit:(Some 1)
            ~files:[ file1; file2; file3 ] in
          Test.assert_true outcome.limit_reached;
          Test.assert_equal ~expected:1 ~actual:outcome.result.summary.total_files;
          Test.assert_equal ~expected:1 ~actual:outcome.result.summary.remaining_diagnostics;
          Ok ()));
  Test.case "cli rejects non-positive diagnostic limits"
    (fun _ctx ->
      with_tempdir "tusk_fix_limit"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          write_file file "type userProfile = int\n";
          let result =
            with_cwd tmpdir (fun () -> run_cli [ "--check"; "--limit"; "0"; Path.to_string file ])
          in
          Test.assert_error result;
          Ok ()));
  Test.case "snake-case-type-names ignores non-type camelCase identifiers"
    (fun _ctx ->
      let source = "let userProfile = 42\n" in
      let pipeline = Tusk_fix.Pipeline.make ~rules:[ Tusk_fix.Rules.Snake_case_type_names.make () ] () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[] ~actual:codes;
      Ok ());
  Test.case "snake-case-type-names ignores module qualifiers in extensible types"
    (fun _ctx ->
      let source = "type Message.t += Added\n" in
      let result = Tusk_fix.Pipeline.run (Tusk_fix.Pipeline.default ()) source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "package rule override disables snake-case-type-names locally"
    (fun _ctx ->
      with_tempdir "tusk_fix_config"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "kernel") in
          let package_toml = Path.(package_dir / Path.v "tusk.toml") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let file = Path.(src_dir / Path.v "file.ml") in
          let interface = Path.(src_dir / Path.v "file.mli") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/kernel\"]\n\n[tusk.fix]\nrules = [\"snake-case-type-names\"]\n";
          write_file package_toml "[package]\nname = \"kernel\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [\"-snake-case-type-names\"]\n\n[lib]\npath = \"src/kernel.ml\"\n";
          write_file file "type userProfile = int\n";
          write_file interface "type user_profile = int\n";
          let scope = Tusk_fix.Config.load_scope ~cwd:tmpdir |> Option.expect ~msg:"expected workspace scope" in
          let pipeline = Tusk_fix.Config.pipeline_for_file (Some scope) file in
          let result = Tusk_fix.Pipeline.run pipeline ~filename:file "type userProfile = int\n" in
          Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
          Ok ()));
  Test.case "workspace ignore patterns exclude matching files"
    (fun _ctx ->
      with_tempdir "tusk_fix_ignore"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let ignored = Path.(src_dir / Path.v "ignored.ml") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nignore = [\"ignored.ml\"]\nrules = [\"snake-case-type-names\"]\n";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/app.ml\"\n";
          write_file ignored "type userProfile = int\n";
          let scope = Tusk_fix.Config.load_scope ~cwd:tmpdir |> Option.expect ~msg:"expected workspace scope" in
          Test.assert_true (Tusk_fix.Config.should_ignore_file (Some scope) ignored);
          Ok ()));
  Test.case "config shorthand enables and disables rules"
    (fun _ctx ->
      with_tempdir "tusk_fix_rules"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let file = Path.(src_dir / Path.v "file.ml") in
          let interface = Path.(src_dir / Path.v "file.mli") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nrules = [\"snake-case-type-names\"]\n";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [\"-snake-case-type-names\"]\n\n[lib]\npath = \"src/app.ml\"\n";
          write_file file "type userProfile = int\n";
          write_file interface "type user_profile = int\n";
          let result = Tusk_fix.Runner.run_files
            ~pipeline_for_file:(Tusk_fix.Config.pipeline_for_file
              (Tusk_fix.Config.load_scope ~cwd:tmpdir))
            ~mode:Tusk_fix.Runner.Check [ file ] in
          Test.assert_equal ~expected:0 ~actual:result.summary.remaining_diagnostics;
          Ok ()));
  Test.case "workspace rule overrides keep builtins enabled by default"
    (fun _ctx ->
      with_tempdir "tusk_fix_default_rules"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let file = Path.(src_dir / Path.v "file.ml") in
          let interface = Path.(src_dir / Path.v "file.mli") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nrules = [\"-snake-case-type-names\"]\n";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/app.ml\"\n";
          write_file file "let renderUser x = x\n";
          write_file interface "val render_user : 'a -> 'a\n";
          let result = Tusk_fix.Runner.run_files
            ~pipeline_for_file:(Tusk_fix.Config.pipeline_for_file
              (Tusk_fix.Config.load_scope ~cwd:tmpdir))
            ~mode:Tusk_fix.Runner.Check [ file ] in
          Test.assert_equal ~expected:1 ~actual:result.summary.remaining_diagnostics;
          Ok ()));
  Test.case "config table uses explicit rule state"
    (fun _ctx ->
      with_tempdir "tusk_fix_rule_state"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "app") in
          let src_dir = Path.(package_dir / Path.v "src") in
          let file = Path.(src_dir / Path.v "file.ml") in
          let interface = Path.(src_dir / Path.v "file.mli") in
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/app\"]\n\n[tusk.fix]\nrules = [{ name = \"snake-case-type-names\", state = \"enabled\" }]\n";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"app\"\nversion = \"0.1.0\"\n\n[tusk.fix]\nrules = [{ name = \"snake-case-type-names\", state = \"disabled\" }]\n\n[lib]\npath = \"src/app.ml\"\n";
          write_file file "type userProfile = int\n";
          write_file interface "type user_profile = int\n";
          let result = Tusk_fix.Runner.run_files
            ~pipeline_for_file:(Tusk_fix.Config.pipeline_for_file
              (Tusk_fix.Config.load_scope ~cwd:tmpdir))
            ~mode:Tusk_fix.Runner.Check [ file ] in
          Test.assert_equal ~expected:0 ~actual:result.summary.remaining_diagnostics;
          Ok ()));
  Test.case "runner apply rewrites camelCase type names"
    (fun _ctx ->
      with_tempdir "tusk_fix_runner"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          write_file file "type userProfile = { name : string }\n";
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Apply file in
          Test.assert_true result.changed;
          Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
          let actual = read_file file in
          let expected = "type user_profile = { name : string }\n" in
          Test.assert_equal ~expected ~actual;
          Ok ()));
  Test.case "check mode reports type-name issues without writing"
    (fun _ctx ->
      with_tempdir "tusk_fix_check"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          let source = "type userProfile = int\n" in
          write_file file source;
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_false result.changed;
          Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
          Test.assert_equal ~expected:source ~actual:(read_file file);
          Ok ()));
  Test.case "check mode reports function-name issues without writing"
    (fun _ctx ->
      with_tempdir "tusk_fix_check"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          let source = "let userProfile x = x\n" in
          write_file file source;
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_false result.changed;
          Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
          Test.assert_equal ~expected:source ~actual:(read_file file);
          Ok ()));
  Test.case "check mode reports module-name issues without writing"
    (fun _ctx ->
      with_tempdir "tusk_fix_check"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          let source = "module Foo_bar = struct end\n" in
          write_file file source;
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_false result.changed;
          Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
          Test.assert_equal ~expected:source ~actual:(read_file file);
          Ok ()));
  Test.case "check mode reports variable-name issues without writing"
    (fun _ctx ->
      with_tempdir "tusk_fix_check"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          let source = "let currentUser = 42\n" in
          write_file file source;
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_false result.changed;
          Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
          Test.assert_equal ~expected:source ~actual:(read_file file);
          Ok ()));
  Test.case "check mode reports prime-variable issues without writing"
    (fun _ctx ->
      with_tempdir "tusk_fix_check"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          let source = "let state' = 42\n" in
          write_file file source;
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_false result.changed;
          Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
          Test.assert_equal ~expected:source ~actual:(read_file file);
          Ok ()));
  Test.case "check mode reports argument-name issues without writing"
    (fun _ctx ->
      with_tempdir "tusk_fix_check"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          let source = "let render userId = userId\n" in
          write_file file source;
          let result = Tusk_fix.Runner.run_file ~mode:Tusk_fix.Runner.Check file in
          Test.assert_false result.changed;
          Test.assert_equal ~expected:1 ~actual:(List.length result.diagnostics);
          Test.assert_equal ~expected:source ~actual:(read_file file);
          Ok ()));
  Test.case "cli checks by default without rewriting files"
    (fun _ctx ->
      with_tempdir "tusk_fix_cli"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          write_file file "type userProfile = int\n";
          let result =
            with_cwd tmpdir (fun () -> run_cli [ Path.to_string file ])
          in
          Test.assert_error result;
          Test.assert_equal ~expected:"type userProfile = int\n" ~actual:(read_file file);
          Ok ()));
  Test.case "cli applies safe fixes only with --apply"
    (fun _ctx ->
      with_tempdir "tusk_fix_cli"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          write_file file "type userProfile = int\n";
          let result =
            with_cwd tmpdir (fun () -> run_cli [ "--apply"; Path.to_string file ])
          in
          Test.assert_ok result;
          Test.assert_equal ~expected:"type user_profile = int\n" ~actual:(read_file file);
          Ok ()));
  Test.case "cli check exits with error when issues remain"
    (fun _ctx ->
      with_tempdir "tusk_fix_cli"
        (fun tmpdir ->
          let file = Path.(tmpdir / Path.v "sample.ml") in
          write_file file "type userProfile = int\n";
          let result =
            with_cwd tmpdir (fun () -> run_cli [ "--check"; Path.to_string file ])
          in
          Test.assert_error result;
          Test.assert_equal ~expected:"type userProfile = int\n" ~actual:(read_file file);
          Ok ()));
  Test.case "pipeline parses interface files with interface entrypoint"
    (fun _ctx ->
      let source = "type ('request, 'response) t\nval create : unit -> unit\n" in
      let result = Tusk_fix.Pipeline.run
        (Tusk_fix.Pipeline.default ())
        ~filename:(Path.v "sample.mli")
        source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.parse_diagnostics);
      Ok ());
  Test.case "scanner skips syn parser corpus inputs"
    (fun _ctx ->
      with_tempdir "tusk_fix_scan"
        (fun tmpdir ->
          let diag_dir = Path.(tmpdir / Path.v "tests" / Path.v "diagnostics") in
          let fixtures_dir = Path.(tmpdir / Path.v "tests" / Path.v "fixtures") in
          let generated_dir = Path.(tmpdir / Path.v "tests" / Path.v "generated") in
          let src_dir = Path.(tmpdir / Path.v "src") in
          Fs.create_dir_all diag_dir |> Result.expect ~msg:"mkdir diagnostics";
          Fs.create_dir_all fixtures_dir |> Result.expect ~msg:"mkdir fixtures";
          Fs.create_dir_all generated_dir |> Result.expect ~msg:"mkdir generated";
          Fs.create_dir_all src_dir |> Result.expect ~msg:"mkdir src";
          write_file Path.(diag_dir / Path.v "bad.ml") "let =\n";
          write_file Path.(fixtures_dir / Path.v "fixture.ml") "let x = 1\n";
          write_file Path.(generated_dir / Path.v "generated.ml") "let y = 2\n";
          write_file Path.(src_dir / Path.v "real.ml") "let z = 3\n";
          let files = Tusk_fix.File_scanner.(scan (create ~root:tmpdir ()))
          |> List.map Path.to_string
          |> List.sort String.compare in
          Test.assert_equal ~expected:[ Path.to_string Path.(src_dir / Path.v "real.ml") ] ~actual:files;
          Ok ()));
  Test.case "scanner prunes ignored subtrees eagerly"
    (fun _ctx ->
      with_tempdir "tusk_fix_scan"
        (fun tmpdir ->
          let ignored_dir = Path.(tmpdir / Path.v "ignored") in
          let kept_dir = Path.(tmpdir / Path.v "src") in
          Fs.create_dir_all ignored_dir |> Result.expect ~msg:"mkdir ignored";
          Fs.create_dir_all kept_dir |> Result.expect ~msg:"mkdir src";
          write_file Path.(ignored_dir / Path.v "bad.ml") "type userProfile = int\n";
          write_file Path.(kept_dir / Path.v "real.ml") "let z = 3\n";
          let files =
            Tusk_fix.File_scanner.(scan
              (
                create ~root:tmpdir
                  ~should_ignore:(fun path ->
                    String.contains (Path.to_string path) "/ignored")
                  ()
              ))
            |> List.map Path.to_string
            |> List.sort String.compare
          in
          Test.assert_equal ~expected:[ Path.to_string Path.(kept_dir / Path.v "real.ml") ] ~actual:files;
          Ok ()));
  Test.case "config scope discovers fix providers from workspace packages"
    (fun _ctx ->
      with_tempdir "tusk_fix_provider_scope"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "std") in
          Fs.create_dir_all package_dir |> Result.expect ~msg:"mkdir package";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/std\"]\n";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"std\"\nversion = \"0.1.0\"\n\n[tusk.fix.provider]\npath = \"fix/no_stdlib_provider.ml\"\nrules = [\"no-stdlib\"]\n";
          Fs.create_dir_all Path.(package_dir / Path.v "fix") |> Result.expect ~msg:"mkdir fix";
          write_file Path.(package_dir / Path.v "fix" / Path.v "no_stdlib_provider.ml") "let name = \"std\"\nlet rules () = []\nlet explanations () = []\n";
          let scope = Tusk_fix.Config.load_scope ~cwd:tmpdir |> Option.expect ~msg:"expected workspace scope" in
          match Tusk_fix.Config.providers (Some scope) with
          | [ provider ] ->
              Test.assert_equal
                ~expected:(Path.to_string
                  Path.(package_dir / Path.v "fix" / Path.v "no_stdlib_provider.ml"))
                ~actual:(Path.to_string provider.Tusk_model.Fix_provider.source_path);
              Ok ()
          | _ -> Error "expected one discovered provider"));
  Test.case "config scope defaults provider path to fix/tusk_fix_rules.ml"
    (fun _ctx ->
      with_tempdir "tusk_fix_provider_default_path"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
          let fix_dir = Path.(package_dir / Path.v "fix") in
          Fs.create_dir_all fix_dir |> Result.expect ~msg:"mkdir fix";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/demo\"]\n";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[tusk.fix.provider]\nrules = [\"demo-rule\"]\n";
          write_file Path.(fix_dir / Path.v "tusk_fix_rules.ml") "let name = \"demo\"\nlet rules () = []\nlet explanations () = []\n";
          let scope = Tusk_fix.Config.load_scope ~cwd:tmpdir |> Option.expect ~msg:"expected workspace scope" in
          match Tusk_fix.Config.providers (Some scope) with
          | [ provider ] ->
              Test.assert_equal
                ~expected:(Path.to_string Path.(fix_dir / Path.v "tusk_fix_rules.ml"))
                ~actual:(Path.to_string provider.Tusk_model.Fix_provider.source_path);
              Test.assert_equal ~expected:[ "demo:demo-rule" ] ~actual:provider.rules;
              Ok ()
          | _ -> Error "expected one discovered provider"));
  Test.case "config scope defaults provider path to fix/tusk_fix_rules/tusk_fix_rules.ml"
    (fun _ctx ->
      with_tempdir "tusk_fix_provider_nested_default_path"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let package_dir = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
          let provider_dir = Path.(package_dir / Path.v "fix" / Path.v "tusk_fix_rules") in
          Fs.create_dir_all provider_dir |> Result.expect ~msg:"mkdir provider dir";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/demo\"]\n";
          write_file Path.(package_dir / Path.v "tusk.toml") "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[tusk.fix.provider]\nrules = [\"demo-rule\"]\n";
          write_file Path.(provider_dir / Path.v "tusk_fix_rules.ml") "let name = \"demo\"\nlet rules () = []\nlet explanations () = []\n";
          let scope = Tusk_fix.Config.load_scope ~cwd:tmpdir |> Option.expect ~msg:"expected workspace scope" in
          match Tusk_fix.Config.providers (Some scope) with
          | [ provider ] ->
              Test.assert_equal
                ~expected:(Path.to_string Path.(provider_dir / Path.v "tusk_fix_rules.ml"))
                ~actual:(Path.to_string provider.Tusk_model.Fix_provider.source_path);
              Test.assert_equal ~expected:[ "demo:demo-rule" ] ~actual:provider.rules;
              Ok ()
          | _ -> Error "expected one discovered provider"));
  Test.case "fixme runner includes provider build dependencies"
    (fun _ctx ->
      with_tempdir "tusk_fix_provider_build_deps"
        (fun tmpdir ->
          let workspace_toml = Path.(tmpdir / Path.v "tusk.toml") in
          let provider_dir = Path.(tmpdir / Path.v "packages" / Path.v "demo") in
          let helper_dir = Path.(tmpdir / Path.v "packages" / Path.v "helper") in
          let fix_dir = Path.(provider_dir / Path.v "fix") in
          let helper_src_dir = Path.(helper_dir / Path.v "src") in
          Fs.create_dir_all fix_dir |> Result.expect ~msg:"mkdir fix";
          Fs.create_dir_all helper_src_dir |> Result.expect ~msg:"mkdir helper";
          write_file workspace_toml "[workspace]\nmembers = [\"packages/demo\", \"packages/helper\"]\n";
          write_file Path.(provider_dir / Path.v "tusk.toml") "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[build-dependencies]\nhelper = { path = \"../helper\" }\n\n[tusk.fix.provider]\nrules = [\"demo-rule\"]\n";
          write_file Path.(helper_dir / Path.v "tusk.toml") "[package]\nname = \"helper\"\nversion = \"0.1.0\"\n\n[lib]\npath = \"src/helper.ml\"\n";
          write_file Path.(helper_src_dir / Path.v "helper.ml") "let value = 1\n";
          write_file Path.(fix_dir / Path.v "tusk_fix_rules.ml") "let name = \"demo\"\nlet rules () = []\nlet explanations () = []\n";
          let providers = [ Tusk_model.Fix_provider.{
              name = "demo";
              package_name = "demo";
              package_path = provider_dir;
              source_path =
                Path.(fix_dir / Path.v "tusk_fix_rules.ml");
              rules = [ "demo:demo-rule" ];
            }; ]
          in
          let plan = Tusk_fix.Fixme_runner.materialize
            ~workspace_root:tmpdir
            ~target_dir_root:Path.(tmpdir / Path.v "_build")
            providers in
          let package_toml = read_file plan.package_toml_path in
          Test.assert_true (String.contains package_toml "helper = { path = \"");
          Ok ()));
  Test.case "fixme runner registry source lists discovered providers"
    (fun _ctx ->
      let providers = [ Tusk_model.Fix_provider.{
          name = "std";
          package_name = "std";
          package_path = Path.v "packages/std";
          source_path = Path.v "/workspace/packages/std/fix/no_stdlib_provider.ml";
          rules = [ "std:no-stdlib" ];
        }; Tusk_model.Fix_provider.{
          name = "suri";
          package_name = "suri";
          package_path = Path.v "packages/suri";
          source_path = Path.v "/workspace/packages/suri/fix/route_style_provider.ml";
          rules = [ "suri:route-style" ];
        }; ]
      in
      let source = Tusk_fix.Fixme_runner.registry_source providers in
      Test.assert_true (String.contains source "Provider_std_std");
      Test.assert_true (String.contains source "Provider_suri_suri");
      Ok ());
  Test.case "fixme runner binary path uses generated build dir"
    (fun _ctx ->
      let provider =
        Tusk_model.Fix_provider.{
          name = "std";
          package_name = "std";
          package_path = Path.v "packages/std";
          source_path = Path.v "/workspace/packages/std/fix/tusk_fix_rules.ml";
          rules = [ "std:no-stdlib" ];
        }
      in
      let plan = Tusk_fix.Fixme_runner.plan
        ~workspace_root:(Path.v "/workspace")
        ~target_dir_root:Path.(Path.v "/workspace" / Path.v "_build")
        [ provider ] in
      let binary_path = Path.to_string plan.binary_path in
      Test.assert_true (String.contains binary_path "/build/release/");
      Test.assert_false (String.contains binary_path "/workspace/_build/release/");
      Ok ());
  Test.case "fixme runner hash changes when provider support sources change"
    (fun _ctx ->
      with_tempdir "tusk_fix_runner_hash"
        (fun tmpdir ->
          let workspace_root = tmpdir in
          let target_dir_root = Path.(workspace_root / Path.v "_build") in
          let package_path = Path.(workspace_root / Path.v "packages" / Path.v "std") in
          let fix_dir = Path.(package_path / Path.v "fix") in
          Fs.create_dir_all fix_dir |> Result.expect ~msg:"failed to create fix dir";
          let provider_source = Path.(fix_dir / Path.v "tusk_fix_rules.ml") in
          let support_source = Path.(fix_dir / Path.v "prefer_result_map_over_manual_match.ml") in
          write_file provider_source "let rules () = []\nlet explanations () = []\n";
          write_file support_source "let explanation = \"old\"\n";
          let provider =
            Tusk_model.Fix_provider.{
              name = "std";
              package_name = "std";
              package_path;
              source_path = provider_source;
              rules = [ "std:no-stdlib" ];
            }
          in
          let first_plan = Tusk_fix.Fixme_runner.plan ~workspace_root ~target_dir_root [ provider ] in
          write_file support_source "let explanation = \"new\"\n";
          let second_plan = Tusk_fix.Fixme_runner.plan ~workspace_root ~target_dir_root [ provider ] in
          Test.assert_false (String.equal first_plan.provider_hash second_plan.provider_hash);
          Ok ()));
  Test.case "rule query collects expressions from the typed CST"
    (fun _ctx ->
      let result = Syn.parse_implementation "let render x = let y = x + 1 in y; y\n" in
      let cst = Syn.build_cst result |> Result.expect ~msg:"expected typed CST for diagnostics-free parse" in
      let expressions = Tusk_fix.Rule_query.expressions
        Tusk_fix.Rule.{ file_path = "sample.ml"; cst } in
      Test.assert_true (List.length expressions >= 5);
      Ok ());
  Test.case "rule query collects let bindings from the typed CST"
    (fun _ctx ->
      let result = Syn.parse_implementation "let render x = x\nlet other y = let z = y in z\n" in
      let cst = Syn.build_cst result |> Result.expect ~msg:"expected typed CST for diagnostics-free parse" in
      let bindings = Tusk_fix.Rule_query.let_bindings Tusk_fix.Rule.{ file_path = "sample.ml"; cst } in
      Test.assert_equal
        ~expected:[ "render"; "other" ]
        ~actual:((bindings |> List.map Syn.Cst.LetBinding.name));
      Ok ());
  Test.case "rule query collects type declarations from implementations and interfaces"
    (fun _ctx ->
      let implementation = Syn.parse_implementation "type user = { name : string }\nlet render x = x\n" in
      let interface = Syn.parse_interface "type service\nval render : int -> int\n" in
      let implementation_cst = Syn.build_cst implementation |> Result.expect ~msg:"expected typed CST for diagnostics-free parse" in
      let interface_cst = Syn.build_cst interface |> Result.expect ~msg:"expected typed CST for diagnostics-free parse" in
      let implementation_types = Tusk_fix.Rule_query.type_declarations
        Tusk_fix.Rule.{ file_path = "sample.ml"; cst = implementation_cst }
      |> List.map
        (fun declaration -> Syn.Cst.Token.text (Syn.Cst.TypeDeclaration.name_token declaration)) in
      let interface_types = Tusk_fix.Rule_query.type_declarations
        Tusk_fix.Rule.{ file_path = "sample.mli"; cst = interface_cst }
      |> List.map
        (fun declaration -> Syn.Cst.Token.text (Syn.Cst.TypeDeclaration.name_token declaration)) in
      Test.assert_equal ~expected:[ "user" ] ~actual:implementation_types;
      Test.assert_equal ~expected:[ "service" ] ~actual:interface_types;
      Ok ());
  Test.case "prefer-record-destructuring-parameters flags immediate record unpacking"
    (fun _ctx ->
      let source = "let encode user = let { name; email; _ } = user in [ name; email ]\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_record_destructuring_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      let codes = diagnostic_rule_ids result.diagnostics in
      Test.assert_equal ~expected:[ "prefer-record-destructuring-parameters" ] ~actual:codes;
      Ok ());
  Test.case "prefer-record-destructuring-parameters ignores non-record unpacking"
    (fun _ctx ->
      let source = "let encode user = let name = user.name in name\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_record_destructuring_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "prefer-record-destructuring-parameters flags repeated field access on one record parameter"
    (fun _ctx ->
      let source = "let encode user = [ user.name; user.email; user.role ]\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_record_destructuring_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal
        ~expected:[ "prefer-record-destructuring-parameters" ]
        ~actual:(diagnostic_rule_ids result.diagnostics);
      Ok ());
  Test.case "prefer-record-destructuring-parameters ignores repeated field access when the whole record is also used"
    (fun _ctx ->
      let source = "let encode user = render user [ user.name; user.email ]\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_record_destructuring_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "prefer-record-destructuring-parameters ignores functions with several positional parameters"
    (fun _ctx ->
      let source = "let encode format user = let { name; email; _ } = user in (format, name, email)\n" in
      let pipeline = Tusk_fix.Pipeline.make
        ~rules:[ Tusk_fix.Rules.Prefer_record_destructuring_parameters.make () ]
        () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Ok ());
  Test.case "prefer-result-map-over-manual-match ignores rebuilt error payloads without crashing"
    (fun _ctx ->
      let source = "let map_result value = match value with | Ok x -> Ok (x + 1) | Error e -> Error (wrap e)\n" in
      let rules =
        Tusk_fix.Pipeline.default_rules ()
        |> List.filter
          (fun rule ->
            String.equal (Tusk_fix.Rule.id rule) "std:prefer-result-map-over-manual-match")
      in
      let pipeline = Tusk_fix.Pipeline.make ~rules () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Test.assert_equal ~expected:0 ~actual:(List.length result.parse_diagnostics);
      Ok ());
  Test.case "default pipeline tolerates standalone top-level docs and comments"
    (fun _ctx ->
      let source = "(** Module doc *)\n\
         (* explanatory comment *)\n\
         let value = 1\n\
        "
      in
      let pipeline = Tusk_fix.Pipeline.make ~rules:(Tusk_fix.Pipeline.default_rules ()) () in
      let result = Tusk_fix.Pipeline.run pipeline source in
      Test.assert_equal ~expected:0 ~actual:(List.length result.parse_diagnostics);
      Ok ());
  Test.case "rule explanations explain record-destructuring parameters"
    (fun _ctx ->
      assert_explanation_contains ~rule_id:"prefer-record-destructuring-parameters" ~snippet:"let { ... } = value in ...";
      Ok ());
  Test.case "rule explanations explain ignored map traversal"
    (fun _ctx ->
      assert_explanation_contains ~rule_id:"std:prefer-iter-over-ignored-map" ~snippet:"List.iter";
      Ok ());
  Test.case "rule explanations explain List.is_empty preference"
    (fun _ctx ->
      assert_explanation_contains ~rule_id:"std:prefer-list-is-empty" ~snippet:"List.is_empty";
      Ok ());
]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"tusk-fix:runner" ~tests ~args:Env.args)
    ~args:Env.args
    ()
