open Std
open Std.Data
open Std.Collections

module Fixture_no_stdlib = struct
  module Api = Fixme

  let package_name = "std"

  let package_rule_id = package_name ^ ":no-stdlib"

  let rule_description = "Detect direct Stdlib, Unix, Sys, and Pervasives usage from the Std package boundary"

  let unix_body = {|
Direct calls into Unix bypass Riot's scheduling and portability boundaries.

In Riot, a blocking Unix call is not just a local implementation detail. It can
stall the cooperative scheduler, delay unrelated actors, and leak host-specific
behavior into packages that are supposed to stay portable.

Keep real operating-system boundaries down in packages that are meant to own
them, like `kernel`. Everywhere else, prefer the Riot-owned abstraction that
already exists, or add one deliberately if the boundary is genuinely missing.
|}

  let sys_body = {|
Direct Sys usage reaches into process-global runtime state instead of going through Riot-owned boundaries.

`Sys` exposes host and runtime details directly from OCaml. Once those calls
spread through ordinary packages, portability decisions and process-global
policy leak into places that should not own them.

Prefer Riot wrappers for system information and runtime behavior. If something
truly belongs at the process or platform boundary, keep it in a package that is
explicitly responsible for that boundary instead of reaching for `Sys`
everywhere.
|}

  let stdlib_body = {|
Code outside the runtime boundary should go through Riot's Std layer instead of referencing Stdlib directly.

Riot is trying to offer one coherent standard surface, not a mixture of
package-local conventions plus ad hoc direct references into `Stdlib`.
Routing code through `Std` gives the ecosystem one owned API surface and leaves
room for shared defaults, portability adjustments, and package-wide style.

When `Std` already owns an API, use it. When it does not, that is usually a
signal to extend `Std` deliberately instead of bypassing it forever.
|}

  let pervasives_body = {|
Pervasives is the historical pre-Stdlib module and should not appear in modern Riot code.

`Pervasives` is legacy OCaml surface area. Riot code should point at the
current owned surface, not an old compatibility layer that survives mostly for
historical reasons.

Replace direct `Pervasives` references with `Std`.
|}

  let rule_explain = String.concat "\n\n" [ unix_body; sys_body; stdlib_body; pervasives_body ]

  let explanation =
    Api.Explanation.{ rule_id = package_rule_id; body = rule_explain; message = rule_description }

  let explanations = fun () -> [ explanation ]

  let forbidden_modules = [ "Stdlib"; "Pervasives"; "Unix"; "Sys" ]

  let replacement_for = function
    | "Stdlib"
    | "Pervasives" -> Some "Std"
    | _ -> None

  let make_message = fun text ->
    match replacement_for text with
    | Some replacement -> "Direct usage of " ^ text ^ " is discouraged. Use " ^ replacement ^ " instead."
    | None -> "Direct usage of " ^ text ^ " is discouraged. Use package-owned Riot abstractions instead."

  let make_suggestion = fun text ->
    match replacement_for text with
    | Some replacement -> Some ("Replace " ^ text ^ " with " ^ replacement)
    | None -> None

  let make_fix = fun token replacement ->
    Api.Fix.make
      ~title:(("Replace " ^ Syn.Ceibo.Red.SyntaxToken.text token ^ " with " ^ replacement))
      ~operations:[ Api.Fix.replace_token_with_text ~target:token ~text:replacement ]

  let make_diagnostic = fun token ->
    let text = Syn.Ceibo.Red.SyntaxToken.text token in
    let suggestion = make_suggestion text in
    let fix = replacement_for text |> Option.map (make_fix token) in
    let kind = Api.Diagnostic.Known { rule_id = package_rule_id; message = make_message text } in
    Api.Diagnostic.make
      ~severity:Warning
      ~kind
      ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
      ?suggestion
      ?fix
      ()

  let dedupe_diagnostics = fun diagnostics ->
    let seen = HashMap.create () in
    List.filter
      (fun diag ->
        let span = Api.Diagnostic.span diag in
        let key = Api.Diagnostic.rule_id diag
        ^ ":"
        ^ Int.to_string span.start
        ^ ":"
        ^ Int.to_string span.end_ in
        match HashMap.get seen key with
        | Some _ -> false
        | None ->
            ignore (HashMap.insert seen key true);
            true)
      diagnostics

  let check_tree = fun (_ctx: Api.Rule.context) red_root ->
    let open Syn.Ceibo.Red in
      let open Syn.SyntaxKind in
        let open_stmts = Api.Traversal.find_by_kind OPEN_STMT red_root in
        let path_exprs = Api.Traversal.find_by_kind PATH_EXPR red_root in
        let field_access_exprs = Api.Traversal.find_by_kind FIELD_ACCESS_EXPR red_root in
        let module_paths = Api.Traversal.find_by_kind MODULE_PATH red_root in
        let module_type_paths = Api.Traversal.find_by_kind MODULE_TYPE_PATH red_root in
        let type_constructors = Api.Traversal.find_by_kind TYPE_CONSTR red_root in
        let diagnostic_for_first_token node =
          match Api.Traversal.first_non_trivia_token node with
          | Some token ->
              let text = SyntaxToken.text token in
              if List.mem text forbidden_modules then
                Some (make_diagnostic token)
              else
                None
          | None -> None
        in
        let diagnostic_for_open_stmt node =
          let non_trivia_children =
            SyntaxNode.children node
            |> Array.to_list
            |> List.filter
              (
                function
                | Token token -> not (Api.Traversal.is_trivia (SyntaxToken.kind token))
                | Node _ -> true
              )
          in
          match non_trivia_children with
          | _open_kw :: Node module_path :: _ ->
              diagnostic_for_first_token module_path
          | _open_kw :: Token token :: _ ->
              let text = SyntaxToken.text token in
              if List.mem text forbidden_modules then
                Some (make_diagnostic token)
              else
                None
          | _ ->
              None
        in
        let diagnostic_for_field_access node =
          match Api.Traversal.first_non_trivia_child node with
          | Some (Node receiver) ->
              diagnostic_for_first_token receiver
          | Some (Token token) ->
              let text = SyntaxToken.text token in
              if List.mem text forbidden_modules then
                Some (make_diagnostic token)
              else
                None
          | None ->
              None
        in
        dedupe_diagnostics
          (List.concat
            [
              List.filter_map diagnostic_for_open_stmt open_stmts;
              List.filter_map diagnostic_for_first_token path_exprs;
              List.filter_map diagnostic_for_field_access field_access_exprs;
              List.filter_map diagnostic_for_first_token module_paths;
              List.filter_map diagnostic_for_first_token module_type_paths;
              List.filter_map diagnostic_for_first_token type_constructors;
            ])

  let rule = fun () ->
    Api.Rule.make
      ~id:package_rule_id
      ~description:rule_description
      ~explain:rule_explain
      ~run:check_tree
      ()
end

module Fixture_std_provider = struct
  let name = "std"

  let rules = fun () -> [ Fixture_no_stdlib.rule () ]

  let explanations = fun () -> Fixture_no_stdlib.explanations ()
end

let tests_dir = Path.v "packages/riot-fix/tests"

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let fixture_filter = fun path ->
  match Path.extension path with
  | Some ".ml" ->
      let name = Path.basename path in
      if
        String.length name >= 4
        && is_digit name.[0]
        && is_digit name.[1]
        && is_digit name.[2]
        && is_digit name.[3]
      then
        `keep
      else
        `skip
  | _ -> `skip

let approved_snapshot_path = fun path ->
  match Path.extension path with
  | Some ext -> Path.add_extension path ~ext:((ext ^ ".expected"))
  | None -> Path.add_extension path ~ext:"expected"

let relativize_path = fun ~workspace_root path ->
  match Path.strip_prefix path ~prefix:workspace_root with
  | Ok relpath -> relpath
  | Error _ -> path

let file_result_to_json = fun ~workspace_root result ->
  let open Json in
    Object [
      (
        "file",
        String (Path.to_string (relativize_path ~workspace_root Riot_fix.Runner.(result.file)))
      );
      ("changed", Bool Riot_fix.Runner.(result.changed));
      (
        "error",
        match Riot_fix.Runner.(result.error) with
        | Some err -> String err
        | None -> Null
      );
      ("applied_fixes", Array (List.map Riot_fix.Fix.to_json Riot_fix.Runner.(result.applied_fixes)));
      (
        "parse_diagnostics",
        Array (List.map Syn.Diagnostic.to_json Riot_fix.Runner.(result.parse_diagnostics))
      );
      (
        "diagnostics",
        Array (List.map Riot_fix.Diagnostic.to_json Riot_fix.Runner.(result.diagnostics))
      );
    ]

let run_result_to_json = fun ~workspace_root result ->
  Json.Object [
    ("summary", Riot_fix.Runner.summary_to_json Riot_fix.Runner.(result.summary));
    (
      "files",
      Json.Array (List.map (file_result_to_json ~workspace_root) Riot_fix.Runner.(result.files))
    );
  ]

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let workspace_root = ctx.test.Test.workspace_root |> Option.expect ~msg:"fixture snapshots require a workspace root" in
  let () = Riot_fix.Provider_registry.register_providers
    [ (module Fixture_std_provider : Riot_fix.Provider.S) ] in
  let result = Riot_fix.Runner.run_files ~mode:Check [ ctx.fixture_path ] in
  let actual_json = run_result_to_json ~workspace_root result in
  Test.Snapshot.assert_with
    ~ctx:ctx.test
    ~render:(fun json -> Json.to_string_pretty json ^ "\n")
    ~actual:actual_json

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:tests_dir
          ~filter:fixture_filter
          ~snapshot_path:(fun path -> Some (approved_snapshot_path path))
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"riot-fix:fixtures" ~tests ~args)
    ~args:Env.args
    ()
