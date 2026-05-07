open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let write_text = fun path content ->
  let* () =
    match Path.parent path with
    | None -> Ok ()
    | Some parent ->
        Fs.create_dir_all parent
        |> Result.map_err ~fn:IO.error_message
  in
  Fs.write content path
  |> Result.map_err ~fn:IO.error_message

let assert_output_not_contains = fun ~cmd (output: command_output) needle ->
  let text = output.stdout ^ output.stderr in
  if String.contains text needle then
    Error (cmd ^ " output unexpectedly contained `" ^ needle ^ "`: " ^ render_output output)
  else
    Ok ()

let assert_no_build_or_trace_output = fun ~cmd output ->
  let* () = assert_output_not_contains ~cmd output "Resolved " in
  let* () = assert_output_not_contains ~cmd output "Ensured " in
  let* () = assert_output_not_contains ~cmd output "Planning " in
  let* () = assert_output_not_contains ~cmd output "Building " in
  assert_output_not_contains ~cmd output "Tracing"

let unsupported_profiler_for_host = fun () ->
  let host = System.host_triple in
  match host.System.TargetTriple.os with
  | "darwin" -> Some ("perf", "perf recording is only supported on Linux hosts in this prototype")
  | "linux" -> Some ("xctrace", "xctrace recording is only supported on Darwin hosts")
  | _ -> None

let test_trace_preserves_workspace_preflight_behaviors =
  Test.case
    ~size:Test.Large
    "riot trace preserves workspace preflight behavior"
    (fun ctx ->
      let workspace_name = "trace-e2e" in
      with_initialized_workspace
        ~init_args:[ "--bin" ]
        ctx
        workspace_name
        (fun workspace_root ->
          let output_path = Path.(workspace_root / Path.v "already.trace") in
          let* list_output = run_riot ctx ~cwd:workspace_root [ "trace"; "--list" ] in
          let* list_output = expect_success ~cmd:"riot trace --list" list_output in
          let* () =
            assert_output_contains
              ~cmd:"riot trace --list"
              list_output
              (workspace_name ^ ":" ^ workspace_name)
          in
          let* () = assert_no_build_or_trace_output ~cmd:"riot trace --list" list_output in
          let* json_without_list =
            run_riot ctx ~cwd:workspace_root [ "trace"; "--json"; workspace_name ]
          in
          let* _ =
            expect_failure_contains
              ~cmd:"riot trace --json"
              ~needle:"riot trace --json is only supported with --list"
              json_without_list
          in
          let* () = assert_no_build_or_trace_output ~cmd:"riot trace --json" json_without_list in
          let* () = write_text output_path "existing trace placeholder\n" in
          let* existing_output =
            run_riot
              ctx
              ~cwd:workspace_root
              [
                "trace";
                "--profiler";
                "auto";
                "--output";
                Path.to_string output_path;
                workspace_name;
              ]
          in
          let* existing_output =
            expect_failure_contains
              ~cmd:"riot trace existing output"
              ~needle:"trace output already exists"
              existing_output
          in
          let* () = assert_contains output_path "existing trace placeholder" in
          let* () =
            assert_no_build_or_trace_output ~cmd:"riot trace existing output" existing_output
          in
          let* force_append =
            run_riot
              ctx
              ~cwd:workspace_root
              [
                "trace";
                "--force";
                "--append";
                "--output";
                Path.to_string output_path;
                workspace_name;
              ]
          in
          let* force_append =
            expect_failure_contains
              ~cmd:"riot trace --force --append"
              ~needle:"--force and --append cannot be used together"
              force_append
          in
          let* () = assert_no_build_or_trace_output ~cmd:"riot trace --force --append" force_append in
          match unsupported_profiler_for_host () with
          | None -> Ok ()
          | Some (profiler, expected_message) ->
              let* unsupported =
                run_riot
                  ctx
                  ~cwd:workspace_root
                  [
                    "trace";
                    "--force";
                    "--profiler";
                    profiler;
                    "--output";
                    Path.to_string output_path;
                    workspace_name;
                  ]
              in
              let* unsupported =
                expect_failure_contains
                  ~cmd:"riot trace unsupported profiler"
                  ~needle:expected_message
                  unsupported
              in
              let* () = assert_contains output_path "existing trace placeholder" in
              assert_no_build_or_trace_output ~cmd:"riot trace unsupported profiler" unsupported))

let test_trace_summary_and_call_tree_report_missing_artifacts =
  Test.case
    ~size:Test.Large
    "riot trace summary and call-tree report missing artifacts"
    (fun ctx ->
      with_tempdir_result
        ~prefix:"riot_e2e_trace_summary_"
        (fun root ->
          let missing_trace = Path.(root / Path.v "missing.trace") in
          let missing_trace_string = Path.to_string missing_trace in
          let* summary = run_riot ctx ~cwd:root [ "trace"; "summary"; missing_trace_string ] in
          let* summary = expect_success ~cmd:"riot trace summary missing" summary in
          let* () = assert_output_contains ~cmd:"riot trace summary missing" summary "exists: false" in
          let* () =
            assert_output_contains ~cmd:"riot trace summary missing" summary "format: xctrace"
          in
          let* call_tree = run_riot ctx ~cwd:root [ "trace"; "call-tree"; missing_trace_string ] in
          let* call_tree = expect_success ~cmd:"riot trace call-tree missing" call_tree in
          let* () =
            assert_output_contains ~cmd:"riot trace call-tree missing" call_tree "exists: false"
          in
          let* summary_json =
            run_riot ctx ~cwd:root [ "trace"; "summary"; "--json"; missing_trace_string ]
          in
          let* summary_json = expect_success ~cmd:"riot trace summary --json missing" summary_json in
          let* () =
            assert_output_contains
              ~cmd:"riot trace summary --json missing"
              summary_json
              {|"type":"trace.summary"|}
          in
          let* () =
            assert_output_contains
              ~cmd:"riot trace summary --json missing"
              summary_json
              {|"exists":false|}
          in
          let* call_tree_json =
            run_riot ctx ~cwd:root [ "trace"; "call-tree"; "--json"; missing_trace_string ]
          in
          let* call_tree_json =
            expect_success ~cmd:"riot trace call-tree --json missing" call_tree_json
          in
          let* () =
            assert_output_contains
              ~cmd:"riot trace call-tree --json missing"
              call_tree_json
              {|"type":"trace.call_tree"|}
          in
          assert_output_contains
            ~cmd:"riot trace call-tree --json missing"
            call_tree_json
            {|"exists":false|}))

let tests = [
  test_trace_preserves_workspace_preflight_behaviors;
  test_trace_summary_and_call_tree_report_missing_artifacts;
]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:riot-trace" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
