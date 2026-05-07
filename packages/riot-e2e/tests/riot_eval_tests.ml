open Std
open Std.Result.Syntax
open Riot_e2e

module Test = Std.Test

let greeter_package = "greeter"

let greeter_message = "hello from greeter"

let create_greeter_package = fun ctx workspace_root ->
  let package_root = Path.(workspace_root / Path.v "packages" / Path.v greeter_package) in
  let* new_output = run_riot ctx ~cwd:workspace_root [ "new"; "--lib"; "./packages/greeter" ] in
  let* _ = expect_success ~cmd:"riot new --lib ./packages/greeter" new_output in
  let* () =
    write_text Path.(package_root / Path.v "src" / Path.v "Greeter.mli") "val message : string\n"
  in
  write_text
    Path.(package_root / Path.v "src" / Path.v "Greeter.ml")
    ("let message = \"" ^ greeter_message ^ "\"\n")

let with_eval_workspace = fun ctx fn ->
  with_initialized_workspace
    ctx
    "riot-eval-e2e"
    (fun workspace_root ->
      let* () = create_greeter_package ctx workspace_root in
      fn workspace_root)

let test_eval_runs_outside_workspace =
  Test.case
    ~size:Test.Large
    "riot eval runs outside a workspace with Std available"
    (fun ctx ->
      with_tempdir_result
        ~prefix:"riot_e2e_eval_detached_"
        (fun root ->
          let* output = run_riot ctx ~cwd:root [ "eval"; "println \"detached eval works\"" ] in
          let* output = expect_success ~cmd:"riot eval outside workspace" output in
          assert_output_contains ~cmd:"riot eval outside workspace" output "detached eval works"))

let test_eval_loads_explicit_workspace_package =
  Test.case
    ~size:Test.Large
    "riot eval -p loads an explicit workspace package"
    (fun ctx ->
      with_eval_workspace
        ctx
        (fun workspace_root ->
          let* output =
            run_riot
              ctx
              ~cwd:workspace_root
              [ "eval"; "-p"; greeter_package; "println Greeter.message" ]
          in
          let* output = expect_success ~cmd:"riot eval -p greeter" output in
          assert_output_contains ~cmd:"riot eval -p greeter" output greeter_message))

let test_run_script_forwards_arguments =
  Test.case
    ~size:Test.Large
    "riot run script forwards arguments through args"
    (fun ctx ->
      with_initialized_workspace
        ctx
        "riot-run-script-e2e"
        (fun workspace_root ->
          let script_path = Path.(workspace_root / Path.v "hello.ml") in
          let* () =
            write_text
              script_path
              {|
let target =
  match args with
  | first :: _ -> first
  | [] -> "missing"

let () = Std.println (Std.String.concat "" [ "script:"; target ])
|}
          in
          let* output = run_riot ctx ~cwd:workspace_root [ "run"; "hello.ml"; "--"; "preserved" ] in
          let* output = expect_success ~cmd:"riot run hello.ml -- preserved" output in
          assert_output_contains ~cmd:"riot run hello.ml -- preserved" output "script:preserved"))

let test_repl_starts_outside_workspace =
  Test.case
    ~size:Test.Large
    "riot repl starts outside a workspace"
    (fun ctx ->
      with_tempdir_result
        ~prefix:"riot_e2e_repl_detached_"
        (fun root ->
          let input = "#quit;;\n" in
          let* output = run_riot_with_stdin ctx ~cwd:root ~stdin:input [ "repl" ] in
          let* output = expect_success ~cmd:"riot repl outside workspace" output in
          let* () =
            assert_output_not_contains ~cmd:"riot repl outside workspace" output "failed to compile"
          in
          let* () =
            assert_output_not_contains ~cmd:"riot repl outside workspace" output "failed to load"
          in
          assert_output_contains ~cmd:"riot repl outside workspace" output "Riot REPL"))

let test_repl_keeps_phrase_definitions_available =
  (* Skipped until REPL phrase modules can use host-runtime interfaces reliably. *)
  Test.skip
    ~size:Test.Large
    "riot repl keeps earlier phrase definitions available"
    (fun _ctx -> Ok ())

let test_repl_loads_package_and_keeps_session_alive =
  (* Skipped until Dynlink can load package closures without Std digest mismatches. *)
  Test.skip
    ~size:Test.Large
    "riot repl #use loads a package and later phrases keep running"
    (fun _ctx -> Ok ())

let tests = [
  test_eval_runs_outside_workspace;
  test_eval_loads_explicit_workspace_package;
  test_run_script_forwards_arguments;
  test_repl_starts_outside_workspace;
  test_repl_keeps_phrase_definitions_available;
  test_repl_loads_package_and_keeps_session_alive;
]

let main ~args = Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"riot-e2e:eval" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
