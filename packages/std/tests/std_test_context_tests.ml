open Std

let test_ctx_includes_suite_and_test_name = Test.case
  "ctx includes suite and test name"
  (fun ctx ->
    Test.assert_equal ~expected:"std_test_context" ~actual:ctx.suite_name;
    Test.assert_equal ~expected:"ctx includes suite and test name" ~actual:ctx.test_name;
    Ok ())

let test_ctx_assigns_one_based_indices = Test.case
  "ctx assigns one-based indices"
  (fun ctx ->
    Test.assert_equal ~expected:2 ~actual:ctx.test_index;
    Ok ())

let test_ctx_exposes_binary_path = Test.case
  "ctx exposes binary path"
  (fun ctx ->
    match ctx.binary_path with
    | Some path when String.contains path "std_test_context_tests" -> Ok ()
    | Some path -> Error ("expected binary path to mention std_test_context_tests, got " ^ path)
    | None -> Error "expected ctx.binary_path to be present")

let test_ctx_derives_package_name = Test.case
  "ctx derives package name"
  (fun ctx ->
    match ctx.package_name with
    | Some "std" -> Ok ()
    | Some package_name -> Error ("expected package name std, got " ^ package_name)
    | None -> Error "expected ctx.package_name to be present")

let test_ctx_derives_workspace_root = Test.case
  "ctx derives workspace root"
  (fun ctx ->
    match ctx.workspace_root with
    | Some workspace_root when String.contains (Path.to_string workspace_root) "riot" -> Ok ()
    | Some workspace_root ->
        Error ("unexpected workspace root: " ^ Path.to_string workspace_root)
    | None -> Error "expected ctx.workspace_root to be present")

let tests = [
  test_ctx_includes_suite_and_test_name;
  test_ctx_assigns_one_based_indices;
  test_ctx_exposes_binary_path;
  test_ctx_derives_package_name;
  test_ctx_derives_workspace_root;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"std_test_context" ~tests ~args)
    ~args:Env.args
    ()
