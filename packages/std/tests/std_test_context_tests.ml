open Std

let test_ctx_includes_suite_and_test_name =
  Test.case
    "ctx includes suite and test name"
    (fun ctx ->
      Test.assert_equal ~expected:"std_test_context" ~actual:ctx.suite_name;
      Test.assert_equal ~expected:"ctx includes suite and test name" ~actual:ctx.test_name;
      Ok ())

let test_ctx_assigns_one_based_indices =
  Test.case
    "ctx assigns one-based indices"
    (fun ctx ->
      Test.assert_equal ~expected:2 ~actual:ctx.test_index;
      Ok ())

let test_ctx_exposes_binary_path =
  Test.case
    "ctx exposes binary path"
    (fun ctx ->
      match ctx.binary_path with
      | Some path when String.contains (Path.to_string path) "std_test_context_tests" -> Ok ()
      | Some path ->
          Error ("expected binary path to mention std_test_context_tests, got "
          ^ Path.to_string path)
      | None -> Error "expected ctx.binary_path to be present")

let test_ctx_derives_package_name =
  Test.case
    "ctx derives package name"
    (fun ctx ->
      match ctx.package_name with
      | Some "std" -> Ok ()
      | Some package_name -> Error ("expected package name std, got " ^ package_name)
      | None -> Error "expected ctx.package_name to be present")

let test_ctx_defaults_built_binaries_to_empty =
  Test.case
    "ctx defaults built binaries to empty"
    (fun ctx ->
      match ctx.built_binaries with
      | [] -> Ok ()
      | _ -> Error "expected std test context to have no owning-package runtime binaries")

let test_ctx_find_binary_looks_up_a_built_binary =
  Test.case
    "ctx find_binary looks up a built binary"
    (fun ctx ->
      let expected = Path.v "/tmp/demo-bin" in
      let ctx = { ctx with built_binaries = [ Test.Context.{ name = "demo"; path = expected } ] } in
      match Test.Context.find_binary ctx "demo" with
      | Some actual when Path.equal actual expected -> Ok ()
      | Some actual -> Error ("expected /tmp/demo-bin, got " ^ Path.to_string actual)
      | None -> Error "expected find_binary to return the requested built binary")

let test_ctx_require_binary_reports_missing_binary =
  Test.case
    "ctx require_binary reports a missing binary"
    (fun ctx ->
      match Test.Context.require_binary ctx "riot" with
      | Error message when String.contains message "required built binary 'riot' was not available" ->
          Ok ()
      | Error message -> Error ("unexpected missing-binary message: " ^ message)
      | Ok path -> Error ("expected missing binary lookup to fail, got " ^ Path.to_string path))

let test_ctx_derives_workspace_root =
  Test.case
    "ctx derives workspace root"
    (fun ctx ->
      match ctx.workspace_root with
      | Some workspace_root ->
          let manifest = Path.(workspace_root / Path.v "riot.toml") in
          if Path.is_file manifest then
            Ok ()
          else
            Error
              ("expected workspace root to contain riot.toml, got "
              ^ Path.to_string workspace_root)
      | None -> Error "expected ctx.workspace_root to be present")

let tests = [
  test_ctx_includes_suite_and_test_name;
  test_ctx_assigns_one_based_indices;
  test_ctx_exposes_binary_path;
  test_ctx_derives_package_name;
  test_ctx_defaults_built_binaries_to_empty;
  test_ctx_find_binary_looks_up_a_built_binary;
  test_ctx_require_binary_reports_missing_binary;
  test_ctx_derives_workspace_root;
]

let main ~args = Test.Cli.main ~name:"std_test_context" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
