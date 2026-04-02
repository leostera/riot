open Std
module Test = Std.Test

let parse_publish = fun args ->
  match ArgParser.get_matches Tusk_cli.Publish.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_publish_accepts_package_option = fun _ctx ->
  match parse_publish [ "publish"; "-p"; "demo" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "demo") ~actual:(ArgParser.get_one matches "package");
      Ok ()

let test_publish_accepts_workspace_flag = fun _ctx ->
  match parse_publish [ "publish"; "--workspace" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "workspace" then
        Ok ()
      else
        Error "expected --workspace flag to be parsed"

let test_publish_accepts_dry_run_flag = fun _ctx ->
  match parse_publish [ "publish"; "--dry-run" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "dry-run" then
        Ok ()
      else
        Error "expected --dry-run flag to be parsed"

let test_publish_accepts_skip_check_flag = fun _ctx ->
  match parse_publish [ "publish"; "--skip-check" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "skip-check" then
        Ok ()
      else
        Error "expected --skip-check flag to be parsed"

let test_publish_conflicting_selection_fails = fun _ctx ->
  match Tusk_cli.Publish.resolve_request ~package_name:(Some "demo") ~workspace_mode:true with
  | Error Tusk_cli.Publish.ConflictingSelection -> Ok ()
  | Ok _ -> Error "expected conflicting publish selection to fail"
  | Error err -> Error ("unexpected publish selection error: " ^ Tusk_cli.Publish.message err)

let tests =
  Test.[
    case "publish: parse -p option" test_publish_accepts_package_option;
    case "publish: parse --workspace flag" test_publish_accepts_workspace_flag;
    case "publish: parse --dry-run flag" test_publish_accepts_dry_run_flag;
    case "publish: parse --skip-check flag" test_publish_accepts_skip_check_flag;
    case "publish: conflicting selection fails" test_publish_conflicting_selection_fails;
  ]

let name = "Tusk CLI Publish Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
