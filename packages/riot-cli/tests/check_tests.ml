open Std
module Test = Std.Test

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let write_file = fun path content ->
  match Fs.create_dir_all (Path.dirname path) with
  | Error err -> Error (IO.error_message err)
  | Ok () -> (
      match Fs.write content path with
      | Ok () -> Ok ()
      | Error err -> Error (IO.error_message err)
    )

let make_capture_writer = fun () ->
  let chunks = ref [] in
  ((fun chunk -> chunks := chunk :: !chunks), fun () -> !chunks |> List.rev |> String.concat "")

let parse_jsonl = fun output ->
  output
  |> String.split_on_char '\n'
  |> List.filter (fun line -> not (String.equal line ""))
  |> List.map (fun line -> Data.Json.of_string line |> Result.expect ~msg:"parse json line")

let parse_check = fun args ->
  match ArgParser.get_matches Riot_cli.Check_cmd.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let make_dependency = fun name ->
  Riot_model.Package.{
    name;
    source = {
      workspace = true;
      builtin = false;
      path = None;
      source_locator = None;
      ref_ = None;
      version = Some Std.Version.any;
    };
  }

let empty_sources: Riot_model.Package.sources = {
  src = [];
  native = [];
  tests = [];
  examples = [];
  bench = [];
}

let make_package = fun ~name ~path ~relative_path ?(dependencies = []) ?(sources = empty_sources) () ->
  Riot_model.Package.make ~name ~path ~relative_path ~dependencies ~sources ()

let make_workspace = fun workspace_root packages ->
  Riot_model.Workspace.make ~root:workspace_root ~packages ()

let test_check_accepts_json_flag = fun _ctx ->
  match parse_check [ "check"; "--json"; "app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "json" then
        Ok ()
      else
        Error "expected --json flag to be parsed"

let test_check_accepts_quiet_flag = fun _ctx ->
  match parse_check [ "check"; "--quiet"; "app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "quiet" then
        Ok ()
      else
        Error "expected --quiet flag to be parsed"

let test_check_accepts_explain_option = fun _ctx ->
  match parse_check [ "check"; "--explain"; "TYP2001" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "TYP2001") ~actual:(ArgParser.get_one matches "explain");
      Ok ()

let test_check_accepts_package_option = fun _ctx ->
  match parse_check [ "check"; "-p"; "colors" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "colors") ~actual:(ArgParser.get_one matches "package");
      Ok ()

let test_check_accepts_path_argument = fun _ctx ->
  match parse_check [ "check"; "packages/app/src/app.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      let expected = Path.v "packages/app/src/app.ml" in
      Test.assert_equal
        ~expected:[ expected ]
        ~actual:((ArgParser.get_many matches "path" |> List.map Path.v));
      Ok ()

let test_check_accepts_multiple_paths = fun _ctx ->
  match parse_check [ "check"; "packages/app/src/app.ml"; "packages/kernel/src/kernel.ml" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      let expected = [ Path.v "packages/app/src/app.ml"; Path.v "packages/kernel/src/kernel.ml" ] in
      Test.assert_equal ~expected ~actual:((ArgParser.get_many matches "path" |> List.map Path.v));
      Ok ()

let test_check_accepts_no_path_without_explain = fun _ctx ->
  match parse_check [ "check" ] with
  | Error err -> Error ("expected check args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:[] ~actual:((ArgParser.get_many matches "path" |> List.map Path.v));
      Ok ()

let test_check_json_streams_single_explicit_file_without_duplicates = fun _ctx ->
  with_tempdir_result "riot_check_json_single_file"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let package_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(package_root / Path.v "src/broken.ml") in
      let workspace = make_workspace
        workspace_root
        [ make_package ~name:"demo" ~path:package_root ~relative_path:(Path.v "packages/demo") () ] in
      match write_file source_path "let x = y\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; "--json"; Path.to_string source_path ]
          |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          (
            match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
            | Ok () -> Error "expected check --json to fail for unbound name"
            | Error _ ->
                let events = parse_jsonl (stdout_contents ()) in
                let count_events kind =
                  events
                  |> List.filter
                    (fun json ->
                      match Data.Json.get_field "type" json with
                      | Some (Data.Json.String actual) -> String.equal actual kind
                      | _ -> false)
                  |> List.length
                in
                Test.assert_equal ~expected:1 ~actual:(count_events "check_start");
                Test.assert_equal ~expected:1 ~actual:(count_events "check_file");
                Test.assert_equal ~expected:1 ~actual:(count_events "check_diagnostic");
                Test.assert_equal ~expected:1 ~actual:(count_events "check_summary");
                let file_event =
                  events
                  |> List.find_opt
                    (fun json ->
                      match Data.Json.get_field "type" json with
                      | Some (Data.Json.String "check_file") -> true
                      | _ -> false)
                  |> Option.expect ~msg:"missing check_file event"
                in
                let expected_path = Some (Data.Json.String "packages/demo/src/broken.ml") in
                let actual_path =
                  match Data.Json.get_field "result" file_event with
                  | Some result_json -> Data.Json.get_field "path" result_json
                  | None -> None
                in
                Test.assert_equal ~expected:expected_path ~actual:actual_path;
                Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                Ok ()
          ))

let test_check_human_output_snapshot = fun ctx ->
  with_tempdir_result "riot_check_human_output"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let package_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(package_root / Path.v "src/broken.ml") in
      let workspace = make_workspace
        workspace_root
        [ make_package ~name:"demo" ~path:package_root ~relative_path:(Path.v "packages/demo") () ] in
      match write_file source_path "let x = y\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; Path.to_string source_path ] |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          (
            match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
            | Ok () -> Error "expected check to fail for unbound name"
            | Error _ ->
                Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                Test.Snapshot.assert_text ~ctx ~actual:(stdout_contents ())
          ))

let test_check_success_is_silent = fun _ctx ->
  with_tempdir_result "riot_check_success_silent"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let package_root = Path.(workspace_root / Path.v "packages/demo") in
      let source_path = Path.(package_root / Path.v "src/ok.ml") in
      let workspace = make_workspace
        workspace_root
        [ make_package ~name:"demo" ~path:package_root ~relative_path:(Path.v "packages/demo") () ] in
      match write_file source_path "let id x = x\n" with
      | Error err -> Error err
      | Ok () ->
          let matches = parse_check [ "check"; Path.to_string source_path ] |> Result.expect ~msg:"parse check args" in
          let stdout, stdout_contents = make_capture_writer () in
          let stderr, stderr_contents = make_capture_writer () in
          Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"check simple file";
          Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
          Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
          Ok ())

let test_check_package_filter_limits_workspace_scan = fun _ctx ->
  with_tempdir_result "riot_check_package_filter"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package ~name:"colors" ~path:colors_root ~relative_path:(Path.v "packages/colors") ();
          make_package ~name:"tty" ~path:tty_root ~relative_path:(Path.v "packages/tty") ();
        ] in
      match write_file colors_source "let color = missing_color\n" with
      | Error err -> Error err
      | Ok () -> (
          match write_file tty_source "let tty = missing_tty\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "colors" ] |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              match Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches with
              | Ok () -> Error "expected package-filtered check to fail for unbound name"
              | Error _ ->
                  let events = parse_jsonl (stdout_contents ()) in
                  let file_paths =
                    events
                    |> List.filter_map
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_file") -> (
                            match Data.Json.get_field "result" json with
                            | Some result_json -> Data.Json.get_field "path" result_json
                            | None -> None
                          )
                        | _ -> None)
                  in
                  Test.assert_equal
                    ~expected:[ Data.Json.String "packages/colors/src/colors.ml" ]
                    ~actual:file_paths;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ()
        ))

let test_check_package_filter_uses_package_session_for_cross_file_exports = fun _ctx ->
  with_tempdir_result "riot_check_package_session"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let colors_root = Path.(workspace_root / Path.v "packages/colors") in
      let helper_source = Path.(colors_root / Path.v "src/helper.ml") in
      let colors_source = Path.(colors_root / Path.v "src/colors.ml") in
      let demo_source = Path.(colors_root / Path.v "examples/blend_demo.ml") in
      let workspace = make_workspace
        workspace_root
        [ make_package ~name:"colors" ~path:colors_root ~relative_path:(Path.v "packages/colors") () ] in
      match write_file helper_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file colors_source "let id x = Helper.twice x\n" with
          | Error err -> Error err
          | Ok () ->
              match write_file demo_source "open Colors\nlet answer = id 21\n" with
              | Error err -> Error err
              | Ok () ->
                  let matches = parse_check [ "check"; "--json"; "-p"; "colors" ]
                  |> Result.expect ~msg:"parse check args" in
                  let stdout, stdout_contents = make_capture_writer () in
                  let stderr, stderr_contents = make_capture_writer () in
                  Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches |> Result.expect ~msg:"package check should use sibling source exports";
                  let events = parse_jsonl (stdout_contents ()) in
                  let file_events =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_file") -> true
                        | _ -> false)
                  in
                  let file_paths =
                    file_events
                    |> List.filter_map
                      (fun json ->
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None)
                  in
                  Test.assert_equal
                    ~expected:[
                      Data.Json.String "packages/colors/examples/blend_demo.ml";
                      Data.Json.String "packages/colors/src/colors.ml";
                      Data.Json.String "packages/colors/src/helper.ml";
                    ]
                    ~actual:file_paths;
                  let diagnostic_count =
                    events
                    |> List.filter
                      (fun json ->
                        match Data.Json.get_field "type" json with
                        | Some (Data.Json.String "check_diagnostic") -> true
                        | _ -> false)
                    |> List.length
                  in
                  Test.assert_equal ~expected:0 ~actual:diagnostic_count;
                  Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
                  Ok ())

let test_check_package_filter_loads_workspace_dependency_summaries = fun _ctx ->
  with_tempdir_result "riot_check_package_dependencies"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let std_root = Path.(workspace_root / Path.v "packages/std") in
      let tty_root = Path.(workspace_root / Path.v "packages/tty") in
      let std_source = Path.(std_root / Path.v "src/std.ml") in
      let tty_source = Path.(tty_root / Path.v "src/tty.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"std"
            ~path:std_root
            ~relative_path:(Path.v "packages/std")
            ~sources:{
              empty_sources with src = [ Path.v "src/std.ml" ];
            }
            ();
          make_package
            ~name:"tty"
            ~path:tty_root
            ~relative_path:(Path.v "packages/tty")
            ~dependencies:[ make_dependency "std" ]
            ~sources:{
              empty_sources with src = [ Path.v "src/tty.ml" ];
            }
            ();
        ] in
      match write_file std_source "let twice x = x + x\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file tty_source "let answer = Std.twice 21\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "tty" ]
              |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches
              |> Result.expect ~msg:"package check should load workspace dependency summaries";
              let events = parse_jsonl (stdout_contents ()) in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let diagnostic_count =
                events
                |> List.filter
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_diagnostic") -> true
                    | _ -> false)
                |> List.length
              in
              Test.assert_equal
                ~expected:[ Data.Json.String "packages/tty/src/tty.ml" ]
                ~actual:file_paths;
              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ())

let test_check_package_filter_merges_bootstrap_and_dependency_module_exports = fun _ctx ->
  with_tempdir_result "riot_check_bootstrap_shadow"
    (fun tmpdir ->
      let workspace_root = Path.(tmpdir / Path.v "workspace") in
      let shadow_root = Path.(workspace_root / Path.v "packages/shadow") in
      let app_root = Path.(workspace_root / Path.v "packages/app") in
      let shadow_source = Path.(shadow_root / Path.v "src/int.ml") in
      let app_source = Path.(app_root / Path.v "src/app.ml") in
      let workspace = make_workspace
        workspace_root
        [
          make_package
            ~name:"shadow"
            ~path:shadow_root
            ~relative_path:(Path.v "packages/shadow")
            ~sources:{
              empty_sources with src = [ Path.v "src/int.ml" ];
            }
            ();
          make_package
            ~name:"app"
            ~path:app_root
            ~relative_path:(Path.v "packages/app")
            ~dependencies:[ make_dependency "shadow" ]
            ~sources:{
              empty_sources with src = [ Path.v "src/app.ml" ];
            }
            ();
        ] in
      match write_file shadow_source "let sentinel = 21\n" with
      | Error err -> Error err
      | Ok () ->
          match write_file app_source "let rendered = Int.to_string Int.sentinel\n" with
          | Error err -> Error err
          | Ok () ->
              let matches = parse_check [ "check"; "--json"; "-p"; "app" ]
              |> Result.expect ~msg:"parse check args" in
              let stdout, stdout_contents = make_capture_writer () in
              let stderr, stderr_contents = make_capture_writer () in
              Riot_cli.Check_cmd.run ~workspace ~stdout ~stderr matches
              |> Result.expect ~msg:"package check should merge dependency and bootstrap module exports";
              let events = parse_jsonl (stdout_contents ()) in
              let file_paths =
                events
                |> List.filter_map
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_file") -> (
                        match Data.Json.get_field "result" json with
                        | Some result_json -> Data.Json.get_field "path" result_json
                        | None -> None
                      )
                    | _ -> None)
              in
              let diagnostic_count =
                events
                |> List.filter
                  (fun json ->
                    match Data.Json.get_field "type" json with
                    | Some (Data.Json.String "check_diagnostic") -> true
                    | _ -> false)
                |> List.length
              in
              Test.assert_equal
                ~expected:[ Data.Json.String "packages/app/src/app.ml" ]
                ~actual:file_paths;
              Test.assert_equal ~expected:0 ~actual:diagnostic_count;
              Test.assert_equal ~expected:"" ~actual:(stderr_contents ());
              Ok ())

let test_check_rejects_package_filter_without_workspace = fun _ctx ->
  let matches = parse_check [ "check"; "--json"; "-p"; "colors" ] |> Result.expect ~msg:"parse check args" in
  let stdout, stdout_contents = make_capture_writer () in
  let stderr, stderr_contents = make_capture_writer () in
  (
    match Riot_cli.Check_cmd.run ~stdout ~stderr matches with
    | Ok () -> Error "expected package-filtered check without workspace to fail"
    | Error _ ->
        Test.assert_equal ~expected:"" ~actual:(stdout_contents ());
        if
          String.contains (stderr_contents ()) "cannot use --package colors outside a riot workspace"
        then
          Ok ()
        else
          Error ("unexpected stderr: " ^ stderr_contents ())
  )

let tests =
  Test.[
    case "check: parse --json flag" test_check_accepts_json_flag;
    case "check: parse --quiet flag" test_check_accepts_quiet_flag;
    case "check: parse --explain option" test_check_accepts_explain_option;
    case "check: parse --package option" test_check_accepts_package_option;
    case "check: parse path argument" test_check_accepts_path_argument;
    case "check: parse multiple path arguments" test_check_accepts_multiple_paths;
    case "check: parse without path" test_check_accepts_no_path_without_explain;
    case "check: json streams a single explicit file without duplicates" test_check_json_streams_single_explicit_file_without_duplicates;
    case "check: human output snapshot" test_check_human_output_snapshot;
    case "check: clean success is silent" test_check_success_is_silent;
    case "check: package filter limits workspace scan" test_check_package_filter_limits_workspace_scan;
    case "check: package filter uses sibling source exports during package scans" test_check_package_filter_uses_package_session_for_cross_file_exports;
    case "check: package filter loads workspace dependency summaries" test_check_package_filter_loads_workspace_dependency_summaries;
    case "check: package filter merges bootstrap and dependency module exports" test_check_package_filter_merges_bootstrap_and_dependency_module_exports;
    case "check: package filter requires workspace" test_check_rejects_package_filter_without_workspace;
  ]

let name = "Riot CLI Check Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
