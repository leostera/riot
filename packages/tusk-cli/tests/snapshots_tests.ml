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

let pending_paths = fun workspace_root ->
  let fixture_pending =
    Path.(workspace_root / Path.v "packages/std/tests/fixtures/sample.expected.new")
  in
  let fixture_approved =
    Path.(workspace_root / Path.v "packages/std/tests/fixtures/sample.expected")
  in
  let workspace_pending =
    Path.(workspace_root / Path.v ".tusk/snapshots/std/suite/case.expected.new")
  in
  let workspace_approved =
    Path.(workspace_root / Path.v ".tusk/snapshots/std/suite/case.expected")
  in
  let build_pending =
    Path.(workspace_root / Path.v "_build/debug/std/ignored.expected.new")
  in
  (fixture_pending, fixture_approved, workspace_pending, workspace_approved, build_pending)

let test_discover_pending_snapshots = Test.case
  "snapshots: discover pending candidates"
  (fun _ctx ->
    with_tempdir_result "snapshots_discover"
      (fun workspace_root ->
        let fixture_pending, _, workspace_pending, _, build_pending = pending_paths workspace_root in
        match write_file fixture_pending "fixture pending\n" with
        | Error msg -> Error msg
        | Ok () -> (
            match write_file workspace_pending "workspace pending\n" with
            | Error msg -> Error msg
            | Ok () -> (
                match write_file build_pending "ignored\n" with
                | Error msg -> Error msg
                | Ok () -> (
                    match Tusk_cli.Snapshots.discover_pending_snapshots ~workspace_root () with
                    | Error err -> Error (IO.error_message err)
                    | Ok snapshots ->
                        let actual = List.map (fun snapshot -> Path.to_string snapshot.Tusk_cli.Snapshots.pending) snapshots in
                        let expected = [ Path.to_string workspace_pending; Path.to_string fixture_pending ] |> List.sort String.compare in
                        let actual = List.sort String.compare actual in
                        Test.assert_equal ~expected ~actual;
                        Ok ()
                  )
              )
          )))

let test_discover_pending_snapshots_filters_by_query = Test.case
  "snapshots: discover pending candidates filters by query"
  (fun _ctx ->
    with_tempdir_result "snapshots_query"
      (fun workspace_root ->
        let fixture_pending, _, workspace_pending, _, _ = pending_paths workspace_root in
        match write_file fixture_pending "fixture pending\n" with
        | Error msg -> Error msg
        | Ok () -> (
            match write_file workspace_pending "workspace pending\n" with
            | Error msg -> Error msg
            | Ok () -> (
                match Tusk_cli.Snapshots.discover_pending_snapshots ~workspace_root ~query:"fixtures/sample" () with
                | Error err -> Error (IO.error_message err)
                | Ok [ snapshot ] ->
                    Test.assert_equal
                      ~expected:(Path.to_string fixture_pending)
                      ~actual:(Path.to_string snapshot.pending);
                    Ok ()
                | Ok snapshots ->
                    Error ("expected one filtered snapshot, got " ^ Int.to_string (List.length snapshots))
              )
          )))

let test_approve_pending_snapshots = Test.case
  "snapshots: approve promotes pending snapshot"
  (fun _ctx ->
    with_tempdir_result "snapshots_approve"
      (fun workspace_root ->
        let _, fixture_approved, _, _, _ = pending_paths workspace_root in
        let pending = Path.add_extension fixture_approved ~ext:"new" in
        match write_file fixture_approved "old approved\n" with
        | Error msg -> Error msg
        | Ok () -> (
            match write_file pending "new approved\n" with
            | Error msg -> Error msg
            | Ok () -> (
                let snapshot = Tusk_cli.Snapshots.{ approved = fixture_approved; pending } in
                match Tusk_cli.Snapshots.approve_pending_snapshots [ snapshot ] with
                | Error err -> Error (IO.error_message err)
                | Ok () ->
                    let approved_content = Fs.read fixture_approved |> Result.expect ~msg:"read approved snapshot" in
                    let pending_exists = Fs.exists pending |> Result.expect ~msg:"stat pending snapshot" in
                    if not (String.equal approved_content "new approved\n") then
                      Error ("unexpected approved snapshot content: " ^ approved_content)
                    else if pending_exists then
                      Error "expected pending snapshot to be removed after approval"
                    else
                      Ok ()
              )
          )))

let test_reject_pending_snapshots = Test.case
  "snapshots: reject removes pending snapshot"
  (fun _ctx ->
    with_tempdir_result "snapshots_reject"
      (fun workspace_root ->
        let _, fixture_approved, _, _, _ = pending_paths workspace_root in
        let pending = Path.add_extension fixture_approved ~ext:"new" in
        match write_file fixture_approved "approved content\n" with
        | Error msg -> Error msg
        | Ok () -> (
            match write_file pending "candidate content\n" with
            | Error msg -> Error msg
            | Ok () -> (
                let snapshot = Tusk_cli.Snapshots.{ approved = fixture_approved; pending } in
                match Tusk_cli.Snapshots.reject_pending_snapshots [ snapshot ] with
                | Error err -> Error (IO.error_message err)
                | Ok () ->
                    let approved_content = Fs.read fixture_approved |> Result.expect ~msg:"read approved snapshot" in
                    let pending_exists = Fs.exists pending |> Result.expect ~msg:"stat pending snapshot" in
                    if not (String.equal approved_content "approved content\n") then
                      Error ("approved snapshot unexpectedly changed: " ^ approved_content)
                    else if pending_exists then
                      Error "expected pending snapshot to be removed after rejection"
                    else
                      Ok ()
              )
          )))

let parse_snapshots = fun args ->
  match ArgParser.get_matches Tusk_cli.Snapshots.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_snapshots_command_parses_subcommands = Test.case
  "snapshots: command parses approve query"
  (fun _ctx ->
    match parse_snapshots [ "snapshots"; "approve"; "fixture" ] with
    | Error err -> Error ("expected snapshots args to parse: " ^ err)
    | Ok matches -> (
        match ArgParser.get_subcommand matches with
        | Some ("approve", approve_matches) ->
            Test.assert_equal ~expected:(Some "fixture") ~actual:(ArgParser.get_one approve_matches "query");
            Ok ()
        | _ -> Error "expected approve subcommand to be selected"
      ))

let tests =
  Test.[
    test_discover_pending_snapshots;
    test_discover_pending_snapshots_filters_by_query;
    test_approve_pending_snapshots;
    test_reject_pending_snapshots;
    test_snapshots_command_parses_subcommands;
  ]

let name = "Tusk CLI Snapshots Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
