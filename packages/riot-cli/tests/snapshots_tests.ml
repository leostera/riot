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
    Path.(workspace_root / Path.v "packages/std/tests/fixtures/sample.expected.new") in
  let fixture_approved = Path.(workspace_root / Path.v "packages/std/tests/fixtures/sample.expected") in
  let custom_pending =
    Path.(workspace_root / Path.v "packages/syn/tests/fixtures/sample.expected_lossless.json.new") in
  let custom_approved =
    Path.(workspace_root / Path.v "packages/syn/tests/fixtures/sample.expected_lossless.json") in
  let workspace_pending =
    Path.(workspace_root / Path.v ".riot/snapshots/std/suite/case.expected.new") in
  let workspace_approved = Path.(workspace_root / Path.v ".riot/snapshots/std/suite/case.expected") in
  let build_pending = Path.(workspace_root / Path.v "_build/debug/std/ignored.expected.new") in
  (
    fixture_pending,
    fixture_approved,
    custom_pending,
    custom_approved,
    workspace_pending,
    workspace_approved,
    build_pending
  )

let test_discover_pending_snapshots =
  Test.case "snapshots: discover pending candidates"
    (fun _ctx ->
      with_tempdir_result "snapshots_discover"
        (fun workspace_root ->
          let fixture_pending, _, custom_pending, _, workspace_pending, _, build_pending = pending_paths
            workspace_root in
          match write_file fixture_pending "fixture pending\n" with
          | Error msg -> Error msg
          | Ok () -> (
              match write_file custom_pending "custom pending\n" with
              | Error msg -> Error msg
              | Ok () -> (
                  match write_file workspace_pending "workspace pending\n" with
                  | Error msg -> Error msg
                  | Ok () -> (
                      match write_file build_pending "ignored\n" with
                      | Error msg -> Error msg
                      | Ok () -> (
                          match Riot_cli.Snapshots.discover_pending_snapshots ~workspace_root () with
                          | Error err -> Error (IO.error_message err)
                          | Ok snapshots ->
                              let actual =
                                List.map
                                  (fun snapshot -> Path.to_string snapshot.Riot_cli.Snapshots.pending)
                                  snapshots
                              in
                              let expected = [
                                Path.to_string workspace_pending;
                                Path.to_string fixture_pending;
                                Path.to_string custom_pending
                              ]
                              |> List.sort String.compare in
                              let actual = List.sort String.compare actual in
                              Test.assert_equal ~expected ~actual;
                              Ok ()
                        )
                    )
                )
            )))

let test_discover_pending_snapshots_filters_by_query =
  Test.case "snapshots: discover pending candidates filters by query"
    (fun _ctx ->
      with_tempdir_result "snapshots_query"
        (fun workspace_root ->
          let fixture_pending, _, custom_pending, _, workspace_pending, _, _ = pending_paths workspace_root in
          match write_file fixture_pending "fixture pending\n" with
          | Error msg -> Error msg
          | Ok () -> (
              match write_file custom_pending "custom pending\n" with
              | Error msg -> Error msg
              | Ok () -> (
                  match write_file workspace_pending "workspace pending\n" with
                  | Error msg -> Error msg
                  | Ok () -> (
                      match Riot_cli.Snapshots.discover_pending_snapshots
                        ~workspace_root
                        ~query:"lossless"
                        () with
                      | Error err ->
                          Error (IO.error_message err)
                      | Ok [ snapshot ] ->
                          Test.assert_equal
                            ~expected:(Path.to_string custom_pending)
                            ~actual:(Path.to_string snapshot.pending);
                          Ok ()
                      | Ok snapshots ->
                          Error ("expected one filtered snapshot, got "
                          ^ Int.to_string (List.length snapshots))
                    )
                )
            )))

let test_approve_pending_snapshots =
  Test.case "snapshots: approve promotes pending snapshot"
    (fun _ctx ->
      with_tempdir_result "snapshots_approve"
        (fun workspace_root ->
          let _, fixture_approved, _, _, _, _, _ = pending_paths workspace_root in
          let pending = Path.add_extension fixture_approved ~ext:"new" in
          match write_file fixture_approved "old approved\n" with
          | Error msg -> Error msg
          | Ok () -> (
              match write_file pending "new approved\n" with
              | Error msg -> Error msg
              | Ok () -> (
                  let snapshot = Riot_cli.Snapshots.{ approved = fixture_approved; pending } in
                  match Riot_cli.Snapshots.approve_pending_snapshots [ snapshot ] with
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

let test_reject_pending_snapshots =
  Test.case "snapshots: reject removes pending snapshot"
    (fun _ctx ->
      with_tempdir_result "snapshots_reject"
        (fun workspace_root ->
          let _, fixture_approved, _, _, _, _, _ = pending_paths workspace_root in
          let pending = Path.add_extension fixture_approved ~ext:"new" in
          match write_file fixture_approved "approved content\n" with
          | Error msg -> Error msg
          | Ok () -> (
              match write_file pending "candidate content\n" with
              | Error msg -> Error msg
              | Ok () -> (
                  let snapshot = Riot_cli.Snapshots.{ approved = fixture_approved; pending } in
                  match Riot_cli.Snapshots.reject_pending_snapshots [ snapshot ] with
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
  match ArgParser.get_matches Riot_cli.Snapshots.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_snapshots_command_parses_subcommands =
  Test.case "snapshots: command parses approve query"
    (fun _ctx ->
      match parse_snapshots [ "snapshots"; "approve"; "fixture" ] with
      | Error err -> Error ("expected snapshots args to parse: " ^ err)
      | Ok matches -> (
          match ArgParser.get_subcommand matches with
          | Some ("approve", approve_matches) ->
              Test.assert_equal
                ~expected:(Some "fixture")
                ~actual:(ArgParser.get_one approve_matches "query");
              Ok ()
          | _ -> Error "expected approve subcommand to be selected"
        ))

let test_parse_review_decision =
  Test.case "snapshots: parse review decision aliases"
    (fun _ctx ->
      let actual = [
        Riot_cli.Snapshots.parse_review_decision "a";
        Riot_cli.Snapshots.parse_review_decision "approve";
        Riot_cli.Snapshots.parse_review_decision "r";
        Riot_cli.Snapshots.parse_review_decision "reject";
        Riot_cli.Snapshots.parse_review_decision "";
        Riot_cli.Snapshots.parse_review_decision "i";
        Riot_cli.Snapshots.parse_review_decision "ignore";
        Riot_cli.Snapshots.parse_review_decision "q";
        Riot_cli.Snapshots.parse_review_decision "quit";
        Riot_cli.Snapshots.parse_review_decision "wat";
      ] in
      let expected = [
        Some `Approve;
        Some `Approve;
        Some `Reject;
        Some `Reject;
        Some `Ignore;
        Some `Ignore;
        Some `Ignore;
        Some `Quit;
        Some `Quit;
        None;
      ] in
      Test.assert_equal ~expected ~actual;
      Ok ())

let test_review_pending_snapshots_with_decider =
  Test.case "snapshots: review applies approve reject ignore decisions"
    (fun _ctx ->
      with_tempdir_result "snapshots_review"
        (fun workspace_root ->
          let fixture_pending, fixture_approved, custom_pending, custom_approved, workspace_pending, workspace_approved, _ =
            pending_paths workspace_root in
          let setup_result =
            match write_file fixture_pending "fixture pending\n" with
            | Error _ as err -> err
            | Ok () -> (
                match write_file custom_pending "custom pending\n" with
                | Error _ as err -> err
                | Ok () -> write_file workspace_pending "workspace pending\n"
              )
          in
          match setup_result with
          | Error msg -> Error msg
          | Ok () -> (
              match Riot_cli.Snapshots.discover_pending_snapshots ~workspace_root () with
              | Error err -> Error (IO.error_message err)
              | Ok snapshots -> (
                  let decide snapshot =
                    match Path.basename snapshot.Riot_cli.Snapshots.pending with
                    | "sample.expected.new" -> Ok `Approve
                    | "sample.expected_lossless.json.new" -> Ok `Reject
                    | "case.expected.new" -> Ok `Ignore
                    | other -> Error (IO.Unknown_error ("unexpected pending snapshot " ^ other))
                  in
                  match Riot_cli.Snapshots.review_pending_snapshots_with_decider
                    ~workspace_root
                    snapshots
                    ~decide with
                  | Error err -> Error (IO.error_message err)
                  | Ok summary ->
                      let fixture_approved_content = Fs.read fixture_approved |> Result.expect ~msg:"read approved fixture snapshot" in
                      let fixture_pending_exists = Fs.exists fixture_pending |> Result.expect ~msg:"stat fixture pending snapshot" in
                      let custom_approved_exists = Fs.exists custom_approved |> Result.expect ~msg:"stat custom approved snapshot" in
                      let custom_pending_exists = Fs.exists custom_pending |> Result.expect ~msg:"stat custom pending snapshot" in
                      let workspace_approved_exists = Fs.exists workspace_approved |> Result.expect ~msg:"stat workspace approved snapshot" in
                      let workspace_pending_exists = Fs.exists workspace_pending |> Result.expect ~msg:"stat workspace pending snapshot" in
                      if not (String.equal fixture_approved_content "fixture pending\n") then
                        Error ("unexpected approved snapshot content: " ^ fixture_approved_content)
                      else if fixture_pending_exists then
                        Error "expected approved pending snapshot to be removed"
                      else if custom_approved_exists then
                        Error "expected rejected snapshot not to create an approved file"
                      else if custom_pending_exists then
                        Error "expected rejected pending snapshot to be removed"
                      else if workspace_approved_exists then
                        Error "expected ignored snapshot not to create an approved file"
                      else if not workspace_pending_exists then
                        Error "expected ignored pending snapshot to remain"
                      else if not (Int.equal summary.Riot_cli.Snapshots.approved_count 1)
                        || not (Int.equal summary.rejected_count 1)
                        || not (Int.equal summary.ignored_count 1)
                        || summary.quit then
                        Error "unexpected review summary"
                      else
                        Ok ()
                )
            )))

let test_review_pending_snapshots_with_quit =
  Test.case "snapshots: review quit stops before mutating snapshots"
    (fun _ctx ->
      with_tempdir_result "snapshots_quit"
        (fun workspace_root ->
          let fixture_pending, fixture_approved, custom_pending, _, _, _, _ = pending_paths workspace_root in
          let setup_result =
            match write_file fixture_pending "fixture pending\n" with
            | Error _ as err -> err
            | Ok () -> write_file custom_pending "custom pending\n"
          in
          match setup_result with
          | Error msg -> Error msg
          | Ok () -> (
              match Riot_cli.Snapshots.discover_pending_snapshots ~workspace_root () with
              | Error err -> Error (IO.error_message err)
              | Ok snapshots -> (
                  let decide _snapshot = Ok `Quit in
                  match Riot_cli.Snapshots.review_pending_snapshots_with_decider
                    ~workspace_root
                    snapshots
                    ~decide with
                  | Error err -> Error (IO.error_message err)
                  | Ok summary ->
                      let approved_exists = Fs.exists fixture_approved |> Result.expect ~msg:"stat approved snapshot" in
                      let fixture_pending_exists = Fs.exists fixture_pending |> Result.expect ~msg:"stat fixture pending snapshot" in
                      let custom_pending_exists = Fs.exists custom_pending |> Result.expect ~msg:"stat custom pending snapshot" in
                      if approved_exists then
                        Error "expected quit not to approve any snapshot"
                      else if not fixture_pending_exists || not custom_pending_exists then
                        Error "expected quit to leave pending snapshots untouched"
                      else if not summary.Riot_cli.Snapshots.quit
                        || not (Int.equal summary.approved_count 0)
                        || not (Int.equal summary.rejected_count 0)
                        || not (Int.equal summary.ignored_count 0) then
                        Error "unexpected quit summary"
                      else
                        Ok ()
                )
            )))

let tests =
  Test.[
    test_discover_pending_snapshots;
    test_discover_pending_snapshots_filters_by_query;
    test_approve_pending_snapshots;
    test_reject_pending_snapshots;
    test_snapshots_command_parses_subcommands;
    test_parse_review_decision;
    test_review_pending_snapshots_with_decider;
    test_review_pending_snapshots_with_quit;
  ]

let name = "Riot CLI Snapshots Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
