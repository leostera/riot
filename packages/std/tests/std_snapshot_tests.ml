open Std

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let make_ctx = fun ?fixture ?(test_name = "snapshot_test") workspace_root ->
  let ctx: Test.ctx = {
    suite_name = "std_snapshot_tests";
    test_name;
    test_index = 1;
    source_file = None;
    binary_path = None;
    workspace_root = Some workspace_root;
    package_name = Some "std";
    fixture;
  } in
  ctx

let snapshot_path = fun workspace_root test_name ->
  Path.(
    workspace_root
    / Path.v ".tusk"
    / Path.v "snapshots"
    / Path.v "std"
    / Path.v "std_snapshot_tests"
    / Path.v test_name
    |> fun path -> Path.add_extension path ~ext:"expected")

let test_snapshot_missing_approved_writes_pending = Test.case
  "snapshot missing approved writes pending"
  (fun _ctx ->
    with_tempdir_result "snapshot_missing"
      (fun workspace_root ->
        let ctx = make_ctx ~test_name:"missing_approved" workspace_root in
        match Test.Snapshot.assert_text ~ctx ~actual:"hello from snapshot\n" with
        | Ok () -> Error "expected snapshot assertion to fail when approved snapshot is missing"
        | Error _ ->
            let pending = snapshot_path workspace_root "missing_approved" |> fun path ->
              Path.of_string (Path.to_string path ^ ".new") |> Result.expect ~msg:"pending snapshot path should be valid"
            in
            let approved_exists = Fs.exists (snapshot_path workspace_root "missing_approved") |> Result.expect ~msg:"stat approved" in
            let pending_exists = Fs.exists pending |> Result.expect ~msg:"stat pending" in
            if approved_exists then
              Error "expected approved snapshot to stay absent"
            else if not pending_exists then
              Error "expected pending snapshot to be written"
            else
              let pending_content = Fs.read pending |> Result.expect ~msg:"read pending snapshot" in
              if String.equal pending_content "hello from snapshot\n" then
                Ok ()
              else
                Error ("unexpected pending snapshot content: " ^ pending_content)))

let test_snapshot_matching_approved_passes = Test.case
  "snapshot matching approved passes"
  (fun _ctx ->
    with_tempdir_result "snapshot_match"
      (fun workspace_root ->
        let approved = snapshot_path workspace_root "matching_approved" in
        match Fs.create_dir_all (Path.dirname approved) with
        | Error err -> Error (IO.error_message err)
        | Ok () -> (
            match Fs.write "matching text\n" approved with
            | Error err -> Error (IO.error_message err)
            | Ok () ->
                let ctx = make_ctx ~test_name:"matching_approved" workspace_root in
                match Test.Snapshot.assert_text ~ctx ~actual:"matching text\n" with
                | Error msg -> Error msg
                | Ok () ->
                    let pending =
                      Path.of_string (Path.to_string approved ^ ".new")
                      |> Result.expect ~msg:"pending snapshot path should be valid"
                    in
                    let pending_exists = Fs.exists pending |> Result.expect ~msg:"stat pending snapshot" in
                    if pending_exists then
                      Error "expected no pending snapshot for matching approved content"
                    else
                      Ok ()
          )))

let test_snapshot_mismatch_writes_pending = Test.case
  "snapshot mismatch writes pending"
  (fun _ctx ->
    with_tempdir_result "snapshot_mismatch"
      (fun workspace_root ->
        let approved = snapshot_path workspace_root "mismatch_snapshot" in
        match Fs.create_dir_all (Path.dirname approved) with
        | Error err -> Error (IO.error_message err)
        | Ok () -> (
            match Fs.write "old text\n" approved with
            | Error err -> Error (IO.error_message err)
            | Ok () ->
                let ctx = make_ctx ~test_name:"mismatch_snapshot" workspace_root in
                match Test.Snapshot.assert_text ~ctx ~actual:"new text\n" with
                | Ok () -> Error "expected snapshot mismatch to fail"
                | Error _ ->
                    let pending =
                      Path.of_string (Path.to_string approved ^ ".new")
                      |> Result.expect ~msg:"pending snapshot path should be valid"
                    in
                    let pending_content = Fs.read pending |> Result.expect ~msg:"read pending snapshot" in
                    if String.equal pending_content "new text\n" then
                      Ok ()
                    else
                      Error ("expected pending snapshot to contain new text, got " ^ pending_content)
          )))

let test_inline_snapshot_mismatch_reports_error = Test.case
  "inline snapshot mismatch reports error"
  (fun ctx ->
    match Test.Snapshot.assert_inline_text ~ctx ~actual:"alpha\nbeta\n" ~expected:"alpha\ncharlie\n" with
    | Ok () -> Error "expected inline snapshot mismatch to fail"
    | Error msg ->
        if String.contains msg "Inline snapshot mismatch." then
          Ok ()
        else
          Error ("unexpected inline snapshot mismatch message: " ^ msg))

let test_json_snapshot_canonicalizes_object_keys = Test.case
  "json snapshot canonicalizes object keys"
  (fun _ctx ->
    with_tempdir_result "snapshot_json"
      (fun workspace_root ->
        let approved = snapshot_path workspace_root "json_snapshot" in
        match Fs.create_dir_all (Path.dirname approved) with
        | Error err -> Error (IO.error_message err)
        | Ok () -> (
            match Fs.write "{\"a\":1,\"b\":2}" approved with
            | Error err -> Error (IO.error_message err)
            | Ok () ->
                let ctx = make_ctx ~test_name:"json_snapshot" workspace_root in
                Test.Snapshot.assert_json
                  ~ctx
                  ~actual:(Data.Json.obj [ ("b", Data.Json.int 2); ("a", Data.Json.int 1) ])
          )))

let tests = [
  test_snapshot_missing_approved_writes_pending;
  test_snapshot_matching_approved_passes;
  test_snapshot_mismatch_writes_pending;
  test_inline_snapshot_mismatch_reports_error;
  test_json_snapshot_canonicalizes_object_keys;
]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"std_snapshot_tests" ~tests ~args)
    ~args:Env.args
    ()
