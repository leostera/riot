open Std
open Typ

let file_summary_for = fun source ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "persisted_summary.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  match Query.file_summary_of snapshot source_id with
  | Some summary -> Ok summary
  | None -> Error "expected file summary for analyzed source"

let assert_roundtrip = fun ~ctx source ->
  match file_summary_for source with
  | Error _ as err -> err
  | Ok summary ->
      let persisted = PersistedSummary.of_file_summary summary in
      let actual_json = PersistedSummary.Json.to_json persisted in
      begin
        match PersistedSummary.Json.of_json actual_json with
        | Error _ as err -> err
        | Ok decoded ->
            let roundtripped_summary = PersistedSummary.to_file_summary decoded in
            let roundtripped_persisted_json = PersistedSummary.Json.to_json decoded in
            let roundtripped_json = FileSummary.to_json roundtripped_summary in
            let original_json = FileSummary.to_json summary in
            if not (actual_json = roundtripped_persisted_json) then
              Error ("persisted summary roundtrip changed persisted json\noriginal:\n"
              ^ Data.Json.to_string_pretty actual_json
              ^ "\nroundtripped:\n"
              ^ Data.Json.to_string_pretty roundtripped_persisted_json)
            else if not (original_json = roundtripped_json) then
              Error ("persisted summary roundtrip changed file summary\noriginal:\n"
              ^ Data.Json.to_string_pretty original_json
              ^ "\nroundtripped:\n"
              ^ Data.Json.to_string_pretty roundtripped_json)
            else
              Test.Snapshot.assert_json
                ~ctx
                ~actual:(Data.Json.Object [
                  ("persisted", actual_json);
                  ("file_summary", original_json);
                ])
      end

let test_trusted_summary_roundtrip = fun ctx -> assert_roundtrip ~ctx "let id x = x\nlet const x _ = x\n"

let test_errored_summary_roundtrip = fun ctx -> assert_roundtrip ~ctx "let broken = missing\n"

let test_type_decl_summary_roundtrip = fun ctx ->
  assert_roundtrip
    ~ctx
    "type point = { x: int; y: int }\nlet origin = { x = 0; y = 0 }\n"

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "trusted summary roundtrips through persisted json" test_trusted_summary_roundtrip;
        Test.case "errored summary roundtrips through persisted json" test_errored_summary_roundtrip;
        Test.case "type declarations roundtrip through persisted json" test_type_decl_summary_roundtrip;
      ] in
      Test.Cli.main ~name:"typ:persisted_summary" ~tests ~args)
    ~args:Env.args
    ()
