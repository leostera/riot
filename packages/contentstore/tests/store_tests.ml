open Std

let test_commit_dir_first_writer_wins = fun _ctx ->
  Fs.with_tempdir ~prefix:"contentstore-commit-dir"
    (fun tmpdir ->
      let store = Contentstore.create ~root:Path.(tmpdir / Path.v "cache") ~policy:Contentstore.Policy.default () in
      let hash = Crypto.hash_string "same-content-address" in
      let first_dir = Path.(tmpdir / Path.v "first") in
      let second_dir = Path.(tmpdir / Path.v "second") in
      let target_file = Path.v "payload.txt" in
      let _ = Fs.create_dir_all first_dir |> Result.expect ~msg:"create first dir should succeed" in
      let _ = Fs.create_dir_all second_dir |> Result.expect ~msg:"create second dir should succeed" in
      let _ =
        Fs.write "first" Path.(first_dir / target_file)
        |> Result.expect ~msg:"write first payload should succeed"
      in
      let _ =
        Fs.write "second" Path.(second_dir / target_file)
        |> Result.expect ~msg:"write second payload should succeed"
      in
      let _ =
        Contentstore.Store.commit_dir store ~hash ~source_dir:first_dir
        |> Result.expect ~msg:"first commit_dir should succeed"
      in
      let _ =
        Contentstore.Store.commit_dir store ~hash ~source_dir:second_dir
        |> Result.expect ~msg:"second commit_dir should also succeed"
      in
      let committed =
        Fs.read_to_string Path.(Contentstore.Store.hash_dir_of store hash / target_file)
        |> Result.expect ~msg:"committed payload should exist"
      in
      if String.equal committed "first" then
        Ok ()
      else
        Error "expected first writer to win for the same content address")
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let test_blob_roundtrip = fun _ctx ->
  Fs.with_tempdir ~prefix:"contentstore-blob"
    (fun tmpdir ->
      let store = Contentstore.create ~root:Path.(tmpdir / Path.v "cache") ~policy:Contentstore.Policy.default () in
      let hash = Crypto.hash_string "blob-roundtrip" in
      let _ =
        Contentstore.Store.save_blob store ~namespace:"typ" ~hash ~content:"hello"
        |> Result.expect ~msg:"save_blob should succeed"
      in
      match Contentstore.Store.load_blob store ~namespace:"typ" ~hash with
      | Some "hello" -> Ok ()
      | Some other -> Error ("unexpected blob content: " ^ other)
      | None -> Error "expected saved blob")
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let test_json_bundle_roundtrip = fun _ctx ->
  Fs.with_tempdir ~prefix:"contentstore-json"
    (fun tmpdir ->
      let store = Contentstore.create ~root:Path.(tmpdir / Path.v "cache") ~policy:Contentstore.Policy.default () in
      let hash = Crypto.hash_string "json-roundtrip" in
      let json =
        Data.Json.Object [
          ("module", Data.Json.String "Colors");
          ("exports", Data.Json.Array [ Data.Json.String "to_string" ]);
        ]
      in
      let _ =
        Contentstore.Store.save_json_bundle store ~namespace:"module-typings" ~hash ~json
        |> Result.expect ~msg:"save_json_bundle should succeed"
      in
      match Contentstore.Store.load_json_bundle store ~namespace:"module-typings" ~hash with
      | Some loaded when Data.Json.to_string loaded = Data.Json.to_string json -> Ok ()
      | Some _ -> Error "expected loaded JSON bundle to match saved JSON bundle"
      | None -> Error "expected saved JSON bundle")
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let test_named_json_bundle_roundtrip = fun _ctx ->
  Fs.with_tempdir ~prefix:"contentstore-named-json"
    (fun tmpdir ->
      let store = Contentstore.create ~root:Path.(tmpdir / Path.v "cache") ~policy:Contentstore.Policy.default () in
      let first = Data.Json.Object [ ("version", Data.Json.Int 1) ] in
      let second = Data.Json.Object [ ("version", Data.Json.Int 2) ] in
      let _ =
        Contentstore.Store.save_named_json_bundle
          store
          ~namespace:"typ/module-typings"
          ~key:"Colors"
          ~json:first
        |> Result.expect ~msg:"first named json save should succeed"
      in
      let _ =
        Contentstore.Store.save_named_json_bundle
          store
          ~namespace:"typ/module-typings"
          ~key:"Colors"
          ~json:second
        |> Result.expect ~msg:"second named json save should succeed"
      in
      match Contentstore.Store.load_named_json_bundle store ~namespace:"typ/module-typings" ~key:"Colors" with
      | Some loaded when Data.Json.to_string loaded = Data.Json.to_string second -> Ok ()
      | Some _ -> Error "expected named json bundle to overwrite previous value"
      | None -> Error "expected saved named json bundle")
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let name = "contentstore_tests"

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "commit_dir keeps first writer" test_commit_dir_first_writer_wins;
        Test.case "blob roundtrip" test_blob_roundtrip;
        Test.case "json bundle roundtrip" test_json_bundle_roundtrip;
        Test.case "named json bundle roundtrip" test_named_json_bundle_roundtrip;
      ] in
      Test.Cli.main ~name ~tests ~args)
    ~args:Env.args
    ()
