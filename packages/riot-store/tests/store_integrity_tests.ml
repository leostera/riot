open Std

module Test = Std.Test
module Store = Riot_store.Store
module Manifest = Riot_store.Manifest

(* Every fixture lives inside [Fs.with_tempdir]; no real cache is touched. *)

let make_workspace = fun tmpdir ->
  Riot_model.Workspace.make
    ~root:tmpdir
    ~target_dir:(Path.v "target")
    ~packages:[]
    ()

let expect = fun msg result -> Result.expect ~msg result

let write_file = fun path content ->
  let _ =
    Fs.create_dir_all (Path.dirname path)
    |> expect "create parent should succeed"
  in
  Fs.write content path
  |> expect "write should succeed"

let read_file = fun path ->
  Fs.read_to_string path
  |> expect "read should succeed"

(* Saves one package artifact with a single [payload.cmx] output. *)

let save_fixture = fun tmpdir ~name ~content ->
  let store = Store.create ~workspace:(make_workspace tmpdir) in
  let sandbox = Path.(tmpdir / Path.v ("sandbox-" ^ name)) in
  let output = Path.(sandbox / Path.v "payload.cmx") in
  write_file output content;
  let hash = Crypto.hash_string ("store-integrity-" ^ name) in
  let _ =
    Store.save store ~package:"fixture" ~input_hash:hash ~sandbox_dir:sandbox ~outs:[ output ]
    |> expect "save should succeed"
  in
  let payload = Path.(Store.hash_dir_of store hash / Path.v "payload.cmx") in
  (store, hash, payload)

let fresh_store = fun tmpdir -> Store.create ~workspace:(make_workspace tmpdir)

let in_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error _ -> Error "tempdir creation failed"

let replace_with_directory = fun path ->
  let _ =
    Fs.remove_file path
    |> expect "remove payload should succeed"
  in
  Fs.create_dir_all path
  |> expect "create directory in place of payload should succeed"

let is_integrity_error = fun result ->
  match result with
  | Error (Store.ArtifactIntegrityFailed _) -> true
  | _ -> false

let test_valid_artifact_round_trips = fun _ctx ->
  in_tempdir
    "store_integrity_valid"
    (fun tmpdir ->
      let (_, hash, _) = save_fixture tmpdir ~name:"valid" ~content:"valid-bytes" in
      let store = fresh_store tmpdir in
      let target_dir = Path.(tmpdir / Path.v "target-valid") in
      match (Store.get store hash, Store.promote store hash ~target_dir) with
      | (Some _, Ok ()) when String.equal
        (read_file Path.(target_dir / Path.v "payload.cmx"))
        "valid-bytes" -> Ok ()
      | (None, _) -> Error "valid artifact should be found"
      | (_, Error err) -> Error ("valid promote failed: " ^ Store.error_message err)
      | _ -> Error "valid promote should copy the stored bytes")

let test_lookup_rejects_same_size_altered_contents = fun _ctx ->
  in_tempdir
    "store_integrity_same_size"
    (fun tmpdir ->
      let (_, hash, payload) = save_fixture tmpdir ~name:"same-size" ~content:"original" in
      write_file payload "tampered";
      match Store.get (fresh_store tmpdir) hash with
      | None -> Ok ()
      | Some _ -> Error "lookup accepted same-size altered contents")

let test_warmed_lookup_rejects_later_mutation = fun _ctx ->
  in_tempdir
    "store_integrity_warmed"
    (fun tmpdir ->
      let (store, hash, payload) = save_fixture tmpdir ~name:"warmed" ~content:"original" in
      match Store.get store hash with
      | None -> Error "warm-up lookup should hit"
      | Some _ -> (
          write_file payload "tampered";
          match Store.get store hash with
          | None -> Ok ()
          | Some _ -> Error "warmed lookup accepted a later mutation"
        ))

let test_lookup_rejects_wrong_size_payload = fun _ctx ->
  in_tempdir
    "store_integrity_wrong_size"
    (fun tmpdir ->
      let (_, hash, payload) = save_fixture tmpdir ~name:"wrong-size" ~content:"original" in
      write_file payload "much longer replacement payload";
      match Store.get (fresh_store tmpdir) hash with
      | None -> Ok ()
      | Some _ -> Error "lookup accepted a payload with the wrong size")

let test_lookup_rejects_directory_payload = fun _ctx ->
  in_tempdir
    "store_integrity_directory"
    (fun tmpdir ->
      let (_, hash, payload) = save_fixture tmpdir ~name:"directory" ~content:"original" in
      replace_with_directory payload;
      match (Store.get (fresh_store tmpdir) hash, Store.exists (fresh_store tmpdir) hash) with
      | (None, false) -> Ok ()
      | _ -> Error "lookup accepted a directory as a file payload")

let test_lookup_rejects_symlink_payload = fun _ctx ->
  in_tempdir
    "store_integrity_symlink"
    (fun tmpdir ->
      let (_, hash, payload) = save_fixture tmpdir ~name:"symlink" ~content:"original" in
      let outside = Path.(tmpdir / Path.v "outside.cmx") in
      write_file outside "original";
      let _ =
        Fs.remove_file payload
        |> expect "remove payload should succeed"
      in
      let _ =
        Fs.symlink ~src:outside ~dst:payload
        |> expect "symlink should succeed"
      in
      match Store.get (fresh_store tmpdir) hash with
      | None -> Ok ()
      | Some _ -> Error "lookup accepted a symlink in place of a stored file")

let test_promote_rejects_corrupted_payload = fun _ctx ->
  in_tempdir
    "store_integrity_promote_corrupt"
    (fun tmpdir ->
      let (_, hash, payload) = save_fixture tmpdir ~name:"promote-corrupt" ~content:"original" in
      write_file payload "tampered";
      let target_dir = Path.(tmpdir / Path.v "target-corrupt") in
      let result = Store.promote (fresh_store tmpdir) hash ~target_dir in
      if not (is_integrity_error result) then
        Error "promote should report an integrity failure for a corrupted payload"
      else if Path.exists Path.(target_dir / Path.v "payload.cmx") then
        Error "promote must not copy a corrupted payload"
      else
        Ok ())

(* A forged manifest whose entry escapes the artifact directory. *)

let test_promote_rejects_escaping_manifest_path = fun _ctx ->
  in_tempdir
    "store_integrity_escape"
    (fun tmpdir ->
      let (store, hash, _) = save_fixture tmpdir ~name:"escape" ~content:"original" in
      let hash_dir = Store.hash_dir_of store hash in
      let target_dir = Path.(tmpdir / Path.v "nested" / Path.v "target-escape") in
      let escaped_source = Path.(Path.dirname hash_dir / Path.v "escaped") in
      write_file escaped_source "escaped";
      let manifest =
        Manifest.create_from_entries
          ()
          ~package:"fixture"
          ~input_hash:(Crypto.Digest.hex hash)
          ~files:[
            Manifest.{
              path = Path.v "../escaped";
              hash = Option.unwrap_or (Manifest.hash_file escaped_source) ~default:"";
              size = 7;
            };
          ]
      in
      let _ =
        Manifest.save manifest ~path:Path.(hash_dir / Path.v "manifest.bin")
        |> expect "forging manifest should succeed"
      in
      let result = Store.promote (fresh_store tmpdir) hash ~target_dir in
      let escaped_target = Path.(tmpdir / Path.v "nested" / Path.v "escaped") in
      if Path.exists escaped_target then
        Error "promote wrote outside its target directory"
      else if not (is_integrity_error result) then
        Error "promote should reject a manifest path that escapes the artifact directory"
      else if Option.is_some (Store.get (fresh_store tmpdir) hash) then
        Error "lookup should reject a manifest path that escapes the artifact directory"
      else
        Ok ())

let test_verification_survives_new_store_instances = fun _ctx ->
  in_tempdir
    "store_integrity_reverify"
    (fun tmpdir ->
      let (_, hash, payload) = save_fixture tmpdir ~name:"reverify" ~content:"original" in
      match (Store.get (fresh_store tmpdir) hash, Store.get (fresh_store tmpdir) hash) with
      | (Some _, Some _) -> (
          write_file payload "tampered";
          match Store.get (fresh_store tmpdir) hash with
          | None -> Ok ()
          | Some _ -> Error "a recorded verification must not hide a later mutation"
        )
      | _ -> Error "repeated lookups of a valid artifact should hit")

let test_corrupted_artifact_is_replaced_on_save = fun _ctx ->
  in_tempdir
    "store_integrity_replace"
    (fun tmpdir ->
      let (_, hash, payload) = save_fixture tmpdir ~name:"replace" ~content:"original" in
      write_file payload "tampered";
      let (store, _, _) = save_fixture tmpdir ~name:"replace" ~content:"original" in
      match Store.get store hash with
      | Some _ when String.equal (read_file payload) "original" -> Ok ()
      | Some _ -> Error "re-saving should replace the corrupted payload"
      | None -> Error "re-saved artifact should be found")

let tests =
  Test.[
    case "valid artifact round trips" test_valid_artifact_round_trips;
    case "lookup rejects same-size altered contents" test_lookup_rejects_same_size_altered_contents;
    case "warmed lookup rejects later mutation" test_warmed_lookup_rejects_later_mutation;
    case "lookup rejects wrong-size payload" test_lookup_rejects_wrong_size_payload;
    case "lookup rejects directory payload" test_lookup_rejects_directory_payload;
    case "lookup rejects symlink payload" test_lookup_rejects_symlink_payload;
    case "promote rejects corrupted payload" test_promote_rejects_corrupted_payload;
    case "promote rejects escaping manifest path" test_promote_rejects_escaping_manifest_path;
    case "verification survives new store instances" test_verification_survives_new_store_instances;
    case "corrupted artifact is replaced on save" test_corrupted_artifact_is_replaced_on_save;
  ]

let name = "Riot Store Integrity Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
