open Std
module Test = Std.Test

type Message.t +=
  | ConcurrentSaveGo
  | ConcurrentSaveComplete of (string * (unit, Riot_store.Store.error) result)

let make_test_workspace = fun tmpdir ->
  Riot_model.Workspace.{
    name = None;
    root = tmpdir;
    target_dir_root =
      Path.(tmpdir / Path.v "target");
    packages = [];
    dependencies = [];
    dev_dependencies = [];
    build_dependencies = [];
    profile_overrides = [];
  }

let read_file = fun path -> Fs.read_to_string path |> Result.expect ~msg:"failed to read file"

let write_output = fun sandbox relative_path content ->
  let output = Path.(sandbox / relative_path) in
  let parent = Path.dirname output in
  let _ = Fs.create_dir_all parent |> Result.expect ~msg:"create output parent should succeed" in
  let _ = Fs.write content output |> Result.expect ~msg:"write output should succeed" in
  output

let write_workspace_cache_config = fun tmpdir ~keep_generations ~max_size ->
  let riot_dir = Path.(tmpdir / Path.v ".riot") in
  let _ = Fs.create_dir_all riot_dir |> Result.expect ~msg:"create .riot should succeed" in
  Fs.write
    ("[riot.cache]\nkeep_generations = "
    ^ Int.to_string keep_generations
    ^ "\nmax_size = \""
    ^ max_size
    ^ "\"\n")
    Path.(riot_dir / Path.v "config.toml")
  |> Result.expect ~msg:"write .riot/config.toml should succeed"

let make_hash = fun ch ->
  String.make ~len:64 ~char:ch

let host_target = Riot_model.Riot_dirs.host_target ()

let count_generation_receipts = fun ~(workspace:Riot_model.Workspace.t) ->
  let generations_dir = Path.(workspace.target_dir_root / Path.v "cache" / Path.v "generations") in
  match Fs.read_dir generations_dir with
  | Error _ -> 0
  | Ok reader ->
      Std.Iter.MutIterator.to_list reader
      |> List.filter
        ~fn:(fun path ->
          let basename = Path.basename path in
          String.ends_with ~suffix:".json" basename)
      |> List.length

let read_cache_state_generation_hashes = fun ~(workspace:Riot_model.Workspace.t) ->
  let path = Path.(workspace.target_dir_root / Path.v "cache" / Path.v "state.json") in
  let content = Fs.read_to_string path |> Result.expect ~msg:"failed to read cache state" in
  let json = Data.Json.of_string content |> Result.expect ~msg:"failed to parse cache state json" in
  match Data.Json.get_field "generation_hashes" json with
  | Some (Data.Json.Array hashes) ->
      List.map hashes
        ~fn:(fun value ->
          Data.Json.get_string value |> Option.expect ~msg:"generation hash should be a string")
  | _ -> []

let read_generation_lane_hashes = fun ~(workspace:Riot_model.Workspace.t) generation_hash ->
  let path =
    Path.(workspace.target_dir_root / Path.v "cache" / Path.v "generations" / Path.v (generation_hash ^ ".json")) in
  let content = Fs.read_to_string path |> Result.expect ~msg:"failed to read generation payload" in
  let json = Data.Json.of_string content |> Result.expect ~msg:"failed to parse generation payload" in
  match Data.Json.get_field "lanes" json with
  | Some (Data.Json.Array lanes) ->
      List.map lanes
        ~fn:(fun lane ->
          match Data.Json.get_field "hashes" lane with
          | Some (Data.Json.Array hashes) ->
              List.map hashes
                ~fn:(fun value ->
                  Data.Json.get_string value |> Option.expect ~msg:"generation lane hash should be a string")
          | _ -> panic "generation lane payload is missing hashes")
  | _ -> panic "generation payload is missing lanes"

let write_cache_entry = fun ~(workspace:Riot_model.Workspace.t) ~profile ~target ~hash ~size ->
  let entry_dir =
    Path.(
      workspace.target_dir_root
      / Path.v profile
      / Path.v (Riot_model.Target.to_string target)
      / Path.v "cache"
      / Path.v hash)
  in
  let payload = Path.(entry_dir / Path.v "artifact.bin") in
  let _ = Fs.create_dir_all entry_dir |> Result.expect ~msg:"create cache entry should succeed" in
  let _ = Fs.write (String.make ~len:size ~char:'x') payload
  |> Result.expect ~msg:"write cache payload should succeed" in
  entry_dir

let test_save_and_promote_nested_outputs = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_nested_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "obj" / Path.v "x") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") in
        let nested_obj = Path.(sandbox / Path.v "obj" / Path.v "x" / Path.v "a.o") in
        let nested_bin = Path.(sandbox / Path.v "bin" / Path.v "app") in
        let _ = Fs.write "obj" nested_obj in
        let _ = Fs.write "bin" nested_bin in
        let hash = Crypto.hash_string "nested-artifact" in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ nested_obj; nested_bin ]
        |> Result.expect ~msg:"save should succeed" in
        let target = Path.(tmpdir / Path.v "out") in
        let _ = Riot_store.Store.promote store hash ~target_dir:target |> Result.expect ~msg:"promote should succeed" in
        let promoted_obj = Path.(target / Path.v "obj" / Path.v "x" / Path.v "a.o") in
        let promoted_bin = Path.(target / Path.v "bin" / Path.v "app") in
        if
          String.equal (read_file promoted_obj) "obj" && String.equal (read_file promoted_bin) "bin"
        then
          Ok ()
        else
          Error "nested files were not promoted correctly")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_get_preserves_relative_paths = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_manifest_path_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "deep" / Path.v "dir") in
        let output = Path.(sandbox / Path.v "deep" / Path.v "dir" / Path.v "x.cmxa") in
        let _ = Fs.write "cmxa" output in
        let hash = Crypto.hash_string "manifest-relative-paths" in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        match Riot_store.Store.get store hash with
        | None -> Error "expected cached artifact"
        | Some artifact ->
            if List.any artifact.files ~fn:(fun path -> Path.equal path (Path.v "deep/dir/x.cmxa")) then
              Ok ()
            else
              Error "artifact paths should keep nested relative paths")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_save_and_promote_self_host_style_outputs = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_self_host_output_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox should succeed" in
        let outputs = [
          ("Kernel__Net__Tcp_listener__Aliases.cmi", "aliases");
          ("Kernel__Time__Monotonic__Unix.o", "native-object");
          ("Kernel__Time__Monotonic__Unix.cmx", "compiled-module");
        ] in
        let out_paths =
          List.map outputs
            ~fn:(fun (name, content) ->
              let path = Path.(sandbox / Path.v name) in
              let _ = Fs.write content path
              |> Result.expect ~msg:("write " ^ name ^ " should succeed") in
              path)
        in
        let hash = Crypto.hash_string "self-host-style-artifacts" in
        let _ = Riot_store.Store.save store ~package:"kernel" ~hash ~sandbox_dir:sandbox ~outs:out_paths
        |> Result.expect ~msg:"save should succeed for self-host-style outputs" in
        let target = Path.(tmpdir / Path.v "out") in
        let _ = Riot_store.Store.promote store hash ~target_dir:target |> Result.expect ~msg:"promote should succeed for self-host-style outputs" in
        let promoted_ok =
          List.all
            outputs
            ~fn:(fun (name, content) ->
              let path = Path.(target / Path.v name) in
              String.equal (read_file path) content)
        in
        if promoted_ok then
          Ok ()
        else
          Error "self-host-style outputs were not preserved through store save/promote")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_exists_requires_manifest_file = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_exists_manifest_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "hash-without-manifest" in
        let hash_dir = Riot_store.Store.hash_dir_of store hash in
        let _ = Fs.create_dir_all hash_dir in
        if Riot_store.Store.exists store hash then
          Error "exists should require manifest.json"
        else
          Ok ())
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_put_if_absent_keeps_first_writer = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_put_if_absent_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox in
        let output = Path.(sandbox / Path.v "x.txt") in
        let hash = Crypto.hash_string "same-hash" in
        let _ = Fs.write "first" output in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"first save should succeed" in
        let _ = Fs.write "second" output in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"second save should succeed" in
        let target = Path.(tmpdir / Path.v "out") in
        let _ = Riot_store.Store.promote store hash ~target_dir:target |> Result.expect ~msg:"promote should succeed" in
        if String.equal (read_file Path.(target / Path.v "x.txt")) "first" then
          Ok ()
        else
          Error "second writer should not replace existing cache entry")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_concurrent_same_hash_saves_share_cache_safely = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_concurrent_same_hash_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "concurrent-shared-hash" in
        let left_sandbox = Path.(tmpdir / Path.v "sandbox-left") in
        let right_sandbox = Path.(tmpdir / Path.v "sandbox-right") in
        let left_output = write_output left_sandbox (Path.v "pkg.cmx") "shared-artifact" in
        let right_output = write_output right_sandbox (Path.v "pkg.cmx") "shared-artifact" in
        let parent = self () in
        let spawn_worker = fun name sandbox output ->
          spawn
            (fun () ->
              let selector msg =
                match msg with
                | ConcurrentSaveGo -> `select msg
                | _ -> `skip
              in
              let _ = receive ~selector () in
              let result =
                Riot_store.Store.save
                  store
                  ~package:"pkg"
                  ~hash
                  ~sandbox_dir:sandbox
                  ~outs:[ output ]
                |> Result.map ~fn:(fun _ -> ())
              in
              send parent (ConcurrentSaveComplete (name, result));
              Ok ())
        in
        let left_worker = spawn_worker "left" left_sandbox left_output in
        let right_worker = spawn_worker "right" right_sandbox right_output in
        send left_worker ConcurrentSaveGo;
        send right_worker ConcurrentSaveGo;
        let selector msg =
          match msg with
          | ConcurrentSaveComplete _ -> `select msg
          | _ -> `skip
        in
        let result1 = receive ~selector () in
        let result2 = receive ~selector () in
        match (result1, result2) with
        | ConcurrentSaveComplete (_, Ok ()), ConcurrentSaveComplete (_, Ok ()) ->
            let target = Path.(tmpdir / Path.v "out") in
            let _ = Riot_store.Store.promote store hash ~target_dir:target
            |> Result.expect ~msg:"promote should succeed after concurrent saves" in
            if String.equal (read_file Path.(target / Path.v "pkg.cmx")) "shared-artifact" then
              Ok ()
            else
              Error "expected concurrent same-hash saves to leave a readable artifact"
        | ConcurrentSaveComplete (name, Error err), _ ->
            Error (name ^ " save failed: " ^ Riot_store.Store.error_message err)
        | _, ConcurrentSaveComplete (name, Error err) ->
            Error (name ^ " save failed: " ^ Riot_store.Store.error_message err)
        | _ ->
            Error "unexpected concurrent save result")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_contended_same_hash_publication = fun _ctx ->
  match Fs.with_tempdir ~prefix:"store_contended_publication"
    (fun root ->
      let store = Riot_store.Store.create ~workspace:(make_test_workspace root) in
      let sandbox = Path.(root / Path.v "sandbox") in
      let output = write_output sandbox (Path.v "payload") "shared-artifact" in
      let hash = Crypto.hash_string "contended-publication" in
      let parent = self () in
      let rec save_four remaining =
        if remaining = 0 then Ok ()
        else
          match Riot_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir:sandbox ~outs:[output] with
          | Error error -> Error error
          | Ok _ -> save_four (remaining - 1)
      in
      let rec spawn_workers index workers =
        if index = 24 then workers
        else
          let worker = spawn (fun () ->
            let selector = function ConcurrentSaveGo -> `select () | _ -> `skip in
            let _ = receive ~selector () in
            let result = save_four 4 in
            send parent (ConcurrentSaveComplete (Int.to_string index, result));
            Ok ()) in
          spawn_workers (index + 1) (worker :: workers)
      in
      let workers = spawn_workers 0 [] in
      List.for_each workers ~fn:(fun worker -> send worker ConcurrentSaveGo);
      let rec collect remaining failures =
        if remaining = 0 then failures
        else
          let selector = function ConcurrentSaveComplete _ as message -> `select message | _ -> `skip in
          match receive ~selector () with
          | ConcurrentSaveComplete (_, Ok ()) -> collect (remaining - 1) failures
          | ConcurrentSaveComplete (name, Error error) ->
              collect (remaining - 1) ((name ^ ": " ^ Riot_store.Store.error_message error) :: failures)
          | _ -> collect (remaining - 1) ("unexpected worker result" :: failures)
      in
      match collect 24 [] with
      | [] ->
          let target = Path.(root / Path.v "out") in
          let _ = Riot_store.Store.promote store hash ~target_dir:target
            |> Result.expect ~msg:"promote contended artifact" in
          if String.equal (read_file Path.(target / Path.v "payload")) "shared-artifact" then Ok ()
          else Error "contended publication changed artifact bytes"
      | failures -> Error (String.concat "; " failures))
  with
  | Ok result -> result
  | Error _ -> Error "tempdir creation failed"

let test_plan_bundle_round_trip = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_plan_bundle_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "plan-bundle" in
        let plan = Std.Data.Json.Object [
          ("version", Std.Data.Json.Int 1);
          ("package", Std.Data.Json.String "pkg");
          ("action_graph", Std.Data.Json.Object [ ("nodes", Std.Data.Json.Array []) ]);
        ] in
        let _ = Riot_store.Store.save_plan_bundle store ~hash ~plan |> Result.expect ~msg:"save_plan_bundle should succeed" in
        match Riot_store.Store.load_plan_bundle store ~hash with
        | None -> Error "expected saved plan bundle"
        | Some loaded ->
            if String.equal (Std.Data.Json.to_string loaded) (Std.Data.Json.to_string plan) then
              Ok ()
            else
              Error "loaded plan bundle should match saved bundle")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_hash_manifest_round_trip_preserves_exports = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_manifest_exports_round_trip_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "exports-hash" in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "lib") in
        let output = Path.(sandbox / Path.v "lib" / Path.v "foo.cmxa") in
        let _ = Fs.write "cmxa" output in
        let exports = [
          Riot_store.Store.{
            name = "foo.cmxa";
            path = Path.v "lib/foo.cmxa";
            action_hash = Crypto.Digest.hex hash
          };
          Riot_store.Store.{
            name = "foo.cmxs";
            path = Path.v "lib/foo.cmxs";
            action_hash = Crypto.Digest.hex hash
          };
        ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~exports
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        match Riot_store.Store.load_manifest store ~hash with
        | None -> Error "expected saved package hash manifest"
        | Some loaded ->
            let loaded = loaded.Riot_store.Manifest.exports in
            if List.length loaded = 2 then
              Ok ()
            else
              Error "expected two package export entries")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_artifact_round_trip_preserves_ocamlc_warnings = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_ocamlc_warnings_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox in
        let output = Path.(sandbox / Path.v "lib.cmx") in
        let _ = Fs.write "compiled" output in
        let hash = Crypto.hash_string "artifact-with-warnings" in
        let exports = [
          Riot_store.Store.{
            name = "lib.cmx";
            path = Path.v "lib.cmx";
            action_hash = Crypto.Digest.hex hash
          };
        ] in
        let warnings = [ "File \"lib.ml\", line 1, characters 0-1:\nWarning: example" ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~ocamlc_warnings:warnings
          ~exports
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        match Riot_store.Store.get store hash with
        | None -> Error "expected saved artifact"
        | Some artifact ->
            if artifact.ocamlc_warnings = warnings && artifact.exports = exports then
              Ok ()
            else
              Error "expected hash manifest payload to round-trip through the store")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_export_source_path_round_trip = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_export_source_path_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "find-export-hash" in
        let action_hash = Crypto.Digest.hex hash in
        let rel_path = Path.v "bin/tool" in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") |> Result.expect ~msg:"create bin dir should succeed" in
        let _ = Fs.write "tool" Path.(sandbox / rel_path) |> Result.expect ~msg:"write export should succeed" in
        let exports = [ Riot_store.Store.{ name = "tool"; path = rel_path; action_hash } ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~exports
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ Path.(sandbox / rel_path) ]
        |> Result.expect ~msg:"save should succeed" in
        let expected = Path.(Riot_store.Store.hash_dir_of store hash / rel_path) in
        match Riot_store.Store.export_source_path store (List.head exports |> Option.expect ~msg:"expected export entry") with
        | None -> Error "expected to resolve package export path"
        | Some resolved ->
            if Path.to_string resolved = Path.to_string expected then
              Ok ()
            else
              Error "resolved export path did not match expected immutable path")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_export_source_path_rejects_absolute_export_paths = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_export_source_path_absolute_path_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "absolute-export-hash" in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox should succeed" in
        let output = Path.(sandbox / Path.v "tool") in
        let _ = Fs.write "tool" output |> Result.expect ~msg:"write output should succeed" in
        let exports = [
          Riot_store.Store.{
            name = "tool";
            path = Path.v "/tmp/not-allowed";
            action_hash = "deadbeef"
          };
        ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~exports
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        match Riot_store.Store.export_source_path store (List.head exports |> Option.expect ~msg:"expected export entry") with
        | None -> Ok ()
        | Some _ -> Error "absolute export paths should be rejected when resolving immutable path")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_load_manifest_returns_none_for_malformed_payload = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_load_manifest_malformed_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let hash = Crypto.hash_string "malformed-manifest" in
        let hash_dir = Riot_store.Store.hash_dir_of store hash in
        let manifest_path = Path.(hash_dir / Path.v "manifest.json") in
        let _ = Fs.create_dir_all hash_dir |> Result.expect ~msg:"create hash dir should succeed" in
        let _ = Fs.write "{\"version\":\"v1\",\"exports\":\"not-an-array\"}" manifest_path
        |> Result.expect ~msg:"write malformed manifest should succeed" in
        match Riot_store.Store.load_manifest store ~hash with
        | None -> Ok ()
        | Some _ -> Error "expected malformed hash manifest to return None")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_materialize_package_exports_from_action_artifact = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_materialize_exports_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") in
        let output = Path.(sandbox / Path.v "bin" / Path.v "tool") in
        let _ = Fs.write "tool-binary" output in
        let action_hash = Crypto.hash_string "materialize-action" in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash:action_hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save action artifact should succeed" in
        let exports = [
          Riot_store.Store.{
            name = "tool";
            path = Path.v "bin/tool";
            action_hash = Crypto.Digest.hex action_hash
          };
        ] in
        let target_dir = Path.(tmpdir / Path.v "out" / Path.v "pkg") in
        let _ = Riot_store.Store.materialize_package_exports store ~exports ~target_dir
        |> Result.expect ~msg:"materialize_package_exports should succeed" in
        let materialized = Path.(target_dir / Path.v "tool") in
        if String.equal (read_file materialized) "tool-binary" then
          Ok ()
        else
          Error "materialized export content did not match source artifact")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_materialize_package_exports_fails_when_source_missing = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_materialize_exports_missing_source_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let missing_action_hash = Crypto.hash_string "missing-action-hash" |> Crypto.Digest.hex in
        let exports = [
          Riot_store.Store.{
            name = "tool";
            path = Path.v "bin/tool";
            action_hash = missing_action_hash
          };
        ] in
        let target_dir = Path.(tmpdir / Path.v "out" / Path.v "pkg") in
        match Riot_store.Store.materialize_package_exports store ~exports ~target_dir with
        | Ok () -> Error "expected missing export source to fail materialization"
        | Error (Riot_store.Store.ExportSourceMissing _) -> Ok ()
        | Error error -> Error ("unexpected error: " ^ Riot_store.Store.error_message error))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_promote_overwrites_existing_target_files = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_promote_overwrite_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox should succeed" in
        let output = Path.(sandbox / Path.v "Kernel__Time__Common.cmx") in
        let _ = Fs.write "fresh" output |> Result.expect ~msg:"write sandbox output should succeed" in
        let hash = Crypto.hash_string "promote-overwrite" in
        let _ = Riot_store.Store.save
          store
          ~package:"kernel"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        let target_dir = Path.(tmpdir / Path.v "sandbox-target") in
        let target_file = Path.(target_dir / Path.v "Kernel__Time__Common.cmx") in
        let _ = Fs.create_dir_all target_dir |> Result.expect ~msg:"create target dir should succeed" in
        let _ = Fs.write "stale" target_file |> Result.expect ~msg:"write stale target file should succeed" in
        let _ = Riot_store.Store.promote store hash ~target_dir |> Result.expect ~msg:"promote should overwrite existing target files" in
        if String.equal (read_file target_file) "fresh" then
          Ok ()
        else
          Error "promote should overwrite stale target files")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_save_and_promote_preserves_executable_permissions = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_promote_permissions_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox should succeed" in
        let output = Path.(sandbox / Path.v "kernel_new_addr_tests") in
        let _ = Fs.write "#!/bin/sh\nexit 0\n" output |> Result.expect ~msg:"write sandbox output should succeed" in
        let _ = Fs.set_permissions output Fs.Permissions.executable
        |> Result.expect ~msg:"mark sandbox output executable should succeed" in
        let hash = Crypto.hash_string "promote-preserves-executable-permissions" in
        let _ = Riot_store.Store.save
          store
          ~package:"kernel"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        let target_dir = Path.(tmpdir / Path.v "sandbox-target") in
        let _ = Riot_store.Store.promote store hash ~target_dir
        |> Result.expect ~msg:"promote should preserve executable permissions" in
        let promoted = Path.(target_dir / Path.v "kernel_new_addr_tests") in
        let source_mode =
          Fs.metadata output
          |> Result.expect ~msg:"read source metadata should succeed"
          |> Fs.Metadata.mode in
        let promoted_mode =
          Fs.metadata promoted
          |> Result.expect ~msg:"read promoted metadata should succeed"
          |> Fs.Metadata.mode in
        if source_mode = promoted_mode then
          Ok ()
        else
          Error "save/promote should preserve executable permissions")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_save_preserves_executable_permissions_in_cache = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_cache_permissions_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox should succeed" in
        let output = Path.(sandbox / Path.v "std_archive_tar_tests") in
        let _ = Fs.write "#!/bin/sh\nexit 0\n" output |> Result.expect ~msg:"write sandbox output should succeed" in
        let _ = Fs.set_permissions output Fs.Permissions.executable
        |> Result.expect ~msg:"mark sandbox output executable should succeed" in
        let hash = Crypto.hash_string "save-preserves-executable-permissions-in-cache" in
        let _ = Riot_store.Store.save
          store
          ~package:"std"
          ~hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save should succeed" in
        let cached = Path.(Riot_store.Store.hash_dir_of store hash / Path.v "std_archive_tar_tests") in
        let source_mode =
          Fs.metadata output
          |> Result.expect ~msg:"read source metadata should succeed"
          |> Fs.Metadata.mode in
        let cached_mode =
          Fs.metadata cached
          |> Result.expect ~msg:"read cached metadata should succeed"
          |> Fs.Metadata.mode in
        if source_mode = cached_mode then
          Ok ()
        else
          Error "save should preserve executable permissions in cache")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_get_returns_none_when_export_source_missing = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_missing_export_cache_hit_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Riot_store.Store.create ~workspace in
        let action_hash = Crypto.hash_string "stale-export-action" in
        let package_hash = Crypto.hash_string "stale-export-package" in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") |> Result.expect ~msg:"create bin dir should succeed" in
        let output = Path.(sandbox / Path.v "bin" / Path.v "tool") in
        let _ = Fs.write "tool-binary" output |> Result.expect ~msg:"write output should succeed" in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~hash:action_hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save action artifact should succeed" in
        let exports = [
          Riot_store.Store.{
            name = "tool";
            path = Path.v "bin/tool";
            action_hash = Crypto.Digest.hex action_hash
          };
        ] in
        let _ = Riot_store.Store.save
          store
          ~package:"pkg"
          ~exports
          ~hash:package_hash
          ~sandbox_dir:sandbox
          ~outs:[ output ]
        |> Result.expect ~msg:"save package artifact should succeed" in
        let action_dir = Riot_store.Store.hash_dir_of store action_hash in
        let _ = Fs.remove_dir_all action_dir |> Result.expect ~msg:"remove action dir should succeed" in
        match Riot_store.Store.get store package_hash with
        | None -> Ok ()
        | Some _ -> Error "expected stale package cache hit to be rejected when export source is missing")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let with_saved_payload = fun prefix fn ->
  match Fs.with_tempdir ~prefix
    (fun tmpdir ->
      let store = Riot_store.Store.create ~workspace:(make_test_workspace tmpdir) in
      let sandbox = Path.(tmpdir / Path.v "sandbox") in
      let _ = Fs.create_dir_all sandbox |> Result.expect ~msg:"create sandbox" in
      let output = Path.(sandbox / Path.v "payload") in
      let _ = Fs.write "original" output |> Result.expect ~msg:"write original payload" in
      let hash = Crypto.hash_string prefix in
      let _ = Riot_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir:sandbox ~outs:[output]
        |> Result.expect ~msg:"save original payload" in
      let cached = Path.(Riot_store.Store.hash_dir_of store hash / Path.v "payload") in
      fn tmpdir store hash sandbox output cached)
  with
  | Ok result -> result
  | Error _ -> Error "tempdir creation failed"

let test_get_rejects_missing_payload = fun _ctx ->
  with_saved_payload "store_missing_payload"
    (fun _root store hash _sandbox _output cached ->
      let _ = Fs.remove_file cached |> Result.expect ~msg:"remove cached payload" in
      match Riot_store.Store.get store hash with
      | None -> Ok ()
      | Some _ -> Error "missing payload was accepted as a cache hit")

let test_get_rejects_changed_payload = fun content _ctx ->
  with_saved_payload "store_changed_payload"
    (fun _root store hash _sandbox _output cached ->
      let _ = Fs.write content cached |> Result.expect ~msg:"damage cached payload" in
      match Riot_store.Store.get store hash with
      | None -> Ok ()
      | Some _ -> Error "damaged payload was accepted as a cache hit")

let test_corrupt_promotion_preserves_working_output = fun _ctx ->
  with_saved_payload "store_corrupt_promotion"
    (fun root store hash _sandbox _output cached ->
      let target = Path.(root / Path.v "out") in
      let _ = Fs.create_dir_all target |> Result.expect ~msg:"create output directory" in
      let working = Path.(target / Path.v "payload") in
      let _ = Fs.write "working" working |> Result.expect ~msg:"write working output" in
      let _ = Fs.write "tampered" cached |> Result.expect ~msg:"damage cached payload" in
      match Riot_store.Store.promote store hash ~target_dir:target with
      | Ok () -> Error "promotion silently accepted corrupt cached bytes"
      | Error _ ->
          if String.equal (read_file working) "working" then Ok ()
          else Error "failed cache verification replaced the working output")

let test_save_refuses_to_reuse_damaged_existing_entry = fun _ctx ->
  with_saved_payload "store_refuse_damaged_entry"
    (fun _root store hash sandbox output cached ->
      let _ = Fs.write "tampered" cached |> Result.expect ~msg:"damage cached payload" in
      match Riot_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir:sandbox ~outs:[output] with
      | Ok _ -> Error "save reported success while retaining a damaged immutable entry"
      | Error _ ->
          if String.equal (read_file cached) "tampered" then Ok ()
          else Error "save deleted corruption evidence instead of reporting failure")

let test_get_rejects_corrupt_referenced_export = fun _ctx ->
  with_saved_payload "store_corrupt_referenced_export"
    (fun _root store action_hash sandbox _output cached ->
      let hash = Crypto.hash_string "package-referencing-corrupt-export" in
      let exports = [Riot_store.Store.{
        name = "payload"; path = Path.v "payload"; action_hash = Crypto.Digest.hex action_hash;
      }] in
      let _ = Riot_store.Store.save store ~package:"pkg" ~exports ~hash ~sandbox_dir:sandbox ~outs:[]
        |> Result.expect ~msg:"save export-only package manifest" in
      let _ = Fs.write "tampered" cached |> Result.expect ~msg:"damage referenced export" in
      match Riot_store.Store.get store hash with
      | None -> Ok ()
      | Some _ -> Error "package cache accepted a corrupt referenced export")

let test_corrupt_export_materialization_preserves_working_output = fun _ctx ->
  with_saved_payload "store_corrupt_export_materialization"
    (fun root store action_hash _sandbox _output cached ->
      let target = Path.(root / Path.v "out") in
      let _ = Fs.create_dir_all target |> Result.expect ~msg:"create export destination" in
      let working = Path.(target / Path.v "payload") in
      let _ = Fs.write "working" working |> Result.expect ~msg:"write working export" in
      let exports = [Riot_store.Store.{
        name = "payload"; path = Path.v "payload"; action_hash = Crypto.Digest.hex action_hash;
      }] in
      let _ = Fs.write "tampered" cached |> Result.expect ~msg:"damage export source" in
      match Riot_store.Store.materialize_package_exports store ~exports ~target_dir:target with
      | Ok () -> Error "export materialization silently accepted corrupt bytes"
      | Error _ ->
          if String.equal (read_file working) "working" then Ok ()
          else Error "failed export verification replaced the working output")

let test_build_root_shared_holders_exclude_cleanup = fun _ctx ->
  match Fs.with_tempdir ~prefix:"store_root_shared_lock"
    (fun root ->
      let target_dir_root = Path.(root / Path.v "target") in
      let module Lock = Riot_store.Build_root_lock in
      let first = Lock.acquire_shared ~target_dir_root |> Result.expect ~msg:"first shared lock" in
      let second = Lock.acquire_shared ~target_dir_root |> Result.expect ~msg:"second shared lock" in
      let exclusive_was_blocked = match Lock.acquire_exclusive ~target_dir_root with
        | Error (Lock.Busy _) -> true
        | Error _ -> false
        | Ok lock -> Lock.release lock; false in
      Lock.release first;
      let remaining_holder_blocked = match Lock.acquire_exclusive ~target_dir_root with
        | Error (Lock.Busy _) -> true
        | Error _ -> false
        | Ok lock -> Lock.release lock; false in
      Lock.release second;
      let exclusive = Lock.acquire_exclusive ~target_dir_root |> Result.expect ~msg:"exclusive after last shared release" in
      let shared_was_blocked = match Lock.acquire_shared ~target_dir_root with
        | Error (Lock.Busy _) -> true
        | Error _ -> false
        | Ok lock -> Lock.release lock; false in
      Lock.release exclusive;
      if exclusive_was_blocked && remaining_holder_blocked && shared_was_blocked then Ok ()
      else Error "shared/exclusive build-root exclusion was violated")
  with
  | Ok result -> result
  | Error _ -> Error "tempdir creation failed"

let test_cleanup_refuses_active_build_and_preserves_stable_lock = fun _ctx ->
  match Fs.with_tempdir ~prefix:"store_root_cleanup_lock"
    (fun root ->
      let workspace = make_test_workspace root in
      let target_dir_root = workspace.Riot_model.Workspace.target_dir_root in
      let _ = Fs.create_dir_all target_dir_root |> Result.expect ~msg:"create target root" in
      let sentinel = Path.(target_dir_root / Path.v "working-output") in
      let _ = Fs.write "working" sentinel |> Result.expect ~msg:"write sentinel" in
      let module Lock = Riot_store.Build_root_lock in
      let lock = Lock.acquire_shared ~target_dir_root |> Result.expect ~msg:"hold active build guard" in
      let force_refused = Result.is_err (Riot_store.Cache_gc.force_clean ~workspace) in
      let policy_refused = Result.is_err (Riot_store.Cache_gc.clean ~workspace) in
      let preserved = String.equal (read_file sentinel) "working" in
      Lock.release lock;
      let _ = Riot_store.Cache_gc.force_clean ~workspace |> Result.expect ~msg:"clean after build release" in
      let root_removed = not (Fs.exists target_dir_root |> Result.unwrap_or ~default:true) in
      let stable_path = Lock.path ~target_dir_root |> Result.expect ~msg:"stable lock path" in
      let lock_survived = Fs.exists stable_path |> Result.unwrap_or ~default:false in
      if force_refused && policy_refused && preserved && root_removed && lock_survived then Ok ()
      else Error "cleanup removed active output or its persistent sibling lock")
  with
  | Ok result -> result
  | Error _ -> Error "tempdir creation failed"

let test_build_root_lock_rejects_root_directory = fun _ctx ->
  match Riot_store.Build_root_lock.acquire_exclusive ~target_dir_root:(Path.v "/") with
  | Error (InvalidRoot _) -> Ok ()
  | Error _ -> Error "expected invalid-root error before filesystem access"
  | Ok lock -> Riot_store.Build_root_lock.release lock; Error "root-directory guard was accepted"

let test_cache_gc_drops_unreferenced_entries_after_generation_overflow = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_cache_gc_generation_overflow_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        write_workspace_cache_config tmpdir ~keep_generations:1 ~max_size:"10 GiB";
        let hash_a = make_hash 'a' in
        let hash_b = make_hash 'b' in
        let entry_a = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_a
          ~size:16 in
        let entry_b = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_b
          ~size:16 in
        let _ = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_a ] } ]
          ~new_entries:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_a } ]
        |> Result.expect ~msg:"first generation should record" in
        let summary =
          let _ = Riot_store.Cache_gc.record_successful_build
            ~workspace
            ~lanes:[
              Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_b ] }
            ]
            ~new_entries:[
              Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_b }
            ]
          |> Result.expect ~msg:"second generation should record" in
          Riot_store.Cache_gc.clean ~workspace |> Result.expect ~msg:"clean should enforce generation retention"
        in
        let entry_a_exists = Fs.exists entry_a |> Result.unwrap_or ~default:false in
        let entry_b_exists = Fs.exists entry_b |> Result.unwrap_or ~default:false in
        if summary.ran_gc && summary.deleted_entries = 1 && not entry_a_exists && entry_b_exists then
          Ok ()
        else
          Error "expected generation overflow GC to keep only the newest live cache entry")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_record_successful_build_tracks_generation_count_in_state = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_cache_gc_generation_count_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        write_workspace_cache_config tmpdir ~keep_generations:4 ~max_size:"10 GiB";
        let hash_a = make_hash 'a' in
        let hash_b = make_hash 'b' in
        let _ = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_a
          ~size:16 in
        let _ = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_b
          ~size:16 in
        let first_summary = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_a ] } ]
          ~new_entries:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_a } ]
        |> Result.expect ~msg:"first generation should record" in
        let second_summary = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_b ] } ]
          ~new_entries:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_b } ]
        |> Result.expect ~msg:"second generation should record" in
        if first_summary.kept_generations = 1 && second_summary.kept_generations = 2 then
          Ok ()
        else
          Error ("expected generation counts 1 then 2, got "
          ^ Int.to_string first_summary.kept_generations
          ^ " then "
          ^ Int.to_string second_summary.kept_generations))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_record_successful_build_dedupes_identical_warm_generation = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_cache_gc_dedupe_generation_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        write_workspace_cache_config tmpdir ~keep_generations:4 ~max_size:"10 GiB";
        let hash_a = make_hash 'a' in
        let _ = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_a
          ~size:16 in
        let first_summary = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_a ] } ]
          ~new_entries:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_a } ]
        |> Result.expect ~msg:"first generation should record" in
        let second_summary = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_a ] } ]
          ~new_entries:[]
        |> Result.expect ~msg:"identical warm generation should be accepted" in
        let receipt_count = count_generation_receipts ~workspace in
        let generation_hashes = read_cache_state_generation_hashes ~workspace in
        if
          first_summary.kept_generations = 1
          && second_summary.kept_generations = 1
          && receipt_count = 1
          && generation_hashes |> List.length |> Int.equal 1
        then
          Ok ()
        else
          Error
            ("expected identical warm generation to keep one receipt, got summaries "
            ^ Int.to_string first_summary.kept_generations
            ^ " and "
            ^ Int.to_string second_summary.kept_generations
            ^ " with "
            ^ Int.to_string receipt_count
            ^ " receipts"))
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_record_successful_build_keeps_new_warm_generation_when_closure_changes = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_cache_gc_distinct_generation_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        write_workspace_cache_config tmpdir ~keep_generations:4 ~max_size:"10 GiB";
        let hash_a = make_hash 'a' in
        let hash_b = make_hash 'b' in
        let _ = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_a
          ~size:16 in
        let _ = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_b
          ~size:16 in
        let _ = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_a ] } ]
          ~new_entries:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_a } ]
        |> Result.expect ~msg:"first generation should record" in
        let second_summary = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_b ] } ]
          ~new_entries:[]
        |> Result.expect ~msg:"distinct warm generation should record" in
        let receipt_count = count_generation_receipts ~workspace in
        let generation_hashes = read_cache_state_generation_hashes ~workspace in
        match generation_hashes with
        | newest_hash :: older_hash :: [] ->
            if
              second_summary.kept_generations = 2
              && receipt_count = 2
              && read_generation_lane_hashes ~workspace newest_hash = [ [ hash_b ] ]
              && read_generation_lane_hashes ~workspace older_hash = [ [ hash_a ] ]
            then
              Ok ()
            else
              Error
                ("expected changed warm generation to keep two receipts, got summary "
                ^ Int.to_string second_summary.kept_generations
                ^ " with "
                ^ Int.to_string receipt_count
                ^ " receipts")
        | _ ->
            Error "expected state.json to keep exactly two generation hashes in recency order")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_record_successful_build_reorders_existing_cached_generation_to_front = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_cache_gc_generation_reorder_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        write_workspace_cache_config tmpdir ~keep_generations:4 ~max_size:"10 GiB";
        let hash_a = make_hash 'a' in
        let hash_b = make_hash 'b' in
        let _ = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_a
          ~size:16 in
        let _ = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_b
          ~size:16 in
        let _ = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_a ] } ]
          ~new_entries:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_a } ]
        |> Result.expect ~msg:"generation A should record" in
        let _ = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_b ] } ]
          ~new_entries:[]
        |> Result.expect ~msg:"generation B should record without new entries" in
        let third_summary = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_a ] } ]
          ~new_entries:[]
        |> Result.expect ~msg:"generation A should move back to the front" in
        let receipt_count = count_generation_receipts ~workspace in
        let generation_hashes = read_cache_state_generation_hashes ~workspace in
        match generation_hashes with
        | newest_hash :: older_hash :: [] ->
            if
              third_summary.kept_generations = 2
              && receipt_count = 2
              && read_generation_lane_hashes ~workspace newest_hash = [ [ hash_a ] ]
              && read_generation_lane_hashes ~workspace older_hash = [ [ hash_b ] ]
            then
              Ok ()
            else
              Error "expected cached rollback generation to reorder state without writing a third payload"
        | _ ->
            Error "expected state.json to keep exactly two generation hashes after rollback reorder")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_cache_gc_shrinks_retained_generations_to_meet_max_size = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"store_cache_gc_max_size_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        write_workspace_cache_config tmpdir ~keep_generations:3 ~max_size:"80 B";
        let hash_a = make_hash 'a' in
        let hash_b = make_hash 'b' in
        let hash_c = make_hash 'c' in
        let entry_a = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_a
          ~size:64 in
        let _ = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_a ] } ]
          ~new_entries:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_a } ]
        |> Result.expect ~msg:"generation A should record" in
        let entry_b = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_b
          ~size:64 in
        let _ = Riot_store.Cache_gc.record_successful_build
          ~workspace
          ~lanes:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_b ] } ]
          ~new_entries:[ Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_b } ]
        |> Result.expect ~msg:"generation B should record" in
        let entry_c = write_cache_entry
          ~workspace
          ~profile:"debug"
          ~target:host_target
          ~hash:hash_c
          ~size:64 in
        let summary =
          let _ = Riot_store.Cache_gc.record_successful_build
            ~workspace
            ~lanes:[
              Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hashes = [ hash_c ] }
            ]
            ~new_entries:[
              Riot_store.Cache_gc.{ profile = "debug"; target = host_target; hash = hash_c }
            ]
          |> Result.expect ~msg:"generation C should record" in
          Riot_store.Cache_gc.clean ~workspace |> Result.expect ~msg:"clean should enforce max_size policy"
        in
        let entry_a_exists = Fs.exists entry_a |> Result.unwrap_or ~default:false in
        let entry_b_exists = Fs.exists entry_b |> Result.unwrap_or ~default:false in
        let entry_c_exists = Fs.exists entry_c |> Result.unwrap_or ~default:false in
        if
          summary.ran_gc
          && summary.kept_generations = 1
          && not entry_a_exists
          && not entry_b_exists
          && entry_c_exists
        then
          Ok ()
        else
          Error "expected max_size GC to drop older retained generations until the cache fits")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.[
    case "save/promote nested outputs" test_save_and_promote_nested_outputs;
    case "save/promote self-host-style outputs" test_save_and_promote_self_host_style_outputs;
    case "get preserves relative paths" test_get_preserves_relative_paths;
    case "exists requires manifest" test_exists_requires_manifest_file;
    case "put-if-absent keeps first writer" test_put_if_absent_keeps_first_writer;
    case "concurrent same-hash saves share cache safely" test_concurrent_same_hash_saves_share_cache_safely;
    case "contended same-hash publication keeps temporary directories private" test_contended_same_hash_publication;
    case "plan bundle round trip" test_plan_bundle_round_trip;
    case "hash manifest round trip preserves exports" test_hash_manifest_round_trip_preserves_exports;
    case "artifact round trip preserves manifest payload" test_artifact_round_trip_preserves_ocamlc_warnings;
    case "export source path round trip" test_export_source_path_round_trip;
    case "export source path rejects absolute paths" test_export_source_path_rejects_absolute_export_paths;
    case "load manifest returns none for malformed payload" test_load_manifest_returns_none_for_malformed_payload;
    case "materialize package exports from action artifacts" test_materialize_package_exports_from_action_artifact;
    case "materialize package exports fails when source missing" test_materialize_package_exports_fails_when_source_missing;
    case "promote overwrites existing target files" test_promote_overwrites_existing_target_files;
    case "save preserves executable permissions in cache" test_save_preserves_executable_permissions_in_cache;
    case "save/promote preserves executable permissions" test_save_and_promote_preserves_executable_permissions;
    case "get returns none when export source missing" test_get_returns_none_when_export_source_missing;
    case "get rejects missing cached payload" test_get_rejects_missing_payload;
    case "get rejects truncated cached payload" (test_get_rejects_changed_payload "x");
    case "get rejects same-size cached payload corruption" (test_get_rejects_changed_payload "tampered");
    case "corrupt cache promotion preserves working output" test_corrupt_promotion_preserves_working_output;
    case "save refuses to reuse damaged immutable entry" test_save_refuses_to_reuse_damaged_existing_entry;
    case "get rejects corrupt referenced export" test_get_rejects_corrupt_referenced_export;
    case "corrupt export materialization preserves working output" test_corrupt_export_materialization_preserves_working_output;
    case "shared build-root holders exclude cleanup" test_build_root_shared_holders_exclude_cleanup;
    case "cleanup refuses active build and preserves sibling lock" test_cleanup_refuses_active_build_and_preserves_stable_lock;
    case "build-root guard rejects root directory" test_build_root_lock_rejects_root_directory;
    case "cache GC records generation count in state" test_record_successful_build_tracks_generation_count_in_state;
    case "cache GC dedupes identical warm generations" test_record_successful_build_dedupes_identical_warm_generation;
    case "cache GC records changed warm generations without new entries" test_record_successful_build_keeps_new_warm_generation_when_closure_changes;
    case "cache GC reorders an existing cached generation to the front" test_record_successful_build_reorders_existing_cached_generation_to_front;
    case "cache GC drops unreferenced entries after generation overflow" test_cache_gc_drops_unreferenced_entries_after_generation_overflow;
    case "cache GC shrinks retained generations to meet max_size" test_cache_gc_shrinks_retained_generations_to_meet_max_size;
  ]

let name = "Riot Store Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
