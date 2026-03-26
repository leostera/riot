open Std

module Test = Std.Test

let make_test_workspace tmpdir =
  Tusk_model.Workspace.
    {
      root = tmpdir;
      target_dir_root = Path.(tmpdir / Path.v "target");
      packages = [];
      profile_overrides = [];
    }

let read_file path =
  Fs.read_to_string path |> Result.expect ~msg:"failed to read file"

let test_save_and_promote_nested_outputs () =
  match
    Fs.with_tempdir ~prefix:"store_nested_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "obj" / Path.v "x") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") in
        let nested_obj = Path.(sandbox / Path.v "obj" / Path.v "x" / Path.v "a.o") in
        let nested_bin = Path.(sandbox / Path.v "bin" / Path.v "app") in
        let _ = Fs.write "obj" nested_obj in
        let _ = Fs.write "bin" nested_bin in
        let hash = Crypto.hash_string "nested-artifact" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir:sandbox
            ~outs:[ nested_obj; nested_bin ]
          |> Result.expect ~msg:"save should succeed"
        in
        let target = Path.(tmpdir / Path.v "out") in
        let _ =
          Tusk_store.Store.promote store hash ~target_dir:target
          |> Result.expect ~msg:"promote should succeed"
        in
        let promoted_obj = Path.(target / Path.v "obj" / Path.v "x" / Path.v "a.o") in
        let promoted_bin = Path.(target / Path.v "bin" / Path.v "app") in
        if
          String.equal (read_file promoted_obj) "obj"
          && String.equal (read_file promoted_bin) "bin"
        then Ok ()
        else Error "nested files were not promoted correctly")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_get_preserves_relative_paths () =
  match
    Fs.with_tempdir ~prefix:"store_manifest_path_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "deep" / Path.v "dir") in
        let output = Path.(sandbox / Path.v "deep" / Path.v "dir" / Path.v "x.cmxa") in
        let _ = Fs.write "cmxa" output in
        let hash = Crypto.hash_string "manifest-relative-paths" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir:sandbox
            ~outs:[ output ]
          |> Result.expect ~msg:"save should succeed"
        in
        match Tusk_store.Store.get store hash with
        | None -> Error "expected cached artifact"
        | Some artifact ->
            if List.mem (Path.v "deep/dir/x.cmxa") artifact.files then Ok ()
            else Error "artifact paths should keep nested relative paths")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_exists_requires_manifest_file () =
  match
    Fs.with_tempdir ~prefix:"store_exists_manifest_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "hash-without-manifest" in
        let hash_dir = Tusk_store.Store.hash_dir_of store hash in
        let _ = Fs.create_dir_all hash_dir in
        if Tusk_store.Store.exists store hash then
          Error "exists should require manifest.json"
        else Ok ())
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_put_if_absent_keeps_first_writer () =
  match
    Fs.with_tempdir ~prefix:"store_put_if_absent_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all sandbox in
        let output = Path.(sandbox / Path.v "x.txt") in
        let hash = Crypto.hash_string "same-hash" in
        let _ = Fs.write "first" output in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir:sandbox
            ~outs:[ output ]
          |> Result.expect ~msg:"first save should succeed"
        in
        let _ = Fs.write "second" output in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir:sandbox
            ~outs:[ output ]
          |> Result.expect ~msg:"second save should succeed"
        in
        let target = Path.(tmpdir / Path.v "out") in
        let _ =
          Tusk_store.Store.promote store hash ~target_dir:target
          |> Result.expect ~msg:"promote should succeed"
        in
        if String.equal (read_file Path.(target / Path.v "x.txt")) "first" then
          Ok ()
        else Error "second writer should not replace existing cache entry")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_plan_bundle_round_trip () =
  match
    Fs.with_tempdir ~prefix:"store_plan_bundle_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "plan-bundle" in
        let plan =
          Std.Data.Json.Object
            [
              ("version", Std.Data.Json.Int 1);
              ("package", Std.Data.Json.String "pkg");
              ( "action_graph",
                Std.Data.Json.Object [ ("nodes", Std.Data.Json.Array []) ] );
            ]
        in
        let _ =
          Tusk_store.Store.save_plan_bundle store ~hash ~plan
          |> Result.expect ~msg:"save_plan_bundle should succeed"
        in
        match Tusk_store.Store.load_plan_bundle store ~hash with
        | None -> Error "expected saved plan bundle"
        | Some loaded ->
            if String.equal (Std.Data.Json.to_string loaded) (Std.Data.Json.to_string plan) then
              Ok ()
            else Error "loaded plan bundle should match saved bundle")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_package_exports_round_trip () =
  match
    Fs.with_tempdir ~prefix:"store_exports_round_trip_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "exports-hash" in
        let exports =
          [
            Tusk_store.Store.
              {
                name = "foo.cmxa";
                path = Path.v "lib/foo.cmxa";
                action_hash = Crypto.Digest.hex hash;
              };
            Tusk_store.Store.
              {
                name = "foo.cmxs";
                path = Path.v "lib/foo.cmxs";
                action_hash = Crypto.Digest.hex hash;
              };
          ]
        in
        let _ =
          Tusk_store.Store.save_package_exports store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test" ~exports
          |> Result.expect ~msg:"save_package_exports should succeed"
        in
        match
          Tusk_store.Store.load_package_exports store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test"
        with
        | None -> Error "expected saved package exports"
        | Some loaded ->
            if List.length loaded = 2 then Ok ()
            else Error "expected two package export entries")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_find_package_export_path_round_trip () =
  match
    Fs.with_tempdir ~prefix:"store_find_export_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let hash = Crypto.hash_string "find-export-hash" in
        let action_hash = Crypto.Digest.hex hash in
        let rel_path = Path.v "bin/tool" in
        let exports =
          [ Tusk_store.Store.{ name = "tool"; path = rel_path; action_hash } ]
        in
        let _ =
          Tusk_store.Store.save_package_exports store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test" ~exports
          |> Result.expect ~msg:"save_package_exports should succeed"
        in
        let expected = Path.(Tusk_store.Store.hash_dir_of store hash / rel_path) in
        match
          Tusk_store.Store.find_package_export_path store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test" ~name:"tool"
        with
        | None -> Error "expected to resolve package export path"
        | Some resolved ->
            if Path.to_string resolved = Path.to_string expected then Ok ()
            else Error "resolved export path did not match expected immutable path")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_find_package_export_path_rejects_absolute_export_paths () =
  match
    Fs.with_tempdir ~prefix:"store_find_export_absolute_path_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let exports =
          [
            Tusk_store.Store.
              {
                name = "tool";
                path = Path.v "/tmp/not-allowed";
                action_hash = "deadbeef";
              };
          ]
        in
        let _ =
          Tusk_store.Store.save_package_exports store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test" ~exports
          |> Result.expect ~msg:"save_package_exports should succeed"
        in
        match
          Tusk_store.Store.find_package_export_path store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test" ~name:"tool"
        with
        | None -> Ok ()
        | Some _ ->
            Error
              "absolute export paths should be rejected when resolving immutable path")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_find_package_export_path_returns_none_when_name_missing () =
  match
    Fs.with_tempdir ~prefix:"store_find_export_missing_name_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let exports =
          [
            Tusk_store.Store.
              { name = "existing"; path = Path.v "bin/existing"; action_hash = "abc" };
          ]
        in
        let _ =
          Tusk_store.Store.save_package_exports store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test" ~exports
          |> Result.expect ~msg:"save_package_exports should succeed"
        in
        match
          Tusk_store.Store.find_package_export_path store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test" ~name:"missing"
        with
        | None -> Ok ()
        | Some _ -> Error "expected missing export name to resolve to None")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_load_package_exports_returns_none_for_malformed_payload () =
  match
    Fs.with_tempdir ~prefix:"store_load_exports_malformed_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let exports_path =
          Path.(
            Tusk_model.Tusk_dirs.cache_dir ~workspace_root:tmpdir
            / Path.v "exports" / Path.v "debug" / Path.v "x86_64-test"
            / Path.v "pkg.json")
        in
        let _ =
          Fs.create_dir_all (Path.dirname exports_path)
          |> Result.expect ~msg:"create exports dir should succeed"
        in
        let _ =
          Fs.write "{\"version\":1,\"exports\":\"not-an-array\"}" exports_path
          |> Result.expect ~msg:"write malformed export manifest should succeed"
        in
        match
          Tusk_store.Store.load_package_exports store ~package:"pkg"
            ~profile:"debug" ~target:"x86_64-test"
        with
        | None -> Ok ()
        | Some _ -> Error "expected malformed export manifest to return None")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let test_materialize_package_exports_from_action_artifact () =
  match
    Fs.with_tempdir ~prefix:"store_materialize_exports_test" (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        let _ = Fs.create_dir_all Path.(sandbox / Path.v "bin") in
        let output = Path.(sandbox / Path.v "bin" / Path.v "tool") in
        let _ = Fs.write "tool-binary" output in
        let action_hash = Crypto.hash_string "materialize-action" in
        let _ =
          Tusk_store.Store.save store ~package:"pkg" ~hash:action_hash
            ~sandbox_dir:sandbox ~outs:[ output ]
          |> Result.expect ~msg:"save action artifact should succeed"
        in
        let exports =
          [
            Tusk_store.Store.
              {
                name = "tool";
                path = Path.v "bin/tool";
                action_hash = Crypto.Digest.hex action_hash;
              };
          ]
        in
        let target_dir = Path.(tmpdir / Path.v "out" / Path.v "pkg") in
        let _ =
          Tusk_store.Store.materialize_package_exports store ~exports ~target_dir
          |> Result.expect ~msg:"materialize_package_exports should succeed"
        in
        let materialized = Path.(target_dir / Path.v "tool") in
        if String.equal (read_file materialized) "tool-binary" then Ok ()
        else Error "materialized export content did not match source artifact")
  with
  | Ok x -> x
  | Error _ -> Error "tempdir creation failed"

let tests =
  Test.
    [
      case "save/promote nested outputs" test_save_and_promote_nested_outputs;
      case "get preserves relative paths" test_get_preserves_relative_paths;
      case "exists requires manifest" test_exists_requires_manifest_file;
      case "put-if-absent keeps first writer" test_put_if_absent_keeps_first_writer;
      case "plan bundle round trip" test_plan_bundle_round_trip;
      case "package exports round trip" test_package_exports_round_trip;
      case "find package export path round trip"
        test_find_package_export_path_round_trip;
      case "find package export path rejects absolute paths"
        test_find_package_export_path_rejects_absolute_export_paths;
      case "find package export path returns none when name missing"
        test_find_package_export_path_returns_none_when_name_missing;
      case "load package exports returns none for malformed payload"
        test_load_package_exports_returns_none_for_malformed_payload;
      case "materialize package exports from action artifacts"
        test_materialize_package_exports_from_action_artifact;
    ]

let name = "Tusk Store Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
