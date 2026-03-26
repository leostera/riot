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

let tests =
  Test.
    [
      case "save/promote nested outputs" test_save_and_promote_nested_outputs;
      case "get preserves relative paths" test_get_preserves_relative_paths;
      case "exists requires manifest" test_exists_requires_manifest_file;
      case "put-if-absent keeps first writer" test_put_if_absent_keeps_first_writer;
      case "plan bundle round trip" test_plan_bundle_round_trip;
    ]

let name = "Tusk Store Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
