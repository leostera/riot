open Std
module Test = Std.Test

let make_test_workspace = fun tmpdir ->
  Tusk_model.Workspace.{
    root = tmpdir;
    target_dir_root =
      Path.(tmpdir / Path.v "target");
    packages = [];
    profile_overrides = [];
  }

let test_cache_store_creation = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        ignore (Tusk_store.Store.create ~workspace);
        let cache_dir = Path.(tmpdir / Path.v "target" / Path.v "debug" / Path.v "cache") in
        match Fs.exists cache_dir with
        | Ok true -> Ok ()
        | Ok false -> Error "Cache directory was not created"
        | Error _ -> Error "Failed to check cache directory")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_simple_file_caching = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        Result.expect (Fs.create_dir_all sandbox) ~msg:"Create sandbox failed";
        let output = Path.(sandbox / Path.v "test.txt") in
        let content = "test content" in
        Result.expect (Fs.write content output) ~msg:"Write failed";
        let hash = Crypto.hash_string "test_action" in
        match Tusk_store.Store.save store ~package:"test" ~hash ~sandbox_dir:sandbox ~outs:[ output ] with
        | Ok artifact ->
            if Tusk_store.Store.exists store hash then
              Ok ()
            else
              Error "Artifact not found in store after save"
        | Error e -> Error ("Save failed: " ^ e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_cache_hit_retrieval = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        Result.expect (Fs.create_dir_all sandbox) ~msg:"Create sandbox failed";
        let output = Path.(sandbox / Path.v "result.cmi") in
        Result.expect (Fs.write "compiled" output) ~msg:"Write failed";
        let hash = Crypto.hash_string "compile_action" in
        let _ = Result.expect
          (Tusk_store.Store.save store ~package:"pkg" ~hash ~sandbox_dir:sandbox ~outs:[ output ])
          ~msg:"Save failed" in
        match Tusk_store.Store.get store hash with
        | Some artifact ->
            if Crypto.Digest.hex artifact.hash = Crypto.Digest.hex hash then
              Ok ()
            else
              Error "Retrieved artifact hash mismatch"
        | None -> Error "Expected to find cached artifact")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_cache_promotion_workflow = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        Result.expect (Fs.create_dir_all sandbox) ~msg:"Create sandbox failed";
        let out1 = Path.(sandbox / Path.v "lib.cma") in
        let out2 = Path.(sandbox / Path.v "lib.cmxa") in
        Result.expect (Fs.write "archive1" out1) ~msg:"Write 1 failed";
        Result.expect (Fs.write "archive2" out2) ~msg:"Write 2 failed";
        let hash = Crypto.hash_string "library_build" in
        let _ = Result.expect
          (Tusk_store.Store.save
            store
            ~package:"mylib"
            ~hash
            ~sandbox_dir:sandbox
            ~outs:[ out1; out2 ])
          ~msg:"Save failed" in
        let target_dir = Path.(tmpdir / Path.v "target" / Path.v "debug" / Path.v "mylib") in
        match Tusk_store.Store.promote store hash ~target_dir with
        | Ok () -> (
            let promoted1 = Path.(target_dir / Path.v "lib.cma") in
            let promoted2 = Path.(target_dir / Path.v "lib.cmxa") in
            match (Fs.exists promoted1, Fs.exists promoted2) with
            | Ok true, Ok true ->
                let content1 = Result.expect (Fs.read_to_string promoted1) ~msg:"Read 1 failed" in
                let content2 = Result.expect (Fs.read_to_string promoted2) ~msg:"Read 2 failed" in
                if String.equal content1 "archive1" && String.equal content2 "archive2" then
                  Ok ()
                else
                  Error "Promoted content mismatch"
            | _ -> Error "Promoted files not found"
          )
        | Error e -> Error ("Promotion failed: " ^ e))
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_different_hashes_isolated = fun _ctx ->
  match
    Fs.with_tempdir ~prefix:"cache_test"
      (fun tmpdir ->
        let workspace = make_test_workspace tmpdir in
        let store = Tusk_store.Store.create ~workspace in
        let sandbox = Path.(tmpdir / Path.v "sandbox") in
        Result.expect (Fs.create_dir_all sandbox) ~msg:"Create sandbox failed";
        let output = Path.(sandbox / Path.v "output.txt") in
        Result.expect (Fs.write "version1" output) ~msg:"Write v1 failed";
        let hash1 = Crypto.hash_string "action_v1" in
        let _ = Result.expect
          (Tusk_store.Store.save
            store
            ~package:"test"
            ~hash:hash1
            ~sandbox_dir:sandbox
            ~outs:[ output ])
          ~msg:"Save v1 failed" in
        Result.expect (Fs.write "version2" output) ~msg:"Write v2 failed";
        let hash2 = Crypto.hash_string "action_v2" in
        let _ = Result.expect
          (Tusk_store.Store.save
            store
            ~package:"test"
            ~hash:hash2
            ~sandbox_dir:sandbox
            ~outs:[ output ])
          ~msg:"Save v2 failed" in
        if Tusk_store.Store.exists store hash1 && Tusk_store.Store.exists store hash2 then
          (
            let target1 = Path.(tmpdir / Path.v "out1") in
            let target2 = Path.(tmpdir / Path.v "out2") in
            Result.expect (Tusk_store.Store.promote store hash1 ~target_dir:target1) ~msg:"Promote v1 failed";
            Result.expect (Tusk_store.Store.promote store hash2 ~target_dir:target2) ~msg:"Promote v2 failed";
            let content1 = Result.expect
              (Fs.read_to_string Path.(target1 / Path.v "output.txt"))
              ~msg:"Read v1 failed" in
            let content2 = Result.expect
              (Fs.read_to_string Path.(target2 / Path.v "output.txt"))
              ~msg:"Read v2 failed" in
            if String.equal content1 "version1" && String.equal content2 "version2" then
              Ok ()
            else
              Error "Hash isolation broken - content mismatch"
          )
        else
          Error "Both hashes should exist in cache")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  let open Test in [
    case "cache store creation" test_cache_store_creation;
    case "simple file caching" test_simple_file_caching;
    case "cache hit retrieval" test_cache_hit_retrieval;
    case "cache promotion workflow" test_cache_promotion_workflow;
    case "different hashes isolated" test_different_hashes_isolated;
  ]

let name = "Tusk Executor Caching Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
