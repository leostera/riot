open Std
module Test = Std.Test
module BuildLock = Riot_build.BuildLock

type Message.t +=
  | BuildLockAcquired of Time.Duration.t
  | BuildLockAcquireFailed of string

let make_workspace = fun root ->
  Riot_model.Workspace.make_realized ~root ~packages:[] ()

let target_dir_root = fun workspace ->
  workspace.Riot_model.Workspace.target_dir_root

let target = fun triple ->
  Riot_model.Target.from_string triple
  |> Result.expect ~msg:("expected valid target triple: " ^ triple)

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error _ -> Error "Tempdir creation failed"

let test_reentrant_acquire_in_same_process = fun _ctx ->
  with_tempdir "riot_build_lock"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let host_target = target "aarch64-apple-darwin" in
      BuildLock.acquire ~target_dir_root:(target_dir_root workspace) ~profile:"debug" ~target:host_target
        (fun () ->
          let parent = self () in
          let _worker =
            spawn
              (fun () ->
                let start = Time.Instant.now () in
                match
                  BuildLock.acquire
                    ~target_dir_root:(target_dir_root workspace)
                    ~profile:"debug"
                    ~target:host_target
                    (fun () ->
                      let waited = Time.Instant.elapsed start in
                      send parent (BuildLockAcquired waited);
                      Ok ())
                with
                | Ok () -> Ok ()
                | Error _ ->
                    send parent (BuildLockAcquireFailed "Failed to acquire build lock");
                    Ok ())
          in
          let selector msg =
            match msg with
            | BuildLockAcquired waited -> `select (Ok waited)
            | BuildLockAcquireFailed reason -> `select (Error reason)
            | _ -> `skip
          in
          let early_result =
            try Some (receive ~selector ~timeout:(Time.Duration.from_millis 200) ()) with
            | Receive_timeout -> None
          in
          match early_result with
          | Some (Ok waited) ->
              if Time.Duration.to_millis waited < 200 then
                Ok ()
              else
                Error "Reentrant acquire in the same process should not block"
          | Some (Error reason) -> Error reason
          | None -> Error "Reentrant acquire in the same process should complete promptly"))

let test_releases_lock_on_exception = fun _ctx ->
  with_tempdir "riot_build_lock"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let host_target = target "aarch64-apple-darwin" in
      let exception Synthetic_failure in
      try
        let _ =
          BuildLock.acquire
            ~target_dir_root:(target_dir_root workspace)
            ~profile:"debug"
            ~target:host_target
            (fun () -> raise Synthetic_failure)
        in
        Error "Expected build lock callback to raise"
      with
      | Synthetic_failure -> (
          match BuildLock.acquire
            ~target_dir_root:(target_dir_root workspace)
            ~profile:"debug"
            ~target:host_target
            (fun () -> Ok ()) with
          | Ok () -> Ok ()
          | Error _ -> Error "Build lock was not released after exception"
        ))

let test_different_targets_do_not_block_each_other = fun _ctx ->
  with_tempdir "riot_build_lock"
    (fun tmpdir ->
      let workspace = make_workspace tmpdir in
      let host_target = target "aarch64-apple-darwin" in
      let linux_target = target "aarch64-unknown-linux-gnu" in
      BuildLock.acquire ~target_dir_root:(target_dir_root workspace) ~profile:"debug" ~target:host_target
        (fun () ->
          let parent = self () in
          let _worker =
            spawn
              (fun () ->
                let start = Time.Instant.now () in
                match
                  BuildLock.acquire
                    ~target_dir_root:(target_dir_root workspace)
                    ~profile:"debug"
                    ~target:linux_target
                    (fun () ->
                      let waited = Time.Instant.elapsed start in
                      send parent (BuildLockAcquired waited);
                      Ok ())
                with
                | Ok () -> Ok ()
                | Error _ ->
                    send
                      parent
                      (BuildLockAcquireFailed "Failed to acquire build lock for second target");
                    Ok ())
          in
          let selector msg =
            match msg with
            | BuildLockAcquired waited -> `select (Ok waited)
            | BuildLockAcquireFailed reason -> `select (Error reason)
            | _ -> `skip
          in
          match receive ~selector ~timeout:(Time.Duration.from_millis 200) () with
          | Ok waited ->
              if Time.Duration.to_millis waited < 200 then
                Ok ()
              else
                Error "Different target locks should not block each other"
          | Error reason -> Error reason))

let test_build_lock_protects_root_from_cleanup = fun _ctx ->
  with_tempdir "riot_build_cleanup_lock"
    (fun root ->
      let workspace = make_workspace root in
      let target_dir_root = target_dir_root workspace in
      let result = BuildLock.acquire ~target_dir_root ~profile:"debug"
        ~target:(target "aarch64-apple-darwin")
        (fun () ->
          match Riot_store.Cache_gc.force_clean ~workspace with
          | Error _ -> Ok ()
          | Ok () -> Error "cleanup removed a root held by BuildLock.acquire") in
      match result with
      | Error _ as error -> error
      | Ok () ->
          match Riot_store.Cache_gc.force_clean ~workspace with
          | Ok () -> Ok ()
          | Error reason -> Error ("build-root guard was not released: " ^ reason))

let tests =
  Test.[
    case "build lock: reentrant acquire in same process" test_reentrant_acquire_in_same_process;
    case "build lock: releases on exception" test_releases_lock_on_exception;
    case "build lock: different targets do not block each other" test_different_targets_do_not_block_each_other;
    case "build lock: protects root from cleanup" test_build_lock_protects_root_from_cleanup;
  ]

let name = "Riot CLI Build Lock Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
