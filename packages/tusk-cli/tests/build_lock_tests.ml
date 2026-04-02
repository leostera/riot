open Std
module Test = Std.Test
module BuildLock = Tusk_build.Client.BuildLock

type Message.t +=
  | BuildLockAcquired of Time.Duration.t
  | BuildLockAcquireFailed of string

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error _ -> Error "Tempdir creation failed"

let test_reentrant_acquire_in_same_process = fun _ctx ->
  with_tempdir "tusk_build_lock"
    (fun tmpdir ->
      BuildLock.acquire ~workspace_root:tmpdir ~profile:"debug" ~target:"aarch64-apple-darwin"
        (fun () ->
          let parent = self () in
          let _worker =
            spawn
              (fun () ->
                let start = Time.Instant.now () in
                match
                  BuildLock.acquire ~workspace_root:tmpdir ~profile:"debug" ~target:"aarch64-apple-darwin"
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
  with_tempdir "tusk_build_lock"
    (fun tmpdir ->
      let exception Synthetic_failure in
      try
        let _ =
          BuildLock.acquire
            ~workspace_root:tmpdir
            ~profile:"debug"
            ~target:"aarch64-apple-darwin"
            (fun () -> raise Synthetic_failure)
        in
        Error "Expected build lock callback to raise"
      with
      | Synthetic_failure -> (
          match BuildLock.acquire
            ~workspace_root:tmpdir
            ~profile:"debug"
            ~target:"aarch64-apple-darwin"
            (fun () -> Ok ()) with
          | Ok () -> Ok ()
          | Error _ -> Error "Build lock was not released after exception"
        ))

let test_different_targets_do_not_block_each_other = fun _ctx ->
  with_tempdir "tusk_build_lock"
    (fun tmpdir ->
      BuildLock.acquire ~workspace_root:tmpdir ~profile:"debug" ~target:"aarch64-apple-darwin"
        (fun () ->
          let parent = self () in
          let _worker =
            spawn
              (fun () ->
                let start = Time.Instant.now () in
                match
                  BuildLock.acquire ~workspace_root:tmpdir ~profile:"debug" ~target:"aarch64-unknown-linux-gnu"
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

let tests =
  Test.[
    case "build lock: reentrant acquire in same process" test_reentrant_acquire_in_same_process;
    case "build lock: releases on exception" test_releases_lock_on_exception;
    case "build lock: different targets do not block each other" test_different_targets_do_not_block_each_other;
  ]

let name = "Tusk CLI Build Lock Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
