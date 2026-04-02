open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test

type Message.t +=
  Ping
  | Timeout_test

let test = fun () ->
  let my_pid = self () in
  (* Test 1: send_after works *)
  let _ = Timer.send_after my_pid Ping ~after:0.1 in
  let msg = receive_any () in
  (
    match msg with
    | Ping -> ()
    | _ -> Kernel.panic "Test 1 failed: Expected Ping"
  );
  (* Test 2: receive timeout works *)
  let got_timeout =
    match
      try
        Some (
          receive
            ~selector:(
              function
              | Timeout_test -> `select ()
              | _ -> `skip
            )
            ~timeout:0.05
            ()
        )
      with
      | Receive_timeout -> None
    with
    | Some _ -> Kernel.panic "Test 2 failed: expected timeout"
    | None -> ()
  in
  let () = got_timeout in
  (* Test 3: timer cancellation *)
  let timer_id = Timer.send_after my_pid Timeout_test ~after:1.0 in
  Timer.cancel timer_id;
  let cancelled_timeout =
    match
      try
        Some (
          receive
            ~selector:(
              function
              | Timeout_test -> `select ()
              | _ -> `skip
            )
            ~timeout:0.1
            ()
        )
      with
      | Receive_timeout -> None
    with
    | Some _ -> Kernel.panic "Test 3 failed: expected timeout after cancel"
    | None -> ()
  in
  let () = cancelled_timeout in
  Result.Ok ()

let test_case = fun _ctx ->
  try test () with
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [ Test.case "timer basic tests" test_case ] in
  let normalize_args = function
    | [] -> [ "timer_basic_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"timer_basic_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ()
