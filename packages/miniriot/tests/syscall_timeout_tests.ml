open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String
module Interest = Kernel.Async.Interest
module Source = Kernel.Async.Source

type Message.t +=
  | Syscall_worker_done
  | Syscall_worker_error of string
  | Syscall_unexpected_success

type tracking_state = {
  mutable registered: bool;
  mutable register_count: int;
  mutable deregister_count: int;
}

module Tracking_source = struct
  type t = tracking_state

  let register = fun state _selector _token _interest ->
    state.register_count <- Int.succ state.register_count;
    if state.registered then
      Result.Error Kernel.IO.Resource_busy
    else (
      state.registered <- true;
      Result.Ok ()
    )

  let reregister = fun state _selector _token _interest -> register state _selector _token _interest

  let deregister = fun state _selector ->
    state.deregister_count <- Int.succ state.deregister_count;
    state.registered <- false;
    Result.Ok ()
end

let int_eq = fun a b ->
  match Int.compare a b with
  | 0 -> true
  | _ -> false

let run_timeout_loop = fun ~parent ~source ~rounds ->
  let rec loop n =
    if int_eq n 0 then
      (
        send parent Syscall_worker_done;
        Result.Ok ()
      )
    else
      let syscall_result =
        try
          let _ =
            syscall ~name:"syscall-timeout-test" ~interest:Interest.readable ~source ~timeout:0.002
              (fun () ->
                send parent Syscall_unexpected_success;
                Result.Ok ())
          in
          `unexpected_success
        with
        | Syscall_timeout -> `timed_out
      in
      match syscall_result with
      | `timed_out -> loop (Int.pred n)
      | `unexpected_success ->
          send parent (Syscall_worker_error "syscall callback unexpectedly executed");
          Result.Error (Failure "syscall callback unexpectedly executed")
  in
  try loop rounds with
  | exn ->
      send parent (Syscall_worker_error (Kernel.Exception.to_string exn));
      Result.Error exn

let test_syscall_timeout_deregisters_wait_registration = fun () ->
  let parent = self () in
  let state = { registered = false; register_count = 0; deregister_count = 0 } in
  let source =
    Source.make (module Tracking_source) state
  in
  let rounds = 32 in
  let _worker =
    spawn (fun () -> run_timeout_loop ~parent ~source ~rounds)
  in
  let outcome =
    receive
      ~selector:(
        function
        | Syscall_worker_done -> `select (Result.Ok ())
        | Syscall_worker_error msg -> `select (Result.Error msg)
        | Syscall_unexpected_success -> `select (Result.Error "unexpected syscall callback success")
        | _ -> `skip
      )
      ~timeout:20.0
      ()
  in
  match outcome with
  | Result.Error _ as err -> err
  | Result.Ok () ->
      if int_eq state.register_count state.deregister_count then
        if state.registered then
          Result.Error "source remained registered after timeout loop"
        else
          Result.Ok ()
      else
        Result.Error (String.concat
          ""
          [
            "syscall registrations leaked: register_count=";
            Int.to_string state.register_count;
            ", deregister_count=";
            Int.to_string state.deregister_count
          ])

let test_case = fun name fn ->
  try fn () with
  | Receive_timeout -> Result.Error (String.concat "" [ "timed out in "; name ])
  | exn -> Result.Error (String.concat
    ""
    [ "unexpected exception in "; name; ": "; Kernel.Exception.to_string exn ])

let () =
  let tests = [
    Test.case
      "syscall timeout deregisters wait registration"
      (fun _ctx -> test_case "syscall timeout deregisters wait registration" test_syscall_timeout_deregisters_wait_registration);
  ] in
  let normalize_args = function
    | [] -> [ "syscall_timeout_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"syscall_timeout_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
