open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test

type Message.t +=
  | Burst of int
  | Sender_started
  | Sender_done
  | Receiver_done of int

let receiver = fun ~parent ~expected ->
  let rec loop seen =
    if Kernel.Int.equal seen expected then
      (
        send parent (Receiver_done seen);
        Result.Ok ()
      )
    else
      let _ =
        receive
          ~selector:(
            function
            | Burst _ -> `select ()
            | _ -> `skip
          )
          ~timeout:5.0
          ()
      in
      loop (Kernel.Int.succ seen)
  in
  loop 0

let sender = fun ~target ~count ~parent ->
  send parent Sender_started;
  let rec loop n =
    if Kernel.Int.equal n 0 then
      (
        send parent Sender_done;
        Result.Ok ()
      )
    else (
      send target (Burst n);
      loop (Kernel.Int.pred n)
    )
  in
  loop count

let test = fun () ->
  let parent = self () in
  let senders = 8 in
  let messages_per_sender = 32 in
  let expected_total = Kernel.Int.mul senders messages_per_sender in
  let receiver_pid =
    spawn (fun () -> receiver ~parent ~expected:expected_total)
  in
  let rec spawn_senders n =
    if Kernel.Int.equal n 0 then
      ()
    else
      let _ =
        spawn (fun () -> sender ~target:receiver_pid ~count:messages_per_sender ~parent)
      in
      spawn_senders (Kernel.Int.pred n)
  in
  spawn_senders senders;
  let rec collect ~sender_started ~sender_done ~receiver_count =
    match receiver_count with
    | Some count when Kernel.Int.equal count expected_total ->
        if Kernel.Int.equal sender_done senders then
          Result.Ok ()
        else
          let next =
            try Some (receive_any ~timeout:20.0 ()) with
            | Receive_timeout -> None
          in
          (
            match next with
            | None -> Result.Error (Kernel.String.concat
              ""
              [
                "timed out while collecting completions (sender_done=";
                Kernel.Int.to_string sender_done;
                ", sender_started=";
                Kernel.Int.to_string sender_started;
                ", expected=";
                Kernel.Int.to_string senders;
                ", receiver_count=";
                Kernel.Int.to_string count;
                ")";
              ])
            | Some Sender_started -> collect
              ~sender_started:(Kernel.Int.succ sender_started)
              ~sender_done
              ~receiver_count
            | Some Sender_done -> collect
              ~sender_started
              ~sender_done:(Kernel.Int.succ sender_done)
              ~receiver_count
            | Some (Receiver_done next_count) -> collect
              ~sender_started
              ~sender_done
              ~receiver_count:(Some next_count)
            | Some _ -> collect ~sender_started ~sender_done ~receiver_count
          )
    | Some count ->
        Result.Error (Kernel.String.concat
          ""
          [
            "receiver count mismatch: expected ";
            Kernel.Int.to_string expected_total;
            ", got ";
            Kernel.Int.to_string count
          ])
    | _ ->
        let next =
          try Some (receive_any ~timeout:20.0 ()) with
          | Receive_timeout -> None
        in
        (
          match next with
          | None ->
              Result.Error (
                Kernel.String.concat ""
                  [
                    "timed out while collecting completions (sender_done=";
                    Kernel.Int.to_string sender_done;
                    ", sender_started=";
                    Kernel.Int.to_string sender_started;
                    ", expected=";
                    Kernel.Int.to_string senders;
                    ", receiver_count=";
                    (
                      match receiver_count with
                      | None -> "none"
                      | Some count -> Kernel.Int.to_string count
                    );
                    ")";
                  ]
              )
          | Some Sender_started -> collect
            ~sender_started:(Kernel.Int.succ sender_started)
            ~sender_done
            ~receiver_count
          | Some Sender_done -> collect ~sender_started ~sender_done:(Kernel.Int.succ sender_done) ~receiver_count
          | Some (Receiver_done count) -> collect
            ~sender_started
            ~sender_done
            ~receiver_count:(Some count)
          | Some _ -> collect ~sender_started ~sender_done ~receiver_count
        )
  in
  collect ~sender_started:0 ~sender_done:0 ~receiver_count:None

let test_case = fun _ctx ->
  try test () with
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [ Test.case "multicore wakeup delivery" test_case ] in
  let normalize_args = function
    | [] -> [ "multicore_wakeup_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"multicore_wakeup_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
