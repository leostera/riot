open Miniriot
open Miniriot.Exception

module Result = Std.Result
module Test = Std.Test
module HashMap = Kernel.Collections.HashMap
module Int = Kernel.Int

type Message.t +=
  | Burst of int * int
  | Receiver_done
  | Receiver_error of string

let sender ~target ~sender_id ~count =
  let rec loop seq =
    match Int.compare seq count with
    | 1 -> Result.Ok ()
    | _ ->
        send target (Burst (sender_id, seq));
        loop (Int.succ seq)
  in
  loop 1

let receiver ~parent ~sender_count ~messages_per_sender =
  let expected_total = Int.mul sender_count messages_per_sender in
  let next_by_sender = HashMap.with_capacity sender_count in
  let rec init sender_id =
    match Int.compare sender_id sender_count with
    | -1 ->
        let _ = HashMap.insert next_by_sender sender_id 1 in
        init (Int.succ sender_id)
    | _ -> ()
  in
  init 0;
  let rec loop seen =
    if Int.equal seen expected_total then (
      send parent Receiver_done;
      Result.Ok ())
    else
      let sender_id, seq =
        receive
          ~selector:(function
            | Burst (sender_id, seq) -> `select (sender_id, seq)
            | _ -> `skip)
          ~timeout:10.0 ()
      in
      let expected =
        match HashMap.get next_by_sender sender_id with
        | Some n -> n
        | None -> 1
      in
      if Int.equal seq expected then (
        let _ = HashMap.insert next_by_sender sender_id (Int.succ expected) in
        loop (Int.succ seen))
      else
        let msg =
          Kernel.String.concat ""
            [ "out of order message for sender ";
              Int.to_string sender_id;
              ": expected "; Int.to_string expected; ", got ";
              Int.to_string seq ]
        in
        send parent (Receiver_error msg);
        Result.Error (Failure msg)
  in
  loop 0

let test_mailbox_preserves_per_sender_order () =
  let parent = self () in
  let sender_count = 8 in
  let messages_per_sender = 128 in

  let receiver_pid =
    spawn (fun () ->
        receiver ~parent ~sender_count ~messages_per_sender)
  in
  let rec spawn_senders sender_id =
    match Int.compare sender_id sender_count with
    | -1 ->
        let _ =
          spawn (fun () ->
              sender ~target:receiver_pid ~sender_id ~count:messages_per_sender)
        in
        spawn_senders (Int.succ sender_id)
    | _ -> ()
  in
  spawn_senders 0;

  match
    receive
      ~selector:(function
        | Receiver_done -> `select (Result.Ok ())
        | Receiver_error msg -> `select (Result.Error msg)
        | _ -> `skip)
      ~timeout:20.0 ()
  with
  | Result.Ok () -> Result.Ok ()
  | Result.Error msg -> Result.Error msg

let test_case () =
  try test_mailbox_preserves_per_sender_order () with
  | Receive_timeout ->
      Result.Error "timed out waiting for mailbox ordering result"
  | exn ->
      Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [ Test.case "mailbox preserves per-sender ordering" test_case ] in
  let normalize_args = function
    | [] -> [ "mailbox_ordering_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"mailbox_ordering_tests" ~tests
        ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args
    ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
