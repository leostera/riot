open Miniriot
open Miniriot.Exception
open Kernel.Ops

module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String
module HashMap = Kernel.Collections.HashMap

type Message.t +=
  | Harness_data of int * int
  | Harness_sender_done of int
  | Harness_result_ok
  | Harness_result_error of string

type scenario = {
  seed : int;
  sender_count : int;
  messages_per_sender : int;
}

let int_eq a b = match Int.compare a b with 0 -> true | _ -> false
let int_lt a b = match Int.compare a b with -1 -> true | _ -> false
let int_gt a b = match Int.compare a b with 1 -> true | _ -> false

let normalize_seed raw_seed =
  let modulo = 1_000_000 in
  let folded = raw_seed mod modulo in
  let non_negative =
    if int_lt folded 0 then
      Int.add folded modulo
    else
      folded
  in
  Int.succ non_negative

let scenario_of_seed raw_seed =
  let seed = normalize_seed raw_seed in
  let rng = Kernel.Random.State.make [| seed; 41 |] in
  {
    seed;
    sender_count = Int.add 2 (Kernel.Random.State.int rng 3);
    messages_per_sender = Int.add 6 (Kernel.Random.State.int rng 9);
  }

let scenario_to_string s =
  String.concat ""
    [
      "{seed="; Int.to_string s.seed;
      "; sender_count="; Int.to_string s.sender_count;
      "; messages_per_sender="; Int.to_string s.messages_per_sender;
      "}";
    ]

let sender ~receiver ~sender_id ~messages_per_sender ~seed =
  let rng = Kernel.Random.State.make [| seed; sender_id; 103 |] in
  let rec loop seq =
    if int_gt seq messages_per_sender then
      (send receiver (Harness_sender_done sender_id); Result.Ok ())
    else (
      if int_eq (Kernel.Random.State.int rng 4) 0 then yield ();
      send receiver (Harness_data (sender_id, seq));
      if int_eq (Kernel.Random.State.int rng 3) 0 then yield ();
      loop (Int.succ seq))
  in
  loop 1

let receiver ~parent scenario =
  let expected_total =
    Int.mul scenario.sender_count scenario.messages_per_sender
  in
  let next_by_sender = HashMap.with_capacity scenario.sender_count in
  let rec init sender_id =
    if int_lt sender_id scenario.sender_count then (
      let _ = HashMap.insert next_by_sender sender_id 1 in
      init (Int.succ sender_id))
    else
      ()
  in
  init 0;

  let fail msg =
    send parent
      (Harness_result_error
         (String.concat ""
            [ msg; " scenario="; scenario_to_string scenario ]));
    Result.Error (Failure msg)
  in

  let rec collect ~seen_data ~seen_done =
    if int_eq seen_data expected_total then (
      send parent Harness_result_ok;
      Result.Ok ())
    else if int_eq seen_done scenario.sender_count then
      fail
        (String.concat ""
           [ "all senders finished but receiver only collected ";
             Int.to_string seen_data; "/"; Int.to_string expected_total;
             " messages" ])
    else
      let event =
        receive
          ~selector:(function
            | Harness_data (sender_id, seq) -> `select (`Data (sender_id, seq))
            | Harness_sender_done sender_id -> `select (`Done sender_id)
            | _ -> `skip)
          ()
      in
      match event with
      | `Done sender_id ->
          if int_lt sender_id 0
             || not (int_lt sender_id scenario.sender_count)
          then
            fail
              (String.concat ""
                 [ "received done from invalid sender_id ";
                   Int.to_string sender_id ])
          else
            collect ~seen_data ~seen_done:(Int.succ seen_done)
      | `Data (sender_id, seq) ->
          if int_lt sender_id 0 || not (int_lt sender_id scenario.sender_count) then
            fail
              (String.concat ""
                 [ "received message from invalid sender_id ";
                   Int.to_string sender_id ])
          else
            let expected =
              match HashMap.get next_by_sender sender_id with
              | Some n -> n
              | None -> 1
            in
            if int_eq seq expected then (
              let _ =
                HashMap.insert next_by_sender sender_id (Int.succ expected)
              in
              collect ~seen_data:(Int.succ seen_data) ~seen_done)
            else
              fail
                (String.concat ""
                   [ "out-of-order message for sender ";
                     Int.to_string sender_id; ": expected ";
                     Int.to_string expected; ", got ";
                     Int.to_string seq ])
  in

  try collect ~seen_data:0 ~seen_done:0 with
  | Receive_timeout ->
      fail "receiver timed out while collecting harness data"
  | exn ->
      fail
        (String.concat ""
           [ "receiver raised unexpected exception: ";
             Kernel.Exception.to_string exn ])

let spawn_senders scenario receiver =
  let rec loop sender_id =
    if int_lt sender_id scenario.sender_count then (
      let _ =
        spawn (fun () ->
            sender ~receiver ~sender_id
              ~messages_per_sender:scenario.messages_per_sender
              ~seed:scenario.seed)
      in
      loop (Int.succ sender_id))
    else
      ()
  in
  loop 0

let run_scenario scenario =
  reset_trace_counters ();
  let parent = self () in
  let receiver_pid = spawn (fun () -> receiver ~parent scenario) in
  spawn_senders scenario receiver_pid;
  try
    receive
      ~selector:(function
        | Harness_result_ok -> `select (Result.Ok ())
        | Harness_result_error msg -> `select (Result.Error msg)
        | _ -> `skip)
      ~timeout:12.0 ()
  with
  | Receive_timeout ->
      Result.Error
        (String.concat ""
           [ "harness timed out waiting for receiver result. scenario=";
             scenario_to_string scenario ])
  | exn ->
      Result.Error
        (String.concat ""
           [ "harness raised unexpected exception: ";
             Kernel.Exception.to_string exn; " scenario=";
             scenario_to_string scenario ])

let test_loom_style_concurrency_harness () =
  (* Loom's model checker iterates many bounded schedules and validates
     invariants each time. This test mirrors that style using deterministic
     seeds to generate many interleaving-heavy actor scenarios. *)
  let seed_count = 4 in
  let rec loop seed =
    if int_gt seed seed_count then
      Result.Ok ()
    else
      let scenario = scenario_of_seed seed in
      match run_scenario scenario with
      | Result.Ok () -> loop (Int.succ seed)
      | Result.Error msg ->
          Result.Error
            (String.concat ""
               [ "seed "; Int.to_string seed; " failed: "; msg ])
  in
  loop 1

let tests =
  [
    Test.case
      "loom-style seeded concurrency harness invariants"
      test_loom_style_concurrency_harness;
  ]

let () =
  let name = "miniriot concurrency harness tests" in
  let normalize_args = function
    | [] -> [ name; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args
    ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
