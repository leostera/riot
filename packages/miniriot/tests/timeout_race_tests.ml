open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test
module Int = Kernel.Int
module String = Kernel.String

type Message.t +=
  | Round_ready of int
  | Round_ping of int
  | Timeout_done
  | Timeout_failed of string

let int_eq = fun a b ->
  match Int.compare a b with
  | 0 -> true
  | _ -> false

let timed_receiver = fun parent ~rounds ->
  let rec loop round =
    if int_eq round rounds then
      (
        send parent Timeout_done;
        Result.Ok ()
      )
    else (
      send parent (Round_ready round);
      let maybe_ping =
        try
          Some (
            receive
              ~selector:(
                function
                | Round_ping idx -> `select idx
                | _ -> `skip
              )
              ~timeout:0.05
              ()
          )
        with
        | Receive_timeout -> None
      in
      match maybe_ping with
      | Some idx when int_eq idx round ->
          loop (Int.succ round)
      | Some idx ->
          let msg = String.concat
            ""
            [ "round mismatch: expected "; Int.to_string round; ", got "; Int.to_string idx ] in
          send parent (Timeout_failed msg);
          Result.Error (Failure msg)
      | None ->
          let msg = String.concat "" [ "receive timed out at round "; Int.to_string round ] in
          send parent (Timeout_failed msg);
          Result.Error (Failure msg)
    )
  in
  loop 0

let await_no_extra_round_ready = fun () ->
  let extra =
    try
      Some (
        receive
          ~selector:(
            function
            | Round_ready round -> `select round
            | _ -> `skip
          )
          ~timeout:0.1
          ()
      )
    with
    | Receive_timeout -> None
  in
  match extra with
  | None -> Result.Ok ()
  | Some round -> Result.Error (String.concat
    ""
    [ "unexpected extra round readiness after completion: "; Int.to_string round ])

let test_receive_timeout_cancel_race = fun () ->
  let rounds = 96 in
  let parent = self () in
  let worker =
    spawn (fun () -> timed_receiver parent ~rounds)
  in
  let rec drive expected_round =
    let event =
      receive
        ~selector:(
          function
          | Round_ready round -> `select (`ready round)
          | Timeout_done -> `select `finished
          | Timeout_failed msg -> `select (`failed msg)
          | _ -> `skip
        )
        ~timeout:20.0
        ()
    in
    match event with
    | `ready round ->
        if int_eq round expected_round then
          (
            let _timer_id = Timer.send_after worker (Round_ping round) ~after:0.001 in
            drive (Int.succ expected_round)
          )
        else
          Result.Error (String.concat
            ""
            [
              "unexpected round readiness: expected ";
              Int.to_string expected_round;
              ", got ";
              Int.to_string round
            ])
    | `finished ->
        if int_eq expected_round rounds then
          await_no_extra_round_ready ()
        else
          Result.Error (String.concat
            ""
            [ "worker finished early at round "; Int.to_string expected_round ])
    | `failed msg ->
        Result.Error msg
  in
  drive 0

let test_case = fun name fn ->
  try fn () with
  | Receive_timeout -> Result.Error (String.concat "" [ "timed out in "; name ])
  | exn -> Result.Error (String.concat
    ""
    [ "unexpected exception in "; name; ": "; Kernel.Exception.to_string exn ])

let () =
  let tests = [
    Test.case
      "receive timeout cancellation race"
      (fun _ctx -> test_case "receive timeout cancellation race" test_receive_timeout_cancel_race);
  ] in
  let normalize_args = function
    | [] -> [ "timeout_race_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"timeout_race_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
