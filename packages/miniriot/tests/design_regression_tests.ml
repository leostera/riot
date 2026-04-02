open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test
module String = Kernel.String

type Message.t +=
  | Probe_ready
  | Probe_start
  | Probe_waiting
  | Probe_noise
  | Probe_expected
  | Probe_timed_out
  | Probe_matched_expected
  | Interval_tick

let selective_receive_timeout_probe = fun parent ->
  send parent Probe_ready;
  let () =
    receive
      ~selector:(
        function
        | Probe_start -> `select ()
        | _ -> `skip
      )
      ~timeout:5.0
      ()
  in
  let outcome =
    try
      let () =
        receive
          ~selector:(
            function
            | Probe_expected -> `select ()
            | _ -> `skip
          )
          ~timeout:0.02
          ()
      in
      `matched_expected
    with
    | Receive_timeout -> `timed_out
  in
  send parent
    (
      match outcome with
      | `timed_out -> Probe_timed_out
      | `matched_expected -> Probe_matched_expected
    );
  Result.Ok ()

let selective_receive_timeout_rearm_probe = fun parent ->
  send parent Probe_ready;
  let () =
    receive
      ~selector:(
        function
        | Probe_start -> `select ()
        | _ -> `skip
      )
      ~timeout:5.0
      ()
  in
  send parent Probe_waiting;
  let outcome =
    try
      let () =
        receive
          ~selector:(
            function
            | Probe_expected -> `select ()
            | _ -> `skip
          )
          ~timeout:0.03
          ()
      in
      `matched_expected
    with
    | Receive_timeout -> `timed_out
  in
  send parent
    (
      match outcome with
      | `timed_out -> Probe_timed_out
      | `matched_expected -> Probe_matched_expected
    );
  Result.Ok ()

let await_probe_ready = fun () ->
  receive
    ~selector:(
      function
      | Probe_ready -> `select ()
      | _ -> `skip
    )
    ~timeout:5.0
    ()

let test_selective_receive_timeout_ignores_unmatched_saved_messages = fun () ->
  let parent = self () in
  let worker =
    spawn (fun () -> selective_receive_timeout_probe parent)
  in
  await_probe_ready ();
  (* The first receive on the worker consumes [Probe_start] and skips
     [Probe_noise] into the save queue. The second receive then waits for
     [Probe_expected] with a short timeout while the save queue is non-empty. *)
  send worker Probe_noise;
  send worker Probe_start;
  let _ = Timer.send_after worker Probe_expected ~after:0.10 in
  match
    receive
      ~selector:(
        function
        | Probe_timed_out -> `select (Result.Ok ())
        | Probe_matched_expected ->
            `select (
              Result.Error "receive matched a delayed message instead of timing out \
                  while unmatched saved messages were present"
            )
        | _ -> `skip
      )
      ~timeout:1.0
      ()
  with
  | Result.Ok () -> Result.Ok ()
  | Result.Error _ as err -> err

let test_selective_receive_timeout_does_not_rearm_after_unmatched_wakeup = fun () ->
  let parent = self () in
  let worker =
    spawn (fun () -> selective_receive_timeout_rearm_probe parent)
  in
  await_probe_ready ();
  send worker Probe_start;
  let () =
    receive
      ~selector:(
        function
        | Probe_waiting -> `select ()
        | _ -> `skip
      )
      ~timeout:5.0
      ()
  in
  let _ = Timer.send_after worker Probe_noise ~after:0.01 in
  let _ = Timer.send_after worker Probe_expected ~after:0.12 in
  match
    receive
      ~selector:(
        function
        | Probe_timed_out -> `select (Result.Ok ())
        | Probe_matched_expected ->
            `select (
              Result.Error "receive rearmed its timeout after an unmatched wakeup \
                  instead of honoring the original deadline"
            )
        | _ -> `skip
      )
      ~timeout:1.0
      ()
  with
  | Result.Ok () -> Result.Ok ()
  | Result.Error _ as err -> err

let test_interval_cancel_stops_future_ticks = fun () ->
  let me = self () in
  let timer_id = Timer.send_interval me Interval_tick ~interval:0.05 in
  let () =
    receive
      ~selector:(
        function
        | Interval_tick -> `select ()
        | _ -> `skip
      )
      ~timeout:1.0
      ()
  in
  Timer.cancel timer_id;
  let extra_tick =
    try
      Some (
        receive
          ~selector:(
            function
            | Interval_tick -> `select ()
            | _ -> `skip
          )
          ~timeout:0.15
          ()
      )
    with
    | Receive_timeout -> None
  in
  match extra_tick with
  | None -> Result.Ok ()
  | Some () -> Result.Error "cancelled interval timer delivered a later tick"

let test_case = fun name fn ->
  try fn () with
  | Receive_timeout -> Result.Error (String.concat "" [ "timed out in "; name ])
  | exn -> Result.Error (String.concat
    ""
    [ "unexpected exception in "; name; ": "; Kernel.Exception.to_string exn ])

let () =
  let tests = [
    Test.case
      "selective receive timeout ignores unmatched saved messages"
      (fun _ctx -> test_case "selective receive timeout ignores unmatched saved messages" test_selective_receive_timeout_ignores_unmatched_saved_messages);
    Test.case
      "selective receive timeout does not rearm after unmatched wakeup"
      (fun _ctx -> test_case "selective receive timeout does not rearm after unmatched wakeup" test_selective_receive_timeout_does_not_rearm_after_unmatched_wakeup);
    Test.case
      "interval cancel stops future ticks"
      (fun _ctx -> test_case "interval cancel stops future ticks" test_interval_cancel_stops_future_ticks);
  ] in
  let normalize_args = function
    | [] -> [ "design_regression_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"design_regression_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ~config:(Miniriot.Config.make ~scheduler_count:4 ()) ()
