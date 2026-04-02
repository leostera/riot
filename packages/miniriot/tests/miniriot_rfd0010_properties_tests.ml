open Miniriot
open Propane
open Kernel.Ops
module Test = Std.Test
module Result = Std.Result

let assert_ok = fun name check ->
  match Property.check check with
  | Property.Success -> Result.Ok ()
  | Property.Failure { counter_example; shrink_steps } -> Result.Error (Kernel.String.concat
    ""
    [
      name;
      " failed\nCounter-example: ";
      counter_example;
      "\nShrink steps: ";
      Kernel.Int.to_string shrink_steps;
    ])
  | Property.Error { exception_; backtrace } -> Result.Error (Kernel.String.concat
    ""
    [ name; " raised exception:\n"; Kernel.Exception.to_string exception_; "\n"; backtrace ])
  | Property.Assumption_violated -> Result.Error (Kernel.String.concat
    ""
    [ name; " had no applicable test cases" ])

let strictly_increasing_pids = fun pids ->
  let rec loop prev current =
    match current with
    | [] -> true
    | head :: tail -> (
        match Pid.compare prev head with
        | -1 -> loop head tail
        | _ -> false
      )
  in
  match pids with
  | []
  | [ _ ] -> true
  | head :: tail -> loop head tail

let pid_uids_increase =
  Property.for_all Arbitrary.int
    (fun raw_n ->
      let n = Kernel.Int.add (Kernel.Int.rem (Kernel.Int.abs raw_n) 32) 1 in
      let pids =
        Kernel.Collections.List.init n (fun _ -> Pid.next ())
      in
      strictly_increasing_pids pids)

let scheduler_count_clamped =
  Property.for_all Arbitrary.int
    (fun n ->
      let cfg = Miniriot.Config.make ~scheduler_count:n () in
      Kernel.Int.equal cfg.scheduler_count (Kernel.Int.max 1 n))

let config_worker_count_matches_scheduler_count =
  Property.for_all Arbitrary.int
    (fun requested ->
      let requested = Kernel.Int.max 1 requested in
      let cfg = Miniriot.Config.make ~scheduler_count:requested () in
      Kernel.Int.equal (Miniriot.Config.worker_count cfg) requested)

let test_pid_monotonicity = fun _ctx -> assert_ok "pid monotonicity" pid_uids_increase

let test_scheduler_count_clamped = fun _ctx -> assert_ok "scheduler_count clamping" scheduler_count_clamped

let test_config_worker_count = fun _ctx -> assert_ok "worker_count accessor mirrors scheduler_count" config_worker_count_matches_scheduler_count

let default_scheduler_count_matches_config =
  Property.for_all Arbitrary.bool
    (fun _ ->
      let default_cfg = Miniriot.Config.default in
      Kernel.Int.equal default_cfg.scheduler_count Miniriot.Config.default_scheduler_count && (
        match Kernel.Int.compare default_cfg.scheduler_count 1 with
        | -1 -> false
        | _ -> true
      ))

let test_default_scheduler_count = fun _ctx -> assert_ok "default scheduler count is exported" default_scheduler_count_matches_config

let scheduler_id_roundtrip =
  Property.for_all Arbitrary.int
    (fun raw ->
      let normalized = Kernel.Int.abs raw mod 1_024 in
      let id = Miniriot.Scheduler_id.of_int normalized in
      Kernel.Int.equal (Miniriot.Scheduler_id.to_int id) normalized)

let test_scheduler_id_roundtrip = fun _ctx -> assert_ok "scheduler id roundtrip" scheduler_id_roundtrip

let tests = [
  Test.property "pid monotonicity" ~examples:128 test_pid_monotonicity;
  Test.property "scheduler_count clamping" ~examples:128 test_scheduler_count_clamped;
  Test.property "scheduler_count API" ~examples:128 test_config_worker_count;
  Test.property "default scheduler config" ~examples:16 test_default_scheduler_count;
  Test.property "scheduler_id roundtrip" ~examples:128 test_scheduler_id_roundtrip;
]

let () =
  let name = "RFD0010 property tests" in
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
  Miniriot.run ~main ~args:Std.Env.args ()
