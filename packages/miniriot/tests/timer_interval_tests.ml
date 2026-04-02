open Miniriot
open Miniriot.Exception
module Result = Std.Result
module Test = Std.Test

type Message.t +=
  Tick
  | Stop

let int_lt = fun a b ->
  match Kernel.Int.compare a b with
  | -1 -> true
  | _ -> false

let int_ge = fun a b ->
  match Kernel.Int.compare a b with
  | -1 -> false
  | _ -> true

let test = fun () ->
  let my_pid = self () in
  let timer_id = Timer.send_interval my_pid Tick ~interval:0.05 in
  let _ = Timer.send_after my_pid Stop ~after:0.3 in
  let rec loop count =
    match receive_any () with
    | Tick ->
        loop (Kernel.Int.succ count)
    | Stop ->
        Timer.cancel timer_id;
        count
    | _ ->
        loop count
  in
  let tick_count = loop 0 in
  Timer.cancel timer_id;
  if int_lt tick_count 4 then
    Result.Error (Kernel.String.concat
      ""
      [ "Unexpected tick count: "; Kernel.Int.to_string tick_count; " (expected ~6)" ])
  else if int_ge tick_count 9 then
    Result.Error (Kernel.String.concat
      ""
      [ "Unexpected tick count: "; Kernel.Int.to_string tick_count; " (expected ~6)" ])
  else
    Result.Ok ()

let test_case = fun _ctx ->
  try test () with
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [ Test.case "timer interval tests" test_case ] in
  let normalize_args = function
    | [] -> [ "timer_interval_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match Test.Cli.main ~name:"timer_interval_tests" ~tests ~args:(normalize_args args) with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ()
