open Miniriot
module Result = Std.Result
module Test = Std.Test

let rec int_list_equal a b =
  match (a, b) with
  | [], [] -> true
  | x :: xs, y :: ys ->
      (match Kernel.Int.compare x y with
      | 0 -> int_list_equal xs ys
      | _ -> false)
  | _ -> false

type Message.t += Tick of int

let test () =
  let () =
    Kernel.println "Setting up 5 timers with different delays..."
  in

  let my_pid = self () in

  let _ = Timer.send_after my_pid (Tick 1) ~after:0.05 in
  let _ = Timer.send_after my_pid (Tick 2) ~after:0.10 in
  let _ = Timer.send_after my_pid (Tick 3) ~after:0.15 in
  let _ = Timer.send_after my_pid (Tick 4) ~after:0.20 in
  let _ = Timer.send_after my_pid (Tick 5) ~after:0.25 in

  let rec collect n acc =
    if Kernel.Int.equal n 0 then Kernel.Collections.List.rev acc
    else
      match receive_any () with
      | Tick i ->
          Kernel.println
            (Kernel.String.concat "" [ "  Received Tick "; Kernel.Int.to_string i ]);
          collect (Kernel.Int.sub n 1) (i :: acc)
      | _ -> collect n acc
  in

  let ticks = collect 5 [] in

  if int_list_equal ticks [ 1; 2; 3; 4; 5 ] then
    Result.Ok ()
  else
    Result.Error "timers fired out of order"

let test_case () =
  try test () with
  | exn -> Result.Error (Kernel.Exception.to_string exn)

let () =
  let tests = [ Test.case "multiple timers in order" test_case ] in
  let normalize_args = function
    | [] -> [ "timer_multiple_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  let main ~args =
    match
      Test.Cli.main ~name:"timer_multiple_tests" ~tests ~args:(normalize_args args)
    with
    | Result.Ok () -> Result.Ok ()
    | Result.Error msg -> Result.Error msg
  in
  Miniriot.run ~main ~args:Std.Env.args ()
