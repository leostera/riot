open Riot

[@@@warning "-8"]
[@@@warning "-38"]

type Riot.Message.t += Loop_stop

let rec loop count =
  match receive () with
  | Loop_stop ->
      Logger.debug (fun f -> f "dead at %d%!" count);
      ()
  | _ ->
      Logger.debug (fun f -> f "count=%d%!" count);
      loop (count + 1)

let main t0 () =
  let (Ok ()) = Logger.start ~print_source:true () in

  let pids =
    List.init 100 (fun _i ->
        spawn (fun () -> loop 0))
  in

  List.iter (fun pid -> send pid Loop_stop) pids;

  wait_pids ~cb:(fun pids -> 
    Logger.info (fun f -> f "awaiting for %d pids" (List.length pids));
    List.iter (fun pid -> Logger.info( fun f -> f "Process %a" Process.pp (get_proc pid))) pids
  ) pids;

  let t1 = Ptime_clock.now () in
  Logger.info (fun f ->
      let delta = Ptime.diff t1 t0 in
      let delta = Ptime.Span.to_float_s delta in
      f "spawned/awaited %d processes in %.3fs" (List.length pids) delta);
  sleep 100.001;
  shutdown ()

let () =
  let t0 = Ptime_clock.now () in
  Logger.set_log_level (Some Info);
  Riot.run @@ main t0
