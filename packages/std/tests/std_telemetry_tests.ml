open Std
open Std.Collections

type Telemetry.event +=
  TestEvent of { value: int }

type Telemetry.event +=
  AnotherEvent of { name: string }

let test_emit_and_receive = Test.case "telemetry: emit and receive" @@ fun _ctx ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  let received = ref [] in
  Telemetry.attach "test-handler"
    (fun event ->
      match event with
      | TestEvent { value } -> received := value :: !received
      | _ -> ());
  Telemetry.emit (TestEvent { value = 42 });
  Telemetry.emit (TestEvent { value = 99 });
  Telemetry.stop ();
  match !received with
  | [99;42] -> Ok ()
  | _ -> Error ("Expected [99; 42], got " ^ String.concat ", " (List.map string_of_int !received))

let test_multiple_handlers = Test.case "telemetry: multiple handlers" @@ fun _ctx ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  let handler1_called = ref false in
  let handler2_called = ref false in
  Telemetry.attach "handler1"
    (fun event ->
      match event with
      | TestEvent _ -> handler1_called := true
      | _ -> ());
  Telemetry.attach "handler2"
    (fun event ->
      match event with
      | TestEvent _ -> handler2_called := true
      | _ -> ());
  Telemetry.emit (TestEvent { value = 1 });
  Telemetry.stop ();
  if !handler1_called && !handler2_called then
    Ok ()
  else
    Error "Both handlers should be called"

let test_handler_replacement = Test.case "telemetry: handler replacement" @@ fun _ctx ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  let first_called = ref false in
  let second_called = ref false in
  Telemetry.attach "my-handler" (fun _ -> first_called := true);
  Telemetry.attach "my-handler" (fun _ -> second_called := true);
  Telemetry.emit (TestEvent { value = 1 });
  Telemetry.stop ();
  if (not !first_called) && !second_called then
    Ok ()
  else
    Error "Only second handler should be called"

let test_detach = Test.case "telemetry: detach handler" @@ fun _ctx ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  let called = ref false in
  Telemetry.attach "test-handler" (fun _ -> called := true);
  Telemetry.detach "test-handler";
  Telemetry.emit (TestEvent { value = 1 });
  if not !called then
    Ok ()
  else
    Error "Handler should not be called after detach"

let test_pattern_matching = Test.case "telemetry: pattern matching" @@ fun _ctx ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  let test_count = ref 0 in
  let another_count = ref 0 in
  Telemetry.attach "counter"
    (fun event ->
      match event with
      | TestEvent _ -> test_count := !test_count + 1
      | AnotherEvent _ -> another_count := !another_count + 1
      | _ -> ());
  Telemetry.emit (TestEvent { value = 1 });
  Telemetry.emit (TestEvent { value = 2 });
  Telemetry.emit (AnotherEvent { name = "test" });
  Telemetry.stop ();
  if !test_count = 2 && !another_count = 1 then
    Ok ()
  else
    Error ("Expected test=2, another=1, got test="
    ^ string_of_int !test_count
    ^ ", another="
    ^ string_of_int !another_count)

let test_handler_exception_isolation = Test.case "telemetry: exception isolation" @@ fun _ctx ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  let good_handler_called = ref false in
  Telemetry.attach "bad-handler" (fun _ -> panic "boom");
  Telemetry.attach "good-handler" (fun _ -> good_handler_called := true);
  Telemetry.emit (TestEvent { value = 1 });
  Telemetry.stop ();
  if !good_handler_called then
    Ok ()
  else
    Error "Good handler should still be called despite bad handler exception"

let test_restart_after_stop = Test.case "telemetry: restart after stop" @@ fun _ctx ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  let first_called = ref 0 in
  Telemetry.attach "first" (fun _ -> first_called := !first_called + 1);
  Telemetry.emit (TestEvent { value = 1 });
  Telemetry.stop ();
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  let second_called = ref 0 in
  Telemetry.attach "second" (fun _ -> second_called := !second_called + 1);
  Telemetry.emit (TestEvent { value = 2 });
  Telemetry.stop ();
  if !first_called = 1 && !second_called = 1 then
    Ok ()
  else
    Error ("Expected restart to isolate handler calls; first="
    ^ string_of_int !first_called
    ^ ", second="
    ^ string_of_int !second_called)

let test_stop_idempotent = Test.case "telemetry: stop idempotent and clears handlers view"
@@ fun _ctx ->
  let _pid = Telemetry.start () in
  Telemetry.detach_all ();
  Telemetry.attach "tmp" (fun _ -> ());
  Telemetry.stop ();
  Telemetry.stop ();
  let handlers = Telemetry.list_handlers () in
  match handlers with
  | [] -> Ok ()
  | _ -> Error ("Expected [] handlers after stop, got " ^ String.concat ", " handlers)

let name = "Telemetry"

let tests = [
  test_emit_and_receive;
  test_multiple_handlers;
  test_handler_replacement;
  test_detach;
  test_pattern_matching;
  test_handler_exception_isolation;
  test_restart_after_stop;
  test_stop_idempotent;
]

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
