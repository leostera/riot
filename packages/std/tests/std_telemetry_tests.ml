open Std
open Std.Collections
open Std.Result.Syntax

module Duration = Time.Duration

type Telemetry.event +=
  | TestEvent of { value: int }

type Telemetry.event +=
  | AnotherEvent of { name: string }

let test_emit_and_receive =
  Test.case "telemetry: emit and receive" @@ fun _ctx ->
    let _pid = Telemetry.start () in
    Telemetry.detach_all ();
    let received = ref [] in
    Telemetry.attach
      "test-handler"
      (fun event ->
        match event with
        | TestEvent { value } -> received := value :: !received
        | _ -> ());
    Telemetry.emit (TestEvent { value = 42 });
    Telemetry.emit (TestEvent { value = 99 });
    Telemetry.stop ();
    match !received with
    | [ 99; 42 ] -> Ok ()
    | _ ->
        Error ("Expected [99; 42], got " ^ String.concat ", " (List.map !received ~fn:Int.to_string))

let test_multiple_handlers =
  Test.case "telemetry: multiple handlers" @@ fun _ctx ->
    let _pid = Telemetry.start () in
    Telemetry.detach_all ();
    let handler1_called = ref false in
    let handler2_called = ref false in
    Telemetry.attach
      "handler1"
      (fun event ->
        match event with
        | TestEvent _ -> handler1_called := true
        | _ -> ());
    Telemetry.attach
      "handler2"
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

let test_handler_replacement =
  Test.case "telemetry: handler replacement" @@ fun _ctx ->
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

let test_detach =
  Test.case "telemetry: detach handler" @@ fun _ctx ->
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

let test_pattern_matching =
  Test.case "telemetry: pattern matching" @@ fun _ctx ->
    let _pid = Telemetry.start () in
    Telemetry.detach_all ();
    let test_count = ref 0 in
    let another_count = ref 0 in
    Telemetry.attach
      "counter"
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
      ^ Int.to_string !test_count
      ^ ", another="
      ^ Int.to_string !another_count)

let test_handler_exception_isolation =
  Test.case "telemetry: exception isolation" @@ fun _ctx ->
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

let test_restart_after_stop =
  Test.case "telemetry: restart after stop" @@ fun _ctx ->
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
      ^ Int.to_string !first_called
      ^ ", second="
      ^ Int.to_string !second_called)

let test_stop_idempotent =
  Test.case "telemetry: stop idempotent and clears handlers view" @@ fun _ctx ->
    let _pid = Telemetry.start () in
    Telemetry.detach_all ();
    Telemetry.attach "tmp" (fun _ -> ());
    Telemetry.stop ();
    Telemetry.stop ();
    let handlers = Telemetry.list_handlers () in
    match handlers with
    | [] -> Ok ()
    | _ -> Error ("Expected [] handlers after stop, got " ^ String.concat ", " handlers)

let test_span_start_and_finish =
  Test.case "telemetry span: start and finish emits lifecycle events" @@ fun _ctx ->
    let _pid = Telemetry.start () in
    Telemetry.detach_all ();
    let events = ref [] in
    Telemetry.attach
      "span-handler"
      (fun event ->
        match event with
        | Telemetry.SpanEvent event -> events := event :: !events
        | _ -> ());
    let parent = Telemetry.Span.start "parent" in
    let child =
      Telemetry.Span.start
        ~span:parent
        ~attributes:[ ("component", Data.Json.String "std-test") ]
        "child"
    in
    Telemetry.Span.finish child;
    Telemetry.Span.finish parent;
    Telemetry.stop ();
    match List.reverse !events with
    | [
        Telemetry.Span.Started started_parent;
        Telemetry.Span.Started started_child;
        Telemetry.Span.Completed { span = completed_child; duration; status; _ };
        Telemetry.Span.Completed { span = completed_parent; _ };
      ] ->
        let* () =
          if
            Telemetry.Span.equal_id (Telemetry.Span.id parent) (Telemetry.Span.id started_parent)
          then
            Ok ()
          else
            Error "parent span id did not match started event"
        in
        let* () =
          match UUID.version (Telemetry.Span.id parent) with
          | Some 7 -> Ok ()
          | Some version ->
              Error ("expected parent span id to be UUIDv7, got UUIDv" ^ Int.to_string version)
          | None -> Error "expected parent span id to have UUID version 7"
        in
        let* () =
          match UUID.version (Telemetry.Span.id child) with
          | Some 7 -> Ok ()
          | Some version ->
              Error ("expected child span id to be UUIDv7, got UUIDv" ^ Int.to_string version)
          | None -> Error "expected child span id to have UUID version 7"
        in
        let* () =
          if
            Telemetry.Span.equal_id (Telemetry.Span.id child) (Telemetry.Span.id started_child)
          then
            Ok ()
          else
            Error "child span id did not match started event"
        in
        let* () =
          match Telemetry.Span.parent_id started_child with
          | Some parent_id when Telemetry.Span.equal_id parent_id (Telemetry.Span.id parent) ->
              Ok ()
          | _ -> Error "child span did not carry parent id"
        in
        let* () =
          match Telemetry.Span.attributes started_child with
          | [ ("component", Data.Json.String "std-test") ] -> Ok ()
          | _ -> Error "child span did not carry attributes"
        in
        let* () =
          if String.equal (Telemetry.Span.name started_child) "child" then
            Ok ()
          else
            Error "child span did not carry name"
        in
        let* () =
          if
            Telemetry.Span.equal_id (Telemetry.Span.id child) (Telemetry.Span.id completed_child)
          then
            Ok ()
          else
            Error "child completed event did not carry child span"
        in
        let* () =
          if
            Telemetry.Span.equal_id (Telemetry.Span.id parent) (Telemetry.Span.id completed_parent)
          then
            Ok ()
          else
            Error "parent completed event did not carry parent span"
        in
        let* () =
          if Duration.compare duration Duration.zero != Order.LT then
            Ok ()
          else
            Error "span completion duration should be non-negative"
        in
        (
          match status with
          | Telemetry.Span.Succeeded -> Ok ()
          | Telemetry.Span.Failed exn ->
              Error ("expected span success, got failure: " ^ Exception.to_string exn)
        )
    | _ -> Error "expected parent and child span lifecycle events"

let test_span_with_span_records_failure =
  Test.case "telemetry: with_span records failure before reraising" @@ fun _ctx ->
    let _pid = Telemetry.start () in
    Telemetry.detach_all ();
    let events = ref [] in
    Telemetry.attach
      "span-handler"
      (fun event ->
        match event with
        | Telemetry.SpanEvent event -> events := event :: !events
        | _ -> ());
    let raised =
      try
        let _ = Telemetry.with_span "failing" (fun _span -> raise (Failure "boom")) in
        false
      with
      | Failure message when String.equal message "boom" -> true
      | _ -> false
    in
    Telemetry.stop ();
    if not raised then
      Error "expected with_span to re-raise the original exception"
    else
      match List.reverse !events with
      | [
          Telemetry.Span.Started started;
          Telemetry.Span.Completed { span = completed; status = Telemetry.Span.Failed exn; _ };
        ] ->
          if
            not (Telemetry.Span.equal_id (Telemetry.Span.id started) (Telemetry.Span.id completed))
          then
            Error "failed span completion did not carry the started span"
          else if String.contains (Exception.to_string exn) "boom" then
            Ok ()
          else
            Error ("expected failure exception to mention boom, got: " ^ Exception.to_string exn)
      | _ -> Error "expected failing span start and completed events"

let test_with_span_links_explicit_parent =
  Test.case "telemetry: with_span links explicit parent span" @@ fun _ctx ->
    let _pid = Telemetry.start () in
    Telemetry.detach_all ();
    let events = ref [] in
    Telemetry.attach
      "span-handler"
      (fun event ->
        match event with
        | Telemetry.SpanEvent event -> events := event :: !events
        | _ -> ());
    let parent_name = ref "" in
    let child_name = ref "" in
    let () =
      Telemetry.with_span "parent" @@ fun parent ->
        parent_name := Telemetry.Span.name parent;
        Telemetry.with_span ~span:parent "child" @@ fun child ->
          child_name := Telemetry.Span.name child
    in
    Telemetry.stop ();
    let rec find_started name = fun __tmp1 ->
      match __tmp1 with
      | [] -> None
      | Telemetry.Span.Started span :: _ when String.equal (Telemetry.Span.name span) name ->
          Some span
      | _ :: rest -> find_started name rest
    in
    let events = List.reverse !events in
    match (find_started "parent" events, find_started "child" events) with
    | (Some parent, Some child) ->
        let* () =
          if String.equal !parent_name "parent" && String.equal !child_name "child" then
            Ok ()
          else
            Error "with_span did not pass active spans to callbacks"
        in
        (
          match Telemetry.Span.parent_id child with
          | Some parent_id when Telemetry.Span.equal_id parent_id (Telemetry.Span.id parent) ->
              Ok ()
          | _ -> Error "child span was not linked to explicit parent span"
        )
    | _ -> Error "expected parent and child started events"

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
  test_span_start_and_finish;
  test_span_with_span_records_failure;
  test_with_span_links_explicit_parent;
]

let main ~args = Test.Cli.main ~execution_mode:Test.Cli.Linear ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
