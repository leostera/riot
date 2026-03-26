open Global

type event = ..

type event +=
  | SpanStarted of { started_at : Time.Instant.t; event : event }
  | SpanCompleted of { completed_at : Time.Instant.t; event : event }

type handler_id = string
type handler = { id : handler_id; fn : event -> unit }

open Kernel.Sync
open Kernel.Collections

module Server = struct
  type state = { handlers : (handler_id, handler) HashMap.t }
  type reply_to = Pid.t
  type request_id = int

  type message =
    | AttachHandler of handler
    | DetachHandler of handler_id
    | DetachAll
    | ListHandlers of {
        reply_to : reply_to;
        request_id : request_id;
      }
    | Emit of event
    | Stop of {
        reply_to : reply_to;
        request_id : request_id;
      }

  type Message.t += Telemetry of message
  type Message.t += HandlerList of {
      request_id : request_id;
      ids : string list;
    }
  type Message.t += Stopped of { request_id : request_id }

  let rec loop state =
    let selector msg =
      match msg with Telemetry msg -> `select msg | _ -> `skip
    in
    match receive ~selector () with
    | AttachHandler handler ->
        let _ = HashMap.insert state.handlers handler.id handler in
        loop state
    | DetachHandler handler_id ->
        let _ = HashMap.remove state.handlers handler_id in
        loop state
    | DetachAll ->
        HashMap.clear state.handlers;
        loop state
    | ListHandlers { reply_to; request_id } ->
        let handler_ids = HashMap.keys state.handlers in
        send reply_to (HandlerList { request_id; ids = handler_ids });
        loop state
    | Emit event ->
        HashMap.iter
          (fun _id handler -> try handler.fn event with _ -> ())
          state.handlers;
        loop state
    | Stop { reply_to; request_id } ->
        send reply_to (Stopped { request_id });
        Ok ()

  let init () =
    let state = { handlers = HashMap.create () } in
    loop state

  let start () = spawn init
end

let pid : Pid.t option Cell.t = cell None
let lock = Mutex.create ()
let request_counter = Atomic.make 0

let with_lock f =
  Mutex.lock lock;
  try
    let result = f () in
    Mutex.unlock lock;
    result
  with exn ->
    Mutex.unlock lock;
    raise exn

let read_pid () = with_lock (fun () -> !pid)

let clear_pid_if_matches candidate =
  with_lock (fun () ->
      match !pid with
      | Some current when Pid.equal current candidate -> pid := None
      | _ -> ())

let next_request_id () = Atomic.fetch_and_add request_counter 1 + 1

let start () =
  with_lock (fun () ->
      match !pid with
      | None ->
          let server_pid = Server.start () in
          pid := Some server_pid;
          server_pid
      | Some pid -> pid)

let emit event =
  match read_pid () with
  | None -> ()
  | Some pid -> send pid Server.(Telemetry (Emit event))

let attach id fn =
  match read_pid () with
  | None -> ()
  | Some pid -> send pid Server.(Telemetry (AttachHandler { id; fn }))

let detach id =
  match read_pid () with
  | None -> ()
  | Some pid -> send pid Server.(Telemetry (DetachHandler id))

let detach_all () =
  match read_pid () with
  | None -> ()
  | Some pid -> send pid Server.(Telemetry DetachAll)

let list_handlers () =
  match read_pid () with
  | None -> []
  | Some pid ->
      let request_id = next_request_id () in
      send pid Server.(Telemetry (ListHandlers { reply_to = self (); request_id }));
      let selector msg =
        match msg with
        | Server.HandlerList { request_id = got; ids }
          when Kernel.Int.equal got request_id
          ->
            `select ids
        | _ -> `skip
      in
      (try
         receive ~selector ~timeout:(Time.Duration.from_millis 100) ()
       with Receive_timeout ->
         clear_pid_if_matches pid;
         [])

let stop () =
  let current_pid =
    with_lock (fun () ->
        let current = !pid in
        pid := None;
        current)
  in
  match current_pid with
  | None -> ()
  | Some pid ->
      let request_id = next_request_id () in
      send pid
        Server.(Telemetry (Stop { reply_to = self (); request_id }));
      let selector msg =
        match msg with
        | Server.Stopped { request_id = got }
          when Kernel.Int.equal got request_id ->
            `select ()
        | _ -> `skip
      in
      (try
         receive ~selector ~timeout:(Time.Duration.from_millis 200) ()
       with Receive_timeout -> ())
