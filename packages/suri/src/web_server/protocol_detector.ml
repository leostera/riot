open Std

type error = [ `Detection_error of string ]

let to_string_error = function
  | `Detection_error msg -> Format.sprintf "Protocol detection error: %s" msg

type state = {
  config : Config.t;
  handler : Socket_pool.Connection.t -> Request.t -> Response.t;
  buffer : string Cell.t;
  detected : bool Cell.t;
}

let make_handler ~config ~handler () = {
  config;
  handler;
  buffer = Cell.make "";
  detected = Cell.make false;
}

(** Detect HTTP/2 by checking for connection preface *)
let is_http2 data =
  let preface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" in
  if String.length data >= String.length preface then
    Some (String.sub data 0 (String.length preface) = preface)
  else
    None  (* Need more data *)

(** Detect HTTP/1.1 by checking for HTTP method *)
let is_http1 data =
  let methods = ["GET"; "POST"; "PUT"; "DELETE"; "HEAD"; "OPTIONS"; "PATCH"; "CONNECT"; "TRACE"] in
  List.exists (fun method_ -> String.starts_with ~prefix:method_ data) methods

let handle_connection _conn state =
  Socket_pool.Handler.Continue state

let handle_data data conn state =
  if Cell.get state.detected then
    (* Should never reach here after switch *)
    Socket_pool.Handler.Error (state, `Detection_error "Handler called after protocol detection")
  else begin
    (* Accumulate data *)
    let current = Cell.get state.buffer in
    Cell.set state.buffer (current ^ data);
    let buffer_data = Cell.get state.buffer in

    (* Try to detect protocol *)
    match is_http2 buffer_data with
    | Some true ->
        (* HTTP/2 detected *)
        Cell.set state.detected true;
        let http2_state = Http2_handler.make_handler
          ~config:state.config
          ~handler:state.handler
          ~sniffed_data:buffer_data
          ()
        in
        Socket_pool.Handler.Switch (Socket_pool.Handler.H {
          handler = (module Http2_handler);
          state = http2_state;
        })

    | Some false ->
        (* Looks like HTTP/1.1 *)
        if is_http1 buffer_data then begin
          Cell.set state.detected true;
          let http1_state = Http1_handler.make_handler
            ~config:state.config
            ~handler:state.handler
            ~sniffed_data:buffer_data
            ()
          in
          Socket_pool.Handler.Switch (Socket_pool.Handler.H {
            handler = (module Http1_handler);
            state = http1_state;
          })
        end else
          Socket_pool.Handler.Error (state, `Detection_error "Unknown protocol")

    | None ->
        (* Need more data - wait for more bytes *)
        if String.length buffer_data > 100 then
          (* Too much data without detection *)
          Socket_pool.Handler.Error (state, `Detection_error "Could not detect protocol after 100 bytes")
        else
          Socket_pool.Handler.Continue state
  end

let handle_error _error _conn state =
  Socket_pool.Handler.Close state

let handle_close _conn _state =
  ()

let handle_shutdown _conn state =
  Socket_pool.Handler.Close state

let handle_message _msg _conn state =
  Socket_pool.Handler.Continue state
