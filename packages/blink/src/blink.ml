open Std
module Connection = Connection
module WebSocket = Websocket
module GRPC = Grpc

type error = Connection.error
type message = Connection.message

let pp_messages fmt msgs =
  Format.fprintf fmt "[";
  List.iteri
    (fun i msg ->
      if i > 0 then Format.fprintf fmt "; ";
      match msg with
      | `Status s ->
          Format.fprintf fmt "`Status(%d %s)" (Net.Http.Status.to_int s)
            (Net.Http.Status.reason_phrase s)
      | `Headers _ -> Format.fprintf fmt "`Headers"
      | `Data d -> Format.fprintf fmt "`Data(%d bytes)" (String.length d)
      | `Done -> Format.fprintf fmt "`Done")
    msgs;
  Format.fprintf fmt "]"

let connect = Connection.connect
let request = Connection.request
let stream = Connection.stream
let messages = Connection.messages
let await = Connection.await
let close = Connection.close
