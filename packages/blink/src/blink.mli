open Std
module Connection : module type of Connection
module WebSocket : module type of Websocket
module GRPC : module type of Grpc

type error = Connection.error
type message = Connection.message

val pp_messages : Format.formatter -> message list -> unit
val connect : Net.Uri.t -> (Connection.t, error) result

val request :
  Connection.t ->
  Net.Http.Request.t ->
  ?body:string ->
  unit ->
  (unit, error) result

val stream : Connection.t -> (message list, error) result

val messages :
  ?on_message:(message list -> unit) ->
  Connection.t ->
  (message list, error) result

val await :
  ?on_message:(message list -> unit) ->
  Connection.t ->
  (Net.Http.Response.t * string, error) result

val close : Connection.t -> unit
