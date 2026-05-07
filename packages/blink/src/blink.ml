open Std

module Transport = Transport
module Protocol = Protocol
module WebSocket = Websocket
module Connection = Connection
module Error = Error
module SSE = Sse
module Client = Client
module RetryPolicy = Client.RetryPolicy

type error = Error.t

type message = Connection.message

let connect = Transport.connect

let request = Connection.request

let stream = Connection.stream

let messages = Connection.messages

let await = Connection.await

let close = Connection.close
