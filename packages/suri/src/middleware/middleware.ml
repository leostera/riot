module Conn = Conn
module Pipeline = Pipeline
module Router = Router
module Logger = Logger

(** Convenience alias for Router.middleware *)
module Request_id = Request_id
module Debugger = Debugger

let router = Router.middleware

(** Convenience alias for Logger.logger *)
let logger = Logger.logger

(** Convenience alias for Request_id.request_id *)
let request_id = Request_id.request_id

(** Convenience alias for Debugger.debugger *)
let debugger = Debugger.debugger

(** Convenience alias for Cors.middleware *)
module Cors = Cors

let cors = Cors.middleware

(** Convenience alias for Session.middleware *)
module Session = Session

let session = Session.middleware

(** Convenience alias for Csrf.middleware *)
module Csrf = Csrf

let csrf = Csrf.middleware

(** Convenience alias for Body_parser.make *)
module Body_parser = Body_parser

let body_parser = Body_parser.make

(** Convenience alias for Static.middleware *)
module Static = Static

let static = Static.middleware

(** Convenience alias for Basic_auth.middleware *)
module Basic_auth = Basic_auth

let basic_auth = Basic_auth.middleware

(** Convenience alias for Basic_auth.middleware_with_validation *)
let basic_auth_with_validation = Basic_auth.middleware_with_validation

(** Convenience alias for Accepts.middleware *)
module Accepts = Accepts

let accepts = Accepts.middleware

(** Convenience alias for Head.middleware *)
module Head = Head

let head = Head.middleware

(** Convenience alias for Runner.middleware *)
module Runner = Runner

(** Convenience alias for Request_runtime.middleware *)
module Runtime = Request_runtime

let runner = Runner.middleware

(** Convenience alias for Method_override.middleware with default param *)
module Method_override = Method_override

let method_override = fun ~conn ~next -> Method_override.middleware () ~conn ~next

module Remote_ip = Remote_ip

(* No convenience alias - Remote_ip.middleware requires ~proxies parameter *)

(** Convenience alias for Etag.middleware with default param *)
module Etag = Etag

let etag = fun ~conn ~next -> Etag.middleware () ~conn ~next

(** Convenience alias for Conditional_get.middleware *)
module Conditional_get = Conditional_get

let conditional_get = Conditional_get.middleware
