(** Suri - Web server framework for OCaml.

    Suri provides high-performance web server components built on Std and
    Miniriot for building HTTP servers, WebSocket servers, and web applications.

    ## Components

    - {!SocketPool} - TCP connection pool with handler abstraction
    - {!WebServer} - HTTP/1.1 server built on SocketPool
    - {!Middleware} - Composable middleware (Router, etc.)
    - {!Channel} - WebSocket communication layer for real-time features
    - {!LiveView} - Server-rendered components with live updates *)

module SocketPool = Socket_pool
(** TCP connection pool with concurrent acceptors and protocol switching *)

module WebServer = Web_server
(** HTTP/1.1 server with request parsing and response handling *)

module Middleware = Middleware
(** Composable middleware framework with routing *)

module Channel = Channel
(** WebSocket handler abstraction for real-time communication *)

module LiveView = Liveview
(** Server-rendered components with live DOM updates over WebSocket *)

module GRPC = Grpc
(** gRPC server implementation over HTTP/2 *)
