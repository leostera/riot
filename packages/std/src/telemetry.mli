(** Telemetry - Process-based dynamic event dispatching system

    Inspired by https://hexdocs.pm/telemetry/readme.html

    Start the telemetry server:
    {[
      let _ = Telemetry.start ()
    ]}

    Create events in your library:
    {[
      type Telemetry.event += BuildStarted of { package : string }

      Telemetry.emit (BuildStarted { package = "my-pkg" })
    ]}

    Attach handlers to consume events:
    {[
      Telemetry.attach "my-handler" (fun event ->
          match event with
          | BuildStarted { package } -> Log.info "Building %s" package
          | _ -> ())
    ]} *)

type event = ..
(** Extensible variant type for telemetry events. Any module can extend this
    with their own events. *)

val start : unit -> Miniriot.Pid.t
(** Start the telemetry server process. Returns the server's PID. Must be called
    before emitting events. Idempotent: returns the existing server PID when the
    server is already running. *)

val emit : event -> unit
(** Emit a telemetry event. All attached handlers will receive it. *)

val attach : string -> (event -> unit) -> unit
(** Attach a named handler for telemetry events. The handler will be called for
    every emitted event. If a handler with the same name already exists, it will
    be replaced. *)

val detach : string -> unit
(** Detach a handler by name. *)

val detach_all : unit -> unit
(** Detach all handlers. Useful for testing. *)

val list_handlers : unit -> string list
(** List all attached handler names. Returns [[]] when telemetry is not running
    or when the current server reference is stale. *)

val stop : unit -> unit
(** Stop the telemetry server. Blocks until all pending events are processed.
    This is useful in tests to ensure all events have been handled before making
    assertions. Safe to call multiple times. *)
