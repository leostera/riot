# Actors

Multicore actors for Riot.

`actors` is Riot's compatibility facade for the actor runtime now owned by
`Std.Runtime`.

The implementation lives in `std`. This package remains so existing code that
depends on `Actors.*` can keep building while the repo migrates toward a
two-package core of `kernel` and `std`.

## Install

```sh
riot add actors
```

## What value it gives you

Everything built on Riot gets these properties because it runs on `actors`:

- Erlang-style processes and mailboxes without pulling in the whole `std`
  surface;
- parallel execution across multiple worker schedulers instead of a single-core
  event loop;
- failure-aware process relationships through links and monitors;
- timer-based wakeups and async syscall integration inside the same actor
  runtime;
- a small runtime package you can build other Riot infrastructure on top of.

For new code, prefer `std` and call the runtime through `Std.Runtime`.

Keep a direct dependency on `actors` only when you are intentionally preserving
older `Actors.*` call sites during migration.

## Quick start

```ocaml
open Std
open Actors

type Message.t +=
  | Ping of Pid.t
  | Pong

let worker = fun () ->
  let sender =
    receive ~selector:(function
      | Ping sender -> `select sender
      | _ -> `skip)
      ()
  in
  send sender Pong;
  Ok ()

let main = fun ~args:_ ->
  let pid = spawn worker in
  send pid (Ping (self ()));
  receive
    ~selector:(function
      | Pong -> `select ()
      | _ -> `skip)
    ();
  Ok ()

let () = Std.Runtime.run ~main ~args:Env.args ()
```

The package also ships a runnable example:

```sh
riot run -p actors ping_pong
```

## The APIs you will actually use

The most important entry points are:

- `run` to start the runtime;
- `spawn` and `spawn_link` to create processes;
- `send`, `receive`, and `receive_any` for mailbox-driven workflows;
- `self` to get the current PID;
- `yield` to spend cooperative reductions explicitly when long-running work
  should give other actors a chance to run;
- `Process.monitor`, `Process.link`, `Process.unlink`, and `Process.demonitor`
  for process lifecycle coordination;
- `Timer.send_after` and `Timer.send_interval` for delayed or repeating
  messages;
- `syscall` for async source integration through the runtime's reactor.

Most day-to-day actor code stays very small:

1. define message constructors on `Message.t`
2. `spawn` a process loop
3. `send` messages to it
4. `receive` the messages you care about

## How it works

At a high level, `actors` runs one normal scheduler worker per configured slot
plus a dedicated reactor domain for timers and async I/O.

The important pieces are:

- **lightweight processes**: spawned actors are green-thread processes with
  their own mailbox and continuation state rather than OS threads;
- **effects for suspension**: `receive`, `yield`, and actor-aware syscalls
  suspend and resume work through OCaml 5 effects, which keeps actor code
  looking direct instead of callback-driven;
- **worker-local runnable queues**: each scheduler owns a local runnable queue
  for the actors it is currently driving;
- **work stealing**: when a worker runs out of local work, it can steal runnable
  actors from busier workers so execution spreads across cores;
- **reactor-driven wakeups**: actors waiting on timers or async sources are
  re-enqueued when those operations become ready.

So the mental model is:

1. `spawn` creates an actor and places it on a worker queue
2. the actor runs until it finishes, yields, blocks on `receive`, or waits on
   async I/O
3. blocked actors sleep without consuming a worker
4. runnable actors can move between workers when the runtime steals work

## Runtime model

You do not need to manually assign actors to cores.

The runtime starts one normal worker scheduler per configured slot plus a
dedicated reactor domain for timers and async I/O. Runnable actors are placed
onto worker-local queues, and idle workers can steal runnable actors from busy
workers. That means a burst of independent actors can spread across multiple
cores without you having to shard work yourself.

The important semantic guarantees remain actor-centric:

- PIDs are runtime-wide.
- `send` targets a PID, not a scheduler.
- messages sent from one sender to one recipient are observed in send order.
- blocked actors resume when their mailbox, timer, or async source makes them
  runnable again.

The runtime is still cooperative at the process level. A process doing a large
amount of CPU work should either block in meaningful runtime operations or call
`yield` periodically so it shares execution fairly with other runnable actors.

## Configuration and observability

The runtime defaults to Riot's normal scheduler sizing, but you can pass an
explicit `Actors.Config.t` to `run` when you want to size the worker pool or
change timer resolution.

For debugging and tuning:

- `enable_trace` / `disable_trace` turn tracing on and off;
- `trace_counters` exposes scheduler counters such as steals, failed steals,
  remote wakeups, and duplicate enqueue races;
- `reset_trace_counters` clears those counters between runs.

This is most useful when you are working on infrastructure code and want to
understand whether the runtime is balancing work the way you expect.

## Example patterns

Patterns that fit naturally in `actors`:

- coordinator/worker execution pools;
- request/response processes;
- background resource managers;
- mailbox-driven protocol implementations;
- timer-triggered retries and timeouts;
- supervision-like lifecycle coordination built from links and monitors.

## Related packages

- `kernel` provides the lower-level async and runtime primitives under
  `Std.Runtime`.
- `std` owns the actor runtime implementation and the higher-level application
  stack built on it.
- `blink` and `suri` are examples of packages that build richer abstractions on
  top of Riot's concurrency model.
