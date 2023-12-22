open Core
open Util
module Poll = Poll

type t = {
  poll : Poll.t;
  poll_timeout : Poll.Timeout.t;
  procs : (Fd.t, Process.t * [ `r | `w | `rw ]) Dashmap.t;
}

(* operation types *)

type op = [ `Abort of Unix.error | `Retry ]
type accept = [ `Connected of Socket.stream_socket * Addr.stream_addr | op ]
type read = [ `Read of int | op ]
type write = [ `Wrote of int | op ]

(* basic api over t *)

let create () =
  {
    poll = Poll.create ();
    poll_timeout = Poll.Timeout.After 500L;
    procs = Dashmap.create 1024;
  }

(* pretty-printing *)

let mode_to_string mode = match mode with `r -> "r" | `rw -> "rw" | `w -> "w"

let pp ppf (t : t) =
  let entries = Dashmap.entries t.procs in
  Format.fprintf ppf "[";
  List.iter
    (fun (fd, (proc, mode)) ->
      Format.fprintf ppf "%a:%a:%s," Fd.pp fd Pid.pp (Process.pid proc)
        (mode_to_string mode))
    entries;
  Format.fprintf ppf "]"

let event_of_mode mode =
  match mode with
  | `r -> Poll.Event.read
  | `rw -> Poll.Event.read_write
  | `w -> Poll.Event.write

let mode_of_event event =
  match event with
  | Poll.Event.{ writable = false; readable = true } -> Some `r
  | Poll.Event.{ writable = true; readable = true } -> Some `rw
  | Poll.Event.{ writable = true; readable = false } -> Some `w
  | Poll.Event.{ writable = false; readable = false } -> None

(* NOTE(leostera): when we add a new Fd.t to our collection here, we need
   to update the current poller so that it knows of it.

   If we don't call [add_fd t fd] then we will never poll on [fd].
*)
let add_fd t fd mode =
  if Fd.is_closed fd then ()
  else
    let unix_fd = Fd.get fd |> Option.get in
    let flags = event_of_mode mode in
    Poll.set t.poll unix_fd flags

let register t proc mode fd =
  add_fd t fd mode;
  Dashmap.insert t.procs fd (proc, mode)

let unregister_process t proc =
  Log.debug (fun f -> f "Unregistering %a" Pid.pp (Process.pid proc));
  let this_proc (_fd, (proc', _mode)) =
    Pid.equal (Process.pid proc) (Process.pid proc')
  in
  Dashmap.remove_by t.procs this_proc

let can_poll t = not (Dashmap.is_empty t.procs)

let poll t fn =
  Poll.clear t.poll;
  let should_wait = ref false in
  Dashmap.iter t.procs (fun (fd, (_proc, mode)) ->
      match Fd.get fd with
      | None -> ()
      | Some unix_fd ->
          should_wait := true;
          Poll.set t.poll unix_fd (event_of_mode mode));
  if not !should_wait then ()
  else
    match Poll.wait t.poll t.poll_timeout with
    | `Timeout -> ()
    | `Ok ->
        Poll.iter_ready t.poll ~f:(fun raw_fd event ->
            match mode_of_event event with
            | None -> ()
            | Some mode ->
                let mode_and_flag (fd, (proc, mode')) =
                  Log.trace (fun f ->
                      f "io_poll(%a=%a,%a=%a): %a" Fd.pp fd Fd.pp fd Fd.Mode.pp
                        mode' Fd.Mode.pp mode Process.pp proc);
                  let same_mode = Fd.Mode.equal mode' mode in
                  match Fd.get fd with
                  | Some fd' -> fd' = raw_fd && same_mode
                  | _ -> false
                in
                Dashmap.find_all_by t.procs mode_and_flag
                |> List.iter (fun (_fd, proc) -> fn proc))

(* sockets api *)
let socket sock_domain sock_type =
  let fd = Unix.socket ~cloexec:true sock_domain sock_type 0 in
  Unix.set_nonblock fd;
  Fd.make fd

let close (_t : t) fd =
  Log.trace (fun f -> f "closing %a" Fd.pp fd);
  Fd.close fd

let getaddrinfo host service =
  Log.debug (fun f -> f "getaddrinfo %s %s" host service);
  match Unix.getaddrinfo host service [] with
  | addr_info -> `Ok addr_info
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let listen (_t : t) ~reuse_addr ~reuse_port ~backlog addr =
  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = socket sock_domain sock_type in
  Fd.use ~op_name:"listen" fd @@ fun sock ->
  Unix.setsockopt sock Unix.SO_REUSEADDR reuse_addr;
  Unix.setsockopt sock Unix.SO_REUSEPORT reuse_port;
  Unix.bind sock sock_addr;
  Unix.listen sock backlog;
  Log.debug (fun f -> f "listening to socket %a on %a" Fd.pp fd Addr.pp addr);
  Ok fd

let connect (_t : t) (addr : Addr.stream_addr) =
  Log.debug (fun f -> f "Connecting to: %a" Addr.pp addr);

  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = socket sock_domain sock_type in

  Fd.use ~op_name:"connect" fd @@ fun sock ->
  match Unix.connect sock sock_addr with
  | () -> `Connected fd
  | exception Unix.(Unix_error (EINPROGRESS, _, _)) -> `In_progress fd
  | exception
      Unix.(Unix_error ((ENOTCONN | EINTR | EAGAIN | EWOULDBLOCK), _, _)) ->
      `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let accept (_t : t) (socket : Fd.t) : accept =
  Fd.use ~op_name:"accept" socket @@ fun fd ->
  Log.debug (fun f -> f "Accepting client at fd=%a" Fd.pp socket);
  match Unix.accept ~cloexec:true fd with
  | raw_fd, client_addr ->
      Unix.set_nonblock raw_fd;
      let addr = Addr.of_unix client_addr in
      let fd = Fd.make raw_fd in
      Log.debug (fun f -> f "connected client with fd=%a" Fd.pp fd);
      `Connected (fd, addr)
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let read (fd : Fd.t) buf off len : read =
  Fd.use ~op_name:"read" fd @@ fun unix_fd ->
  Log.debug (fun f -> f "Reading from fd=%a" Fd.pp fd);
  match Unix.read unix_fd buf off len with
  | len ->
      Log.debug (fun f -> f "read %d bytes from fd=%a" len Fd.pp fd);
      `Read len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason

let write (fd : Fd.t) buf off len : write =
  Fd.use ~op_name:"write" fd @@ fun unix_fd ->
  Log.debug (fun f -> f "Writing to fd=%a" Fd.pp fd);
  match Unix.write unix_fd buf off len with
  | len -> `Wrote len
  | exception Unix.(Unix_error ((EINTR | EAGAIN | EWOULDBLOCK), _, _)) -> `Retry
  | exception Unix.(Unix_error (reason, _, _)) -> `Abort reason
