open Riot_api

module Logger = Logger.Make (struct
  let namespace = [ "riot"; "net"; "socket" ]
end)

type listen_opts = {
  reuse_addr : bool;
  reuse_port : bool;
  backlog : int;
  addr : Net.Addr.tcp_addr;
}

type timeout = Infinity | Bounded of float
type unix_error = [ `Unix_error of Unix.error ]
type ('ok, 'err) result = ('ok, ([> unix_error ] as 'err)) Stdlib.result

let default_listen_opts =
  {
    reuse_addr = true;
    reuse_port = true;
    backlog = 128;
    addr = Net.Addr.loopback;
  }

let close socket =
  let sch = Scheduler.get_current_scheduler () in
  let this = self () in
  Logs.trace (fun f ->
      f "Process %a: Closing socket fd=%a" Pid.pp this Fd.pp socket);
  Io.close sch.io_tbl socket

let listen ?(opts = default_listen_opts) ~port () =
  let sch = Scheduler.get_current_scheduler () in
  let { reuse_addr; reuse_port; backlog; addr } = opts in
  let addr = Net.Addr.tcp addr port in
  Logger.trace (fun f -> f "Listening on 0.0.0.0:%d" port);
  Io.listen sch.io_tbl ~reuse_port ~reuse_addr ~backlog addr

let rec accept ?(timeout = Infinity) (socket : Net.listen_socket) =
  syscall "accept" `r socket @@ fun socket ->
  let sch = Scheduler.get_current_scheduler () in
  match Io.accept sch.io_tbl socket with
  | `Abort reason -> Error (`Unix_error reason)
  | `Retry -> accept ~timeout socket
  | `Connected (socket, addr) -> Ok (socket, addr)

let controlling_process _socket ~new_owner:_ = Ok ()

let rec receive ?(timeout = Infinity) ~len socket =
  syscall "read" `r socket @@ fun socket ->
  let bytes = Bytes.create len in
  match Io.read socket bytes 0 len with
  | `Abort reason -> Error (`Unix_error reason)
  | `Retry -> receive ~timeout ~len socket
  | `Read len ->
      let data = Bigstringaf.create len in
      Bigstringaf.blit_from_bytes bytes ~src_off:0 data ~dst_off:0 ~len;
      Ok data

let rec send data socket =
  syscall "write" `w socket @@ fun socket ->
  let off = 0 in
  let len = Bigstringaf.length data in
  let bytes = Bytes.create len in
  Bigstringaf.blit_to_bytes data ~src_off:off bytes ~dst_off:0 ~len;
  match Io.write socket bytes off len with
  | `Abort reason -> Error (`Unix_error reason)
  | `Retry -> send data socket
  | `Wrote bytes -> Ok bytes
