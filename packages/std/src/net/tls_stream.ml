(** TLS stream for encrypted connections *)
open Global
open IO

(* Result monad for cleaner error handling *)

let ( let* ) = fun x f ->
  Result.and_then x f

type error =
  | Closed
  | Handshake_failed of string
  | System_error of IO.error
  | Network_read_failed of Tcp_stream.error
  | Network_write_failed of Tcp_stream.error
  | Tls_not_available
  | Unsupported_vectored_operation

type 'src t = {
  reader: ('src, error) IO.Reader.t;
  writer: ('src, error) IO.Writer.t;
  engine: Kernel.Net.Tls.engine;
  mutable state:
    [
      `Active
      | `Eof
      | `Error of exn
    ];
  network_in_buf: bytes;
  network_out_buf: bytes;
}

(* Helper: Read encrypted data from network and pump into TLS engine *)

let read_from_network = fun t ->
  match IO.read t.reader t.network_in_buf with
  | Ok n ->
      if n = 0 then
        Error Closed
      else
        (
          let _ = Kernel.Net.Tls.pump_encrypted_in t.engine t.network_in_buf ~pos:0 ~len:n in
          Ok ()
        )
  | Error err -> Error err

(* Already wrapped by map_err *)

(* Helper: Flush encrypted data from TLS engine to network *)

let flush_to_network = fun t ->
  let rec flush_loop () =
    let n = Kernel.Net.Tls.read_encrypted_out t.engine t.network_out_buf in
    if n = 0 then
      Ok ()
    else
      (
        let data = Bytes.sub_string t.network_out_buf 0 n in
        match IO.write_all t.writer ~buf:data with
        | Ok () -> flush_loop ()
        | Error err -> Error err
      )
  in
  flush_loop ()

(* Perform TLS handshake *)

let do_handshake = fun t ->
  let rec handshake_loop () =
    if Kernel.Net.Tls.handshake_complete t.engine then
      Ok ()
    else
      (
        (* Call do_handshake to advance the state machine *)
        match Kernel.Net.Tls.do_handshake t.engine with
        | Handshake_done ->
            Ok ()
        | Need_network_read ->
            (* TLS needs encrypted data from network *)
            (* But first, flush any pending output (e.g., ClientHello) *)
            let* () = flush_to_network t in
            let* () = read_from_network t in
            handshake_loop ()
        | Need_network_write ->
            (* TLS needs to send encrypted data to network *)
            let* () = flush_to_network t in
            handshake_loop ()
      )
  in
  handshake_loop ()

let of_client_io = fun ~reader ~writer ~hostname () ->
  (* Initialize OpenSSL if not already done *)
  (
    try Kernel.Net.Tls.init () with
    | _ -> ()
  );
  (* Check if TLS is available *)
  if not (Kernel.Net.Tls.is_available ()) then
    Error Tls_not_available
  else
    let engine = Kernel.Net.Tls.create_client_engine ~hostname in
    let t = {
      reader;
      writer;
      engine;
      state = `Active;
      network_in_buf = Bytes.create 16_384;
      network_out_buf = Bytes.create 16_384;
    }
    in
    match do_handshake t with
    | Ok () -> Ok t
    | Error e -> Error e

let of_server_io = fun ~reader ~writer ~cert_file ~key_file () ->
  (* Initialize OpenSSL if not already done *)
  (
    try Kernel.Net.Tls.init () with
    | _ -> ()
  );
  if not (Kernel.Net.Tls.is_available ()) then
    Error Tls_not_available
  else
    let engine = Kernel.Net.Tls.create_server_engine ~cert_file ~key_file in
    let t = {
      reader;
      writer;
      engine;
      state = `Active;
      network_in_buf = Bytes.create 16_384;
      network_out_buf = Bytes.create 16_384;
    }
    in
    match do_handshake t with
    | Ok () -> Ok t
    | Error e -> Error e

let network_read_failed = fun x -> Network_read_failed x

let network_write_failed = fun x -> Network_write_failed x

let of_tcp_socket = fun ~mode sock ->
  let reader = Tcp_stream.to_reader sock |> IO.Reader.map_err ~fn:network_read_failed in
  let writer = Tcp_stream.to_writer sock |> IO.Writer.map_err ~fn:network_write_failed in
  match mode with
  | `Client hostname -> of_client_io ~reader ~writer ~hostname ()
  | `Server (cert_file, key_file) -> of_server_io ~reader ~writer ~cert_file ~key_file ()

let of_tcp_client = fun ~hostname tcp -> of_tcp_socket ~mode:(`Client hostname) tcp

let of_tcp_server = fun ~cert_file ~key_file tcp ->
  of_tcp_socket ~mode:(`Server (cert_file, key_file)) tcp

(* Read plaintext from TLS stream *)

let read_plaintext t dst : (int, error) result =
  let rec read_loop () =
    match Kernel.Net.Tls.read_decrypted t.engine dst ~pos:0 ~len:(Bytes.length dst) with
    | Read n ->
        Ok n
    | Eof ->
        Ok 0
    | Need_network_read ->
        (* TLS needs more encrypted data from network *)
        let* () = read_from_network t in
        read_loop ()
    | Need_network_write ->
        (* TLS needs to send encrypted data to network *)
        let* () = flush_to_network t in
        read_loop ()
  in
  match t.state with
  | `Eof ->
      Ok 0
  | `Error e ->
      raise e
  | `Active -> (
      match read_loop () with
      | Ok 0 ->
          t.state <- `Eof;
          Ok 0
      | Ok n ->
          Ok n
      | Error e ->
          t.state <- `Error (Failure "TLS read error");
          Error e
    )

(* Write plaintext to TLS stream *)

let write_plaintext t src : (int, error) result =
  let src_bytes = Bytes.of_string src in
  let src_len = Bytes.length src_bytes in
  let rec write_loop pos remaining =
    if remaining = 0 then
      Ok src_len
    else
      match Kernel.Net.Tls.write_plaintext t.engine src_bytes ~pos ~len:remaining with
      | Written n ->
          (* Flush encrypted data to network *)
          let* () = flush_to_network t in
          write_loop (pos + n) (remaining - n)
      | Need_network_read ->
          (* SSL needs more input (renegotiation?) *)
          let* () = read_from_network t in
          write_loop pos remaining
      | Need_network_write ->
          (* Need to flush first *)
          let* () = flush_to_network t in
          write_loop pos remaining
  in
  match t.state with
  | `Eof ->
      Error Closed
  | `Error e ->
      raise e
  | `Active -> (
      match write_loop 0 src_len with
      | Ok n -> Ok n
      | Error e ->
          t.state <- `Error (Failure "TLS write error");
          Error e
    )

(* Expose as reader *)

let to_reader : type src. src t -> (src t, error) IO.Reader.t = fun tls_stream ->
  let module Read = struct
    type nonrec t = src t

    type err = error

    let read = fun (tls: t) ?timeout:_ buf -> read_plaintext tls buf

    let read_vectored = fun _t _bufs -> Error Unsupported_vectored_operation
  end in
  IO.Reader.of_read_src (module Read) tls_stream

(* Expose as writer *)

let to_writer : type src. src t -> (src t, error) IO.Writer.t = fun tls ->
  let module Write = struct
    type nonrec t = src t

    type err = error

    let write = fun t ~buf -> write_plaintext t buf

    let write_owned_vectored = fun _t ~bufs:_ -> Error Unsupported_vectored_operation

    let flush = fun _t -> Ok ()
  end in
  IO.Writer.of_write_src (module Write) tls

(* TLS information *)

let alpn_protocol = fun t -> Kernel.Net.Tls.alpn_protocol t.engine

let close = fun _t -> ()

(* Don't close underlying transport *)
