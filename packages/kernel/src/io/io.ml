open Prelude
module Iovec = Iovec

type slice_validation = {
  pos: int;
  len: int;
  buffer_len: int;
}

let validate_slice = fun buf ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    Result.Error { pos; len; buffer_len = Bytes.length buf }
  else
    Result.Ok ()

module FFI = struct
  external read: int -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_fs_file_read"

  external write: int -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_fs_file_write"

  external readv: int -> Iovec.t -> (int, int) Result.t = "kernel_new_fs_file_readv"

  external writev: int -> Iovec.t -> (int, int) Result.t = "kernel_new_fs_file_writev"
end

let to_source = fun fd ->
  let module Source = struct
    type nonrec t = int

    let register = fun fd selector token interest ->
      Async.Adapter.Selector.register selector ~fd ~token ~interest

    let reregister = fun fd selector token interest ->
      Async.Adapter.Selector.reregister selector ~fd ~token ~interest

    let deregister = fun fd selector -> Async.Adapter.Selector.deregister selector ~fd
  end in
  Async.Source.make (module Source) fd

module Stdin = struct
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t

  let error_to_string = fun value ->
    match value with
    | InvalidSlice { pos; len; buffer_len } -> String.concat
      ""
      [
        "invalid buffer slice: pos=";
        Int.to_string pos;
        ", len=";
        Int.to_string len;
        ", buffer_len=";
        Int.to_string buffer_len;
      ]
    | System error -> System_error.to_string error

  let read = fun ?pos ?len buffer ->
    let pos = Option.unwrap_or pos ~default:0 in
    let len =
      match len with
      | Some len -> len
      | None -> Bytes.length buffer - pos
    in
    match validate_slice buffer ~pos ~len with
    | Result.Ok () -> FFI.read 0 buffer pos len
    |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))
    | Result.Error { pos; len; buffer_len } -> Result.Error (InvalidSlice { pos; len; buffer_len })

  let read_vectored = fun iovec ->
    FFI.readv 0 iovec |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))

  let flush = fun () -> Result.Ok ()

  let to_source = fun () -> to_source 0
end

module Stdout = struct
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t

  let error_to_string = fun value ->
    match value with
    | InvalidSlice { pos; len; buffer_len } -> String.concat
      ""
      [
        "invalid buffer slice: pos=";
        Int.to_string pos;
        ", len=";
        Int.to_string len;
        ", buffer_len=";
        Int.to_string buffer_len;
      ]
    | System error -> System_error.to_string error

  let write = fun ?pos ?len buffer ->
    let pos = Option.unwrap_or pos ~default:0 in
    let len =
      match len with
      | Some len -> len
      | None -> Bytes.length buffer - pos
    in
    match validate_slice buffer ~pos ~len with
    | Result.Ok () -> FFI.write 1 buffer pos len
    |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))
    | Result.Error { pos; len; buffer_len } -> Result.Error (InvalidSlice { pos; len; buffer_len })

  let write_vectored = fun iovec ->
    FFI.writev 1 iovec |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))

  let flush = fun () -> Result.Ok ()

  let to_source = fun () -> to_source 1
end

module Stderr = struct
  type error =
    | InvalidSlice of { pos: int; len: int; buffer_len: int }
    | System of System_error.t

  let error_to_string = fun value ->
    match value with
    | InvalidSlice { pos; len; buffer_len } -> String.concat
      ""
      [
        "invalid buffer slice: pos=";
        Int.to_string pos;
        ", len=";
        Int.to_string len;
        ", buffer_len=";
        Int.to_string buffer_len;
      ]
    | System error -> System_error.to_string error

  let write = fun ?pos ?len buffer ->
    let pos = Option.unwrap_or pos ~default:0 in
    let len =
      match len with
      | Some len -> len
      | None -> Bytes.length buffer - pos
    in
    match validate_slice buffer ~pos ~len with
    | Result.Ok () -> FFI.write 2 buffer pos len
    |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))
    | Result.Error { pos; len; buffer_len } -> Result.Error (InvalidSlice { pos; len; buffer_len })

  let write_vectored = fun iovec ->
    FFI.writev 2 iovec |> Result.map_err ~fn:(fun code -> System (System_error.from_code code))

  let flush = fun () -> Result.Ok ()

  let to_source = fun () -> to_source 2
end
