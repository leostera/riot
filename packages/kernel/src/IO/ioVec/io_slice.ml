open Prelude

type t

type error = Error.t

external unsafe_create: int -> t = "kernel_new_iovec_slice_create"

external unsafe_length: t -> int = "%caml_ba_dim_1"

external unsafe_get: t -> int -> char = "%caml_ba_unsafe_ref_1"

external unsafe_set: t -> int -> char -> unit = "%caml_ba_unsafe_set_1"

external unsafe_sub: t -> offset:int -> len:int -> t = "caml_ba_sub"

external unsafe_blit: src:t -> src_offset:int -> dst:t -> dst_offset:int -> len:int -> unit
  = "kernel_new_iovec_slice_blit" [@@noalloc]

external unsafe_blit_from_bytes: bytes -> src_offset:int -> dst:t -> dst_offset:int -> len:int -> unit
  = "kernel_new_iovec_slice_blit_from_bytes" [@@noalloc]

external unsafe_blit_from_string: string -> src_offset:int -> dst:t -> dst_offset:int -> len:int -> unit
  = "kernel_new_iovec_slice_blit_from_string" [@@noalloc]

external unsafe_blit_to_bytes: src:t -> src_offset:int -> dst:bytes -> dst_offset:int -> len:int -> unit
  = "kernel_new_iovec_slice_blit_to_bytes" [@@noalloc]

let empty = unsafe_create 0

let validate_size = fun size ->
  if size < 0 then
    Error (Error.Negative_size size)
  else
    Ok ()

let validate_index = fun buffer_len at ->
  if at < 0 || at >= buffer_len then
    Error (Error.Index_out_of_bounds { buffer_length = buffer_len; at })
  else
    Ok ()

let validate_range = fun buffer_len ~off ~len ->
  if off < 0 then
    Error (Error.Negative_offset off)
  else if len < 0 then
    Error (Error.Negative_length len)
  else if off > buffer_len || len > buffer_len - off then
    Error (Error.Range_out_of_bounds { buffer_length = buffer_len; offset = off; len })
  else
    Ok ()

let create = fun ~size ->
  match validate_size size with
  | Ok () -> Ok (unsafe_create size)
  | Error _ as error -> error

let length = unsafe_length

let sub = fun value ~off ~len ->
  match validate_range (length value) ~off ~len with
  | Ok () -> Ok (unsafe_sub value ~offset:off ~len)
  | Error _ as error -> error

let sub_unchecked = fun value ~off ~len ->
  match sub value ~off ~len with
  | Ok slice -> slice
  | Error error -> System_error.panic
    ("Kernel.IO.IoVec.IoSlice.sub_unchecked: " ^ Error.message error)

let shift = fun value by ->
  let value_len = length value in
  if by < 0 || by > value_len then
    Error (Error.Shift_out_of_bounds { buffer_length = value_len; by })
  else
    Ok (unsafe_sub value ~offset:by ~len:(value_len - by))

let shift_unchecked = fun value by ->
  match shift value by with
  | Ok slice -> slice
  | Error error -> System_error.panic
    ("Kernel.IO.IoVec.IoSlice.shift_unchecked: " ^ Error.message error)

let split_at = fun value at ->
  let value_len = length value in
  if at < 0 || at > value_len then
    Error (Error.Split_out_of_bounds { buffer_length = value_len; at })
  else
    Ok (unsafe_sub value ~offset:0 ~len:at, unsafe_sub value ~offset:at ~len:(value_len - at))

let split_at_unchecked = fun value at ->
  match split_at value at with
  | Ok slices -> slices
  | Error error -> System_error.panic
    ("Kernel.IO.IoVec.IoSlice.split_at_unchecked: " ^ Error.message error)

let get = fun value ~at ->
  match validate_index (length value) at with
  | Ok () -> Ok (unsafe_get value at)
  | Error _ as error -> error

let get_unchecked = fun value ~at -> unsafe_get value at

let set = fun value ~at char ->
  match validate_index (length value) at with
  | Ok () ->
      unsafe_set value at char;
      Ok ()
  | Error _ as error -> error

let set_unchecked = fun value ~at char ->
  match set value ~at char with
  | Ok () -> ()
  | Error error -> System_error.panic
    ("Kernel.IO.IoVec.IoSlice.set_unchecked: " ^ Error.message error)

let validate_bytes_range = fun buffer_len ~off ~len ->
  if off < 0 then
    Error (Error.Negative_offset off)
  else if len < 0 then
    Error (Error.Negative_length len)
  else if off > buffer_len || len > buffer_len - off then
    Error (Error.Range_out_of_bounds { buffer_length = buffer_len; offset = off; len })
  else
    Ok ()

let blit = fun ~src ~src_off ~dst ~dst_off ~len ->
  match validate_range (length src) ~off:src_off ~len with
  | Error _ as error -> error
  | Ok () -> (
      match validate_range (length dst) ~off:dst_off ~len with
      | Ok () ->
          unsafe_blit ~src ~src_offset:src_off ~dst ~dst_offset:dst_off ~len;
          Ok ()
      | Error _ as error -> error
    )

let blit_unchecked = fun ~src ~src_off ~dst ~dst_off ~len ->
  match blit ~src ~src_off ~dst ~dst_off ~len with
  | Ok () -> ()
  | Error error -> System_error.panic
    ("Kernel.IO.IoVec.IoSlice.blit_unchecked: " ^ Error.message error)

let blit_from_bytes = fun src ~src_off dst ~dst_off ~len ->
  match validate_bytes_range (Bytes.length src) ~off:src_off ~len with
  | Error _ as error -> error
  | Ok () -> (
      match validate_range (length dst) ~off:dst_off ~len with
      | Ok () ->
          unsafe_blit_from_bytes src ~src_offset:src_off ~dst ~dst_offset:dst_off ~len;
          Ok ()
      | Error _ as error -> error
    )

let blit_from_bytes_unchecked = fun src ~src_off dst ~dst_off ~len ->
  match blit_from_bytes src ~src_off dst ~dst_off ~len with
  | Ok () -> ()
  | Error error -> System_error.panic
    ("Kernel.IO.IoVec.IoSlice.blit_from_bytes_unchecked: " ^ Error.message error)

let blit_from_string = fun src ~src_off dst ~dst_off ~len ->
  match validate_bytes_range (String.length src) ~off:src_off ~len with
  | Error _ as error -> error
  | Ok () -> (
      match validate_range (length dst) ~off:dst_off ~len with
      | Ok () ->
          unsafe_blit_from_string src ~src_offset:src_off ~dst ~dst_offset:dst_off ~len;
          Ok ()
      | Error _ as error -> error
    )

let blit_from_string_unchecked = fun src ~src_off dst ~dst_off ~len ->
  match blit_from_string src ~src_off dst ~dst_off ~len with
  | Ok () -> ()
  | Error error -> System_error.panic
    ("Kernel.IO.IoVec.IoSlice.blit_from_string_unchecked: " ^ Error.message error)

let blit_to_bytes = fun src ~src_off dst ~dst_off ~len ->
  match validate_range (length src) ~off:src_off ~len with
  | Error _ as error -> error
  | Ok () -> (
      match validate_bytes_range (Bytes.length dst) ~off:dst_off ~len with
      | Ok () ->
          unsafe_blit_to_bytes ~src ~src_offset:src_off ~dst ~dst_offset:dst_off ~len;
          Ok ()
      | Error _ as error -> error
    )

let blit_to_bytes_unchecked = fun src ~src_off dst ~dst_off ~len ->
  match blit_to_bytes src ~src_off dst ~dst_off ~len with
  | Ok () -> ()
  | Error error -> System_error.panic
    ("Kernel.IO.IoVec.IoSlice.blit_to_bytes_unchecked: " ^ Error.message error)

let from_string = fun ?(off = 0) ?len value ->
  let len =
    match len with
    | Some len -> len
    | None -> String.length value - off
  in
  match validate_bytes_range (String.length value) ~off ~len with
  | Error _ as error -> error
  | Ok () -> (
      match create ~size:len with
      | Error _ as error -> error
      | Ok slice ->
          blit_from_string_unchecked value ~src_off:off slice ~dst_off:0 ~len;
          Ok slice
    )

let from_bytes = fun ?(off = 0) ?len value ->
  let len =
    match len with
    | Some len -> len
    | None -> Bytes.length value - off
  in
  match validate_bytes_range (Bytes.length value) ~off ~len with
  | Error _ as error -> error
  | Ok () -> (
      match create ~size:len with
      | Error _ as error -> error
      | Ok slice ->
          blit_from_bytes_unchecked value ~src_off:off slice ~dst_off:0 ~len;
          Ok slice
    )

let starts_with = fun value ~prefix ->
  let prefix_len = String.length prefix in
  if prefix_len > length value then
    false
  else
    let rec loop index =
      if index >= prefix_len then
        true
      else if get_unchecked value ~at:index != String.get_unchecked prefix ~at:index then
        false
      else
        loop (index + 1)
    in
    loop 0

let equal_string = fun value string ->
  starts_with value ~prefix:string && length value = String.length string

let index_char = fun value needle ->
  let rec loop index =
    if index >= length value then
      None
    else if get_unchecked value ~at:index = needle then
      Some index
    else
      loop (index + 1)
  in
  loop 0

let index_string = fun value needle ->
  let needle_len = String.length needle in
  if needle_len = 0 then
    Some 0
  else
    let value_len = length value in
    let rec matches start needle_index =
      if needle_index >= needle_len then
        true
      else if
        get_unchecked value ~at:(start + needle_index) != String.get_unchecked needle ~at:needle_index
      then
        false
      else
        matches start (needle_index + 1)
    in
    let rec loop index =
      if index + needle_len > value_len then
        None
      else if matches index 0 then
        Some index
      else
        loop (index + 1)
    in
    loop 0

let to_string = fun value ->
  let out = Caml_runtime.bytes_create (length value) in
  blit_to_bytes_unchecked value ~src_off:0 out ~dst_off:0 ~len:(length value);
  Caml_runtime.bytes_unsafe_to_string out

let to_bytes = fun value ->
  let out = Caml_runtime.bytes_create (length value) in
  blit_to_bytes_unchecked value ~src_off:0 out ~dst_off:0 ~len:(length value);
  out
