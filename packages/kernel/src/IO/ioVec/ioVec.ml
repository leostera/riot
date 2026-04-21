open Prelude
module IoSlice = Io_slice

type segment = IoSlice.t

type t = segment array

type error = Error.t

let empty = [||]

let create = fun ?(count = 1) ~size () ->
  if count <= 0 then
    Error (Error.Invalid_count count)
  else if size < 0 then
    Error (Error.Negative_size size)
  else
    let base = size / count in
    let remainder = size mod count in
    Ok (
      Array.init ~count
        ~fn:(fun index ->
          let chunk =
            if index < remainder then
              base + 1
            else
              base
          in
          match IoSlice.create ~size:chunk with
          | Ok segment -> segment
          | Error error -> System_error.panic ("Kernel.IO.IoVec.create: " ^ Error.message error))
    )

let with_capacity = fun size -> create ~size ()

let from_slices = fun segments -> segments

let copy_bytes = fun source -> IoSlice.from_bytes source

let copy_string = fun source -> IoSlice.from_string source

let from_bytes = fun buffer ->
  match copy_bytes buffer with
  | Ok slice -> Ok [|slice|]
  | Error _ as error -> error

let from_string = fun value ->
  match copy_string value with
  | Ok slice -> Ok [|slice|]
  | Error _ as error -> error

let from_bytes_array = fun buffers ->
  let count = Array.length buffers in
  let rec loop index acc =
    if index >= count then
      Ok (Array.from_list (List.reverse acc))
    else
      match copy_bytes (Array.get_unchecked buffers ~at:index) with
      | Ok slice -> loop (index + 1) (slice :: acc)
      | Error _ as error -> error
  in
  loop 0 []

let from_string_array = fun values ->
  let count = Array.length values in
  let rec loop index acc =
    if index >= count then
      Ok (Array.from_list (List.reverse acc))
    else
      match copy_string (Array.get_unchecked values ~at:index) with
      | Ok slice -> loop (index + 1) (slice :: acc)
      | Error _ as error -> error
  in
  loop 0 []

let length = fun segments ->
  Array.fold_left segments ~fn:(fun total segment -> total + IoSlice.length segment) ~acc:0

let for_each = fun ~fn segments -> Array.for_each segments ~fn

let sub = fun ?(pos = 0) ~len segments ->
  let total = length segments in
  if pos < 0 then
    Error (Error.Negative_offset pos)
  else if len < 0 then
    Error (Error.Negative_length len)
  else if pos > total || len > total - pos then
    Error (Error.Range_out_of_bounds { buffer_length = total; offset = pos; len })
  else
    let rec reverse_append left right =
      match left with
      | [] -> right
      | head :: tail -> reverse_append tail (head :: right)
    in
    let rec loop index cursor acc =
      if index >= Array.length segments || cursor >= pos + len then
        Ok (Array.from_list (reverse_append acc []))
      else
        let segment = Array.get_unchecked segments ~at:index in
        let segment_length = IoSlice.length segment in
        let segment_start = cursor in
        let segment_end = cursor + segment_length in
        if segment_end <= pos then
          loop (index + 1) segment_end acc
        else
          let start_offset =
            if pos > segment_start then
              pos - segment_start
            else
              0
          in
          let available = segment_length - start_offset in
          let remaining = pos + len - (segment_start + start_offset) in
          let take =
            if available < remaining then
              available
            else
              remaining
          in
          match IoSlice.sub segment ~off:start_offset ~len:take with
          | Ok next -> loop (index + 1) segment_end (next :: acc)
          | Error _ as error -> error
    in
    loop 0 0 []

let to_bytes = fun segments ->
  let total = length segments in
  let out = Bytes.create ~size:total in
  let rec loop index cursor =
    if index >= Array.length segments then
      out
    else
      let segment = Array.get_unchecked segments ~at:index in
      IoSlice.blit_to_bytes_unchecked
        segment
        ~src_off:0
        out
        ~dst_off:cursor
        ~len:(IoSlice.length segment);
      loop (index + 1) (cursor + IoSlice.length segment)
  in
  let _ = loop 0 0 in
  out

let to_string = fun segments -> Bytes.to_string (to_bytes segments)
