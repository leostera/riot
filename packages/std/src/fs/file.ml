open Global
module Buffer = IO.Buffer
module Bytes = Kernel.Bytes
module Iovec = IO.Iovec

type t = Kernel.Fs.File.t

type error = Kernel.Fs.File.error

module OpenFlags = struct
  type t = Kernel.Fs.File.open_flag =
    | ReadOnly
    | WriteOnly
    | ReadWrite
    | Create
    | Truncate
    | Append
    | Exclusive
end

let error_to_string = Kernel.Fs.File.error_to_string

let kernel_path = fun path -> Kernel.Path.from_string (Path.to_string path)

let wrap_result: type value error. (value, error) Kernel.Result.t -> (value, error) result = function
  | Ok value -> Ok value
  | Error error -> Error error

let open_with_flags = fun path flags ~mode ->
  wrap_result
    (Kernel.Fs.File.open_file (kernel_path path) ~flags ~permissions:(Permissions.to_mode mode))

let create = fun path ->
  wrap_result
    (Kernel.Fs.File.open_write
      (kernel_path path)
      ~create:true
      ~truncate:true
      ~append:false
      ~perm:(Permissions.to_mode Permissions.read_write))

let create_new = fun path ->
  wrap_result
    (Kernel.Fs.File.open_file
      (kernel_path path)
      ~flags:[ Kernel.Fs.File.WriteOnly; Kernel.Fs.File.Create; Kernel.Fs.File.Exclusive ]
      ~permissions:(Permissions.to_mode Permissions.read_write))

let open_read = fun path -> wrap_result (Kernel.Fs.File.open_read (kernel_path path))

let open_write = fun path ->
  wrap_result
    (Kernel.Fs.File.open_write
      (kernel_path path)
      ~create:true
      ~truncate:false
      ~append:false
      ~perm:(Permissions.to_mode Permissions.read_write))

let open_append = fun path ->
  wrap_result
    (Kernel.Fs.File.open_write
      (kernel_path path)
      ~create:true
      ~truncate:false
      ~append:true
      ~perm:(Permissions.to_mode Permissions.read_write))

let open_read_write = fun path ->
  wrap_result
    (Kernel.Fs.File.open_file
      (kernel_path path)
      ~flags:[ Kernel.Fs.File.ReadWrite ]
      ~permissions:(Permissions.to_mode Permissions.read_write))

let try_lock_exclusive = fun file -> wrap_result (Kernel.Fs.File.try_lock_exclusive file)

let try_lock_shared = fun file -> wrap_result (Kernel.Fs.File.try_lock_shared file)

let unlock = fun file -> wrap_result (Kernel.Fs.File.unlock file)

let to_source = Kernel.Fs.File.to_source

let is_would_block = function
  | Kernel.Fs.File.System error -> Kernel.SystemError.would_block error
  | Kernel.Fs.File.InvalidSlice _ -> false

let read = fun file buffer ~offset ~len ->
  let source = Kernel.Fs.File.to_source file in
  let rec loop () =
    match Kernel.Fs.File.read file buffer ~pos:offset ~len with
    | Ok bytes_read -> Ok bytes_read
    | Error err when is_would_block err -> Runtime.syscall
      ~name:"Fs.File.read"
      ~interest:Kernel.Async.Interest.readable
      ~source
      loop
    | Error err -> Error err
  in
  loop ()

let read_to_end = fun file ->
  let buf = Buffer.create ~size:4_096 in
  let chunk = Bytes.create ~size:4_096 in
  let rec loop () =
    match read file chunk ~offset:0 ~len:4_096 with
    | Ok 0 ->
        Ok (Buffer.contents buf)
    | Ok n ->
        Buffer.add_subbytes buf chunk 0 n;
        loop ()
    | Error err ->
        Error err
  in
  loop ()

let read_exact = fun file buffer ~offset ~len ->
  let rec loop pos remaining =
    if remaining = 0 then
      Ok ()
    else
      match read file buffer ~offset:pos ~len:remaining with
      | Ok 0 -> Error (Kernel.Fs.File.System Kernel.SystemError.EndOfFile)
      | Ok n -> loop (pos + n) (remaining - n)
      | Error err -> Error err
  in
  loop offset len

let read_line = fun file ->
  let buf = Buffer.create ~size:256 in
  let chunk = Bytes.create ~size:1 in
  let rec loop () =
    match read file chunk ~offset:0 ~len:1 with
    | Ok 0 ->
        Ok (Buffer.contents buf)
    | Ok 1 ->
        let c = Bytes.get_unchecked chunk ~at:0 in
        Buffer.add_char buf c;
        if c = '\n' then
          Ok (Buffer.contents buf)
        else
          loop ()
    | Ok _ ->
        Ok (Buffer.contents buf)
    | Error err ->
        Error err
  in
  loop ()

let write = fun file buffer ~offset ~len ->
  let source = Kernel.Fs.File.to_source file in
  let rec loop () =
    match Kernel.Fs.File.write file buffer ~pos:offset ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error err when is_would_block err -> Runtime.syscall
      ~name:"Fs.File.write"
      ~interest:Kernel.Async.Interest.writable
      ~source
      loop
    | Error err -> Error err
  in
  loop ()

let write_string = fun file str ->
  write file (Kernel.Bytes.from_string str) ~offset:0 ~len:(String.length str)

let write_all = fun file str ->
  let buffer = Kernel.Bytes.from_string str in
  let len = String.length str in
  let rec loop pos remaining =
    if remaining = 0 then
      Ok ()
    else
      match write file buffer ~offset:pos ~len:remaining with
      | Ok 0 -> Ok ()
      | Ok n -> loop (pos + n) (remaining - n)
      | Error err -> Error err
  in
  loop 0 len

let metadata = fun file -> wrap_result (Kernel.Fs.File.fstat file)

let to_reader = fun file ->
  let read_bytes = read in
  let module Read = struct
    type nonrec t = t

    type err = error

    let read = fun file ?timeout:_ buf -> read_bytes file buf ~offset:0 ~len:(Bytes.length buf)

    let read_vectored = fun file bufs ->
      let total_len = Iovec.length bufs in
      let scratch = Bytes.create ~size:total_len in
      match read_bytes file scratch ~offset:0 ~len:total_len with
      | Error err -> Error err
      | Ok read_len ->
          let copied = ref 0 in
          Iovec.for_each bufs
            ~fn:(fun { Kernel.IO.Iovec.buffer; offset; length } ->
              let remaining = read_len - !copied in
              if remaining > 0 then
                let chunk_len = min length remaining in
                Bytes.blit_unchecked
                  scratch
                  ~src_offset:!copied
                  ~dst:buffer
                  ~dst_offset:offset
                  ~len:chunk_len;
                copied := !copied + chunk_len);
          Ok read_len

    let direct_string = fun _file -> None
  end in
  IO.Reader.of_read_src (module Read) file

let to_writer = fun file ->
  let write_bytes = write in
  let module Write = struct
    type nonrec t = t

    type err = error

    let write = fun file ~buf -> write_string file buf

    let write_owned_vectored = fun file ~bufs ->
      let total_len = Iovec.length bufs in
      let scratch = Iovec.to_bytes bufs in
      write_bytes file scratch ~offset:0 ~len:total_len

    let flush = fun _file -> Ok ()
  end in
  IO.Writer.of_write_src (module Write) file

let close = fun file -> wrap_result (Kernel.Fs.File.close file)
