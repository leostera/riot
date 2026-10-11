open Std
module Test = Std.Test
module File = Kernel.Fs.File
module Iovec = Kernel.IO.Iovec

(* Explicit compiler-runtime primitive, used only to exercise native buffer
   lifetimes while another domain performs file I/O. No stdlib dependency. *)
external full_major: unit -> unit = "caml_gc_full_major"

let expect = fun result ->
  match result with
  | Kernel.Result.Ok value -> value
  | Error error -> raise (Failure (File.error_to_string error))

let with_collector = fun fn ->
  let stop = Sync.Atomic.make false in
  let started = Sync.Atomic.make false in
  let collections = Sync.Atomic.make 0 in
  let collector = Kernel.Thread.spawn (fun () ->
    Sync.Atomic.set started true;
    let rec collect remaining =
      if remaining > 0 && not (Sync.Atomic.get stop) then (
        full_major ();
        let _ = Sync.Atomic.fetch_and_add collections 1 in
        collect (remaining - 1)) in
    collect 256) in
  while not (Sync.Atomic.get started) do () done;
  let finish () =
    Sync.Atomic.set stop true;
    Kernel.Thread.join collector in
  let result = try fn () with exn -> finish (); raise exn in
  finish ();
  if Sync.Atomic.get collections = 0 then raise (Failure "GC stress completed without a collection");
  result

let test_roundtrip = fun ~vectored _ctx ->
  match Fs.with_tempdir ~prefix:"kernel_file_gc" (fun root ->
    let path = Kernel.Path.from_string (Path.to_string Path.(root / Path.v "payload")) in
    let payload = Kernel.String.make ~len:354 ~char:'x' in
    let writer = expect (File.open_write ~truncate:true path) in
    let write_record () =
      let buffer = Kernel.Bytes.from_string payload in
      if vectored then (
        let iov = Iovec.sub ~pos:0 ~len:354 (Iovec.from_bytes buffer) in
        let rec write remaining position =
          if remaining > 0 then (
            let n = expect (File.write_vectored writer (Iovec.sub ~pos:position ~len:remaining iov)) in
            if n <= 0 then raise (Failure "writev made no progress");
            write (remaining - n) (position + n)) in
        write 354 0)
      else (
        let rec write remaining position =
          if remaining > 0 then (
            let n = expect (File.write writer ~pos:position ~len:remaining buffer) in
            if n <= 0 then raise (Failure "write made no progress");
            write (remaining - n) (position + n)) in
        write 354 0) in
    (try
      with_collector (fun () -> for _ = 1 to 1024 do write_record () done);
      let _ = expect (File.close writer) in
      ()
    with exn -> let _ = File.close writer in raise exn);
    let reader = expect (File.open_read path) in
    try
      let result = with_collector (fun () ->
        let rec check remaining =
          if remaining = 0 then Ok ()
          else
            let buffer = Kernel.Bytes.create ~size:354 in
            let iov = Iovec.from_bytes buffer in
            let rec read position =
              if position < 354 then (
                let n = expect (if vectored then
                    File.read_vectored reader (Iovec.sub ~pos:position ~len:(354 - position) iov)
                  else File.read reader ~pos:position ~len:(354 - position) buffer) in
                if n <= 0 then raise (Failure "read reached unexpected EOF");
                read (position + n)) in
            read 0;
            if Kernel.String.equal (Kernel.Bytes.to_string buffer) payload then check (remaining - 1)
            else Error ("file bytes changed under GC at record " ^ Int.to_string (1024 - remaining)) in
        check 1024) in
      let _ = expect (File.close reader) in
      result
    with exn -> let _ = File.close reader in raise exn) with
  | Ok result -> result
  | Error error -> Error (IO.error_message error)

let test_short_vectored_read_preserves_unread_bytes = fun _ctx ->
  match Fs.with_tempdir ~prefix:"kernel_file_short_read" (fun root ->
    let path = Path.(root / Path.v "payload") in
    Fs.write "abcde" path |> Result.expect ~msg:"write seed failed";
    let reader = expect (File.open_read (Kernel.Path.from_string (Path.to_string path))) in
    try
      let iov = Iovec.create ~count:2 ~size:12 () in
      Iovec.for_each iov ~fn:(fun segment ->
        Kernel.Bytes.fill segment.buffer ~offset:segment.offset ~len:segment.length ~char:'#');
      let n = expect (File.read_vectored reader (Iovec.sub ~pos:2 ~len:8 iov)) in
      let eof = expect (File.read_vectored reader iov) in
      let _ = expect (File.close reader) in
      if n = 5 && eof = 0 && Iovec.to_string iov = "##abcde#####" then Ok ()
      else Error "short vectored read changed bytes outside returned data"
    with exn -> let _ = File.close reader in raise exn) with
  | Ok result -> result
  | Error error -> Error (IO.error_message error)

let tests = Test.[
  case ~size:Large "scalar file roundtrip retains young buffers under GC" (test_roundtrip ~vectored:false);
  case ~size:Large "vectored file roundtrip retains young buffers under GC" (test_roundtrip ~vectored:true);
  case "short vectored read preserves unread slices at EOF" test_short_vectored_read_preserves_unread_bytes;
]

let () = Actors.run ~config:(Runtime.Config.make ~scheduler_count:2 ())
  ~main:(Test.Cli.main ~name:"kernel:file-gc" ~tests) ~args:Env.args ()
