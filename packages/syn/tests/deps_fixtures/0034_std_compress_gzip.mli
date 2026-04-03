open Global

(** Gzip codec failures. *)
type error =
  | Kernel_error of Kernel.Compress.Gzip.error
  (** The underlying incremental gzip engine rejected the stream. *)
  | Truncated_input
(** The gzip stream ended before the decoder reached a complete end state. *)
(** Opaque reader state produced by [`to_reader`]. *)
type ('src, 'read_err) reader
(** Errors returned by the reader produced from a gzip source. *)
type 'read_err read_error =
  | Source_error of 'read_err
  (** The upstream compressed source reader failed. *)
  | Gzip_error of error
(** The gzip payload was malformed or incomplete. *)
(** Errors returned by streaming decompression into a writer. *)
type ('read_err, 'write_err) stream_error =
  | Stream_source_error of 'read_err
  (** The compressed source reader failed. *)
  | Stream_destination_error of 'write_err
  (** The destination writer failed. *)
  | Stream_gzip_error of error
(** The gzip engine rejected the payload or output stream. *)
(** Errors returned by file-based decompression helpers. *)
type file_error =
  | File_io_error of Fs.error
  (** Opening, reading, writing, or closing a file failed. *)
  | File_gzip_error of error

(** The gzip engine rejected the payload or output stream. *)

(** Wrap a compressed reader as a decompressed reader.

    The returned reader incrementally inflates gzip data as it is consumed.
    This is the most composable entry point and is intended to layer naturally
    with other streaming APIs such as [`Std.Archive.Tar.entries`] or
    [`Std.Archive.Tar.extract`].

    ## Example

    ```ocaml
    open Std

    let extract_tar_gz archive_path output_dir =
      match Fs.File.open_read archive_path with
      | Error _ -> Error "failed to open archive"
      | Ok file ->
          Kernel.Fun.protect
            ~finally:(fun () -> ignore (Fs.File.close file))
            (fun () ->
              let gz_reader = Compress.Gzip.to_reader (Fs.File.to_reader file) in
              Archive.Tar.extract gz_reader ~into:output_dir
              |> Result.map_err (fun _ -> "failed to extract tar.gz"))
    ```
*)
val to_reader:
  ('src, 'read_err) IO.Reader.t -> (('src, 'read_err) reader, 'read_err read_error) IO.Reader.t

(** Stream-compress data from a reader into a gzip writer.

    This function reads uncompressed bytes from the source reader, emits gzip
    bytes into the destination writer, and flushes the destination writer when
    compression completes.

    ## Example

    ```ocaml
    open Std

    let gzip src dst =
      match Compress.Gzip.compress src dst with
      | Ok () -> Log.info "stream compressed"
      | Error _ -> Log.error "gzip compression failed"
    ```
*)
val compress:
  ('src, 'read_err) IO.Reader.t ->
  ('dst, 'write_err) IO.Writer.t ->
  (unit, ('read_err, 'write_err) stream_error) result

(** Stream-decompress gzip data from a reader into a writer.

    This function processes input incrementally and flushes the destination
    writer when decompression completes.

    ## Example

    ```ocaml
    open Std

    let gunzip src dst =
      match Compress.Gzip.decompress src dst with
      | Ok () -> Log.info "stream decompressed"
      | Error _ -> Log.error "gzip decompression failed"
    ```
*)
val decompress:
  ('src, 'read_err) IO.Reader.t ->
  ('dst, 'write_err) IO.Writer.t ->
  (unit, ('read_err, 'write_err) stream_error) result

(** Compress a file into gzip format.

    ## Example

    ```ocaml
    open Std

    let () =
      match Compress.Gzip.compress_file ~src:(Path.v "payload.txt") ~dst:(Path.v "payload.txt.gz") with
      | Ok () -> Log.info "file compressed"
      | Error _ -> Log.error "file compression failed"
    ```
*)
val compress_file: src:Path.t -> dst:Path.t -> (unit, file_error) result

(** Decompress a gzip-compressed file into another file.

    ## Example

    ```ocaml
    open Std

    let () =
      match Compress.Gzip.decompress_file ~src:(Path.v "payload.txt.gz") ~dst:(Path.v "payload.txt") with
      | Ok () -> Log.info "file decompressed"
      | Error _ -> Log.error "file compression failed"
    ```
*)
val decompress_file: src:Path.t -> dst:Path.t -> (unit, file_error) result

(** Compress a string entirely in memory.

    This is a convenience helper for tests, fixtures, and small payloads. Use
    [`compress`] for large or streaming inputs.

    ## Example

    ```ocaml
    open Std

    match Compress.Gzip.compress_string "hello\n" with
    | Ok payload -> Log.info "encoded %d bytes" (String.length payload)
    | Error _ -> Log.error "failed to encode gzip payload"
    ```
*)
val compress_string: string -> (string, error) result

(** Decompress a gzip-compressed string entirely in memory.

    This is a convenience helper for tests, fixtures, and small payloads. Use
    [`to_reader`] or [`decompress`] for large or streaming inputs.

    ## Example

    ```ocaml
    open Std

    let payload = "\x1f\x8b..." in
    match Compress.Gzip.decompress_string payload with
    | Ok text -> Log.info "decoded: %s" text
    | Error _ -> Log.error "invalid gzip payload"
    ```
*)
val decompress_string: string -> (string, error) result
