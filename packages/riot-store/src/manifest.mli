(** Manifest for tracking stored build artifacts *)
open Std

type version =
  | V2
type file_entry = {
  path: Path.t;
  hash: string;
  size: int;
}
type export_entry = {
  name: string;
  path: Path.t;
  action_hash: string;
}
type t = {
  version: version;
  package: string;
  input_hash: string;
  output_hash: string;
  timestamp: Std.Time.SystemTime.t;
  size_bytes: int64;
  files: file_entry list;
  ocamlc_warnings: string list;
  exports: export_entry list;
}
type metadata = {
  input_hash: string;
  output_hash: string;
  size_bytes: int64;
  ocamlc_warnings: string list;
  exports: export_entry list;
}

val create:
  ?base_dir:Path.t ->
  ?ocamlc_warnings:string list ->
  ?exports:export_entry list ->
  unit ->
  package:string ->
  input_hash:string ->
  files:(Path.t * int) list ->
  t

val create_from_entries:
  ?ocamlc_warnings:string list ->
  ?exports:export_entry list ->
  unit ->
  package:string ->
  input_hash:string ->
  files:file_entry list ->
  t

(** SHA512 hex digest of a file, or [None] when it cannot be read. *)
val hash_file: Path.t -> string option

(** Save manifest to a JSON file. *)
val save: t -> path:Path.t -> (unit, string) result

(** Load manifest from a JSON file. *)
val load: path:Path.t -> (t, string) result

(** Load manifest metadata without decoding the stored file list. *)
val load_metadata: path:Path.t -> (metadata, string) result

(** Save manifest metadata without storing the file list. *)
val save_metadata: metadata -> path:Path.t -> (unit, string) result

val metadata_to_string: metadata -> (string, string) result

val metadata_of_string: string -> (metadata, string) result

val file_entry_serializer: file_entry Serde.Ser.t

val file_entry_deserializer: file_entry Serde.De.t

val export_entry_serializer: export_entry Serde.Ser.t

val export_entry_deserializer: export_entry Serde.De.t

val serializer: t Serde.Ser.t

val deserializer: t Serde.De.t

val metadata_serializer: metadata Serde.Ser.t

val metadata_deserializer: metadata Serde.De.t
