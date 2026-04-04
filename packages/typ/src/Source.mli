open Std

(** Logical source tracked by one [Session]. *)
type kind =
  (** Source text backed by an on-disk file. *)
  | File
  (** Source text synthesized for a fragment-oriented query. *)
  | Fragment
  (** Source text produced by a generator or macro lane. *)
  | Generated

type origin =
  (** Host path for a file-backed source. *)
  | Path of Path.t
  (** Human-readable label for fragments and synthetic inputs. *)
  | Label of string

type t = {
  (** Stable identity preserved across text updates to this logical source. *)
  source_id: SourceId.t;
  (** Host-declared source category. *)
  kind: kind;
  (** Host-owned origin label. *)
  origin: origin;
  (** Current full text for this source revision. *)
  text: string;
  (** Monotonic revision number for this source snapshot. *)
  revision: int;
}

(** Build one logical source record. *)
val make:
  source_id:SourceId.t ->
  kind:kind ->
  origin:origin ->
  revision:int ->
  text:string ->
  t

(** Replace the source text while preserving [source_id]. *)
val update_text: t -> revision:int -> text:string -> t

(** Render the best available human-facing label for this source. *)
val display_name: t -> string
