(** Source-level names and dotted paths.

    A `Surface_path.t` is the path shape the source program wrote or the checker
    wants to print back to a user: `x`, `Result.t`, `A.B.value`, and so on.

    Key properties:

    - It is about **surface spelling**, not binding resolution.
    - Two paths with the same text can resolve to different bindings in
      different environments.
    - It is not an identifier validator. Constructors currently accept raw
      strings because the syntax layer has already tokenized the source.

    Use a narrower type, such as a future `ValueName.t`, when the model needs to
    represent one local value binding rather than an arbitrary dotted path. *)
type t

(** Empty path sentinel.

    This is used in places where the model needs a path value before a real
    source path is available. Prefer a domain-specific `option` in new APIs
    when absence is meaningful. *)
val empty: t

(** `is_empty path` is `true` only for `empty`. *)
val is_empty: t -> bool

(** `from_name name` builds a single-segment surface path.

    The string is stored as provided. This function does not check whether
    `name` is a valid OCaml/Riot identifier, constructor name, operator name, or
    module name. *)
val from_name: string -> t

(** `from_segments segments` builds a dotted path from left to right.

    For example, `from_segments ["A"; "B"; "t"]` represents `A.B.t`. An empty
    list produces `empty`. Segment strings are stored as provided. *)
val from_segments: string list -> t

(** `to_segments path` returns the path segments from left to right.

    For `empty`, this returns `[]`. *)
val to_segments: t -> string list

(** `to_string path` renders the path by joining segments with dots.

    For `empty`, this returns the empty string. *)
val to_string: t -> string

(** Structural equality over path segments. *)
val equal: t -> t -> bool

(** Structural ordering over path segments, suitable for deterministic maps,
    sets, and snapshots. *)
val compare: t -> t -> Std.Order.t

(** Serializer for persisting surface paths in checker summaries and snapshots. *)
val serializer: t Serde.Ser.t
