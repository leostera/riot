open Std

(** Source position used by query APIs. *)
type t = {
  (** Zero-based byte offset into the current source text. *)
  offset: int;
}

(** Build a position from a byte offset. *)
val make: offset:int -> t

(** Test whether a position lies inside one source span, inclusively. *)
val is_within_span: t -> Syn.Ceibo.Span.t -> bool
