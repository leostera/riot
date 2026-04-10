(** The extensible actor message type. Runtime and application modules extend
    it with their own variants. *)
type t = ..
(** A message wrapped with a unique envelope identifier. *)
type envelope = private {
  msg: t;
  uid: int;
}

(** Wrap a message in a fresh envelope. *)
val envelope: t -> envelope
