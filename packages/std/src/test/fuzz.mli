open Global

(** Fuzzing metadata for [Std.Test.fuzz] cases. *)
module Corpus: sig
  type t

  val empty: t

  val bytes: string list -> t

  val strings: string list -> t

  val file: Path.t -> t

  val files: Path.t list -> t

  val dir: ?extensions:string list -> Path.t -> t

  val merge: t list -> t

  val inline_inputs: t -> string list

  val file_paths: t -> Path.t list

  val replay_inputs: t -> (string * string) list
end

module Mutator: sig
  type t = {
    dictionary: string list;
    max_len: int option;
    splicing: bool;
  }

  val bytes: t

  val text: t

  val dictionary: string list -> t

  val with_dictionary: string list -> t -> t

  val with_max_len: int -> t -> t

  val with_splicing: t -> t

  val without_splicing: t -> t
end
