open Std

(** Mutable prototype type representation used inside one inference query. *)
type label =
  (** Ordinary unlabeled arrow parameter. *)
  | Nolabel
  (** Labeled arrow parameter introduced with `~label:`. *)
  | Labelled of string
  (** Optional arrow parameter introduced with `?label:`. *)
  | Optional of string
type var = {
  (** Stable inference-variable identity inside one query. *)
  id: int;
  (** Query-local mutable link used by unification. *)
  mutable link: t option;
}

and t =
  (** Built-in integer type. *)
  | Int
  (** Built-in floating-point type. *)
  | Float
  (** Built-in boolean type. *)
  | Bool
  (** Built-in string type. *)
  | String
  (** Built-in character type. *)
  | Char
  (** Built-in unit type. *)
  | Unit
  (** Built-in option type. *)
  | Option of t
  (** Built-in result type. *)
  | Result of t * t
  (** Built-in array type. *)
  | Array of t
  (** Built-in list type. *)
  | List of t
  (** Sequence type used by helpers such as [String.to_seq] and [List.of_seq]. *)
  | Seq of t
  (** Named algebraic or abstract type, optionally applied to arguments. *)
  | Named of { name: string; arguments: t list }
  (** Tuple type. *)
  | Tuple of t list
  (** Function type. *)
  | Arrow of { label: label; lhs: t; rhs: t }
  (** Inference variable. *)
  | Var of var
  (** Recovery hole produced by lenient lowering or inference. *)
  | Hole of int

(** Chase mutable links until a canonical representative is reached. *)
val prune: t -> t

(** Set-like union over integer identifiers while preserving left-to-right bias. *)
val union: int list -> int list -> int list

(** Remove every element of the right list from the left list. *)
val diff: int list -> int list -> int list

(** Collect free inference-variable identifiers from a type. *)
val free_vars: t -> int list

(** Collect the free inference-variable identifiers that occur only covariantly
    in the type. *)
val covariant_vars: t -> int list

(** Check whether the given inference variable occurs inside the type. *)
val occurs: int -> t -> bool
