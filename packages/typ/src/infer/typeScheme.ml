type mode =
  | Local
  | Generalized

type t = {
  (** Generic variable identities that are instantiated at each value use. *)
  quantifier: Ast.TypeVar.t list;
  (** Type body; unlisted generic identities remain rigid in this scheme. *)
  body: Ast.Type.t;
}

let monomorphic body = { quantifier = []; body }
