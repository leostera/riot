(** Interface rendering helpers. *)
open Std

(** Render a checked file as an inferred interface.

    This is the temporary rendering bridge for the existing `Typ.Check` path.
    It turns checked top-level bindings into `.mli`-style text so fixture tests
    can compare inferred public types against the OCaml oracle output.

    The new `Typ.Infer` path will likely grow its own renderer over
    `Infer.ModuleInterface.t`; keep this module focused on the old
    `Check.Typings.t` result shape until that handoff happens. *)
val from_typings: Check.Typings.t -> string

(** Render exported values as an inferred interface.

    This is the narrow bridge used by the new `Typ.Infer` path. The caller owns
    the ordering of the iterator, so environments that need source-order output
    should provide a source-ordered stream. *)
val from_values: (Ast.ident * Ast.Type.t) Iter.Iterator.t -> string
