(** Render a checked file as an inferred interface.

    This is the temporary rendering bridge for the existing `Typ.Check` path.
    It turns checked top-level bindings into `.mli`-style text so fixture tests
    can compare inferred public types against the OCaml oracle output.

    The new `Typ.Infer` path will likely grow its own renderer over
    `Infer.ModuleInterface.t`; keep this module focused on the old
    `Check.Typings.t` result shape until that handoff happens. *)
val from_typings: Check.Typings.t -> string
