open Std

(** Lower a clean [Syn.Cst] file into the prototype semantic layers.

    Unsupported syntax is preserved through recovery items or hole expressions,
    and the resulting lowering diagnostics are stored directly on the returned
    [SemanticTree.file].
*)
val lower_source_file: source:Source.t -> Syn.Cst.source_file -> SemanticTree.file
