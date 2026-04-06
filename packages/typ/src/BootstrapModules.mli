open Std

(** Seeded module typings used by the prototype host configuration.

    These typings are a temporary bridge while the build and LSP hosts learn
    to load real type exports from cached package outputs. They intentionally
    use the same [ModuleTypings] shape that those hosts will later persist in
    and restore from [riot_store]. *)
val summaries: ModuleTypings.t list
