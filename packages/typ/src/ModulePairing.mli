open Std

(** Paired module result for one logical module name.

    A logical module may be backed by an implementation source, an interface
    source, or both. Pairing reconciles those per-source analyses into one
    canonical [ModuleTypings.t] value and adjusted per-source analyses that
    include signature-inclusion diagnostics when an implementation does not
    satisfy its interface. *)
type t = {
  (** Canonical reusable module typings for the logical module. *)
  module_typings: ModuleTypings.t;
  (** Per-source analyses adjusted with signature-inclusion diagnostics. *)
  analyses_by_source: (SourceId.t * SourceAnalysis.t) list;
}

(** Pair all analyzed sources for one logical module name. *)
val of_sources: module_name:string -> (Source.t * SourceAnalysis.t) list -> t
