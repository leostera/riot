(**
   Source snapshot used by the type checker.

   `Source.t` is the checker-owned wrapper around the text being checked. It
   gives parsing, AST construction, diagnostics, and later source-backed fixes a
   common value to thread through APIs instead of passing raw strings
   everywhere.

   The current representation stores only the file text. Keep it opaque so the
   model can grow filename, digest, line-index, or content-store metadata
   without changing call sites.
*)
type t

(** `make ~text` creates a source snapshot from the complete source text. *)
val make: text:string -> t
