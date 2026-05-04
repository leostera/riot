open Std

(** Controls whether a profile field inherits its base value or supplies an override. *)
type 'a override =
  | Inherit
  (** Keep the value from the base profile *)
  | Override of 'a

(** Replace with this value *)

(** Partially specified profile fields. *)
type profile_override = {
  kind: Ocaml_compiler.compilation_kind override;
  inline: int option override;
  no_assert: bool override;
  compact: bool override;
  unsafe: bool override;
  no_alias_deps: bool override;
  open_modules: string list override;
  warnings: Ocaml_compiler.warning list override;
  errors: Ocaml_compiler.warning list override;
  cc_flags: string list override;
  ld_flags: string list override;
  ocamlc_flags: string list override;
}
(** Build profile configuration with individual flag fields *)
type t = {
  name: string;
  (** Profile name: "debug", "release", etc. *)
  kind: Ocaml_compiler.compilation_kind;
  (* Optimization flags *)
  inline: int option;
  (** -inline N: inlining threshold *)
  no_assert: bool;
  (** -noassert: remove assertions *)
  compact: bool;
  (** -compact: optimize for code size *)
  unsafe: bool;
  (** -unsafe: disable bounds checking *)
  (* Module handling *)
  no_alias_deps: bool;
  (** -no-alias-deps: no deps for module aliases *)
  open_modules: string list;
  (** -open Module: auto-open modules *)
  (* Warnings configuration *)
  warnings: Ocaml_compiler.warning list;
  (** Warnings to enable *)
  errors: Ocaml_compiler.warning list;
  (** Warnings to treat as errors *)
  (* Additional flags *)
  cc_flags: string list;
  (** C compiler flags (passed with -ccopt) *)
  ld_flags: string list;
  (** Linker flags (passed with -cclib) *)
  ocamlc_flags: string list;
  (** Additional raw ocamlc/ocamlopt flags *)
}

(** Default debug profile - native code with debug symbols and minimal optimization *)
val debug: t

(** Default release profile - optimized, strict *)
val release: t

(** Default fuzz profile - native debug build with AFL-compatible instrumentation *)
val fuzz: t

(**
   Merge two profiles - override takes precedence per-field
   For booleans and kind: replaced
   For lists: appended (cc_flags, ld_flags, ocamlc_flags) or replaced (warnings, errors, open_modules)
   For optional int: override if Some, keep base if None
*)
val merge: t -> t -> t

(** Apply a profile_override to a base profile *)
val apply_override: t -> profile_override -> t

(** Apply overrides from a list by looking up the profile's name *)
val apply_overrides: t -> (string * profile_override) list -> t

(** Parse profile from TOML table, using base as defaults for missing fields *)
val from_toml: (string * Std.Data.Toml.value) list -> base:t -> t

(** Parse profile_override from TOML table *)
val override_from_toml: (string * Std.Data.Toml.value) list -> profile_override

(** Convert profile to OCaml compiler flags list *)
val to_compiler_flags: t -> string list

(** Hash profile into a Sha256 hasher state *)
val hash: Crypto.Sha256.state -> t -> unit

(** Convert profile to JSON *)
val to_json: t -> Std.Data.Json.t
