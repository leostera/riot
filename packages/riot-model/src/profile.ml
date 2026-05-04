open Std

module Vector = Collections.Vector

(** Override behavior - either inherit from base or override with new value *)
type 'a override =
  | Inherit
  (** Keep the value from the base profile *)
  | Override of 'a

(** Replace with this value *)

(** Profile override - partially specified profile fields *)
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
let debug = {
  name = "debug";
  kind = Ocaml_compiler.Native;
  inline = Some 0;
  no_assert = false;
  compact = false;
  unsafe = false;
  no_alias_deps = false;
  open_modules = [];
  warnings = [];
  errors = Ocaml_compiler.[ LabelsOmitted; PartialMatch; UnusedVariable; UnusedOpen; UnusedMatch; ];
  cc_flags = [];
  ld_flags = [];
  ocamlc_flags = [ "-g" ];
}

(** Default release profile - optimized, strict *)
let release = {
  name = "release";
  kind = Ocaml_compiler.Native;
  inline = Some 100;
  no_assert = true;
  compact = true;
  unsafe = false;
  no_alias_deps = false;
  open_modules = [];
  warnings = [];
  errors = [ Ocaml_compiler.All ];
  cc_flags = [];
  ld_flags = [];
  ocamlc_flags = [];
}

(** Default fuzz profile - native debug build with AFL-compatible instrumentation *)
let fuzz = {
  name = "fuzz";
  kind = Ocaml_compiler.Native;
  inline = Some 0;
  no_assert = false;
  compact = false;
  unsafe = false;
  no_alias_deps = false;
  open_modules = [];
  warnings = [];
  errors = Ocaml_compiler.[ LabelsOmitted; PartialMatch; UnusedVariable; UnusedOpen; UnusedMatch; ];
  cc_flags = [];
  ld_flags = [];
  ocamlc_flags = [ "-g"; "-afl-instrument" ];
}

(** Merge two profiles - override takes precedence per-field *)
let merge = fun base override ->
  {
    name = override.name;
    kind = override.kind;
    inline =
      (
        match override.inline with
        | None -> base.inline
        | some -> some
      );
    no_assert = override.no_assert;
    compact = override.compact;
    unsafe = override.unsafe;
    no_alias_deps = override.no_alias_deps;
    open_modules = override.open_modules;
    warnings = override.warnings;
    errors = override.errors;
    cc_flags = base.cc_flags @ override.cc_flags;
    ld_flags = base.ld_flags @ override.ld_flags;
    ocamlc_flags = base.ocamlc_flags @ override.ocamlc_flags;
  }

(** Apply a profile override to a base profile *)
let apply_override = fun base (override: profile_override) ->
  {
    name = base.name;
    kind =
      (
        match override.kind with
        | Inherit -> base.kind
        | Override k -> k
      );
    inline =
      (
        match override.inline with
        | Inherit -> base.inline
        | Override opt -> opt
      );
    no_assert =
      (
        match override.no_assert with
        | Inherit -> base.no_assert
        | Override b -> b
      );
    compact =
      (
        match override.compact with
        | Inherit -> base.compact
        | Override b -> b
      );
    unsafe =
      (
        match override.unsafe with
        | Inherit -> base.unsafe
        | Override b -> b
      );
    no_alias_deps =
      (
        match override.no_alias_deps with
        | Inherit -> base.no_alias_deps
        | Override b -> b
      );
    open_modules =
      (
        match override.open_modules with
        | Inherit -> base.open_modules
        | Override l -> l
      );
    warnings =
      (
        match override.warnings with
        | Inherit -> base.warnings
        | Override l -> l
      );
    errors =
      (
        match override.errors with
        | Inherit -> base.errors
        | Override l -> l
      );
    cc_flags =
      (
        match override.cc_flags with
        | Inherit -> base.cc_flags
        | Override l -> base.cc_flags @ l
      );
    ld_flags =
      (
        match override.ld_flags with
        | Inherit -> base.ld_flags
        | Override l -> base.ld_flags @ l
      );
    ocamlc_flags =
      (
        match override.ocamlc_flags with
        | Inherit -> base.ocamlc_flags
        | Override l -> base.ocamlc_flags @ l
      );
  }

(** Apply overrides from a list by looking up the profile's name *)
let apply_overrides = fun base overrides ->
  Log.debug ("[PROFILE] apply_overrides: looking for profile '" ^ base.name ^ "' in overrides");
  Log.debug
    ("[PROFILE] available overrides: "
    ^ String.concat ", " (List.map overrides ~fn:(fun (name, _override) -> name)));
  match Fields.get base.name overrides with
  | None ->
      Log.debug ("[PROFILE] No override found for '" ^ base.name ^ "'");
      base
  | Some override ->
      Log.debug ("[PROFILE] Found override for '" ^ base.name ^ "', applying");
      apply_override base override

(** Parse profile_override from TOML table *)
let override_from_toml: (string * Std.Data.Toml.value) list -> profile_override = fun table_items ->
  let open Std.Data.Toml in
  let get_string_list key =
    match Fields.get key table_items with
    | Some (Array arr) ->
        Override (
          List.filter_map
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | String s -> Some s
              | _ -> None)
            arr
        )
    | _ -> Inherit
  in
  let get_int_opt key =
    match Fields.get key table_items with
    | Some (String s) -> (
        match Int.parse s with
        | Some value -> Override (Some value)
        | None -> Inherit
      )
    | _ -> Inherit
  in
  let get_bool key =
    match Fields.get key table_items with
    | Some (Bool b) -> Override b
    | _ -> Inherit
  in
  let get_compilation_kind () =
    match Fields.get "kind" table_items with
    | Some (String "bytecode") -> Override Ocaml_compiler.Bytecode
    | Some (String "native") -> Override Ocaml_compiler.Native
    | _ -> Inherit
  in
  {
    kind = get_compilation_kind ();
    inline = get_int_opt "inline";
    no_assert = get_bool "no_assert";
    compact = get_bool "compact";
    unsafe = get_bool "unsafe";
    no_alias_deps = get_bool "no_alias_deps";
    open_modules = get_string_list "open_modules";
    warnings = Inherit;
    errors = Inherit;
    cc_flags = get_string_list "cc_flags";
    ld_flags = get_string_list "ld_flags";
    ocamlc_flags = get_string_list "ocamlc_flags";
  }

(** Parse profile from TOML table *)
let from_toml: (string * Std.Data.Toml.value) list -> base:t -> t = fun table_items ~base ->
  let open Std.Data.Toml in
  let get_string_list key =
    match Fields.get key table_items with
    | Some (Array arr) ->
        List.filter_map
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | String s -> Some s
            | _ -> None)
          arr
    | _ -> base.open_modules
  in
  let get_int_opt key =
    match Fields.get key table_items with
    | Some (String s) -> (
        match Int.parse s with
        | Some value -> Some value
        | None -> base.inline
      )
    | _ -> base.inline
  in
  let get_bool key default =
    match Fields.get key table_items with
    | Some (Bool b) -> b
    | _ -> default
  in
  let get_compilation_kind () =
    match Fields.get "kind" table_items with
    | Some (String "bytecode") -> Ocaml_compiler.Bytecode
    | Some (String "native") -> Ocaml_compiler.Native
    | _ -> base.kind
  in
  {
    name = base.name;
    kind = get_compilation_kind ();
    inline = get_int_opt "inline";
    no_assert = get_bool "no_assert" base.no_assert;
    compact = get_bool "compact" base.compact;
    unsafe = get_bool "unsafe" base.unsafe;
    no_alias_deps = get_bool "no_alias_deps" base.no_alias_deps;
    open_modules = get_string_list "open_modules";
    warnings = base.warnings;
    errors = base.errors;
    cc_flags =
      (
        match Fields.get "cc_flags" table_items with
        | Some (Array arr) ->
            List.filter_map
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | String s -> Some s
                | _ -> None)
              arr
        | _ -> base.cc_flags
      );
    ld_flags =
      (
        match Fields.get "ld_flags" table_items with
        | Some (Array arr) ->
            List.filter_map
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | String s -> Some s
                | _ -> None)
              arr
        | _ -> base.ld_flags
      );
    ocamlc_flags =
      (
        match Fields.get "ocamlc_flags" table_items with
        | Some (Array arr) ->
            List.filter_map
              ~fn:(fun __tmp1 ->
                match __tmp1 with
                | String s -> Some s
                | _ -> None)
              arr
        | _ -> base.ocamlc_flags
      );
  }

(** Convert profile to OCaml compiler flags *)
let to_compiler_flags = fun profile ->
  let flags =
    Vector.with_capacity
      ~size:(7 + List.length profile.open_modules + List.length profile.ocamlc_flags)
  in
  (
    match profile.inline with
    | Some n -> Vector.push flags ~value:(Ocaml_compiler.Inline n)
    | None -> ()
  );
  (
    if profile.no_assert then
      Vector.push flags ~value:Ocaml_compiler.NoAssert
  );
  (
    if profile.compact then
      Vector.push flags ~value:Ocaml_compiler.Compact
  );
  (
    if profile.unsafe then
      Vector.push flags ~value:Ocaml_compiler.Unsafe
  );
  (
    if profile.no_alias_deps then
      Vector.push flags ~value:Ocaml_compiler.NoAliasDeps
  );
  profile.open_modules
  |> List.for_each ~fn:(fun m -> Vector.push flags ~value:(Ocaml_compiler.Open m));
  (
    if List.is_empty profile.warnings then
      ()
    else
      Vector.push flags ~value:(Ocaml_compiler.Warning profile.warnings)
  );
  (
    if List.is_empty profile.errors then
      ()
    else
      Vector.push flags ~value:(Ocaml_compiler.WarnError profile.errors)
  );
  profile.ocamlc_flags
  |> List.for_each ~fn:(fun flag -> Vector.push flags ~value:(Ocaml_compiler.Raw flag));
  Ocaml_compiler.flags_to_string
    (
      Vector.to_array flags
      |> Array.to_list
    )

(** Hash profile into a Sha256 hasher state *)
let hash = fun state profile ->
  let module H = Crypto.Sha256 in
  H.write state profile.name;
  H.write
    state
    (
      match profile.kind with
      | Ocaml_compiler.Bytecode -> "bytecode"
      | Native -> "native"
    );
  (
    match profile.inline with
    | Some n -> H.write state (Int.to_string n)
    | None -> H.write state "none"
  );
  H.write state (Bool.to_string profile.no_assert);
  H.write state (Bool.to_string profile.compact);
  H.write state (Bool.to_string profile.unsafe);
  H.write state (Bool.to_string profile.no_alias_deps);
  List.for_each profile.open_modules ~fn:(H.write state);
  List.for_each profile.warnings ~fn:(fun w -> H.write state (Ocaml_compiler.warning_to_string w));
  List.for_each profile.errors ~fn:(fun w -> H.write state (Ocaml_compiler.warning_to_string w));
  List.for_each profile.cc_flags ~fn:(H.write state);
  List.for_each profile.ld_flags ~fn:(H.write state);
  List.for_each profile.ocamlc_flags ~fn:(H.write state)

(** Convert profile to JSON *)
let to_json = fun profile ->
  let open Data.Json in
  obj
    [
      ("name", string profile.name);
      ("kind", string
        (
          match profile.kind with
          | Ocaml_compiler.Bytecode -> "bytecode"
          | Native -> "native"
        ));
      ("inline", match profile.inline with
      | Some n -> int n
      | None -> Null);
      ("no_assert", bool profile.no_assert);
      ("compact", bool profile.compact);
      ("unsafe", bool profile.unsafe);
      ("no_alias_deps", bool profile.no_alias_deps);
      ("open_modules", array (List.map profile.open_modules ~fn:string));
      ("cc_flags", array (List.map profile.cc_flags ~fn:string));
      ("ld_flags", array (List.map profile.ld_flags ~fn:string));
      ("ocamlc_flags", array (List.map profile.ocamlc_flags ~fn:string));
    ]
