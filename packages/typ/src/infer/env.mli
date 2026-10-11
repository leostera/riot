open Std

(**
   Query-local inference environment.

   `Env.t` owns the semantic lookup tables used by the new one-shot checker.
   Value bindings are lexically scoped. Type declarations, constructors, and
   nested modules are flat within the current module and are resolved by walking
   the current module chain outward.
*)
type t
(**
   Exported contents of a checked module.

   A summary stores only the names introduced directly by that module. Parent
   module bindings are intentionally not copied into child summaries.
*)
type module_summary
(**
   Record-field lookup payload.

   Record fields are registered from record type declarations. `owner` is the
   type declaration that introduced the field, and `field` is the field
   declaration itself.
*)
type record_field_info = {
  owner: Ast.type_declaration;
  field: Ast.record_field_declaration;
}
(**
   Field metadata for an inline-record constructor payload.

   Inline-record fields are not registered in the ordinary record-field
   namespace. They are resolved through the constructor description that owns
   them.
*)
type inline_record_field = {
  declaration: Ast.record_field_declaration;
  type_: Ast.Type.t;
}
(**
   Hidden record payload attached to a variant constructor.

   This mirrors OCaml's inlined-record constructor descriptions: the payload has
   a nominal semantic type, but no normal surface type binding.
*)
type inline_record = {
  owner: Ast.type_declaration;
  constructor: Ast.type_constructor;
  payload_type: Ast.Type.t;
  fields: inline_record_field list;
}
(** Typed constructor argument metadata. *)
type constructor_arguments =
  | Tuple of Ast.Type.t list
  | InlineRecord of inline_record
(**
   Rich constructor lookup payload.

   Constructors are callable values, but the checker also needs their semantic
   argument shape to type constructor-specific syntax such as inline records.
*)
type constructor_description = {
  name: Ast.ident;
  scheme: TypeScheme.t;
  result: Ast.Type.t;
  arguments: constructor_arguments;
}

(** Create an empty environment with one root module. *)
val create: unit -> t

(**
   Push a lexical value scope inside the current module.

   Values added in the new scope shadow outer values and disappear after
   `pop_scope`. Type, constructor, and module declarations are not affected by
   lexical value scopes.
*)
val push_scope: t -> t

(**
   Pop the current lexical value scope.

   Popping the root value scope is a no-op.
*)
val pop_scope: t -> t

(**
   Enter a nested module.

   The new module starts with empty direct exports but can resolve unqualified
   names through its parent module chain.
*)
val push_module: t -> name:Ast.ident -> t

(**
   Leave the current nested module.

   The popped module is summarized and registered in the parent module table.
   Popping the root module is a no-op.
*)
val pop_module: t -> t

(**
   Add or replace a value binding in the current lexical value scope.

   At the current module root this creates or updates an exported value. Inside
   a pushed value scope it creates a local value that is not exported.
*)
val add_value: t -> name:Ast.ident -> scheme:TypeScheme.t -> t

(** True when a value binding exists in the current scope/module chain. *)
val has_value: t -> name:Ast.ident -> bool

(** Find the nearest type scheme currently bound to `name`, if any. *)
val get_value: t -> name:Ast.ident -> TypeScheme.t option

(**
   Add or replace a value-constructor binding in the current module.

   Constructors share expression syntax with values but live in a separate
   namespace. Lexical value scopes do not affect where constructors are stored.
*)
val add_constructor: t -> name:Ast.ident -> description:constructor_description -> t

(** True when a constructor exists in the current module chain. *)
val has_constructor: t -> name:Ast.ident -> bool

(** Find the nearest constructor description currently bound to `name`, if any. *)
val get_constructor: t -> name:Ast.ident -> constructor_description option

(**
   Add or replace a record-field binding in the current module.

   Record fields live in their own module-level namespace. Lexical value scopes
   do not affect where fields are stored; this should be called while
   registering a record type declaration.
*)
val add_record_field: t -> name:Ast.ident -> info:record_field_info -> t

(** True when a record field exists in the current module chain. *)
val has_record_field: t -> name:Ast.ident -> bool

(** Find the nearest record field currently bound to `name`, if any. *)
val get_record_field: t -> name:Ast.ident -> record_field_info option

(**
   Add or replace a type declaration in the current module.

   Type declarations live in their own namespace. Lexical value scopes do not
   affect where types are stored.
*)
val add_type: t -> name:Ast.ident -> declaration:Ast.type_declaration -> t

(** True when a type declaration exists in the current module chain. *)
val has_type: t -> name:Ast.ident -> bool

(** Find the nearest type declaration currently bound to `name`, if any. *)
val get_type: t -> name:Ast.ident -> Ast.type_declaration option

(** True when a nested module exists in the current module chain. *)
val has_module: t -> name:Ast.ident -> bool

(** Find a nested module summary by unqualified name. *)
val get_module: t -> name:Ast.ident -> module_summary option

(** Resolve a nested module summary by path segments. *)
val get_module_path: t -> path:Ast.ident list -> module_summary option

(**
   Open a module summary into unqualified lookups at this source position.

   A later open shadows earlier bindings it provides; later declarations in the
   current module shadow names introduced by the open. Opened names are not
   copied into the module's exports.
*)
val open_module: t -> path:Ast.ident -> t option

(** Add a module alias for an already-bound target path. *)
val add_module_alias: t -> name:Ast.ident -> target:Ast.ident -> t option

(**
   Iterate over values exported directly by a module summary.

   Parent-module bindings are intentionally not included.
*)
val module_values: module_summary -> (Ast.ident * TypeScheme.t) Std.Iter.Iterator.t

(** Find a value exported directly by a module summary. *)
val module_get_value: module_summary -> name:Ast.ident -> TypeScheme.t option

(** True when a module summary exports `name` as a value. *)
val module_has_value: module_summary -> name:Ast.ident -> bool

(** Find a constructor exported directly by a module summary. *)
val module_get_constructor: module_summary -> name:Ast.ident -> constructor_description option

(** True when a module summary exports `name` as a constructor. *)
val module_has_constructor: module_summary -> name:Ast.ident -> bool

(** Find a record field exported directly by a module summary. *)
val module_get_record_field: module_summary -> name:Ast.ident -> record_field_info option

(** True when a module summary exports `name` as a record field. *)
val module_has_record_field: module_summary -> name:Ast.ident -> bool

(**
   Iterate over types exported directly by a module summary.

   Parent-module bindings are intentionally not included.
*)
val module_types: module_summary -> (Ast.ident * Ast.type_declaration) Std.Iter.Iterator.t

(** Find a type exported directly by a module summary. *)
val module_get_type: module_summary -> name:Ast.ident -> Ast.type_declaration option

(** True when a module summary exports `name` as a type. *)
val module_has_type: module_summary -> name:Ast.ident -> bool

(** Find a nested module exported directly by a module summary. *)
val module_get_module: module_summary -> name:Ast.ident -> module_summary option

(** True when a module summary exports `name` as a nested module. *)
val module_has_module: module_summary -> name:Ast.ident -> bool

(** Iterate over nested modules exported directly by a module summary. *)
val module_modules: module_summary -> (Ast.ident * module_summary) Std.Iter.Iterator.t

(**
   Iterate over exported value bindings from the root module.

   Local lexical scopes and nested-module contents are intentionally ignored.
   Bindings are yielded in source addition order, with replacement bindings
   ordered by their last addition.
*)
val exports: t -> (Ast.ident * TypeScheme.t) Iter.Iterator.t

(**
   Iterate over exported type declarations from the root module.

   Nested-module contents are intentionally ignored. Declarations are yielded in
   source addition order, with replacement declarations ordered by their last
   addition.
*)
val exported_types: t -> (Ast.ident * Ast.type_declaration) Iter.Iterator.t

(**
   Iterate over nested modules exported directly by the root module.

   Nested-module contents are exposed as module summaries so callers can copy
   or render them recursively.
*)
val exported_modules: t -> (Ast.ident * module_summary) Iter.Iterator.t
