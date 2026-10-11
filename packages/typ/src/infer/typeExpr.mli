(**
   Conversion from source-shaped type syntax to inference types.

   `Ast.core_type` is the type syntax retained in `Typ.Ast`; `Ast.Type.t` is the
   algebra manipulated by unification. This module is the boundary between
   those two representations.
*)

(** Convert an arrow label from source syntax to solver type syntax. *)
val arrow_label_to_type_label: Ast.arrow_label -> Ast.Type.Label.t

(** Convert a source type expression into an inference type and annotate it. *)
val core_type_to_type: State.t -> Ast.core_type -> Ast.Type.t

(** Convert an annotation while sharing one solver variable per named type variable. *)
val core_type_to_type_scoped: State.t -> Ast.core_type -> Ast.Type.t

(** Build the nominal type represented by a declaration. *)
val type_declaration_to_type: ?arguments:Ast.Type.t list -> Ast.type_declaration -> Ast.Type.t

(**
   Create fresh solver variables for a declaration's type parameters.

   The returned scope is intended to be installed with `State.with_type_params`
   while converting the declaration body.
*)
val fresh_type_parameters:
  State.t ->
  Ast.type_parameter list ->
  State.type_param_scope * Ast.Type.t list

(**
   Instantiate a record field lookup with fresh type parameters.

   Returns the concrete owner record type and the concrete field type for this
   lookup site.
*)
val instantiate_record_field:
  State.t ->
  State.InferenceEnv.record_field_info ->
  Ast.Type.t * Ast.Type.t
