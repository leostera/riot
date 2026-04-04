open Std

(** Body-local semantic structure for one lowered source. *)
type pattern_desc =
  (** Variable binder pattern. *)
  | PVar of string
  (** Wildcard pattern. *)
  | PWildcard
  (** Integer literal pattern. *)
  | PInt of string
  (** Boolean literal pattern. *)
  | PBool of bool
  (** String literal pattern. *)
  | PString of string
  (** Unit pattern. *)
  | PUnit
  (** Tuple pattern with child pattern IDs. *)
  | PTuple of PatId.t list
  (** Recovery pattern preserved after unsupported surface syntax. *)
  | PUnsupported of string

type pattern_node = {
  (** Best-effort stable pattern identifier. *)
  pat_id: PatId.t;
  (** Source origin for this pattern node. *)
  origin_id: OriginId.t;
  (** Semantic payload for the pattern. *)
  desc: pattern_desc;
}

type match_case = {
  (** Pattern tested by this case. *)
  pattern_id: PatId.t;
  (** Body expression evaluated when the pattern matches. *)
  body_id: ExprId.t;
}

type expr_desc =
  (** Variable reference. *)
  | EVar of string
  (** Integer literal expression. *)
  | EInt of string
  (** Boolean literal expression. *)
  | EBool of bool
  (** String literal expression. *)
  | EString of string
  (** Unit expression. *)
  | EUnit
  (** Tuple expression with child expression IDs. *)
  | ETuple of ExprId.t list
  (** Function expression with parameter patterns and one body expression. *)
  | EFun of PatId.t list * ExprId.t
  (** Application with one callee and positional arguments. *)
  | EApply of ExprId.t * ExprId.t list
  (** Let-expression with local binding IDs and one body expression. *)
  | ELet of BindingId.t list * ExprId.t
  (** Conditional expression. *)
  | EIf of ExprId.t * ExprId.t * ExprId.t
  (** Match expression with normalized cases. *)
  | EMatch of ExprId.t * match_case list
  (** Unsupported semantic node that still reached the inferencer. *)
  | EUnsupported of string
  (** Recovery hole introduced during lowering. *)
  | EHole of string

and expr_node = {
  (** Best-effort stable expression identifier. *)
  expr_id: ExprId.t;
  (** Source origin for this expression node. *)
  origin_id: OriginId.t;
  (** Semantic payload for the expression. *)
  desc: expr_desc;
}

and binding = {
  (** Stable binding identifier. *)
  binding_id: BindingId.t;
  (** Source origin for this binding. *)
  origin_id: OriginId.t;
  (** Simple binder name when one exists. *)
  name: string option;
  (** Pattern bound by this binding. *)
  pattern_id: PatId.t;
  (** Value expression assigned by this binding. *)
  value_id: ExprId.t;
  (** Whether the binding participates in a recursive group. *)
  recursive: bool;
}

(** Arena-style storage for patterns, bindings, and expressions. *)
type t

(** Empty body arena. *)
val empty: t

(** Build one arena from prepared node lists. *)
val of_lists:
  patterns:pattern_node list ->
  expressions:expr_node list ->
  bindings:binding list ->
  t

(** Enumerate all stored patterns. *)
val patterns: t -> pattern_node list

(** Enumerate all stored expressions. *)
val expressions: t -> expr_node list

(** Enumerate all stored bindings. *)
val bindings: t -> binding list

(** Find one pattern node by [PatId]. *)
val find_pattern: t -> PatId.t -> pattern_node option

(** Find one expression node by [ExprId]. *)
val find_expr: t -> ExprId.t -> expr_node option

(** Find one binding node by [BindingId]. *)
val find_binding: t -> BindingId.t -> binding option

(** Encode the arena as structured JSON for snapshot tests and tooling. *)
val to_json: t -> Data.Json.t

(** Render the body arena as debug text. *)
val to_string: t -> string
