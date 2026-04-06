open Std

(** Body-local semantic structure for one lowered source. *)
type pattern_desc =
  (** Variable binder pattern. *)
  | PVar of string
  (** Wildcard pattern. *)
  | PWildcard
  (** Integer literal pattern. *)
  | PInt of string
  (** Floating-point literal pattern. *)
  | PFloat of string
  (** Boolean literal pattern. *)
  | PBool of bool
  (** String literal pattern. *)
  | PString of string
  (** Character literal pattern. *)
  | PChar of string
  (** Unit pattern. *)
  | PUnit
  (** Tuple pattern with child pattern IDs. *)
  | PTuple of PatId.t list
  (** Constructor pattern with a stable constructor name and lowered payloads. *)
  | PConstructor of { constructor: string; arguments: PatId.t list }
  (** Record pattern with lowered field patterns and explicit openness. *)
  | PRecord of { fields: record_pattern_field list; open_: bool }
  (** List pattern with lowered element patterns. *)
  | PList of PatId.t list
  (** Alias pattern that binds the matched value under an extra name. *)
  | PAlias of { pattern_id: PatId.t; alias: string }
  (** Lenient polymorphic-variant pattern with an optional payload pattern. *)
  | PPolyVariant of { tag: string; payload: PatId.t option }
  (** Recovery pattern preserved after unsupported surface syntax. *)
  | PUnsupported of string

and record_pattern_field = {
  (** Stable field label name as it appeared in the source. *)
  label: string;
  (** Lowered child pattern bound for this field. *)
  pattern_id: PatId.t;
}
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
type label =
  (** Ordinary unlabeled argument or parameter. *)
  | Positional
  (** A labeled argument or parameter introduced with `~label:`. *)
  | Labeled of string
  (** An optional argument or parameter introduced with `?label:`. *)
  | Optional of string
type function_parameter = {
  (** Calling-convention label preserved from the source surface. *)
  label: label;
  (** Semantic pattern bound for this parameter. *)
  pattern_id: PatId.t;
}
type apply_argument = {
  (** Calling-convention label preserved from the call site. *)
  label: label;
  (** Lowered argument value expression. *)
  value_id: ExprId.t;
}
type expr_desc =
  (** Variable reference. *)
  | EVar of string
  (** Integer literal expression. *)
  | EInt of string
  (** Floating-point literal expression. *)
  | EFloat of string
  (** Boolean literal expression. *)
  | EBool of bool
  (** String literal expression. *)
  | EString of string
  (** Character literal expression. *)
  | EChar of string
  (** Unit expression. *)
  | EUnit
  (** Tuple expression with child expression IDs. *)
  | ETuple of ExprId.t list
  (** Array expression with child expression IDs. *)
  | EArray of ExprId.t list
  (** Sequence expression evaluated left-to-right, returning the last type. *)
  | ESequence of ExprId.t list
  (** Function expression with parameter patterns and one body expression. *)
  | EFun of function_parameter list * ExprId.t
  (** Application with one callee and labeled or positional arguments. *)
  | EApply of ExprId.t * apply_argument list
  (** Record literal or record update with lowered field expressions. *)
  | ERecord of { base_id: ExprId.t option; fields: record_expr_field list }
  (** Record field access off one receiver expression. *)
  | EFieldAccess of { receiver_id: ExprId.t; label: string }
  (** Indexed access into one collection expression at one index expression. *)
  | EIndex of ExprId.t * ExprId.t
  (** Let-expression with local binding IDs and one body expression. *)
  | ELet of BindingId.t list * ExprId.t
  (** Conditional expression. *)
  | EIf of ExprId.t * ExprId.t * ExprId.t
  (** Match expression with normalized cases. *)
  | EMatch of ExprId.t * match_case list
  (** Try-expression with normalized exception handler cases. *)
  | ETry of ExprId.t * match_case list
  (** Lenient polymorphic-variant expression with an optional payload. *)
  | EPolyVariant of { tag: string; payload: ExprId.t option }
  (** Local module open expression with the lowered body expression. *)
  | ELocalOpen of { module_path: string; body_id: ExprId.t }
  (** Unsupported semantic node that still reached the inferencer. *)
  | EUnsupported of string
  (** Recovery hole introduced during lowering. *)
  | EHole of string

and record_expr_field = {
  (** Stable field label name as it appeared in the source. *)
  label: string;
  (** Lowered child expression used for this field. *)
  value_id: ExprId.t;
}

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
  (** Lexical module path that owns this binding, empty at top level. *)
  scope_path: string list;
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
val of_lists: patterns:pattern_node list -> expressions:expr_node list -> bindings:binding list -> t

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
