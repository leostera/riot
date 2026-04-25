type function_type = {
  parameter: type_expr;
  result: type_expr;
}

and type_constructor = {
  path: Model.Surface_path.t;
  arguments: type_expr list;
}

and poly_variant_bound =
  | Exact
  | Upper
  | Lower

and poly_variant = {
  bound: poly_variant_bound;
  tags: string list;
}

and type_expr =
  | Int
  | Bool
  | Char
  | String
  | Float
  | Unit
  | List of type_expr
  | Option of type_expr
  | Tuple of type_expr list
  | Arrow of function_type
  | TypeConstructor of type_constructor
  | PolyVariant of poly_variant
  | Var of int
type scheme = {
  forall: int list;
  body: type_expr;
}
type value_binding = {
  binding_id: Model.Binding_id.t;
  entity_id: Model.Entity_id.t;
  scheme: scheme;
}
type t = {
  next_binding_stamp: int;
  values: value_binding list;
}
val empty: t

val value_binding_serializer: value_binding Serde.Ser.t

val serializer: t Serde.Ser.t
