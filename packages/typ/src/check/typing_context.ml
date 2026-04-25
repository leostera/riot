open Std
open Std.Collections

type function_type = {
  parameter: type_expr;
  result: type_expr;
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

let empty = { next_binding_stamp = 0; values = [] }

let rec type_expr_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.variant
        [
          Serde.Ser.Variant.unit "Int"
            (fun value ->
              match value with
              | Int -> true
              | _ -> false);
          Serde.Ser.Variant.unit "Bool"
            (fun value ->
              match value with
              | Bool -> true
              | _ -> false);
          Serde.Ser.Variant.unit "Char"
            (fun value ->
              match value with
              | Char -> true
              | _ -> false);
          Serde.Ser.Variant.unit "String"
            (fun value ->
              match value with
              | String -> true
              | _ -> false);
          Serde.Ser.Variant.unit "Float"
            (fun value ->
              match value with
              | Float -> true
              | _ -> false);
          Serde.Ser.Variant.unit "Unit"
            (fun value ->
              match value with
              | Unit -> true
              | _ -> false);
          Serde.Ser.Variant.newtype "List" type_expr_serializer
            (fun value ->
              match value with
              | List value -> Some value
              | _ -> None);
          Serde.Ser.Variant.newtype "Option" type_expr_serializer
            (fun value ->
              match value with
              | Option value -> Some value
              | _ -> None);
          Serde.Ser.Variant.newtype "Tuple" (Serde.Ser.contramap
            Array.from_list
            (Serde.Ser.array type_expr_serializer))
            (fun value ->
              match value with
              | Tuple values -> Some values
              | _ -> None);
          Serde.Ser.Variant.newtype "Arrow" function_type_serializer
            (fun value ->
              match value with
              | Arrow value -> Some value
              | _ -> None);
          Serde.Ser.Variant.newtype "Var" Serde.Ser.int
            (fun value ->
              match value with
              | Var value -> Some value
              | _ -> None);
        ]
      in
      serializer.run backend state value);
}

and function_type_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.record
        (Serde.Ser.fields
          [
            Serde.Ser.field "parameter" type_expr_serializer (fun value -> value.parameter);
            Serde.Ser.field "result" type_expr_serializer (fun value -> value.result);
          ]) in
      serializer.run backend state value);
}

let scheme_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field
        "forall"
        (Serde.Ser.contramap Array.from_list (Serde.Ser.array Serde.Ser.int))
        (fun value -> value.forall);
      Serde.Ser.field "body" type_expr_serializer (fun value -> value.body);
    ])

let value_binding_serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "binding_id" Model.Binding_id.serializer (fun value -> value.binding_id);
      Serde.Ser.field "entity_id" Model.Entity_id.serializer (fun value -> value.entity_id);
      Serde.Ser.field "scheme" scheme_serializer (fun value -> value.scheme);
    ])

let serializer = Serde.Ser.record
  (Serde.Ser.fields
    [
      Serde.Ser.field "next_binding_stamp" Serde.Ser.int (fun value -> value.next_binding_stamp);
      Serde.Ser.field
        "values"
        (Serde.Ser.contramap Array.from_list (Serde.Ser.array value_binding_serializer))
        (fun value -> value.values);
    ])
