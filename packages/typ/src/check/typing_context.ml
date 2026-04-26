open Std
open Std.Collections

type arg_label =
  | NoLabel
  | Labelled of string
  | Optional of string

type function_type = {
  label: arg_label;
  parameter: type_expr;
  result: type_expr;
}

and type_constructor = {
  path: Model.Surface_path.t;
  arguments: type_expr list;
}

and alias_type = {
  type_: type_expr;
  id: int;
}

and poly_variant_bound =
  | Exact
  | Upper
  | Lower

and poly_variant_field = {
  tag: string;
  payload: type_expr option;
}

and poly_variant = {
  bound: poly_variant_bound;
  fields: poly_variant_field list;
}

and package_type_constraint = {
  type_name: Model.Surface_path.t;
  manifest: type_expr;
}

and package_type = {
  binder: string option;
  module_type: Model.Surface_path.t;
  constraints: package_type_constraint list;
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
  | Alias of alias_type
  | PolyVariant of poly_variant
  | Package of package_type
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

(* [type_expr] is recursive, and Serde serializers are values rather than
   generated from type declarations. Keep the serializer group mutually
   recursive so nested arrows, constructors, packages, and rows can all call
   back into [type_expr_serializer]. *)

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
          Serde.Ser.Variant.newtype "TypeConstructor" type_constructor_serializer
            (fun value ->
              match value with
              | TypeConstructor value -> Some value
              | _ -> None);
          Serde.Ser.Variant.newtype "Alias" alias_type_serializer
            (fun value ->
              match value with
              | Alias value -> Some value
              | _ -> None);
          Serde.Ser.Variant.newtype "PolyVariant" poly_variant_serializer
            (fun value ->
              match value with
              | PolyVariant value -> Some value
              | _ -> None);
          Serde.Ser.Variant.newtype "Package" package_type_serializer
            (fun value ->
              match value with
              | Package value -> Some value
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

and arg_label_serializer = Serde.Ser.variant
  [ Serde.Ser.Variant.unit "NoLabel"
      (
        function
        | NoLabel -> true
        | _ -> false
      ); Serde.Ser.Variant.newtype "Labelled" Serde.Ser.string
      (
        function
        | Labelled label -> Some label
        | _ -> None
      ); Serde.Ser.Variant.newtype "Optional" Serde.Ser.string
      (
        function
        | Optional label -> Some label
        | _ -> None
      ); ]

and function_type_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.record
        (Serde.Ser.fields
          [
            Serde.Ser.field "label" arg_label_serializer (fun value -> value.label);
            Serde.Ser.field "parameter" type_expr_serializer (fun value -> value.parameter);
            Serde.Ser.field "result" type_expr_serializer (fun value -> value.result);
          ]) in
      serializer.run backend state value);
}

and type_constructor_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.record
        (Serde.Ser.fields
          [
            Serde.Ser.field "path" Model.Surface_path.serializer (fun value -> value.path);
            Serde.Ser.field
              "arguments"
              (Serde.Ser.contramap Array.from_list (Serde.Ser.array type_expr_serializer))
              (fun value -> value.arguments);
          ]) in
      serializer.run backend state value);
}

and alias_type_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.record
        (Serde.Ser.fields
          [
            Serde.Ser.field "type" type_expr_serializer (fun value -> value.type_);
            Serde.Ser.field "id" Serde.Ser.int (fun value -> value.id);
          ]) in
      serializer.run backend state value);
}

and poly_variant_bound_serializer = Serde.Ser.variant
  [ Serde.Ser.Variant.unit "Exact"
      (fun value ->
        match value with
        | Exact -> true
        | _ -> false); Serde.Ser.Variant.unit "Upper"
      (fun value ->
        match value with
        | Upper -> true
        | _ -> false); Serde.Ser.Variant.unit "Lower"
      (fun value ->
        match value with
        | Lower -> true
        | _ -> false); ]

and poly_variant_field_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.record
        (Serde.Ser.fields
          [
            Serde.Ser.field "tag" Serde.Ser.string (fun value -> value.tag);
            Serde.Ser.field
              "payload"
              (Serde.Ser.option type_expr_serializer)
              (fun value -> value.payload);
          ]) in
      serializer.run backend state value);
}

and poly_variant_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.record
        (Serde.Ser.fields
          [
            Serde.Ser.field "bound" poly_variant_bound_serializer (fun value -> value.bound);
            Serde.Ser.field
              "fields"
              (Serde.Ser.contramap Array.from_list (Serde.Ser.array poly_variant_field_serializer))
              (fun value -> value.fields);
          ]) in
      serializer.run backend state value);
}

and package_type_constraint_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.record
        (Serde.Ser.fields
          [
            Serde.Ser.field "type_name" Model.Surface_path.serializer (fun value -> value.type_name);
            Serde.Ser.field "manifest" type_expr_serializer (fun value -> value.manifest);
          ]) in
      serializer.run backend state value);
}

and package_type_serializer = {
  Serde.Ser.run =
    (fun backend state value ->
      let serializer = Serde.Ser.record
        (Serde.Ser.fields
          [
            Serde.Ser.field "binder" (Serde.Ser.option Serde.Ser.string) (fun value -> value.binder);
            Serde.Ser.field
              "module_type"
              Model.Surface_path.serializer
              (fun value -> value.module_type);
            Serde.Ser.field
              "constraints"
              (Serde.Ser.contramap
                Array.from_list
                (Serde.Ser.array package_type_constraint_serializer))
              (fun value -> value.constraints);
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
