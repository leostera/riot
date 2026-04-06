open Std

type constructor = {
  name: string;
  scheme: TypeScheme.t;
}

type label = {
  name: string;
  field_type: TypeRepr.t;
  mutable_: bool;
}

type t = {
  type_name: string;
  param_ids: int list;
  constructors: constructor list;
  labels: label list;
}

let constructor_entries = fun decl ->
  decl.constructors
  |> List.map (fun (constructor: constructor) -> (constructor.name, constructor.scheme))

let constructor_to_json = fun (constructor: constructor) ->
  Data.Json.Object [
    ("name", Data.Json.String constructor.name);
    ("scheme", Data.Json.String (TypePrinter.scheme_to_string constructor.scheme));
  ]

let label_to_json = fun (label: label) ->
  Data.Json.Object [
    ("name", Data.Json.String label.name);
    ("field_type", Data.Json.String (TypePrinter.type_to_string label.field_type));
    ("mutable", Data.Json.Bool label.mutable_);
  ]

let to_json = fun decl ->
  let fields = [
    ("type_name", Data.Json.String decl.type_name);
    ("constructors", Data.Json.Array (List.map constructor_to_json decl.constructors));
  ] in
  let fields =
    match decl.labels with
    | [] -> fields
    | labels -> fields @ [ ("labels", Data.Json.Array (List.map label_to_json labels)); ]
  in
  Data.Json.Object fields

let to_string = fun decl ->
  let constructors =
    match decl.constructors with
    | [] -> "none"
    | constructors -> constructors
    |> List.map
      (fun (constructor: constructor) ->
        constructor.name ^ " : " ^ TypePrinter.scheme_to_string constructor.scheme)
    |> String.concat ", "
  in
  let labels =
    match decl.labels with
    | [] -> "none"
    | labels -> labels
    |> List.map
      (fun (label: label) ->
        let mutability =
          if label.mutable_ then
            "mutable "
          else
            ""
        in
        label.name ^ " : " ^ mutability ^ TypePrinter.type_to_string label.field_type)
    |> String.concat ", "
  in
  decl.type_name ^ " { constructors = " ^ constructors ^ "; labels = " ^ labels ^ " }"
