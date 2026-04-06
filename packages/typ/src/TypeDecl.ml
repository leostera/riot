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

type poly_variant_bound =
  | Exact
  | UpperBound
  | LowerBound

type poly_variant_tag = {
  name: string;
  payload_type: TypeRepr.t option;
}

type manifest =
  | Alias of TypeRepr.t
  | PolyVariant of {
      bound: poly_variant_bound;
      tags: poly_variant_tag list;
      inherited: TypeRepr.t list
    }

type t = {
  type_name: string;
  param_ids: int list;
  constructors: constructor list;
  labels: label list;
  manifest: manifest option;
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

let poly_variant_bound_to_string = function
  | Exact -> "exact"
  | UpperBound -> "upper"
  | LowerBound -> "lower"

let poly_variant_tag_to_json = fun (tag: poly_variant_tag) ->
  let fields = [ ("name", Data.Json.String tag.name) ] in
  let fields =
    match tag.payload_type with
    | Some payload_type -> fields
    @ [ ("payload_type", Data.Json.String (TypePrinter.type_to_string payload_type)) ]
    | None -> fields
  in
  Data.Json.Object fields

let manifest_to_json = function
  | Alias manifest_type -> Data.Json.Object [
    ("tag", Data.Json.String "alias");
    ("type", Data.Json.String (TypePrinter.type_to_string manifest_type));
  ]
  | PolyVariant { bound; tags; inherited } -> Data.Json.Object [
    ("tag", Data.Json.String "poly_variant");
    ("bound", Data.Json.String (poly_variant_bound_to_string bound));
    ("tags", Data.Json.Array (List.map poly_variant_tag_to_json tags));
    (
      "inherited",
      Data.Json.Array (List.map
        (fun inherited -> Data.Json.String (TypePrinter.type_to_string inherited))
        inherited)
    );
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
  let fields =
    match decl.manifest with
    | Some manifest -> fields @ [ ("manifest", manifest_to_json manifest) ]
    | None -> fields
  in
  Data.Json.Object fields

let poly_variant_tag_to_string = fun (tag: poly_variant_tag) ->
  match tag.payload_type with
  | Some payload_type -> "`" ^ tag.name ^ " of " ^ TypePrinter.type_to_string payload_type
  | None -> "`" ^ tag.name

let manifest_to_string = function
  | Alias manifest_type -> "= " ^ TypePrinter.type_to_string manifest_type
  | PolyVariant { bound; tags; inherited } ->
      let prefix =
        match bound with
        | Exact -> ""
        | UpperBound -> ">"
        | LowerBound -> "<"
      in
      let members = (List.map poly_variant_tag_to_string tags)
      @ (List.map TypePrinter.type_to_string inherited) in
      "= [" ^ prefix ^ " " ^ String.concat " | " members ^ " ]"

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
    | labels ->
        labels |> List.map
          (fun (label: label) ->
            let mutability =
              if label.mutable_ then
                "mutable "
              else
                ""
            in
            label.name ^ " : " ^ mutability ^ TypePrinter.type_to_string label.field_type) |> String.concat
          ", "
  in
  let manifest =
    match decl.manifest with
    | Some manifest -> "; manifest = " ^ manifest_to_string manifest
    | None -> ""
  in
  decl.type_name ^ " { constructors = " ^ constructors ^ "; labels = " ^ labels ^ manifest ^ " }"
