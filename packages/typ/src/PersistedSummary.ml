open Std

type t = FileSummary.t

let of_file_summary = fun (summary: FileSummary.t) -> summary

let to_file_summary = fun (summary: t) -> (summary: FileSummary.t)

let source_id = fun (summary: FileSummary.t) -> summary.source_id

let exports = fun (summary: FileSummary.t) -> FileSummary.exports summary

let type_decls = fun (summary: FileSummary.t) -> FileSummary.type_decls summary

let json_type_name = function
  | Data.Json.Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"

let error_expected = fun expected actual ->
  Error ("expected " ^ expected ^ " but got " ^ json_type_name actual)

let get_object = function
  | Data.Json.Object fields -> Ok fields
  | other -> error_expected "object" other

let get_array = function
  | Data.Json.Array values -> Ok values
  | other -> error_expected "array" other

let get_string = function
  | Data.Json.String value -> Ok value
  | other -> error_expected "string" other

let get_int = function
  | Data.Json.Int value -> Ok value
  | other -> error_expected "int" other

let field = fun name fields ->
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("missing field " ^ name)

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error _ as err -> err

let label_to_json = function
  | TypeRepr.Nolabel -> Data.Json.Object [ ("tag", Data.Json.String "nolabel") ]
  | TypeRepr.Labelled label -> Data.Json.Object [
    ("tag", Data.Json.String "labeled");
    ("label", Data.Json.String label);
  ]
  | TypeRepr.Optional label -> Data.Json.Object [
    ("tag", Data.Json.String "optional");
    ("label", Data.Json.String label);
  ]

let rec type_to_json = fun ty ->
  match TypeRepr.prune ty with
  | TypeRepr.Int -> Data.Json.Object [ ("tag", Data.Json.String "int") ]
  | TypeRepr.Float -> Data.Json.Object [ ("tag", Data.Json.String "float") ]
  | TypeRepr.Bool -> Data.Json.Object [ ("tag", Data.Json.String "bool") ]
  | TypeRepr.String -> Data.Json.Object [ ("tag", Data.Json.String "string") ]
  | TypeRepr.Char -> Data.Json.Object [ ("tag", Data.Json.String "char") ]
  | TypeRepr.Unit -> Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | TypeRepr.Option element -> Data.Json.Object [
    ("tag", Data.Json.String "option");
    ("element", type_to_json element);
  ]
  | TypeRepr.Result (ok_ty, error_ty) -> Data.Json.Object [
    ("tag", Data.Json.String "result");
    ("ok", type_to_json ok_ty);
    ("error", type_to_json error_ty);
  ]
  | TypeRepr.Array element -> Data.Json.Object [
    ("tag", Data.Json.String "array");
    ("element", type_to_json element);
  ]
  | TypeRepr.List element -> Data.Json.Object [
    ("tag", Data.Json.String "list");
    ("element", type_to_json element);
  ]
  | TypeRepr.Seq element -> Data.Json.Object [
    ("tag", Data.Json.String "seq");
    ("element", type_to_json element);
  ]
  | TypeRepr.Named { name; arguments } -> Data.Json.Object [
    ("tag", Data.Json.String "named");
    ("name", Data.Json.String name);
    ("arguments", Data.Json.Array (List.map type_to_json arguments));
  ]
  | TypeRepr.Tuple members -> Data.Json.Object [
    ("tag", Data.Json.String "tuple");
    ("members", Data.Json.Array (List.map type_to_json members));
  ]
  | TypeRepr.Arrow { label; lhs; rhs } -> Data.Json.Object [
    ("tag", Data.Json.String "arrow");
    ("label", label_to_json label);
    ("lhs", type_to_json lhs);
    ("rhs", type_to_json rhs);
  ]
  | TypeRepr.Var { id; link=None } -> Data.Json.Object [
    ("tag", Data.Json.String "var");
    ("id", Data.Json.Int id);
  ]
  | TypeRepr.Var { link=Some linked; _ } -> type_to_json linked
  | TypeRepr.Hole id -> Data.Json.Object [
    ("tag", Data.Json.String "hole");
    ("id", Data.Json.Int id);
  ]

let scheme_to_json = fun (TypeScheme.Forall (quantified, body)) ->
  Data.Json.Object [
    ("quantified", Data.Json.Array (List.map (fun id -> Data.Json.Int id) quantified));
    ("body", type_to_json body);
  ]

let exports_to_json = fun exports ->
  Data.Json.Array (exports
  |> List.map
    (fun (name, scheme) ->
      Data.Json.Object [ ("name", Data.Json.String name); ("scheme", scheme_to_json scheme); ]))

let constructor_to_json = fun (constructor: TypeDecl.constructor) ->
  Data.Json.Object [
    ("name", Data.Json.String constructor.name);
    ("scheme", scheme_to_json constructor.scheme);
  ]

let label_to_json = fun (label: TypeDecl.label) ->
  Data.Json.Object [
    ("name", Data.Json.String label.name);
    ("field_type", type_to_json label.field_type);
    ("mutable", Data.Json.Bool label.mutable_);
  ]

let manifest_to_json = function
  | TypeDecl.Alias manifest_type -> Data.Json.Object [
    ("tag", Data.Json.String "alias");
    ("type", type_to_json manifest_type);
  ]
  | TypeDecl.PolyVariant { bound; tags; inherited } ->
      let bound =
        match bound with
        | TypeDecl.Exact -> "exact"
        | TypeDecl.UpperBound -> "upper"
        | TypeDecl.LowerBound -> "lower"
      in
      let tag_to_json (tag: TypeDecl.poly_variant_tag) =
        let fields = [ ("name", Data.Json.String tag.name) ] in
        let fields =
          match tag.payload_type with
          | Some payload_type -> fields @ [ ("payload_type", type_to_json payload_type) ]
          | None -> fields
        in
        Data.Json.Object fields
      in
      Data.Json.Object [
        ("tag", Data.Json.String "poly_variant");
        ("bound", Data.Json.String bound);
        ("tags", Data.Json.Array (List.map tag_to_json tags));
        ("inherited", Data.Json.Array (List.map type_to_json inherited));
      ]

let type_decl_to_json = fun (type_decl: FileSummary.type_decl) ->
  let fields = [
    ("scope_path", Data.Json.Array (List.map (fun segment -> Data.Json.String segment) type_decl.scope_path));
    ("type_name", Data.Json.String type_decl.declaration.type_name);
    ("param_ids", Data.Json.Array (List.map (fun id -> Data.Json.Int id) type_decl.declaration.param_ids));
    (
      "constructors",
      Data.Json.Array (List.map constructor_to_json type_decl.declaration.constructors)
    );
    ("labels", Data.Json.Array (List.map label_to_json type_decl.declaration.labels));
  ] in
  let fields =
    match type_decl.declaration.manifest with
    | Some manifest -> fields @ [ ("manifest", manifest_to_json manifest) ]
    | None -> fields
  in
  Data.Json.Object fields

let type_decls_to_json = fun type_decls ->
  Data.Json.Array (List.map type_decl_to_json type_decls)

let label_of_json = fun json ->
  let* fields = get_object json in
  let* tag_json = field "tag" fields in
  let* tag = get_string tag_json in
  match tag with
  | "nolabel" ->
      Ok TypeRepr.Nolabel
  | "labeled" ->
      let* label_json = field "label" fields in
      let* label = get_string label_json in
      Ok (TypeRepr.Labelled label)
  | "optional" ->
      let* label_json = field "label" fields in
      let* label = get_string label_json in
      Ok (TypeRepr.Optional label)
  | other ->
      Error ("unknown persisted type label tag " ^ other)

let rec type_of_json = fun json ->
  let* fields = get_object json in
  let* tag_json = field "tag" fields in
  let* tag = get_string tag_json in
  match tag with
  | "int" ->
      Ok TypeRepr.Int
  | "float" ->
      Ok TypeRepr.Float
  | "bool" ->
      Ok TypeRepr.Bool
  | "string" ->
      Ok TypeRepr.String
  | "char" ->
      Ok TypeRepr.Char
  | "unit" ->
      Ok TypeRepr.Unit
  | "option" ->
      let* element_json = field "element" fields in
      let* element = type_of_json element_json in
      Ok (TypeRepr.Option element)
  | "result" ->
      let* ok_json = field "ok" fields in
      let* error_json = field "error" fields in
      let* ok_ty = type_of_json ok_json in
      let* error_ty = type_of_json error_json in
      Ok (TypeRepr.Result (ok_ty, error_ty))
  | "array" ->
      let* element_json = field "element" fields in
      let* element = type_of_json element_json in
      Ok (TypeRepr.Array element)
  | "list" ->
      let* element_json = field "element" fields in
      let* element = type_of_json element_json in
      Ok (TypeRepr.List element)
  | "seq" ->
      let* element_json = field "element" fields in
      let* element = type_of_json element_json in
      Ok (TypeRepr.Seq element)
  | "named" ->
      let* name_json = field "name" fields in
      let* name = get_string name_json in
      let* arguments_json = field "arguments" fields in
      let* arguments_json = get_array arguments_json in
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | head :: tail ->
            let* argument = type_of_json head in
            loop (argument :: acc) tail
      in
      let* arguments = loop [] arguments_json in
      Ok (TypeRepr.Named { name; arguments })
  | "tuple" ->
      let* members_json = field "members" fields in
      let* members_json = get_array members_json in
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | head :: tail ->
            let* member = type_of_json head in
            loop (member :: acc) tail
      in
      let* members = loop [] members_json in
      Ok (TypeRepr.Tuple members)
  | "arrow" ->
      let* label_json = field "label" fields in
      let* lhs_json = field "lhs" fields in
      let* rhs_json = field "rhs" fields in
      let* label = label_of_json label_json in
      let* lhs = type_of_json lhs_json in
      let* rhs = type_of_json rhs_json in
      Ok (TypeRepr.Arrow { label; lhs; rhs })
  | "var" ->
      let* id_json = field "id" fields in
      let* id = get_int id_json in
      Ok (TypeRepr.Var { id; link = None })
  | "hole" ->
      let* id_json = field "id" fields in
      let* id = get_int id_json in
      Ok (TypeRepr.Hole id)
  | other ->
      Error ("unknown persisted type tag " ^ other)

let scheme_of_json = function
  | Data.Json.Object fields -> (
      match (List.assoc_opt "quantified" fields, List.assoc_opt "body" fields) with
      | (Some (Data.Json.Array quantified_json), Some body_json) ->
          let rec parse_quantified acc = function
            | [] -> Ok (List.rev acc)
            | Data.Json.Int id :: rest -> parse_quantified (id :: acc) rest
            | other :: _ -> Error ("expected quantified type variable id int but got "
            ^ json_type_name other)
          in
          begin
            match parse_quantified [] quantified_json, type_of_json body_json with
            | Ok quantified, Ok body -> Ok (TypeScheme.Forall (quantified, body))
            | Error err, _ -> Error err
            | _, Error err -> Error err
          end
      | _ -> Error "expected persisted type scheme object with quantified and body fields"
    )
  | other -> Error ("expected persisted type scheme object but got " ^ json_type_name other)

let exports_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* fields = get_object value in
        let* name_json = field "name" fields in
        let* scheme_json = field "scheme" fields in
        let* name = get_string name_json in
        let* scheme = scheme_of_json scheme_json in
        loop ((name, scheme) :: acc) rest
  in
  loop [] values

let int_list_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* value = get_int value in
        loop (value :: acc) rest
  in
  loop [] values

let string_list_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* value = get_string value in
        loop (value :: acc) rest
  in
  loop [] values

let constructor_of_json = fun json ->
  let* fields = get_object json in
  let* name_json = field "name" fields in
  let* scheme_json = field "scheme" fields in
  let* name = get_string name_json in
  let* scheme = scheme_of_json scheme_json in
  Ok ({ TypeDecl.name = name; scheme }: TypeDecl.constructor)

let label_decl_of_json = fun json ->
  let* fields = get_object json in
  let* name_json = field "name" fields in
  let* field_type_json = field "field_type" fields in
  let* mutable_json = field "mutable" fields in
  let* name = get_string name_json in
  let* field_type = type_of_json field_type_json in
  let mutable_ =
    match mutable_json with
    | Data.Json.Bool mutable_ -> Ok mutable_
    | other -> error_expected "bool" other
  in
  let* mutable_ = mutable_ in
  Ok ({ TypeDecl.name = name; field_type; mutable_ }: TypeDecl.label)

let poly_variant_tag_of_json = fun json ->
  let* fields = get_object json in
  let* name_json = field "name" fields in
  let* name = get_string name_json in
  let payload_type =
    match List.assoc_opt "payload_type" fields with
    | Some payload_type_json -> type_of_json payload_type_json |> Result.map Option.some
    | None -> Ok None
  in
  let* payload_type = payload_type in
  Ok ({ TypeDecl.name = name; payload_type }: TypeDecl.poly_variant_tag)

let manifest_of_json = fun json ->
  let* fields = get_object json in
  let* tag_json = field "tag" fields in
  let* tag = get_string tag_json in
  match tag with
  | "alias" ->
      let* type_json = field "type" fields in
      let* manifest_type = type_of_json type_json in
      Ok (TypeDecl.Alias manifest_type)
  | "poly_variant" ->
      let* bound_json = field "bound" fields in
      let* tags_json = field "tags" fields in
      let* inherited_json = field "inherited" fields in
      let* bound = get_string bound_json in
      let bound =
        match bound with
        | "exact" -> Ok TypeDecl.Exact
        | "upper" -> Ok TypeDecl.UpperBound
        | "lower" -> Ok TypeDecl.LowerBound
        | other -> Error ("unknown persisted poly-variant bound " ^ other)
      in
      let* bound = bound in
      let* tags_json = get_array tags_json in
      let rec parse_tags acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* tag = poly_variant_tag_of_json value in
            parse_tags (tag :: acc) rest
      in
      let* tags = parse_tags [] tags_json in
      let* inherited_json = get_array inherited_json in
      let rec parse_inherited acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* inherited = type_of_json value in
            parse_inherited (inherited :: acc) rest
      in
      let* inherited = parse_inherited [] inherited_json in
      Ok (TypeDecl.PolyVariant { bound; tags; inherited })
  | other ->
      Error ("unknown persisted type manifest tag " ^ other)

let type_decl_of_json = fun json ->
  let* fields = get_object json in
  let* scope_path_json = field "scope_path" fields in
  let* type_name_json = field "type_name" fields in
  let* param_ids_json = field "param_ids" fields in
  let* constructors_json = field "constructors" fields in
  let* labels_json = field "labels" fields in
  let* scope_path = string_list_of_json scope_path_json in
  let* type_name = get_string type_name_json in
  let* param_ids = int_list_of_json param_ids_json in
  let* constructors_json = get_array constructors_json in
  let rec parse_constructors acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* constructor = constructor_of_json value in
        parse_constructors (constructor :: acc) rest
  in
  let* constructors = parse_constructors [] constructors_json in
  let* labels_json = get_array labels_json in
  let rec parse_labels acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* label = label_decl_of_json value in
        parse_labels (label :: acc) rest
  in
  let* labels = parse_labels [] labels_json in
  let manifest =
    match List.assoc_opt "manifest" fields with
    | Some manifest_json -> manifest_of_json manifest_json |> Result.map Option.some
    | None -> Ok None
  in
  let* manifest = manifest in
  Ok {
    FileSummary.scope_path = scope_path;
    declaration = {
      TypeDecl.type_name = type_name;
      param_ids;
      constructors;
      labels;
      manifest;
    };
  }

let type_decls_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* type_decl = type_decl_of_json value in
        loop (type_decl :: acc) rest
  in
  loop [] values

module Json = struct
  let to_json = fun summary ->
    let export_result =
      match summary.FileSummary.export_result with
      | FileSummary.TrustedExport { exports } -> Data.Json.Object [
        ("tag", Data.Json.String "trusted_export");
        ("exports", exports_to_json exports);
      ]
      | FileSummary.ErroredExport { exports } -> Data.Json.Object [
        ("tag", Data.Json.String "errored_export");
        ("exports", exports_to_json exports);
      ]
      | FileSummary.NoExport -> Data.Json.Object [
        ("tag", Data.Json.String "no_export");
        ("exports", Data.Json.Array []);
      ]
    in
    Data.Json.Object [
      ("source_id", Data.Json.Int (SourceId.to_int summary.source_id));
      ("export_result", export_result);
      ("type_decls", type_decls_to_json summary.type_decls);
    ]

  let of_json = fun json ->
    let* fields = get_object json in
    let* source_id_json = field "source_id" fields in
    let* export_result_json = field "export_result" fields in
    let* source_id = get_int source_id_json in
    let* export_result_fields = get_object export_result_json in
    let* tag_json = field "tag" export_result_fields in
    let* exports_json = field "exports" export_result_fields in
    let* tag = get_string tag_json in
    let* exports = exports_of_json exports_json in
    let export_result =
      match tag with
      | "trusted_export" -> Ok (FileSummary.TrustedExport { exports })
      | "errored_export" -> Ok (FileSummary.ErroredExport { exports })
      | "no_export" -> Ok FileSummary.NoExport
      | other -> Error ("unknown persisted export_result tag " ^ other)
    in
    let* export_result = export_result in
    let type_decls =
      match List.assoc_opt "type_decls" fields with
      | Some type_decls_json -> type_decls_of_json type_decls_json
      | None -> Ok []
    in
    let* type_decls = type_decls in
    Ok { FileSummary.source_id = SourceId.of_int source_id; export_result; type_decls }
end
