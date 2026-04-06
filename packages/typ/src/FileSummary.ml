open Std

type type_decl = {
  scope_path: string list;
  declaration: TypeDecl.t;
}

type exports = (string * TypeScheme.t) list

type export_result =
  | TrustedExport of { exports: exports }
  | ErroredExport of { exports: exports }
  | NoExport

type t = {
  source_id: SourceId.t;
  export_result: export_result;
  type_decls: type_decl list;
}

let trusted = fun ~source_id ?(type_decls = []) exports ->
  { source_id; export_result = TrustedExport { exports }; type_decls }

let errored = fun ~source_id ?(type_decls = []) exports ->
  { source_id; export_result = ErroredExport { exports }; type_decls }

let missing = fun ~source_id ?(type_decls = []) () -> { source_id; export_result = NoExport; type_decls }

let exports = function
  | { export_result=TrustedExport { exports }; _ }
  | { export_result=ErroredExport { exports }; _ } -> exports
  | { export_result=NoExport; _ } -> []

let type_decls = fun summary -> summary.type_decls

let exports_to_json = fun exports ->
  Data.Json.Array (exports
  |> List.map
    (fun (name, scheme) ->
      Data.Json.Object [
        ("name", Data.Json.String name);
        ("scheme", Data.Json.String (TypePrinter.scheme_to_string scheme));
      ]))

let to_json = fun summary ->
  let export_result =
    match summary.export_result with
    | TrustedExport { exports } -> Data.Json.Object [
      ("tag", Data.Json.String "trusted_export");
      ("exports", exports_to_json exports);
    ]
    | ErroredExport { exports } -> Data.Json.Object [
      ("tag", Data.Json.String "errored_export");
      ("exports", exports_to_json exports);
    ]
    | NoExport -> Data.Json.Object [
      ("tag", Data.Json.String "no_export");
      ("exports", Data.Json.Array []);
    ]
  in
  Data.Json.Object [
    ("source_id", Data.Json.Int (SourceId.to_int summary.source_id));
    ("export_result", export_result);
  ]

let render_exports = fun exports ->
  match exports with
  | [] -> "none"
  | _ -> exports
  |> List.map (fun (name, scheme) -> name ^ " : " ^ TypePrinter.scheme_to_string scheme)
  |> String.concat ", "

let to_string = fun summary ->
  match summary.export_result with
  | TrustedExport { exports } -> "  "
  ^ SourceId.to_string summary.source_id
  ^ " trusted ["
  ^ render_exports exports
  ^ "]\n"
  | ErroredExport { exports } -> "  "
  ^ SourceId.to_string summary.source_id
  ^ " errored ["
  ^ render_exports exports
  ^ "]\n"
  | NoExport -> "  " ^ SourceId.to_string summary.source_id ^ " no-export\n"
