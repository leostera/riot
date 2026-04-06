open Std
open Typ

let export_names = function
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) -> List.map fst exports
  | Some FileSummary.NoExport
  | None -> []

let export_scheme = fun snapshot source_id name ->
  match Query.export_of snapshot source_id with
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) -> List.assoc_opt name exports
  |> Option.map TypePrinter.scheme_to_string
  | Some FileSummary.NoExport
  | None -> None

let inferred_type_at = fun snapshot source_id offset ->
  Query.type_at snapshot source_id (Position.make ~offset) |> function
  | Some ty -> Some (TypePrinter.type_to_string ty)
  | None -> None

let offset_of_substring = fun text needle ->
  let text_length = String.length text in
  let needle_length = String.length needle in
  let max_start = text_length - needle_length in
  let rec loop start =
    if start > max_start then
      None
    else if String.sub text start needle_length = needle then
      Some start
    else
      loop (start + 1)
  in
  if needle_length = 0 then
    Some 0
  else if needle_length > text_length then
    None
  else
    loop 0

let has_unbound_name = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.exists
    (
      function
      | Query.Typing (Diagnostic.UnboundName _) -> true
      | _ -> false
    )

let diagnostic_strings = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.map
    (
      function
      | Query.Parse diagnostic -> Syn.Diagnostic.to_string diagnostic
      | Query.Lowering diagnostic
      | Query.Typing diagnostic -> Diagnostic.to_string diagnostic
    )

let trace_debug = fun snapshot source_id ->
  match Query.analysis_of_source snapshot source_id with
  | None -> []
  | Some analysis ->
      let item_lines = analysis.item_traces
      |> List.map
        (fun (trace: Check_result.item_trace) ->
          "item "
          ^ ItemId.to_string trace.item_id
          ^ " -> ["
          ^ String.concat ", " (List.map fst trace.exports_after)
          ^ "]") in
      let expr_lines = analysis.expr_traces
      |> List.map
        (fun (trace: Check_result.expr_trace) ->
          "expr "
          ^ ExprId.to_string trace.expr_id
          ^ " -> ["
          ^ String.concat ", " (List.map fst trace.env_before)
          ^ "]") in
      item_lines @ expr_lines

let persisted_summary_jsons = fun snapshot ->
  Snapshot.persisted_summaries snapshot |> List.map PersistedSummary.Json.to_json

let module_summary_jsons = fun snapshot ->
  Snapshot.module_summaries snapshot |> List.map ModuleSummary.Json.to_json

let prepare_snapshot_or_error = fun session ~roots ->
  match Session.prepare_snapshot session ~roots with
  | Ok snapshot -> Ok snapshot
  | Error missing -> Error ("unexpected missing requirements: "
  ^ Data.Json.to_string (MissingRequirements.to_json missing))

let test_source_id_stays_stable_across_updates = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "stable.ml")
    ~text:"let x = 1" in
  let snapshot_before = Session.snapshot session in
  let session = Session.update_source_text session source_id ~text:"let y = 2" in
  let snapshot_after = Session.snapshot session in
  let before_names = export_names (Query.export_of snapshot_before source_id) in
  let after_names = export_names (Query.export_of snapshot_after source_id) in
  let () = Test.assert_equal ~expected:[ "x" ] ~actual:before_names in
  let () = Test.assert_equal ~expected:[ "y" ] ~actual:after_names in
  Ok ()

let test_snapshots_remain_immutable_after_updates = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "immutable.ml")
    ~text:"let x = 1" in
  let snapshot_before = Session.snapshot session in
  let session = Session.update_source_text session source_id ~text:"let x = true" in
  let snapshot_after = Session.snapshot session in
  let before_type = inferred_type_at snapshot_before source_id 8 in
  let after_type = inferred_type_at snapshot_after source_id 8 in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:before_type in
  let () = Test.assert_equal ~expected:(Some "bool") ~actual:after_type in
  Ok ()

let test_type_at_uses_smallest_indexed_expression = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let id x = x\nlet answer = id 42\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "type_at.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let callee_type = inferred_type_at snapshot source_id 26 in
  let argument_type = inferred_type_at snapshot source_id 29 in
  let () = Test.assert_equal ~expected:(Some "int -> int") ~actual:callee_type in
  let () = Test.assert_equal ~expected:(Some "int") ~actual:argument_type in
  Ok ()

let test_snapshot_exposes_implicit_file_modules = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, colors_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
  let (session, demo_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "blend_demo.ml")
    ~text:"open Colors\nlet midpoint = RGB.blend 1 2\nlet label = to_string \"ok\"\n" in
  let snapshot = Session.snapshot session in
  let demo_has_unbound_name = has_unbound_name snapshot demo_source_id in
  let demo_diagnostics = diagnostic_strings snapshot demo_source_id in
  let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
  let label_type = inferred_type_at snapshot demo_source_id 58 in
  let color_exports = export_names (Query.export_of snapshot colors_source_id) in
  let () = Test.assert_equal ~expected:[ "RGB.blend"; "to_string" ] ~actual:color_exports in
  if demo_has_unbound_name then
    Error (String.concat "\n" (demo_diagnostics @ trace_debug snapshot demo_source_id))
  else
    let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
    let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
    Ok ()

let test_snapshot_collects_persisted_summaries = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "alpha.ml")
    ~text:"let id x = x\n" in
  let (session, _) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "beta.ml")
    ~text:"let broken = missing\n" in
  let snapshot = Session.snapshot session in
  let summaries = persisted_summary_jsons snapshot in
  let tags =
    summaries
    |> List.filter_map
      (
        function
        | Data.Json.Object fields -> (
            match List.assoc_opt "export_result" fields with
            | Some (Data.Json.Object export_fields) -> (
                match List.assoc_opt "tag" export_fields with
                | Some (Data.Json.String tag) -> Some tag
                | _ -> None
              )
            | _ -> None
          )
        | _ -> None
      )
  in
  let () = Test.assert_equal ~expected:[ "trusted_export"; "errored_export" ] ~actual:tags in
  Ok ()

let test_source_input_hash_ignores_source_id_and_revision = fun _ctx ->
  let source_a = Source.make
    ~source_id:(SourceId.of_int 0)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:1
    ~text:"let id x = x\n" in
  let source_b = Source.make
    ~source_id:(SourceId.of_int 99)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:42
    ~text:"let id x = x\n" in
  let source_c = Source.make
    ~source_id:(SourceId.of_int 99)
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~revision:43
    ~text:"let id x = (x, x)\n" in
  let hash_a = Source.input_hash source_a |> Crypto.Digest.hex in
  let hash_b = Source.input_hash source_b |> Crypto.Digest.hex in
  let hash_c = Source.input_hash source_c |> Crypto.Digest.hex in
  let () = Test.assert_equal ~expected:hash_a ~actual:hash_b in
  if String.equal hash_a hash_c then
    Error "expected source input hash to change when source text changes"
  else
    Ok ()

let test_snapshot_uses_loaded_module_summaries = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, colors_source_id) = Session.create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_colors =
    match Query.module_summary_of seed_snapshot colors_source_id with
    | Some summary -> summary
    | None -> panic "expected seed module summary"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_colors ] in
  let session = Session.empty ~config in
  let (session, demo_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "blend_demo.ml")
    ~text:"open Colors\nlet midpoint = RGB.blend 1 2\nlet label = to_string \"ok\"\n" in
  let snapshot = Session.snapshot session in
  let demo_has_unbound_name = has_unbound_name snapshot demo_source_id in
  let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
  let label_type = inferred_type_at snapshot demo_source_id 58 in
  let summary_modules =
    module_summary_jsons snapshot
    |> List.filter_map
      (
        function
        | Data.Json.Object fields -> (
            match List.assoc_opt "module_name" fields with
            | Some (Data.Json.String module_name) -> Some module_name
            | _ -> None
          )
        | _ -> None
      )
  in
  let () = Test.assert_equal ~expected:[ "Blend_demo" ] ~actual:summary_modules in
  if demo_has_unbound_name then
    Error (String.concat "\n" (diagnostic_strings snapshot demo_source_id))
  else
    let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
    let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
    Ok ()

let test_snapshot_uses_sibling_source_record_types = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, colors_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let source = "open Colors\n" ^ "let origin = { x = 0; y = 0 }\n" ^ "let total point = point.x + point.y\n" in
  let (session, demo_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "blend_demo.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot demo_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let seed_summary =
      match Query.module_summary_of snapshot colors_source_id with
      | Some summary -> summary
      | None -> panic "expected colors module summary"
    in
    let type_decl_names = ModuleSummary.type_decls seed_summary
    |> List.map (fun (type_decl: FileSummary.type_decl) -> type_decl.declaration.type_name) in
    let field_access_offset =
      let access_start = offset_of_substring source "point.x +" |> Option.expect ~msg:"expected record field access in test source" in
      access_start + String.length "point."
    in
    let origin_type = export_scheme snapshot demo_source_id "origin" in
    let total_type = export_scheme snapshot demo_source_id "total" in
    let field_access_type = inferred_type_at snapshot demo_source_id field_access_offset in
    let () = Test.assert_equal ~expected:[ "point" ] ~actual:type_decl_names in
    let () = Test.assert_equal ~expected:(Some "Colors.point") ~actual:origin_type in
    let () = Test.assert_equal ~expected:(Some "Colors.point -> int") ~actual:total_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:field_access_type in
    Ok ()

let test_snapshot_uses_loaded_module_record_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, colors_source_id) = Session.create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_colors =
    match Query.module_summary_of seed_snapshot colors_source_id with
    | Some summary -> summary
    | None -> panic "expected colors module summary"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_colors ] in
  let session = Session.empty ~config in
  let source = "open Colors\n" ^ "let origin = { x = 0; y = 0 }\n" ^ "let total point = point.x + point.y\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let origin_type = export_scheme snapshot source_id "origin" in
    let total_type = export_scheme snapshot source_id "total" in
    let () = Test.assert_equal ~expected:(Some "Colors.point") ~actual:origin_type in
    let () = Test.assert_equal ~expected:(Some "Colors.point -> int") ~actual:total_type in
    Ok ()

let test_include_reexports_loaded_module_record_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = Session.create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_summary_of seed_snapshot helpers_source_id with
    | Some summary -> summary
    | None -> panic "expected helper module summary"
  in
  let consumer_config = Config.default
  |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let consumer_session = Session.empty ~config:consumer_config in
  let (consumer_session, consumer_source_id) = Session.create_source
    consumer_session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:"include Helpers\n" in
  let consumer_snapshot = Session.snapshot consumer_session in
  let consumer_diagnostics = diagnostic_strings consumer_snapshot consumer_source_id in
  if not (List.is_empty consumer_diagnostics) then
    Error (String.concat "\n" consumer_diagnostics)
  else
    let consumer_summary =
      match Query.module_summary_of consumer_snapshot consumer_source_id with
      | Some summary -> summary
      | None -> panic "expected consumer module summary"
    in
    let exported_type_decls = ModuleSummary.type_decls consumer_summary
    |> List.map
      (fun (type_decl: FileSummary.type_decl) ->
        (type_decl.scope_path, type_decl.declaration.type_name)) in
    let client_config = Config.default
    |> Config.with_loaded_modules ~loaded_modules:[ consumer_summary ] in
    let client_session = Session.empty ~config:client_config in
    let client_source = "let origin = { x = 0; y = 0 }\nlet total point = point.x + point.y\n" in
    let (client_session, client_source_id) = Session.create_source
      client_session
      ~kind:Source.File
      ~origin:(Source.Label "client.ml")
      ~text:client_source in
    let client_snapshot = Session.snapshot client_session in
    let client_diagnostics = diagnostic_strings client_snapshot client_source_id in
    if not (List.is_empty client_diagnostics) then
      Error (String.concat "\n" client_diagnostics)
    else
      let origin_type = export_scheme client_snapshot client_source_id "origin" in
      let total_type = export_scheme client_snapshot client_source_id "total" in
      let () = Test.assert_equal ~expected:[ ([], "point") ] ~actual:exported_type_decls in
      let () = Test.assert_equal ~expected:(Some "Consumer.point") ~actual:origin_type in
      let () = Test.assert_equal ~expected:(Some "Consumer.point -> int") ~actual:total_type in
      Ok ()

let test_module_alias_reexports_loaded_module_record_types = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = Session.create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_summary_of seed_snapshot helpers_source_id with
    | Some summary -> summary
    | None -> panic "expected helper module summary"
  in
  let consumer_config = Config.default
  |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let consumer_session = Session.empty ~config:consumer_config in
  let (consumer_session, consumer_source_id) = Session.create_source
    consumer_session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:"module Util = Helpers\n" in
  let consumer_snapshot = Session.snapshot consumer_session in
  let consumer_diagnostics = diagnostic_strings consumer_snapshot consumer_source_id in
  if not (List.is_empty consumer_diagnostics) then
    Error (String.concat "\n" consumer_diagnostics)
  else
    let consumer_summary =
      match Query.module_summary_of consumer_snapshot consumer_source_id with
      | Some summary -> summary
      | None -> panic "expected consumer module summary"
    in
    let exported_type_decls = ModuleSummary.type_decls consumer_summary
    |> List.map
      (fun (type_decl: FileSummary.type_decl) ->
        (type_decl.scope_path, type_decl.declaration.type_name)) in
    let client_config = Config.default
    |> Config.with_loaded_modules ~loaded_modules:[ consumer_summary ] in
    let client_session = Session.empty ~config:client_config in
    let client_source = "let origin = { x = 0; y = 0 }\nlet total point = point.x + point.y\n" in
    let (client_session, client_source_id) = Session.create_source
      client_session
      ~kind:Source.File
      ~origin:(Source.Label "client.ml")
      ~text:client_source in
    let client_snapshot = Session.snapshot client_session in
    let client_diagnostics = diagnostic_strings client_snapshot client_source_id in
    if not (List.is_empty client_diagnostics) then
      Error (String.concat "\n" client_diagnostics)
    else
      let origin_type = export_scheme client_snapshot client_source_id "origin" in
      let total_type = export_scheme client_snapshot client_source_id "total" in
      let () = Test.assert_equal ~expected:[ ([ "Util" ], "point") ] ~actual:exported_type_decls in
      let () = Test.assert_equal ~expected:(Some "Consumer.Util.point") ~actual:origin_type in
      let () = Test.assert_equal ~expected:(Some "Consumer.Util.point -> int") ~actual:total_type in
      Ok ()

let test_include_reexports_loaded_module_summaries = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = Session.create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"let id x = x\nlet wrap value = Some value\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_summary_of seed_snapshot helpers_source_id with
    | Some summary -> summary
    | None -> panic "expected helper module summary"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let session = Session.empty ~config in
  let source = "include Helpers\nlet answer = wrap (id 1)\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let id_type = export_scheme snapshot source_id "id" in
    let wrap_type = export_scheme snapshot source_id "wrap" in
    let answer_type = export_scheme snapshot source_id "answer" in
    let exported_names = export_names (Query.export_of snapshot source_id) in
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a") ~actual:id_type in
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a option") ~actual:wrap_type in
    let () = Test.assert_equal ~expected:(Some "int option") ~actual:answer_type in
    let () = Test.assert_equal ~expected:[ "answer"; "id"; "wrap" ] ~actual:exported_names in
    Ok ()

let test_module_alias_reexports_loaded_module_summaries = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = Session.create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"let id x = x\nlet wrap value = Some value\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_summary_of seed_snapshot helpers_source_id with
    | Some summary -> summary
    | None -> panic "expected helper module summary"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let session = Session.empty ~config in
  let source = "module Util = Helpers\nlet answer = Util.wrap (Util.id 1)\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let util_id_type = export_scheme snapshot source_id "Util.id" in
    let util_wrap_type = export_scheme snapshot source_id "Util.wrap" in
    let answer_type = export_scheme snapshot source_id "answer" in
    let exported_names = export_names (Query.export_of snapshot source_id) in
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a") ~actual:util_id_type in
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a option") ~actual:util_wrap_type in
    let () = Test.assert_equal ~expected:(Some "int option") ~actual:answer_type in
    let () = Test.assert_equal ~expected:[ "Util.id"; "Util.wrap"; "answer" ] ~actual:exported_names in
    Ok ()

let test_module_alias_reexports_same_named_local_modules = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, _cell_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "cell.ml")
    ~text:"let create value = value\n" in
  let (session, sync_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "sync.ml")
    ~text:"module Cell = Cell\nlet answer = Cell.create 1\n" in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot sync_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let create_type = export_scheme snapshot sync_source_id "Cell.create" in
    let answer_type = export_scheme snapshot sync_source_id "answer" in
    let exported_names = export_names (Query.export_of snapshot sync_source_id) in
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a") ~actual:create_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:answer_type in
    let () = Test.assert_equal ~expected:[ "Cell.create"; "answer" ] ~actual:exported_names in
    Ok ()

let test_sibling_source_uses_loaded_module_record_reexport = fun _ctx ->
  let seed_session = Session.empty ~config:Config.default in
  let (seed_session, helpers_source_id) = Session.create_source
    seed_session
    ~kind:Source.File
    ~origin:(Source.Label "helpers.ml")
    ~text:"type point = { x: int; y: int }\n" in
  let seed_snapshot = Session.snapshot seed_session in
  let loaded_helpers =
    match Query.module_summary_of seed_snapshot helpers_source_id with
    | Some summary -> summary
    | None -> panic "expected helper module summary"
  in
  let config = Config.default |> Config.with_loaded_modules ~loaded_modules:[ loaded_helpers ] in
  let session = Session.empty ~config in
  let consumer_source = "include Helpers\n" ^ "let origin = { x = 0; y = 0 }\n" in
  let (session, _consumer_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "consumer.ml")
    ~text:consumer_source in
  let client_source = "let total = Consumer.origin.x + Consumer.origin.y\n" in
  let (session, client_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "client.ml")
    ~text:client_source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot client_source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let field_access_offset =
      let access_start = offset_of_substring client_source "Consumer.origin.x +"
      |> Option.expect ~msg:"expected record field access in test source" in
      access_start + String.length "Consumer.origin."
    in
    let total_type = export_scheme snapshot client_source_id "total" in
    let field_access_type = inferred_type_at snapshot client_source_id field_access_offset in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:total_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:field_access_type in
    Ok ()

let test_prepare_snapshot_is_rooted = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, colors_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "colors.ml")
    ~text:"module RGB = struct let blend x y = x end\nlet to_string value = value\n" in
  let (session, demo_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "blend_demo.ml")
    ~text:"open Colors\nlet midpoint = RGB.blend 1 2\nlet label = to_string \"ok\"\n" in
  match prepare_snapshot_or_error session ~roots:[ demo_source_id ] with
  | Error message -> Error message
  | Ok snapshot ->
      let root_ids = Snapshot.roots snapshot in
      let demo_has_unbound_name = has_unbound_name snapshot demo_source_id in
      let demo_analysis_exists = Option.is_some (Query.analysis_of_source snapshot demo_source_id) in
      let colors_analysis_exists = Option.is_some
        (Query.analysis_of_source snapshot colors_source_id) in
      let midpoint_type = inferred_type_at snapshot demo_source_id 34 in
      let label_type = inferred_type_at snapshot demo_source_id 58 in
      let () = Test.assert_equal ~expected:[ demo_source_id ] ~actual:root_ids in
      let () = Test.assert_equal ~expected:true ~actual:demo_analysis_exists in
      let () = Test.assert_equal ~expected:false ~actual:colors_analysis_exists in
      if demo_has_unbound_name then
        Error (String.concat "\n" (diagnostic_strings snapshot demo_source_id))
      else
        let () = Test.assert_equal ~expected:(Some "int -> int -> int") ~actual:midpoint_type in
        let () = Test.assert_equal ~expected:(Some "string -> string") ~actual:label_type in
        Ok ()

let test_prepare_snapshot_reports_missing_roots = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let missing_root = SourceId.of_int 99 in
  match Session.prepare_snapshot session ~roots:[ missing_root ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report the missing root"
  | Error missing ->
      let actual = MissingRequirements.to_json missing in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_root_source");
          ("source_id", Data.Json.Int 99);
        ]
      ] in
      let () = Test.assert_equal ~expected ~actual in
      Ok ()

let test_prepare_snapshot_reports_missing_module_summary = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "uses_missing_module.ml")
    ~text:"open Missing_module\nlet answer = 1\n" in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report missing module summaries"
  | Error missing ->
      let actual = MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int source_id) ]);
        ]
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_collects_transitive_missing_modules = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "dependent.ml")
    ~text:"open Inner\nlet answer = 1\n" in
  let (session, _inner_source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "inner.ml")
    ~text:"open Missing_module\nlet value = 42\n" in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report transitive missing module summaries"
  | Error missing ->
      let actual = MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          ("requested_by", Data.Json.Array [ Data.Json.Int 1 ]);
        ]
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_prepare_snapshot_collects_missing_module_for_qualified_reference = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "qualified.ml")
    ~text:"let answer = Missing_module.value 1\n" in
  match Session.prepare_snapshot session ~roots:[ source_id ] with
  | Ok _ -> Error "expected rooted snapshot preparation to report missing module summaries for qualified access"
  | Error missing ->
      let actual = MissingRequirements.to_json missing |> Data.Json.to_string in
      let expected = Data.Json.Array [
        Data.Json.Object [
          ("tag", Data.Json.String "missing_module_summary");
          ("module_name", Data.Json.String "Missing_module");
          ("requested_by", Data.Json.Array [ Data.Json.Int (SourceId.to_int source_id) ]);
        ]
      ]
      |> Data.Json.to_string in
      if String.equal expected actual then
        Ok ()
      else
        Error (String.concat "\n" [ "expected"; expected; "actual"; actual ])

let test_check_source_recovers_when_snapshot_preparation_reports_missing_module_summaries = fun _ctx ->
  let report = Check.check_source ~filename:(Path.v "uses_missing_module.ml") "open Missing_module\nlet answer = Missing_module.value 1\n" in
  let diagnostics = List.length report.parse_diagnostics
  + List.length report.lowering_diagnostics
  + List.length report.typing_diagnostics in
  if diagnostics > 0 then
    Ok ()
  else
    Error "expected one-shot check_source to surface diagnostics instead of panicking"

let test_match_guards_typecheck_in_pattern_scope = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let classify value =\n"
  ^ "  match value with\n"
  ^ "  | Some n when n > 0 -> n\n"
  ^ "  | Some _ -> 0\n"
  ^ "  | None -> 0\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "match_guard.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let guard_binding_offset = offset_of_substring source "n > 0" |> Option.expect ~msg:"expected match guard in test source" in
    let classify_type = export_scheme snapshot source_id "classify" in
    let guard_binding_type = inferred_type_at snapshot source_id guard_binding_offset in
    let () = Test.assert_equal ~expected:(Some "(int option) -> int") ~actual:classify_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:guard_binding_type in
    Ok ()

let test_optional_arguments_can_be_omitted_and_reordered = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let make_key = fun ?(kind = 0) ?(mods = 1) code -> code + kind + mods\n"
  ^ "let omitted = make_key 3\n"
  ^ "let reordered = make_key ~mods:4 3\n"
  ^ "let explicit = make_key ~kind:5 ~mods:6 7\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "optional_apply.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let reordered_offset = offset_of_substring source "make_key ~mods:4 3" |> Option.expect ~msg:"expected reordered call in test source" in
    let explicit_offset = offset_of_substring source "make_key ~kind:5 ~mods:6 7"
    |> Option.expect ~msg:"expected explicit call in test source" in
    let make_key_type = export_scheme snapshot source_id "make_key" in
    let omitted_type = export_scheme snapshot source_id "omitted" in
    let reordered_type = export_scheme snapshot source_id "reordered" in
    let explicit_type = export_scheme snapshot source_id "explicit" in
    let reordered_callee_type = inferred_type_at snapshot source_id reordered_offset in
    let explicit_callee_type = inferred_type_at snapshot source_id explicit_offset in
    let () = Test.assert_equal ~expected:(Some "?kind:int -> ?mods:int -> int -> int") ~actual:make_key_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:omitted_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:reordered_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:explicit_type in
    let () = Test.assert_equal ~expected:(Some "?kind:int -> ?mods:int -> int -> int") ~actual:reordered_callee_type in
    let () = Test.assert_equal ~expected:(Some "?kind:int -> ?mods:int -> int -> int") ~actual:explicit_callee_type in
    Ok ()

let test_records_flow_through_snapshot_queries = fun _ctx ->
  let expect label expected actual =
    if actual = expected then
      Ok ()
    else
      Error (
        label ^ ": expected " ^ (
          match expected with
          | Some value -> value
          | None -> "<none>"
        ) ^ " but got " ^ (
          match actual with
          | Some value -> value
          | None -> "<none>"
        )
      )
  in
  let session = Session.empty ~config:Config.default in
  let source = "type point = { x: int; y: int }\n"
  ^ "let origin = { x = 0; y = 0 }\n"
  ^ "let move_x point dx = { point with x = point.x + dx }\n"
  ^ "let total = fun { x; y } -> x + y\n"
  ^ "let answer = total (move_x origin 3)\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "records.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let field_access_offset =
      let access_start = offset_of_substring source "point.x +" |> Option.expect ~msg:"expected record field access in test source" in
      access_start + String.length "point."
    in
    let origin_type = export_scheme snapshot source_id "origin" in
    let move_x_type = export_scheme snapshot source_id "move_x" in
    let total_type = export_scheme snapshot source_id "total" in
    let answer_type = export_scheme snapshot source_id "answer" in
    let field_access_type = inferred_type_at snapshot source_id field_access_offset in
    match expect "origin type" (Some "point") origin_type with
    | Error _ as error -> error
    | Ok () -> (
        match expect "move_x type" (Some "point -> int -> point") move_x_type with
        | Error _ as error -> error
        | Ok () -> (
            match expect "total type" (Some "point -> int") total_type with
            | Error _ as error -> error
            | Ok () -> (
                match expect "answer type" (Some "int") answer_type with
                | Error _ as error -> error
                | Ok () -> expect "field access type" (Some "int") field_access_type
              )
          )
      )

let test_fun_cases_keep_preceding_parameters = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let choose = fun base ~delta ->\n"
  ^ "  function\n"
  ^ "  | true -> base + delta\n"
  ^ "  | false -> base\n"
  ^ "\n"
  ^ "let picked = choose 1 ~delta:2 true\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "fun_cases_with_params.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let delta_offset = offset_of_substring source "base + delta"
    |> Option.expect ~msg:"expected labeled parameter reference in test source"
    |> fun start -> start + String.length "base + " in
    let choose_type = export_scheme snapshot source_id "choose" in
    let picked_type = export_scheme snapshot source_id "picked" in
    let delta_type = inferred_type_at snapshot source_id delta_offset in
    let () = Test.assert_equal ~expected:(Some "int -> ~delta:int -> bool -> int") ~actual:choose_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:picked_type in
    let () = Test.assert_equal ~expected:(Some "int") ~actual:delta_type in
    Ok ()

let test_expansive_bindings_stay_monomorphic = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let id x = x\n" ^ "let alias = id id\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "value_restriction.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let id_type = export_scheme snapshot source_id "id" in
    let alias_type = export_scheme snapshot source_id "alias" in
    let () = Test.assert_equal ~expected:(Some "'a. 'a -> 'a") ~actual:id_type in
    let () = Test.assert_equal ~expected:(Some "'a -> 'a") ~actual:alias_type in
    Ok ()

let test_nonexpansive_list_bindings_still_generalize = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let empty = []\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "list_value_restriction.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let empty_type = export_scheme snapshot source_id "empty" in
    let () = Test.assert_equal ~expected:(Some "'a. 'a list") ~actual:empty_type in
    Ok ()

let test_expansive_covariant_lists_still_generalize = fun _ctx ->
  let session = Session.empty ~config:Config.default in
  let source = "let make _ = []\n" ^ "let xs = make ()\n" in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "relaxed_value_restriction.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let xs_type = export_scheme snapshot source_id "xs" in
    let () = Test.assert_equal ~expected:(Some "'a. 'a list") ~actual:xs_type in
    Ok ()

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "source id stays stable across updates" test_source_id_stays_stable_across_updates;
        Test.case "snapshots remain immutable after updates" test_snapshots_remain_immutable_after_updates;
        Test.case "type_at uses smallest indexed expression" test_type_at_uses_smallest_indexed_expression;
        Test.case "snapshot exposes implicit file modules" test_snapshot_exposes_implicit_file_modules;
        Test.case "snapshot collects persisted summaries" test_snapshot_collects_persisted_summaries;
        Test.case "source input hash ignores source id and revision" test_source_input_hash_ignores_source_id_and_revision;
        Test.case "snapshot uses loaded module summaries" test_snapshot_uses_loaded_module_summaries;
        Test.case "snapshot uses sibling source record types" test_snapshot_uses_sibling_source_record_types;
        Test.case "snapshot uses loaded module record types" test_snapshot_uses_loaded_module_record_types;
        Test.case "include reexports loaded module record types" test_include_reexports_loaded_module_record_types;
        Test.case "module alias reexports loaded module record types" test_module_alias_reexports_loaded_module_record_types;
        Test.case "include reexports loaded module summaries" test_include_reexports_loaded_module_summaries;
        Test.case "module aliases reexport loaded module summaries" test_module_alias_reexports_loaded_module_summaries;
        Test.case "module aliases reexport same-named local modules" test_module_alias_reexports_same_named_local_modules;
        Test.case "sibling sources use loaded module record reexports" test_sibling_source_uses_loaded_module_record_reexport;
        Test.case "prepare_snapshot is rooted" test_prepare_snapshot_is_rooted;
        Test.case "prepare_snapshot reports missing roots" test_prepare_snapshot_reports_missing_roots;
        Test.case "prepare_snapshot reports missing module summaries" test_prepare_snapshot_reports_missing_module_summary;
        Test.case "prepare_snapshot collects transitive missing modules" test_prepare_snapshot_collects_transitive_missing_modules;
        Test.case "prepare_snapshot collects missing modules from qualified references" test_prepare_snapshot_collects_missing_module_for_qualified_reference;
        Test.case "check_source recovers when rooted preparation reports missing module summaries" test_check_source_recovers_when_snapshot_preparation_reports_missing_module_summaries;
        Test.case "match guards typecheck in pattern scope" test_match_guards_typecheck_in_pattern_scope;
        Test.case "optional arguments can be omitted and reordered" test_optional_arguments_can_be_omitted_and_reordered;
        Test.case "expansive bindings stay monomorphic" test_expansive_bindings_stay_monomorphic;
        Test.case "nonexpansive list bindings still generalize" test_nonexpansive_list_bindings_still_generalize;
        Test.case "expansive covariant lists still generalize" test_expansive_covariant_lists_still_generalize;
        Test.case "fun cases keep preceding parameters in scope" test_fun_cases_keep_preceding_parameters;
        Test.case "records flow through snapshot queries" test_records_flow_through_snapshot_queries;
      ]
      in
      Test.Cli.main ~name:"typ:session" ~tests ~args)
    ~args:Env.args
    ()
