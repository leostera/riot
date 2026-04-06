open Std
open Typ

let inferred_type_at = fun snapshot source_id offset ->
  Query.type_at snapshot source_id (Position.make ~offset) |> function
  | Some ty -> Some (TypePrinter.type_to_string ty)
  | None -> None

let export_scheme = fun snapshot source_id name ->
  match Query.export_of snapshot source_id with
  | Some (FileSummary.TrustedExport { exports })
  | Some (FileSummary.ErroredExport { exports }) -> List.assoc_opt name exports
  |> Option.map TypePrinter.scheme_to_string
  | Some FileSummary.NoExport
  | None -> None

let diagnostic_strings = fun snapshot source_id ->
  Query.diagnostics snapshot source_id |> List.map
    (
      function
      | Query.Parse diagnostic -> Syn.Diagnostic.to_string diagnostic
      | Query.Lowering diagnostic
      | Query.Typing diagnostic -> Diagnostic.to_string diagnostic
    )

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

let expect_substring_offset = fun source needle ->
  match offset_of_substring source needle with
  | Some offset -> offset
  | None -> Result.expect ~msg:(("expected substring in test source: " ^ needle)) (Error ())

let expect_optional_string = fun ~label ~expected ~actual ->
  if actual = expected then
    Ok ()
  else
    Error (label
    ^ ": expected "
    ^ Option.unwrap_or ~default:"<none>" expected
    ^ " but got "
    ^ Option.unwrap_or ~default:"<none>" actual)

let test_constructor_resolution_uses_expected_type = fun _ctx ->
  let source = String.concat "\n"
    [
      "type key = Enter | Left | Right";
      "type mouse_button = Left | Middle | Right";
      "type event = Key of key | Mouse of mouse_button";
      "let key_to_string = function";
      "  | Enter -> \"enter\"";
      "  | Left -> \"left\"";
      "  | Right -> \"right\"";
      "let button_to_string = function";
      "  | Left -> \"left\"";
      "  | Middle -> \"middle\"";
      "  | Right -> \"right\"";
      "let make_key code = Key code";
      "let make_button button = Mouse button";
      "let right_key = make_key Right";
      "let right_button = make_button Right";
      "";
    ]
  in
  let session = Session.empty ~config:Config.default in
  let (session, source_id) = Session.create_source
    session
    ~kind:Source.File
    ~origin:(Source.Label "constructor_resolution.ml")
    ~text:source in
  let snapshot = Session.snapshot session in
  let diagnostics = diagnostic_strings snapshot source_id in
  if not (List.is_empty diagnostics) then
    Error (String.concat "\n" diagnostics)
  else
    let key_pattern_type = inferred_type_at
      snapshot
      source_id
      (expect_substring_offset source "| Left -> \"left\"") in
    let button_pattern_type = inferred_type_at
      snapshot
      source_id
      (expect_substring_offset source "| Middle -> \"middle\"") in
    let make_key_scheme = export_scheme snapshot source_id "make_key" in
    let make_button_scheme = export_scheme snapshot source_id "make_button" in
    match expect_optional_string ~label:"key pattern" ~expected:(Some "key") ~actual:key_pattern_type with
    | Error _ as err -> err
    | Ok () -> (
        match expect_optional_string
          ~label:"button pattern"
          ~expected:(Some "mouse_button")
          ~actual:button_pattern_type with
        | Error _ as err -> err
        | Ok () ->
            match expect_optional_string ~label:"make_key" ~expected:(Some "key -> event") ~actual:make_key_scheme with
            | Error _ as err -> err
            | Ok () -> expect_optional_string
              ~label:"make_button"
              ~expected:(Some "mouse_button -> event")
              ~actual:make_button_scheme
      )

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests = [
        Test.case "constructor resolution uses expected types across patterns and expressions" test_constructor_resolution_uses_expected_type;
      ] in
      Test.Cli.main ~name:"typ:constructor_resolution" ~tests ~args)
    ~args:Env.args
    ()
