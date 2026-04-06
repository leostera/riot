open Std
module Error = Markdown_error

type found_token = {
  kind: string;
  text: string;
}

type kind =
  | Invalid_markdown of { found: found_token }
  | Unsupported_feature of { found: found_token; feature: string }
  | Unclosed_fenced_code_block of { found: found_token; opener: string }
  | Unexpected_control_character of { found: found_token; code: int }
  | Parser_internal of { message: string; found: found_token }

type t = {
  kind: kind;
  span: Ceibo.Span.t;
}

let make = fun ~kind ~span -> { kind; span }

let invalid_markdown = fun ~found ~span -> make ~kind:(Invalid_markdown { found }) ~span

let unsupported_feature = fun ~found ~feature ~span ->
  make ~kind:(Unsupported_feature { found; feature }) ~span

let unclosed_fenced_code_block = fun ~found ~opener ~span ->
  make ~kind:(Unclosed_fenced_code_block { found; opener }) ~span

let unexpected_control_character = fun ~found ~code ~span ->
  make ~kind:(Unexpected_control_character { found; code }) ~span

let parser_internal = fun ~found ~message ~span -> make ~kind:(Parser_internal { message; found }) ~span

let found_token = fun diag ->
  match diag.kind with
  | Invalid_markdown { found }
  | Unsupported_feature { found; _ }
  | Unclosed_fenced_code_block { found; _ }
  | Unexpected_control_character { found; _ }
  | Parser_internal { found; _ } -> found

let expected_message = fun diag ->
  match diag.kind with
  | Invalid_markdown _ -> "valid markdown"
  | Unsupported_feature { feature; _ } -> feature
  | Unclosed_fenced_code_block _ -> "matching closing fence"
  | Unexpected_control_character _ -> "printable character"
  | Parser_internal _ -> "stable parser recovery"

let fix_message = fun diag ->
  match diag.kind with
  | Invalid_markdown _ -> Some "Check markdown syntax near the reported offset."
  | Unsupported_feature { feature; _ } -> Some ("Unsupported feature: " ^ feature)
  | Unclosed_fenced_code_block { opener; _ } -> Some ("Close the " ^ opener ^ " fence with a matching close marker.")
  | Unexpected_control_character _ -> Some "Remove the control character from the source."
  | Parser_internal _ -> Some "Please open an issue and include the input text."

let hint_message = fun diag ->
  match diag.kind with
  | Parser_internal { message; _ } -> message
  | Unsupported_feature { feature; _ } -> "CommonMark extension currently unsupported: " ^ feature
  | Unclosed_fenced_code_block { opener; _ } -> "Code fences opened by " ^ opener ^ " must be closed with the same fence."
  | Unexpected_control_character _ -> "Use spaces, newlines, or escaped text instead."
  | Invalid_markdown _ -> "Adjust the input text and retry."

let main_message = fun diag ->
  match diag.kind with
  | Invalid_markdown { found } -> "Invalid markdown: " ^ found.text
  | Unsupported_feature { found; feature } -> "Unsupported markdown feature `"
  ^ feature
  ^ "` at "
  ^ found.text
  | Unclosed_fenced_code_block { found; opener } -> "Unclosed fenced code block started with "
  ^ opener
  ^ " at "
  ^ found.text
  | Unexpected_control_character { found; code } -> "Unexpected control character U+"
  ^ Int.to_string code
  ^ " ("
  ^ found.text
  ^ ")"
  | Parser_internal { message; _ } -> "Parser internal error: " ^ message

let to_string = fun diag ->
  let diag_id =
    match diag.kind with
    | Invalid_markdown _ -> "markdown_invalid_markdown"
    | Unsupported_feature _ -> "markdown_unsupported_feature"
    | Unclosed_fenced_code_block _ -> "markdown_unclosed_fenced_code_block"
    | Unexpected_control_character _ -> "markdown_unexpected_control_character"
    | Parser_internal _ -> "markdown_parser_internal"
  in
  let fix = Option.unwrap_or ~default:"" (fix_message diag) in
  let hint = hint_message diag in
  let parts = [
    "Parse error \"";
    diag_id;
    "\" at ";
    Ceibo.Span.to_string diag.span;
    ": ";
    main_message diag;
    "\nfix: ";
    fix;
    "\nhint: ";
    hint
  ]
  in
  String.concat "" parts

let error_id = fun diag ->
  match diag.kind with
  | Invalid_markdown _ -> "markdown_invalid_markdown"
  | Unsupported_feature _ -> "markdown_unsupported_feature"
  | Unclosed_fenced_code_block _ -> "markdown_unclosed_fenced_code_block"
  | Unexpected_control_character _ -> "markdown_unexpected_control_character"
  | Parser_internal _ -> "markdown_parser_internal"

let id = fun diag -> error_id diag

let to_json = fun diag ->
  let found = found_token diag in
  let kind_payload =
    match diag.kind with
    | Invalid_markdown _ -> Data.Json.obj
      [
        ("id", Data.Json.string (error_id diag));
        ("name", Data.Json.string "Invalid_markdown");
        (
          "found",
          Data.Json.obj
            [ ("kind", Data.Json.string found.kind); ("text", Data.Json.string found.text) ]
        );
      ]
    | Unsupported_feature { feature; _ } -> Data.Json.obj
      [
        ("id", Data.Json.string (error_id diag));
        ("name", Data.Json.string "Unsupported_feature");
        ("feature", Data.Json.string feature);
        (
          "found",
          Data.Json.obj
            [ ("kind", Data.Json.string found.kind); ("text", Data.Json.string found.text) ]
        );
      ]
    | Unclosed_fenced_code_block { opener; _ } -> Data.Json.obj
      [
        ("id", Data.Json.string (error_id diag));
        ("name", Data.Json.string "Unclosed_fenced_code_block");
        ("opener", Data.Json.string opener);
        (
          "found",
          Data.Json.obj
            [ ("kind", Data.Json.string found.kind); ("text", Data.Json.string found.text) ]
        );
      ]
    | Unexpected_control_character { code; _ } -> Data.Json.obj
      [
        ("id", Data.Json.string (error_id diag));
        ("name", Data.Json.string "Unexpected_control_character");
        ("code", Data.Json.int code);
        (
          "found",
          Data.Json.obj
            [ ("kind", Data.Json.string found.kind); ("text", Data.Json.string found.text) ]
        );
      ]
    | Parser_internal { message; _ } -> Data.Json.obj
      [
        ("id", Data.Json.string (error_id diag));
        ("name", Data.Json.string "Parser_internal");
        ("message", Data.Json.string message);
        (
          "found",
          Data.Json.obj
            [ ("kind", Data.Json.string found.kind); ("text", Data.Json.string found.text) ]
        );
      ]
  in
  Data.Json.obj
    [
      ("kind", kind_payload);
      (
        "span",
        Data.Json.obj
          [
            ("start", Data.Json.int diag.span.Ceibo.Span.start);
            ("end", Data.Json.int diag.span.Ceibo.Span.end_);
          ]
      );
      ("main_message", Data.Json.string (main_message diag));
      ("id", Data.Json.string (error_id diag));
    ]

let from_json = fun json ->
  match json with
  | Data.Json.Object fields -> (
      let kind_field = List.assoc_opt "kind" fields in
      let span_field = List.assoc_opt "span" fields in
      let kind_obj =
        match kind_field with
        | Some (Object fields) -> Some fields
        | Some (String kind) -> Some [ ("id", Data.Json.string kind) ]
        | _ -> None
      in
      match (kind_obj, span_field) with
      | Some kind_fields, Some (Object span_fields) -> (
          let id =
            match List.assoc_opt "id" kind_fields with
            | Some (Data.Json.String value) -> Some value
            | Some _
            | None -> None
          in
          let start =
            match List.assoc_opt "start" span_fields with
            | Some (Data.Json.Int value) -> value
            | Some _
            | None -> 0
          in
          let end_ =
            match List.assoc_opt "end" span_fields with
            | Some (Data.Json.Int value) -> value
            | Some _
            | None -> start
          in
          let found =
            let found_fields =
              match List.assoc_opt "found" kind_fields with
              | Some (Object fields) -> fields
              | _ -> []
            in
            let kind =
              match List.assoc_opt "kind" found_fields with
              | Some (Data.Json.String value) -> value
              | _ -> "token"
            in
            let text =
              match List.assoc_opt "text" found_fields with
              | Some (Data.Json.String value) -> value
              | _ -> ""
            in
            { kind; text }
          in
          let span = Ceibo.Span.make ~start ~end_ in
          match id with
          | Some "markdown_invalid_markdown" ->
              Ok { kind = Invalid_markdown { found }; span }
          | Some "markdown_unsupported_feature" -> (
              let feature =
                match List.assoc_opt "feature" kind_fields with
                | Some (Data.Json.String value) -> value
                | _ -> "unsupported"
              in
              Ok { kind = Unsupported_feature { found; feature }; span }
            )
          | Some "markdown_unclosed_fenced_code_block" -> (
              let opener =
                match List.assoc_opt "opener" kind_fields with
                | Some (Data.Json.String value) -> value
                | _ -> "```"
              in
              Ok { kind = Unclosed_fenced_code_block { found; opener }; span }
            )
          | Some "markdown_unexpected_control_character" -> (
              let code =
                match List.assoc_opt "code" kind_fields with
                | Some (Data.Json.Int value) -> value
                | _ -> 0
              in
              Ok { kind = Unexpected_control_character { found; code }; span }
            )
          | Some "markdown_parser_internal" -> (
              let message =
                match List.assoc_opt "message" kind_fields with
                | Some (Data.Json.String value) -> value
                | _ -> ""
              in
              Ok { kind = Parser_internal { found; message }; span }
            )
          | Some unknown ->
              Error ("unknown diagnostic id: " ^ unknown)
          | None ->
              Error "expected diagnostic id"
        )
      | _ -> Error "invalid diagnostic json shape"
    )
  | _ -> Error "expected diagnostic json object"
