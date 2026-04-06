open Std
module Diagnostic = Commonmark_diagnostic

type source_layout = {
  lines: string list;
  line_starts: int list;
}

let make_source_layout = fun source ->
  let lines = String.split_on_char '\n' source in
  let rec build_starts remaining offset acc =
    match remaining with
    | [] -> List.rev acc
    | head :: tail -> build_starts tail (offset + String.length head + 1) (offset :: acc)
  in
  let line_starts = build_starts lines 0 [] in
  { lines; line_starts }

let nth_opt = fun list index ->
  let rec loop idx values =
    match values with
    | [] -> None
    | head :: tail ->
        if idx = index then
          Some head
        else
          loop (idx + 1) tail
  in
  loop 0 list

let line_for_pos = fun layout pos ->
  let rec loop remaining index current best =
    match remaining with
    | [] -> (best, current)
    | head :: tail ->
        if head > pos then
          (best, current)
        else
          loop tail (index + 1) head index
  in
  if layout.line_starts = [] then
    (0, 0)
  else
    let line_idx, line_start = loop layout.line_starts 0 0 0 in
    (line_idx, Int.max 0 (pos - line_start))

let extract_code_snippet = fun layout (span: Ceibo.Span.t) ->
  if layout.lines = [] then
    None
  else
    let line_idx, start_col = line_for_pos layout span.Ceibo.Span.start in
    match nth_opt layout.lines line_idx with
    | None -> None
    | Some code_line ->
        let pointer = String.make start_col ' ' ^ "^" in
        Some (code_line, pointer, line_idx + 1)

let format = fun ~file ~source diagnostics ->
  let layout = make_source_layout source in
  let format_one diag =
    let fix = Diagnostic.fix_message diag in
    let hint = Diagnostic.hint_message diag in
    let expected = Diagnostic.expected_message diag in
    let main_message = Diagnostic.main_message diag in
    let line =
      match extract_code_snippet layout diag.span with
      | None -> "  |\n"
      | Some (code_line, pointer, line_num) -> file
      ^ "\n"
      ^ Int.to_string line_num
      ^ " | "
      ^ code_line
      ^ "\n"
      ^ "  | "
      ^ pointer
      ^ " expected "
      ^ expected
      ^ "\n"
    in
    let rendered_fix =
      match fix with
      | Some fix_msg -> "\n  fix: " ^ fix_msg
      | None -> ""
    in
    main_message ^ "\n" ^ line ^ "  hint: " ^ hint ^ rendered_fix ^ "\n"
  in
  String.concat "\n" (List.map format_one diagnostics)

let print = fun ~file ~source diagnostics -> print (format ~file ~source diagnostics)
