open Std

let has_char = fun subject char ->
  let rec loop index =
    if index >= String.length subject then
      false
    else if subject.[index] = char then
      true
    else
      loop (index + 1)
  in
  loop 0

let normalize_newlines = fun source ->
  if not (has_char source '\r') then
    source
  else
    let buffer = IO.Buffer.create (String.length source) in
    String.iter
      (fun char ->
        if not (Char.equal char '\r') then
          IO.Buffer.add_char buffer char)
      source;
    IO.Buffer.contents buffer

let make_token = fun ~kind ~start ~end_ ~text ->
  { Markdown_token.kind; span = Ceibo.Span.make ~start ~end_; text }

let tokenize = fun source ->
  let length = String.length source in
  let rec loop index line_start acc =
    if index >= length then
      let acc =
        if line_start < length then
          let text = String.sub source line_start (length - line_start) in
          make_token ~kind:Markdown_token.Line_text ~start:line_start ~end_:length ~text :: acc
        else
          acc
      in
      List.rev (make_token ~kind:Markdown_token.EOF ~start:length ~end_:length ~text:"" :: acc)
    else if source.[index] = '\n' then
      let text = String.sub source line_start (index - line_start) in
      let acc =
        make_token ~kind:Markdown_token.Newline ~start:index ~end_:(index + 1) ~text:"\n"
        :: make_token ~kind:Markdown_token.Line_text ~start:line_start ~end_:index ~text
        :: acc
      in
      loop (index + 1) (index + 1) acc
    else
      loop (index + 1) line_start acc
  in
  loop 0 0 []
