open Std
open Commonmark_parser

let escape_html = fun text ->
  let length = String.length text in
  let buffer = IO.Buffer.create length in
  let rec loop index =
    if index >= length then
      ()
    else
      (
        match text.[index] with
        | '&' -> IO.Buffer.add_string buffer "&amp;"
        | '<' -> IO.Buffer.add_string buffer "&lt;"
        | '>' -> IO.Buffer.add_string buffer "&gt;"
        | '"' -> IO.Buffer.add_string buffer "&quot;"
        | '\'' -> IO.Buffer.add_string buffer "&#39;"
        | c -> IO.Buffer.add_char buffer c
      );
      loop (index + 1)
  in
  ignore (loop 0);
  IO.Buffer.contents buffer

let escape_attribute = fun text ->
  let length = String.length text in
  let buffer = IO.Buffer.create length in
  let rec loop index =
    if index >= length then
      ()
    else
      (
        match text.[index] with
        | '&' -> IO.Buffer.add_string buffer "&amp;"
        | '"' -> IO.Buffer.add_string buffer "&quot;"
        | '\'' -> IO.Buffer.add_string buffer "&#39;"
        | '<' -> IO.Buffer.add_string buffer "&lt;"
        | '>' -> IO.Buffer.add_string buffer "&gt;"
        | c -> IO.Buffer.add_char buffer c
      );
      loop (index + 1)
  in
  ignore (loop 0);
  IO.Buffer.contents buffer

let render_inlines = fun inlines ->
  let rec loop nodes =
    let buffer = IO.Buffer.create 32 in
    let rec append acc =
      match acc with
      | [] -> IO.Buffer.contents buffer
      | head :: tail ->
          (
            match head with
            | Text text ->
                IO.Buffer.add_string buffer (escape_html text)
            | Emphasis children ->
                IO.Buffer.add_string buffer "<em>";
                IO.Buffer.add_string buffer (loop children);
                IO.Buffer.add_string buffer "</em>"
            | Strong children ->
                IO.Buffer.add_string buffer "<strong>";
                IO.Buffer.add_string buffer (loop children);
                IO.Buffer.add_string buffer "</strong>"
            | Code_span text ->
                IO.Buffer.add_string buffer "<code>";
                IO.Buffer.add_string buffer (escape_html text);
                IO.Buffer.add_string buffer "</code>"
            | Raw_html html ->
                IO.Buffer.add_string buffer html
            | Link { label; destination } ->
                IO.Buffer.add_string buffer "<a href=\"";
                IO.Buffer.add_string buffer (escape_attribute destination);
                IO.Buffer.add_string buffer "\">";
                IO.Buffer.add_string buffer (loop label);
                IO.Buffer.add_string buffer "</a>"
          );
          append tail
    in
    append nodes
  in
  loop inlines

let rec render_block = fun block ->
  let render_children blocks = blocks |> List.map render_block |> String.concat "" in
  match block with
  | Heading { level; inlines; _ } ->
      let heading = Int.max 1 (Int.min 6 level) in
      let content = render_inlines inlines in
      "<h" ^ Int.to_string heading ^ ">" ^ content ^ "</h" ^ Int.to_string heading ^ ">\n"
  | Paragraph { inlines; _ } ->
      "<p>" ^ render_inlines inlines ^ "</p>\n"
  | Block_quote { blocks; _ } ->
      "<blockquote>\n" ^ render_children blocks ^ "</blockquote>\n"
  | List { ordered; items; _ } ->
      let open_tag =
        if ordered then
          "ol"
        else
          "ul"
      in
      let children =
        items
        |> List.map
          (fun item ->
            let item_html = render_children item in
            "<li>\n" ^ item_html ^ "</li>\n")
        |> String.concat ""
      in
      "<" ^ open_tag ^ ">\n" ^ children ^ "</" ^ open_tag ^ ">\n"
  | List_item { blocks; _ } ->
      "<li>\n" ^ render_children blocks ^ "</li>\n"
  | Code_block { code; info; _ } ->
      let content = escape_html code in
      (
        if String.length info = 0 then
          "<pre><code>" ^ content ^ "</code></pre>\n"
        else
          "<pre><code class=\"language-" ^ escape_attribute info ^ "\">" ^ content ^ "</code></pre>\n"
      )
  | Horizontal_rule _ ->
      "<hr />\n"
  | Raw_html { html; _ } ->
      html ^ "\n"
  | Error_block { message; _ } ->
      "<!-- " ^ escape_html message ^ " -->\n"

let render = fun blocks -> blocks |> List.map render_block |> String.concat ""
