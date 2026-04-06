open Std
open Markdown_parser

let escape_html = fun text ->
  let length = String.length text in
  let buffer = IO.Buffer.create length in
  let rec loop index =
    if index >= length then
      ()
    else (
        match text.[index] with
        | '&' -> IO.Buffer.add_string buffer "&amp;"
        | '<' -> IO.Buffer.add_string buffer "&lt;"
        | '>' -> IO.Buffer.add_string buffer "&gt;"
        | '"' -> IO.Buffer.add_string buffer "&quot;"
        | c -> IO.Buffer.add_char buffer c
      ;
      loop (index + 1)
    )
  in
  ignore (loop 0);
  IO.Buffer.contents buffer

let escape_attribute = fun text ->
  let length = String.length text in
  let buffer = IO.Buffer.create length in
  let rec loop index =
    if index >= length then
      ()
    else (
        match text.[index] with
        | '&' -> IO.Buffer.add_string buffer "&amp;"
        | '"' -> IO.Buffer.add_string buffer "&quot;"
        | '<' -> IO.Buffer.add_string buffer "&lt;"
        | '>' -> IO.Buffer.add_string buffer "&gt;"
        | c -> IO.Buffer.add_char buffer c
      ;
      loop (index + 1)
    )
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
            | Strikethrough children ->
                IO.Buffer.add_string buffer "<del>";
                IO.Buffer.add_string buffer (loop children);
                IO.Buffer.add_string buffer "</del>"
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

let rec render_item_blocks = fun ~tight blocks ->
  let render_item_block = fun block ->
    match block with
    | Paragraph { inlines; _ } when tight ->
        render_inlines inlines
    | _ ->
        render_block block
  in
  blocks |> List.map render_item_block |> String.concat ""

and render_block = fun block ->
  let render_children blocks = blocks |> List.map render_block |> String.concat "" in
  let render_list_item = fun ~tight item ->
    match item with
    | [ Task_list_item { checked; blocks; _ } ] ->
        let checkbox =
          if checked then
            "<input type=\"checkbox\" checked disabled />"
          else
            "<input type=\"checkbox\" disabled />"
        in
        "<li class=\"task-list-item\">\n" ^ checkbox ^ render_item_blocks ~tight:false blocks ^ "</li>\n"
    | [ List_item { blocks; _ } ] ->
        if tight then
          "<li>" ^ render_item_blocks ~tight blocks ^ "</li>\n"
        else
          "<li>\n" ^ render_item_blocks ~tight blocks ^ "</li>\n"
    | blocks ->
        if tight then
          "<li>" ^ render_item_blocks ~tight blocks ^ "</li>\n"
        else
          "<li>\n" ^ render_item_blocks ~tight blocks ^ "</li>\n"
  in
  let render_aligned_cell = fun tag alignment cell ->
    let content = render_inlines cell in
    let align_attr =
      match alignment with
      | Default -> ""
      | Left -> " align=\"left\""
      | Center -> " align=\"center\""
      | Right -> " align=\"right\""
    in
    "<" ^ tag ^ align_attr ^ ">" ^ content ^ "</" ^ tag ^ ">\n"
  in
  let render_table_row = fun tag row ->
    let cells =
      List.combine row.alignments row.cells
      |> List.map (fun (alignment, cell) -> render_aligned_cell tag alignment cell)
      |> String.concat ""
    in
    "<tr>\n" ^ cells ^ "</tr>\n"
  in
  match block with
  | Heading { level; inlines; _ } ->
      let heading = Int.max 1 (Int.min 6 level) in
      let content = render_inlines inlines in
      "<h" ^ Int.to_string heading ^ ">" ^ content ^ "</h" ^ Int.to_string heading ^ ">\n"
  | Paragraph { inlines; _ } ->
      "<p>" ^ render_inlines inlines ^ "</p>\n"
  | Block_quote { blocks; _ } ->
      "<blockquote>\n" ^ render_children blocks ^ "</blockquote>\n"
  | List { ordered; tight; items; _ } ->
      let open_tag =
        if ordered then
          "ol"
        else
          "ul"
      in
      let children = items |> List.map (render_list_item ~tight) |> String.concat "" in
      "<" ^ open_tag ^ ">\n" ^ children ^ "</" ^ open_tag ^ ">\n"
  | Task_list_item { checked; blocks; _ } ->
      let checkbox =
        if checked then
          "<input type=\"checkbox\" checked disabled />"
        else
          "<input type=\"checkbox\" disabled />"
      in
      "<li class=\"task-list-item\">\n" ^ checkbox ^ render_children blocks ^ "</li>\n"
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
  | Table { header; rows; _ } ->
      let thead = "<thead>\n" ^ render_table_row "th" header ^ "</thead>\n" in
      let tbody =
        if rows = [] then
          ""
        else
          "<tbody>\n"
          ^ (rows |> List.map (render_table_row "td") |> String.concat "")
          ^ "</tbody>\n"
      in
      "<table>\n" ^ thead ^ tbody ^ "</table>\n"
  | Error_block { message; _ } ->
      "<!-- " ^ escape_html message ^ " -->\n"

let render = fun blocks -> blocks |> List.map render_block |> String.concat ""
