open Std
open Gooey

let make_config = fun () ->
  Config.make
    ~viewport:(Viewport.make ~width:80.0 ~height:24.0)
    ~text_measurer:Config.default_text_measurer
    ()

let test_complex_nested_layout = fun _ctx ->
  let elem = Element.column
    [
      Element.container
        ~style:Style.(empty |> bg (`rgb (50, 50, 50)) |> padding (Padding.all 2))
        [ Element.text ~style:Style.(empty |> bold) "Header" ];
      Element.row [ Element.text "A"; Element.text "B"; ];
      Element.text "Footer";
    ] in
  let commands = layout ~config:(make_config ()) elem in
  let text_count =
    List.fold_left
      (fun acc cmd ->
        match cmd.Render.command_type with
        | Text _ -> acc + 1
        | _ -> acc)
      0
      commands
  in
  if text_count = 4 then
    Ok ()
  else
    Error ("Expected 4 text commands, got " ^ Int.to_string text_count)

let test_flexbox_style_layout = fun _ctx ->
  let elem = Element.row [ Element.text "Left"; Element.spacer ~flex:1.0 (); Element.text "Right"; ] in
  let commands = layout ~config:(make_config ()) elem in
  let text_positions =
    List.filter_map
      (fun cmd ->
        match cmd.Render.command_type with
        | Text { content; _ } -> Some (content, cmd.bounding_box.x)
        | _ -> None)
      commands
  in
  match text_positions with
  | [("Left", x1);("Right", x2)] when x1 = 0.0 && x2 > 10.0 -> Ok ()
  | _ -> Error "Spacer should push 'Right' text far to the right"

let test_responsive_percent_sizing = fun _ctx ->
  let elem = Element.row
    [
      Element.container ~style:Style.(empty |> width (Percent 0.3) |> bg (`rgb (255, 0, 0))) [];
      Element.container ~style:Style.(empty |> width (Percent 0.7) |> bg (`rgb (0, 255, 0))) [];
    ] in
  let commands = layout ~config:(make_config ()) elem in
  let widths =
    List.filter_map
      (fun cmd ->
        match cmd.Render.command_type with
        | Rectangle _ -> Some cmd.bounding_box.width
        | _ -> None)
      commands
  in
  if widths = [ 24.0; 56.0 ] then
    Ok ()
  else
    Error ("Expected widths [24.0; 56.0] (30% and 70% of 80.0), got ["
    ^ String.concat "; " (List.map Float.to_string widths)
    ^ "]")

let test_card_ui_pattern = fun _ctx ->
  let card = Element.column
    ~style:Style.(empty
    |> width (Fixed 60.0)
    |> bg (`rgb (255, 255, 255))
    |> border ~width:1 ~color:(`rgb (200, 200, 200)) ()
    |> padding (Padding.all 4))
    [
      Element.container ~style:Style.(empty |> height (Fixed 20.0) |> bg (`rgb (220, 220, 220))) [];
      Element.text ~style:Style.(empty |> bold) "Card Title";
      Element.text "Description";
    ] in
  let commands = layout ~config:(make_config ()) card in
  let rect_count =
    List.fold_left
      (fun acc cmd ->
        match cmd.Render.command_type with
        | Rectangle _ -> acc + 1
        | _ -> acc)
      0
      commands
  in
  let text_count =
    List.fold_left
      (fun acc cmd ->
        match cmd.Render.command_type with
        | Text _ -> acc + 1
        | _ -> acc)
      0
      commands
  in
  if rect_count >= 2 && text_count = 2 then
    Ok ()
  else
    Error ("Expected >=2 rectangles and 2 texts, got "
    ^ Int.to_string rect_count
    ^ " rectangles and "
    ^ Int.to_string text_count
    ^ " texts")

let test_grid_like_layout = fun _ctx ->
  let grid = Element.column
    [
      Element.row
        [
          Element.container ~style:Style.(empty |> grow |> bg (`rgb (255, 0, 0))) [];
          Element.container ~style:Style.(empty |> grow |> bg (`rgb (0, 255, 0))) [];
        ];
      Element.row
        [
          Element.container ~style:Style.(empty |> grow |> bg (`rgb (0, 0, 255))) [];
          Element.container ~style:Style.(empty |> grow |> bg (`rgb (255, 255, 0))) [];
        ];
    ] in
  let commands = layout ~config:(make_config ()) grid in
  let colors =
    List.filter_map
      (fun cmd ->
        match cmd.Render.command_type with
        | Rectangle { color; _ } -> Some color
        | _ -> None)
      commands
  in
  if colors = [ `rgb (255, 0, 0); `rgb (0, 255, 0); `rgb (0, 0, 255); `rgb (255, 255, 0); ] then
    Ok ()
  else
    Error "Grid layout should produce 4 rectangles with correct colors"

let test_alignment_with_fixed_sizes = fun _ctx ->
  let elem = Element.column
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 30.0) |> height (Fixed 10.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width (Fixed 40.0) |> height (Fixed 15.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  let commands = layout ~config:(make_config ()) elem in
  let boxes =
    List.filter_map
      (fun cmd ->
        match cmd.Render.command_type with
        | Rectangle _ -> Some (
          cmd.bounding_box.x,
          cmd.bounding_box.y,
          cmd.bounding_box.width,
          cmd.bounding_box.height
        )
        | _ -> None)
      commands
  in
  if boxes = [ (0.0, 0.0, 30.0, 10.0); (0.0, 10.0, 40.0, 15.0); ] then
    Ok ()
  else
    Error "Fixed-size elements should be positioned correctly in column"

let tests =
  Test.[
    case "Complex nested layout" test_complex_nested_layout;
    case "Flexbox-style layout with spacer" test_flexbox_style_layout;
    case "Responsive percent sizing" test_responsive_percent_sizing;
    case "Card UI pattern" test_card_ui_pattern;
    case "Grid-like layout" test_grid_like_layout;
    case "Alignment with fixed sizes" test_alignment_with_fixed_sizes;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"integration" ~tests ~args) ~args:Env.args ()
