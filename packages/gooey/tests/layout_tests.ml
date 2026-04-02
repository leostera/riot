open Std
open Gooey

let make_config = fun () ->
  Config.make
    ~viewport:(Viewport.make ~width:200.0 ~height:100.0)
    ~text_measurer:Config.default_text_measurer
    ()

let test_layout_single_text = fun _ctx ->
  let elem = Element.text "Hello" in
  let commands = layout ~config:(make_config ()) elem in
  if List.length commands != 1 then
    Error ("Expected 1 command, got " ^ Int.to_string (List.length commands))
  else
    match List.hd commands with
    | { Render.command_type=Text { content; _ }; bounding_box; _ } ->
        if content != "Hello" then
          Error "Text content should be 'Hello'"
        else if bounding_box.width <= 0.0 then
          Error "Bounding box width should be positive"
        else if bounding_box.height <= 0.0 then
          Error "Bounding box height should be positive"
        else
          Ok ()
    | _ -> Error "Expected Text command"

let test_layout_row = fun _ctx ->
  let elem = Element.row [ Element.text "A"; Element.text "B"; Element.text "C"; ] in
  let commands = layout ~config:(make_config ()) elem in
  if List.length commands != 3 then
    Error ("Expected 3 commands, got " ^ Int.to_string (List.length commands))
  else
    let texts =
      List.map
        (fun cmd ->
          match cmd.Render.command_type with
          | Text { content; _ } -> content
          | _ -> "")
        commands
    in
    if texts = [ "A"; "B"; "C" ] then
      Ok ()
    else
      Error ("Expected texts [A; B; C], got [" ^ String.concat "; " texts ^ "]")

let test_layout_column = fun _ctx ->
  let elem = Element.column [ Element.text "A"; Element.text "B" ] in
  let commands = layout ~config:(make_config ()) elem in
  let texts =
    List.map
      (fun cmd ->
        match cmd.Render.command_type with
        | Text { content; _ } -> content
        | _ -> "")
      commands
  in
  if texts = [ "A"; "B" ] then
    Ok ()
  else
    Error ("Expected texts [A; B], got [" ^ String.concat "; " texts ^ "]")

let test_layout_with_padding = fun _ctx ->
  let elem = Element.container ~style:Style.(empty |> padding (Padding.all 10)) [ Element.text "Hi" ] in
  let commands = layout ~config:(make_config ()) elem in
  if List.length commands != 1 then
    Error ("Expected 1 command, got " ^ Int.to_string (List.length commands))
  else
    match List.hd commands with
    | { Render.command_type=Text _; bounding_box; _ } ->
        (* Text should be offset by padding *)
        if bounding_box.x = 10.0 && bounding_box.y = 10.0 then
          Ok ()
        else
          Error ("Expected text at (10.0, 10.0), got ("
          ^ Float.to_string bounding_box.x
          ^ ", "
          ^ Float.to_string bounding_box.y
          ^ ")")
    | _ -> Error "Expected Text command"

let test_layout_with_child_gap = fun _ctx ->
  let elem = Element.row ~style:Style.(empty |> child_gap 5) [ Element.text "A"; Element.text "B" ] in
  let commands = layout ~config:(make_config ()) elem in
  let positions =
    List.filter_map
      (fun cmd ->
        match cmd.Render.command_type with
        | Text _ -> Some cmd.bounding_box.x
        | _ -> None)
      commands
  in
  (* First at 0.0, second at width(A)=40.0 + gap=5.0 = 45.0 *)
  if List.length positions = 2 && List.hd positions = 0.0 && List.nth positions 1 > 5.0 then
    Ok ()
  else
    Error ("Expected positions starting at 0.0 with gap, got ["
    ^ String.concat "; " (List.map Float.to_string positions)
    ^ "]")

let test_layout_grow_sizing = fun _ctx ->
  let elem = Element.container ~style:Style.(empty |> grow) [] in
  let commands = layout ~config:(make_config ()) elem in
  (* Empty container with no background produces no commands *)
  if List.length commands = 0 then
    Ok ()
  else
    Error "Empty grow container should produce no commands"

let test_layout_fixed_sizing = fun _ctx ->
  let elem = Element.container
    ~style:Style.(empty |> width (Fixed 50.0) |> height (Fixed 30.0) |> bg (`rgb (0, 0, 0)))
    [] in
  let commands = layout ~config:(make_config ()) elem in
  if List.length commands != 1 then
    Error ("Expected 1 command, got " ^ Int.to_string (List.length commands))
  else
    match List.hd commands with
    | { Render.command_type=Rectangle _; bounding_box; _ } ->
        if bounding_box.width = 50.0 && bounding_box.height = 30.0 then
          Ok ()
        else
          Error ("Expected 50.0x30.0, got "
          ^ Float.to_string bounding_box.width
          ^ "x"
          ^ Float.to_string bounding_box.height)
    | _ -> Error "Expected Rectangle command"

let test_nested_layout = fun _ctx ->
  let elem = Element.column
    [
      Element.text "Title";
      Element.row [ Element.text "A"; Element.text "B" ];
      Element.text "Footer";
    ] in
  let commands = layout ~config:(make_config ()) elem in
  let texts =
    List.filter_map
      (fun cmd ->
        match cmd.Render.command_type with
        | Text { content; _ } -> Some content
        | _ -> None)
      commands
  in
  if texts = [ "Title"; "A"; "B"; "Footer" ] then
    Ok ()
  else
    Error ("Expected [Title; A; B; Footer], got [" ^ String.concat "; " texts ^ "]")

let tests =
  Test.[
    case "Layout single text" test_layout_single_text;
    case "Layout row" test_layout_row;
    case "Layout column" test_layout_column;
    case "Layout with padding" test_layout_with_padding;
    case "Layout with child_gap" test_layout_with_child_gap;
    case "Layout GROW sizing" test_layout_grow_sizing;
    case "Layout FIXED sizing" test_layout_fixed_sizing;
    case "Nested layout" test_nested_layout;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"layout" ~tests ~args) ~args:Env.args ()
