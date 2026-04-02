open Std
open Gooey

let make_config = fun () ->
  Config.make
    ~viewport:(Viewport.make ~width:100.0 ~height:100.0)
    ~text_measurer:Config.default_text_measurer
    ()

let test_render_text_command = fun _ctx ->
  let elem = Element.text ~style:Style.(empty |> fg (`rgb (255, 0, 0))) "Test" in
  let commands = layout ~config:(make_config ()) elem in
  match List.hd commands with
  | { Render.command_type=Text { color=`rgb (255, 0, 0); _ }; _ } -> Ok ()
  | _ -> Error "Expected Text command with rgb(255, 0, 0) color"

let test_render_background = fun _ctx ->
  let elem = Element.container ~style:Style.(empty |> bg (`rgb (100, 150, 200))) [] in
  let commands = layout ~config:(make_config ()) elem in
  match List.hd commands with
  | { Render.command_type=Rectangle { color=`rgb (100, 150, 200); _ }; _ } -> Ok ()
  | _ -> Error "Expected Rectangle command with rgb(100, 150, 200) color"

let test_render_border = fun _ctx ->
  let elem = Element.container
    ~style:Style.(empty |> border ~width:2 ~color:(`rgb (50, 50, 50)) ())
    [] in
  let commands = layout ~config:(make_config ()) elem in
  let has_border =
    List.exists
      (fun cmd ->
        match cmd.Render.command_type with
        | Border { color=`rgb (50, 50, 50); _ } -> true
        | _ -> false)
      commands
  in
  if has_border then
    Ok ()
  else
    Error "Expected Border command with rgb(50, 50, 50) color"

let test_render_background_and_border = fun _ctx ->
  let elem = Element.container
    ~style:Style.(empty |> bg (`rgb (255, 255, 255)) |> border ~width:2 ~color:(`rgb (0, 0, 0)) ())
    [] in
  let commands = layout ~config:(make_config ()) elem in
  let has_rect =
    List.exists
      (fun cmd ->
        match cmd.Render.command_type with
        | Rectangle { color=`rgb (255, 255, 255); _ } -> true
        | _ -> false)
      commands
  in
  let has_border =
    List.exists
      (fun cmd ->
        match cmd.Render.command_type with
        | Border { color=`rgb (0, 0, 0); _ } -> true
        | _ -> false)
      commands
  in
  if has_rect && has_border then
    Ok ()
  else if not has_rect then
    Error "Missing background rectangle"
  else
    Error "Missing border"

let test_render_z_index_sorting = fun _ctx ->
  let elem = Element.container
    [
      Element.text ~style:Style.(empty |> z_index 2) "Top";
      Element.text ~style:Style.(empty |> z_index 0) "Bottom";
      Element.text ~style:Style.(empty |> z_index 1) "Middle";
    ] in
  let commands = layout ~config:(make_config ()) elem in
  let z_indices =
    List.map (fun cmd -> cmd.Render.z_index) commands
  in
  if z_indices = [ 0; 1; 2 ] then
    Ok ()
  else
    Error ("Expected z_indices [0; 1; 2], got ["
    ^ String.concat "; " (List.map Int.to_string z_indices)
    ^ "]")

let test_render_corner_radius = fun _ctx ->
  let elem = Element.container
    ~style:Style.(empty |> bg (`rgb (255, 0, 0)) |> border ~radius:(CornerRadius.all 8.0) ())
    [] in
  let commands = layout ~config:(make_config ()) elem in
  (* Should have background rectangle with corner radius from border *)
  let has_rect =
    List.exists
      (fun cmd ->
        match cmd.Render.command_type with
        | Rectangle { color=`rgb (255, 0, 0); corner_radius; _ } -> corner_radius.top_left = 8.0
        && corner_radius.top_right = 8.0
        && corner_radius.bottom_left = 8.0
        && corner_radius.bottom_right = 8.0
        | _ -> false)
      commands
  in
  if has_rect then
    Ok ()
  else
    Error "Expected Rectangle with corner radius"

let test_render_command_bounding_boxes = fun _ctx ->
  let elem = Element.row [ Element.text "A"; Element.text "B"; ] in
  let commands = layout ~config:(make_config ()) elem in
  let all_valid =
    List.for_all
      (fun (cmd: Render.command) -> cmd.bounding_box.width > 0.0 && cmd.bounding_box.height > 0.0)
      commands
  in
  if all_valid then
    Ok ()
  else
    Error "Some commands have invalid bounding boxes"

let tests =
  Test.[
    case "Render text command" test_render_text_command;
    case "Render background" test_render_background;
    case "Render border" test_render_border;
    case "Render background and border" test_render_background_and_border;
    case "Render z_index sorting" test_render_z_index_sorting;
    case "Render corner radius" test_render_corner_radius;
    case "Render command bounding boxes" test_render_command_bounding_boxes;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"render" ~tests ~args) ~args:Env.args ()
