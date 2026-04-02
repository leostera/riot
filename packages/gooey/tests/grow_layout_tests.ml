open Std
open Gooey

(* Test that Grow correctly distributes space *)

let text_measurer = fun text _style ->
  let width = float_of_int (String.length text) in
  let height = 1.0 in
  Viewport.make ~width ~height

let test_three_column_with_grow = fun _ctx ->
  (* Left: Fixed 20, Middle: Grow, Right: Fixed 15
     In an 80-wide viewport:
     - Left should be 20
     - Right should be 15
     - Middle should be 80 - 20 - 15 = 45
  *)
  let ui = Element.row
    [
      Element.container
        ~style:((Style.empty |> Style.width (Style.Fixed 20.0) |> Style.bg (`rgb (255, 0, 0))))
        [ Element.text "Left" ];
      Element.container
        ~style:((Style.empty |> Style.width Style.Grow |> Style.bg (`rgb (0, 255, 0))))
        [ Element.text "Middle" ];
      Element.container
        ~style:((Style.empty |> Style.width (Style.Fixed 15.0) |> Style.bg (`rgb (0, 0, 255))))
        [ Element.text "Right" ];
    ] in
  let viewport = Viewport.make ~width:80.0 ~height:24.0 in
  let config = Config.make ~viewport ~text_measurer () in
  let commands = Gooey.layout ~config ui in
  (* Find the bounding boxes for each container *)
  let rectangles =
    List.filter_map
      (fun cmd ->
        match cmd.Render.command_type with
        | Render.Rectangle _ -> Some cmd.bounding_box
        | _ -> None)
      commands
  in
  (* Should have 3 rectangles for the 3 containers *)
  let rect_count = List.length rectangles in
  if rect_count != 3 then
    Error ("Expected 3 rectangles, got " ^ Int.to_string rect_count)
  else
    begin
      (* Get the widths *)
      let widths =
        List.map (fun (bbox: Geometry.Rect.t) -> bbox.width) rectangles
      in
      (* Check widths *)
      match widths with
      | [left_width;middle_width;right_width] ->
          (* Check left width *)
          if Float.abs (left_width -. 20.0) > 0.01 then
            Error ("Left width should be 20, got " ^ Float.to_string left_width)
          else if Float.abs (right_width -. 15.0) > 0.01 then
            Error ("Right width should be 15, got " ^ Float.to_string right_width)
          else if Float.abs (middle_width -. 45.0) > 0.01 then
            Error ("Middle width should be 45 (80 - 20 - 15), got " ^ Float.to_string middle_width)
          else
            Ok ()
      | _ -> Error ("Expected 3 widths, got "
      ^ Int.to_string (List.length widths)
      ^ ": "
      ^ String.concat ", " (List.map Float.to_string widths))
    end

let tests = Test.[ case "Three columns with Grow in middle" test_three_column_with_grow; ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"grow_layout_tests" ~tests ~args)
    ~args:Env.args
    ()
