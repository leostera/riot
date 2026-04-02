open Std
open Gooey

let test_empty_style = fun _ctx ->
  let s = Style.empty in
  if s.direction = Style.LeftToRight && s.child_gap = 0 && s.z_index = 0 then
    Ok ()
  else
    Error "Style.empty should have correct defaults"

let test_padding_helpers = fun _ctx ->
  (* Verify we can create padding structures *)
  let _p1 = Style.Padding.all 10 in
  let _p2 = Style.Padding.symmetric ~h:20 ~v:10 in
  let _p3 = Style.Padding.make ~left:1 ~right:2 ~top:3 ~bottom:4 () in
  Ok ()

let test_margin_helpers = fun _ctx ->
  (* Verify we can create margin structures *)
  let _m1 = Style.Margin.all 5 in
  let _m2 = Style.Margin.symmetric ~h:15 ~v:8 in
  Ok ()

let test_style_builder = fun _ctx ->
  let s = Style.(empty |> bg (`rgb (255, 0, 0)) |> grow) in
  if s.background = Some (`rgb (255, 0, 0)) && s.sizing.width = Style.Grow then
    Ok ()
  else
    Error "Style builder should chain operations correctly"

let test_sizing_variants = fun _ctx ->
  let s1 = Style.(empty |> width (Fixed 100.0)) in
  if s1.sizing.width != Style.Fixed 100.0 then
    Error "Fixed sizing failed"
  else
    let s2 = Style.(empty |> width (Percent 0.5)) in
    if s2.sizing.width != Style.Percent 0.5 then
      Error "Percent sizing failed"
    else
      let s3 = Style.(empty |> width Fit) in
      if s3.sizing.width != Style.Fit then
        Error "Fit sizing failed"
      else
        let s4 = Style.(empty |> width Grow) in
        if s4.sizing.width != Style.Grow then
          Error "Grow sizing failed"
        else
          Ok ()

let test_direction_helpers = fun _ctx ->
  let s1 = Style.(empty |> row) in
  if s1.direction != Style.LeftToRight then
    Error "row should set LeftToRight direction"
  else
    let s2 = Style.(empty |> column) in
    if s2.direction != Style.TopToBottom then
      Error "column should set TopToBottom direction"
    else
      Ok ()

let test_border = fun _ctx ->
  let s = Style.(empty |> border ~width:2 ~color:(`rgb (100, 100, 100)) ()) in
  if s.border_width = 2 && s.border_color = Some (`rgb (100, 100, 100)) then
    Ok ()
  else
    Error "Border styling failed"

let tests =
  Test.[
    case "Empty style defaults" test_empty_style;
    case "Padding helpers" test_padding_helpers;
    case "Margin helpers" test_margin_helpers;
    case "Style builder pattern" test_style_builder;
    case "Sizing variants" test_sizing_variants;
    case "Direction helpers" test_direction_helpers;
    case "Border" test_border;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"style" ~tests ~args) ~args:Env.args ()
