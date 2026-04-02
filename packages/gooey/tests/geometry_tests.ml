open Std
open Gooey

let test_point_creation = fun _ctx ->
  let p = Geometry.Point.make ~x:10.0 ~y:20.0 in
  if p.x = 10.0 && p.y = 20.0 then
    let zero = Geometry.Point.zero in
    if zero.x = 0.0 && zero.y = 0.0 then
      Ok ()
    else
      Error "Point.zero should be (0.0, 0.0)"
  else
    Error "Point.make should create point with given coordinates"

let test_rect_creation = fun _ctx ->
  let r = Geometry.Rect.make ~x:5.0 ~y:10.0 ~width:100.0 ~height:50.0 in
  if r.x = 5.0 && r.y = 10.0 && r.width = 100.0 && r.height = 50.0 then
    let zero = Geometry.Rect.zero in
    if zero.x = 0.0 && zero.y = 0.0 && zero.width = 0.0 && zero.height = 0.0 then
      Ok ()
    else
      Error "Rect.zero should have all fields as 0.0"
  else
    Error "Rect.make should create rect with given dimensions"

let test_viewport_creation = fun _ctx ->
  let v = Viewport.make ~width:80.0 ~height:24.0 in
  if v.width = 80.0 && v.height = 24.0 then
    Ok ()
  else
    Error "Viewport.make should create viewport with given dimensions"

let tests =
  Test.[
    case "Point creation" test_point_creation;
    case "Rect creation" test_rect_creation;
    case "Viewport creation" test_viewport_creation;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"geometry" ~tests ~args) ~args:Env.args ()
