open Std
open Gooey

let test_text_element = fun _ctx ->
  let elem = Element.text "Hello" in
  match elem with
  | Element.Text { content; _ } when content = "Hello" -> Ok ()
  | _ -> Error "Element.text should create Text element with correct content"

let test_text_element_with_style = fun _ctx ->
  let custom_style = Style.(empty |> bold |> fg (`rgb (255, 0, 0))) in
  let elem = Element.text ~style:custom_style "Styled" in
  match elem with
  | Element.Text { content; style } when content = "Styled"
  && style.font_weight = Style.Bold
  && style.foreground = Some (`rgb (255, 0, 0)) -> Ok ()
  | _ -> Error "Text element should preserve custom style"

let test_container_element = fun _ctx ->
  let child1 = Element.text "A" in
  let child2 = Element.text "B" in
  let elem = Element.container [ child1; child2 ] in
  match elem with
  | Element.Container { children; _ } when List.length children = 2 -> Ok ()
  | _ -> Error "Container should hold correct number of children"

let test_row_element = fun _ctx ->
  let elem = Element.row [ Element.text "A" ] in
  match elem with
  | Element.Container { style; _ } when style.direction = Style.LeftToRight -> Ok ()
  | _ -> Error "Row should have LeftToRight direction"

let test_column_element = fun _ctx ->
  let elem = Element.column [ Element.text "A" ] in
  match elem with
  | Element.Container { style; _ } when style.direction = Style.TopToBottom -> Ok ()
  | _ -> Error "Column should have TopToBottom direction"

let test_spacer_element = fun _ctx ->
  let elem = Element.spacer ~flex:2.0 () in
  match elem with
  | Element.Container { children; style } when List.length children = 0
  && style.sizing.width = Style.Fixed 2.0
  && style.sizing.height = Style.Grow -> Ok ()
  | _ -> Error "Spacer should be empty container with fixed width and grow height"

let test_empty_element = fun _ctx ->
  let elem = Element.Empty in
  match elem with
  | Element.Empty -> Ok ()
  | _ -> Error "Empty should be Empty variant"

let tests =
  Test.[
    case "Text element" test_text_element;
    case "Text element with style" test_text_element_with_style;
    case "Container element" test_container_element;
    case "Row element" test_row_element;
    case "Column element" test_column_element;
    case "Spacer element" test_spacer_element;
    case "Empty element" test_empty_element;
  ]

let () =
  Miniriot.run ~main:(fun ~args -> Test.Cli.main ~name:"element" ~tests ~args) ~args:Env.args ()
