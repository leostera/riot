open Std

let position = fun line character -> Lsp.Position.{ line; character }

let expect_position = fun actual ~line ~character ->
  if actual.Lsp.Position.line = line && actual.character = character then
    Ok ()
  else
    Error
      ("expected position "
      ^ Int.to_string line
      ^ ":"
      ^ Int.to_string character
      ^ ", got "
      ^ Int.to_string actual.line
      ^ ":"
      ^ Int.to_string actual.character)

let expect_offset = fun result ~expected ->
  match result with
  | Ok offset when offset = expected -> Ok ()
  | Ok offset ->
      Error ("expected offset " ^ Int.to_string expected ^ ", got " ^ Int.to_string offset)
  | Error message -> Error ("expected offset " ^ Int.to_string expected ^ ", got error: " ^ message)

let test_position_of_offset_counts_surrogate_pairs = fun _ctx ->
  let actual = Lsp.Utf16.position_of_offset "a😀b" ~offset:5 in
  expect_position actual ~line:0 ~character:3

let test_offset_of_position_counts_surrogate_pairs = fun _ctx ->
  let actual = Lsp.Utf16.offset_of_position "a😀b" (position 0 3) in
  expect_offset actual ~expected:5

let test_offset_of_position_rejects_split_surrogate_pair = fun _ctx ->
  match Lsp.Utf16.offset_of_position "😀" (position 0 1) with
  | Error "position splits a UTF-16 surrogate pair" -> Ok ()
  | Error message -> Error ("unexpected error: " ^ message)
  | Ok offset -> Error ("expected surrogate split error, got offset " ^ Int.to_string offset)

let test_range_of_offsets_multiline = fun _ctx ->
  let range = Lsp.Utf16.range_of_offsets "a\r\n😀\nZ" ~start_offset:3 ~end_offset:7 in
  if range.Lsp.Range.start_.line = 1
     && range.start_.character = 0
     && range.end_.line = 1
     && range.end_.character = 2
  then
    Ok ()
  else
    Error
      ("expected range 1:0 -> 1:2, got "
      ^ Int.to_string range.start_.line
      ^ ":"
      ^ Int.to_string range.start_.character
      ^ " -> "
      ^ Int.to_string range.end_.line
      ^ ":"
      ^ Int.to_string range.end_.character)

let tests =
  Test.[
    case "position_of_offset counts surrogate pairs" test_position_of_offset_counts_surrogate_pairs;
    case "offset_of_position counts surrogate pairs" test_offset_of_position_counts_surrogate_pairs;
    case "offset_of_position rejects split surrogate pair"
      test_offset_of_position_rejects_split_surrogate_pair;
    case "range_of_offsets handles multiline utf16 ranges" test_range_of_offsets_multiline;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"utf16" ~tests ~args) ~args:Env.args ()
