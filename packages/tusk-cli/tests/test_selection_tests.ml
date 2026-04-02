open Std
module Test = Std.Test
module Test_selection = Tusk_cli.Test_selection

let test_parse_request_keeps_global_query = fun _ctx ->
  let request = Test_selection.parse_request ~pattern:(Some "hello") ~legacy_package:None in
  Test.assert_equal ~expected:None ~actual:request.package_filter;
  Test.assert_equal ~expected:(Some "hello") ~actual:request.query;
  Ok ()

let test_parse_request_uses_package_flag_for_narrowing = fun _ctx ->
  let request = Test_selection.parse_request ~pattern:(Some "hello") ~legacy_package:(Some "std") in
  Test.assert_equal ~expected:(Some "std") ~actual:request.package_filter;
  Test.assert_equal ~expected:(Some "hello") ~actual:request.query;
  Ok ()

let test_parse_request_preserves_raw_query_text = fun _ctx ->
  let request = Test_selection.parse_request ~pattern:(Some "std:hello") ~legacy_package:None in
  Test.assert_equal ~expected:None ~actual:request.package_filter;
  Test.assert_equal ~expected:(Some "std:hello") ~actual:request.query;
  Ok ()

let test_extra_args_omits_query_when_absent = fun _ctx ->
  let request = Test_selection.parse_request ~pattern:None ~legacy_package:(Some "std") in
  let actual = Test_selection.extra_args request [ "--format"; "json" ] in
  Test.assert_equal ~expected:[ "--format"; "json" ] ~actual;
  Ok ()

let test_extra_args_prefixes_query_when_present = fun _ctx ->
  let request = Test_selection.parse_request ~pattern:(Some "_long") ~legacy_package:None in
  let actual = Test_selection.extra_args request [ "--format"; "json" ] in
  Test.assert_equal ~expected:[ "_long"; "--format"; "json" ] ~actual;
  Ok ()

let tests =
  Test.[
    case "test selection: keep global query" test_parse_request_keeps_global_query;
    case "test selection: use package flag for narrowing" test_parse_request_uses_package_flag_for_narrowing;
    case "test selection: preserve raw query text" test_parse_request_preserves_raw_query_text;
    case "test selection: omit query when absent" test_extra_args_omits_query_when_absent;
    case "test selection: prefix query when present" test_extra_args_prefixes_query_when_present;
  ]

let name = "Tusk CLI Test Selection Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
