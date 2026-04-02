open Std
module Test = Std.Test

let parse_login = fun args ->
  match ArgParser.get_matches Tusk_cli.Login.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let parse_logout = fun args ->
  match ArgParser.get_matches Tusk_cli.Logout.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_login_accepts_token_option = fun _ctx ->
  match parse_login [ "login"; "--token"; "root-secret" ] with
  | Error err -> Error ("expected login args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "root-secret") ~actual:(ArgParser.get_one matches "token");
      Ok ()

let test_logout_accepts_no_args = fun _ctx ->
  match parse_logout [ "logout" ] with
  | Error err -> Error ("expected logout args to parse: " ^ err)
  | Ok _ -> Ok ()

let tests =
  Test.[
    case "auth: login parses --token" test_login_accepts_token_option;
    case "auth: logout parses without args" test_logout_accepts_no_args;
  ]

let name = "Tusk CLI Auth Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
