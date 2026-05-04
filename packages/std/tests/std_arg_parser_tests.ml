open Std

module Test = Std.Test

let command_with_optional_subcommand = fun () ->
  let open ArgParser in
  let open ArgParser.Arg in
  command "tool"
  |> args
    [
      flag "list"
      |> long "list";
    ]
  |> allow_no_subcommand
  |> subcommands
    [
      command "clean"
      |> args
        [
          flag "force"
          |> long "force";
        ];
    ]

let test_command_allows_flags_without_subcommand = fun _ctx ->
  match ArgParser.get_matches (command_with_optional_subcommand ()) [ "tool"; "--list" ] with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches ->
      if not (ArgParser.get_flag matches "list") then
        Error "expected --list to be set"
      else if Option.is_some (ArgParser.get_subcommand matches) then
        Error "expected no subcommand"
      else
        Ok ()

let test_command_still_parses_subcommand = fun _ctx ->
  match ArgParser.get_matches (command_with_optional_subcommand ()) [ "tool"; "clean"; "--force" ] with
  | Error err -> Error (ArgParser.error_message err)
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("clean", sub_matches) ->
          if ArgParser.get_flag sub_matches "force" then
            Ok ()
          else
            Error "expected subcommand --force to be set"
      | Some (name, _) -> Error ("unexpected subcommand: " ^ name)
      | None -> Error "expected clean subcommand"
    )

let tests = [
  Test.case
    "arg parser allows flags without optional subcommand"
    test_command_allows_flags_without_subcommand;
  Test.case "arg parser still parses optional subcommands" test_command_still_parses_subcommand;
]

let main ~args = Test.Cli.main ~name:"std_arg_parser_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
