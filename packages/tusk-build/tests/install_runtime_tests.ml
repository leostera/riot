open Std
module Test = Std.Test

let test_install_event_to_json_serializes_promoted_binary = fun _ctx ->
  match Tusk_build.install_event_to_json
    (Tusk_build.PromotedBinary { binary = "demo"; destination = Path.v "/tmp/demo"; global = true }) with
  | Some (Data.Json.Object fields) ->
      Test.assert_equal
        ~expected:(Some (Data.Json.String "PromotedBinary"))
        ~actual:(List.assoc_opt "type" fields);
      Test.assert_equal
        ~expected:(Some (Data.Json.Bool true))
        ~actual:(List.assoc_opt "global" fields);
      Ok ()
  | Some json ->
      Error ("expected JSON object, got " ^ Data.Json.to_string json)
  | None ->
      Error "expected JSON output for promoted binary event"

let test_install_error_message_names_missing_binary = fun _ctx ->
  Test.assert_equal
    ~expected:"binary 'demo' not found in workspace"
    ~actual:(Tusk_build.install_error_message (Tusk_build.BinaryNotFound { binary_name = "demo" }));
  Ok ()

let tests =
  let open Test in [
    case "install runtime: promoted binary event json" test_install_event_to_json_serializes_promoted_binary;
    case "install runtime: missing binary message" test_install_error_message_names_missing_binary;
  ]

let name = "Tusk Build Install Runtime Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
