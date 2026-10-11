open Std

module Test = Std.Test
module Action = Riot_planner.Action

let foreign_action = fun ?(outputs = [Path.v "libdemo.a"]) ?(env = [("CC", "clang")]) build_cmd ->
  Action.BuildForeignDependency {
    name = "native-demo";
    path = Path.v "native/demo";
    build_cmd;
    outputs;
    env;
  }

let assert_distinct_commands = fun left right ->
  if
    Crypto.Hash.equal (Action.hash (foreign_action left)) (Action.hash (foreign_action right))
  then
    Error "different foreign argv lists shared an action hash"
  else
    Ok ()

let argument_order_changes_hash = fun _ctx ->
  assert_distinct_commands
    [ "cp"; "source"; "destination" ]
    [ "cp"; "destination"; "source" ]

let executable_position_changes_hash = fun _ctx ->
  assert_distinct_commands
    [ "first"; "second" ]
    [ "second"; "first" ]

let argument_boundaries_change_hash = fun _ctx ->
  assert_distinct_commands
    [ "tool"; "a"; "bc" ]
    [ "tool"; "ab"; "c" ]

let empty_argument_changes_hash = fun _ctx -> assert_distinct_commands [ "tool" ] [ "tool"; "" ]

let empty_argument_position_changes_hash = fun _ctx ->
  assert_distinct_commands
    [ "tool"; ""; "value" ]
    [ "tool"; "value"; "" ]

let repeated_argument_changes_hash = fun _ctx ->
  assert_distinct_commands
    [ "tool"; "value" ]
    [ "tool"; "value"; "value" ]

let same_command_is_deterministic = fun _ctx ->
  let action = foreign_action [ "cargo"; "build"; "--release" ] in
  if Crypto.Hash.equal (Action.hash action) (Action.hash action) then
    Ok ()
  else
    Error "identical foreign actions must hash deterministically"

let json_roundtrip_preserves_hash = fun _ctx ->
  let action = foreign_action [ "tool"; ""; "a:b"; "two words" ] in
  match Action.from_json (Action.to_json action) with
  | Error error -> Error error
  | Ok decoded ->
      if Crypto.Hash.equal (Action.hash action) (Action.hash decoded) then
        Ok ()
      else
        Error "foreign action hash changed after JSON roundtrip"

let legacy_encoding_is_invalidated = fun _ctx ->
  let action = foreign_action [ "--release"; "build"; "cargo" ] in
  let legacy_hash =
    Crypto.Sha256.hash_string
      "BuildForeignDependencynative-demonative/demo--releasebuildcargolibdemo.aCC=clang"
  in
  if Crypto.Hash.equal (Action.hash action) legacy_hash then
    Error "foreign action still shares the legacy hash namespace"
  else
    Ok ()

let output_and_env_order_are_normalized = fun _ctx ->
  let outputs = [ Path.v "libdemo.a"; Path.v "demo.h" ] in
  let env = [ ("CC", "clang"); ("MODE", "release") ] in
  let argv = [ "cargo"; "build" ] in
  let left = foreign_action ~outputs ~env argv in
  let right = foreign_action ~outputs:(List.reverse outputs) ~env:(List.reverse env) argv in
  if Crypto.Hash.equal (Action.hash left) (Action.hash right) then
    Ok ()
  else
    Error "foreign output and distinct environment-key order must remain normalized"

let output_boundaries_change_hash = fun _ctx ->
  let left = foreign_action ~outputs:[ Path.v "a"; Path.v "bc" ] [ "tool" ] in
  let right = foreign_action ~outputs:[ Path.v "ab"; Path.v "c" ] [ "tool" ] in
  if Crypto.Hash.equal (Action.hash left) (Action.hash right) then
    Error "foreign output path boundaries shared a hash"
  else
    Ok ()

let environment_boundaries_change_hash = fun _ctx ->
  let left = foreign_action ~env:[ ("A", "B"); ("C", "D") ] [ "tool" ] in
  let right = foreign_action ~env:[ ("A", "BC=D") ] [ "tool" ] in
  if Crypto.Hash.equal (Action.hash left) (Action.hash right) then
    Error "foreign environment entry boundaries shared a hash"
  else
    Ok ()

let tests =
  Test.[
    case "foreign action hash preserves argument order" argument_order_changes_hash;
    case "foreign action hash preserves executable position" executable_position_changes_hash;
    case "foreign action hash preserves argument boundaries" argument_boundaries_change_hash;
    case "foreign action hash preserves empty arguments" empty_argument_changes_hash;
    case
      "foreign action hash preserves empty argument position"
      empty_argument_position_changes_hash;
    case "foreign action hash preserves repeated arguments" repeated_argument_changes_hash;
    case "foreign action hash is deterministic" same_command_is_deterministic;
    case "foreign action hash survives JSON roundtrip" json_roundtrip_preserves_hash;
    case "foreign action hash invalidates legacy encoding" legacy_encoding_is_invalidated;
    case "foreign action hash normalizes output and env order" output_and_env_order_are_normalized;
    case "foreign action hash preserves output boundaries" output_boundaries_change_hash;
    case "foreign action hash preserves environment boundaries" environment_boundaries_change_hash;
  ]

let name = "riot-planner:action-hash"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
