open Std
open Std.Collections

let tests_dir = Path.v "packages/krasny/tests"

let fixtures_dir = Path.(tests_dir / Path.v "fixtures")

let manifest_path = Path.(tests_dir / Path.v "format_expectations.txt")

let parse_file = fun path ->
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  Syn.parse ~filename:path source

let tracked_fixtures = fun () ->
  let manifest = Fs.read manifest_path |> Result.expect ~msg:"failed to read krasny fixture manifest" in
  let tracked = HashSet.create () in
  let lines = manifest |> String.split_on_char '\n' |> List.map String.trim in
  let rec loop = function
    | [] -> tracked
    | line :: rest ->
        if String.equal line "" || String.starts_with ~prefix:"#" line then
          loop rest
        else
          let relpath = Path.of_string line |> Result.expect ~msg:"fixture manifest entry should be valid UTF-8" in
          let name = Path.basename relpath in
          let () =
            ignore (HashSet.insert tracked name)
          in
          loop rest
  in
  loop lines

let fixture_filter = fun tracked path ->
  if HashSet.contains tracked (Path.basename path) then
    `keep
  else
    `skip

let approved_snapshot_path = fun path ->
  match Path.extension path with
  | Some ext -> Path.add_extension path ~ext:((ext ^ ".expected"))
  | None -> Path.add_extension path ~ext:"expected"

let assert_roundtrip_hash = fun ~fixture_path ~formatted ->
  let formatted_parse = Syn.parse ~filename:fixture_path formatted in
  let original_hash = Krasny.syntax_hash formatted_parse in
  let reformatted = Krasny.format formatted_parse |> Result.expect ~msg:"formatted fixture should reformat" in
  let reparsed = Syn.parse ~filename:fixture_path reformatted in
  let reparsed_hash = Krasny.syntax_hash reparsed in
  Test.assert_equal ~expected:original_hash ~actual:reparsed_hash;
  Ok ()

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let parsed = parse_file ctx.fixture_path in
  let formatted = Krasny.format parsed |> Result.expect ~msg:"fixture should format" in
  match Test.Snapshot.assert_text ~ctx:ctx.test ~actual:formatted with
  | Error _ as err -> err
  | Ok () -> assert_roundtrip_hash ~fixture_path:ctx.fixture_path ~formatted

let () =
  Actors.run
    ~main:(fun ~args ->
      let tracked = tracked_fixtures () in
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:(fixture_filter tracked)
          ~snapshot_path:(fun path -> Some (approved_snapshot_path path))
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"krasny:fixtures" ~tests ~args)
    ~args:Env.args
    ()
