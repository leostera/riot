open Std
open Std.Collections

let tests_dir = Path.v "packages/krasny/tests"

let fixtures_dir = Path.(tests_dir / Path.v "fixtures")

let manifest_path = Path.(tests_dir / Path.v "format_expectations.txt")

let native_fixture_prefixes = [ "010"; "020"; "050"; "071"; "080"; "081"; "082"; "097"; "098" ]

let native_fixture_names = [
  "0910_docstring_before_local_open_let.ml";
  "0930_structure_attribute_before_let.ml";
  "0933_comment_attribute_between_lets.ml";
  "0935_unary_value_declaration.mli";
  "0936_nested_signature_value.mli";
  "0937_match_case_body_comment.ml";
  "9101_real_krasny_doc.ml";
  "9103_real_krasny_solver.ml";
  "9104_real_krasny_source.ml";
  "9105_real_krasny_entry.ml";
  "9106_real_krasny_main.ml";
  "9107_real_std_float.ml";
  "9109_real_syn_keyword.ml";
]

let parse_file = fun path ->
  let source = Fs.read path |> Result.expect ~msg:"fixture file should exist" in
  Syn.parse ~filename:path source

let should_track_fixture = fun relpath ->
  let name = Path.basename relpath in
  List.exists (fun prefix -> String.starts_with ~prefix name) native_fixture_prefixes
  || List.exists (String.equal name) native_fixture_names

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
            if should_track_fixture relpath then
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
