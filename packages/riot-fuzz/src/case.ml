open Std
open Std.Result.Syntax
open Types

let selected_fuzz_cases = fun suites ->
  suites
  |> List.flat_map
    ~fn:(fun (suite: Riot_test.Test_runtime.listed_test_suite) ->
      match suite.binary_path with
      | None -> []
      | Some binary_path ->
          suite.tests
          |> List.filter_map
            ~fn:(fun case ->
              match case.Riot_test.Test_runtime.test_type with
              | Riot_test.Test_runtime.Fuzz _ ->
                  Some {
                    suite = suite.suite;
                    case;
                    binary_path;
                    source_path = suite.source_path;
                  }
              | Riot_test.Test_runtime.Test
              | Riot_test.Test_runtime.Property _ -> None))

let collect_cases = fun ?on_event ~workspace ~package_filters ~filter () ->
  let* selection =
    Riot_test.Test_selection.parse_request
      ~filter
      ~package_filters
      ~size_filter:Riot_test.Test_selection.All
      ~flaky_only:false
    |> Result.map_err ~fn:(fun error -> Error.Test_error error)
  in
  let extra_args = Riot_test.Test_selection.extra_args selection [] in
  Riot_test.Test_runtime.list_tests
    ?on_event
    {
      workspace;
      package_filters = selection.package_filters;
      suite_filter = selection.suite_filter;
      profile = "fuzz";
      extra_args;
    }
  |> Result.map_err
    ~fn:(fun err -> Error.Test_error (Riot_test.Test_runtime.test_error_message err))
  |> Result.map ~fn:selected_fuzz_cases

let slugify = fun value ->
  let is_slug_char char =
    let code = Char.to_int char in
    (code >= Char.to_int 'a' && code <= Char.to_int 'z')
    || (code >= Char.to_int 'A' && code <= Char.to_int 'Z')
    || (code >= Char.to_int '0' && code <= Char.to_int '9')
    || Char.equal char '-'
    || Char.equal char '_'
  in
  let bytes = IO.Bytes.from_string value in
  for idx = 0 to IO.Bytes.length bytes - 1 do
    let char = IO.Bytes.get_unchecked bytes ~at:idx in
    let char =
      if is_slug_char char then
        Char.lowercase_ascii char
      else
        '_'
    in
    IO.Bytes.set_unchecked bytes ~at:idx ~char
  done;
  IO.Bytes.to_string bytes

let case_dir = fun ~(workspace:Riot_model.Workspace.t) (fuzz_case: fuzz_case) ->
  let package_name = Riot_model.Package_name.to_string fuzz_case.suite.package_name in
  Path.(workspace.root
  / Path.v ".riot"
  / Path.v "fuzzing"
  / Path.v package_name
  / Path.v (slugify fuzz_case.suite.suite_name)
  / Path.v (slugify fuzz_case.case.name))

let target_for_case = fun ~(workspace:Riot_model.Workspace.t) (fuzz_case: fuzz_case) ->
  let ctx_json =
    Riot_test.Test_runtime.suite_context_json
      ~workspace_root:workspace.root
      ~package_name:fuzz_case.suite.package_name
      ?source_file:fuzz_case.source_path
      ~binary_path:fuzz_case.binary_path
      ~built_binaries:[]
      ()
  in
  {
    program = Path.to_string fuzz_case.binary_path;
    args = (fun ~input_path -> [
      "run-fuzz-case";
      fuzz_case.case.name;
      "--input";
      Path.to_string input_path;
      "--json";
      "--ctx";
      ctx_json;
    ]);
    env = [];
    cwd = Some workspace.root;
  }

let path_in_workspace = fun ~(workspace:Riot_model.Workspace.t) path ->
  if Path.is_absolute path then
    path
  else
    Path.(workspace.root / path)

let corpus_for_case = fun ~(workspace:Riot_model.Workspace.t) (fuzz_case: fuzz_case) ->
  match fuzz_case.case.Riot_test.Test_runtime.fuzz_corpus with
  | None -> { inputs = []; files = [] }
  | Some corpus ->
      { inputs = corpus.inputs; files = List.map corpus.files ~fn:(path_in_workspace ~workspace) }

let mutator_for_case = fun (fuzz_case: fuzz_case) ->
  match fuzz_case.case.Riot_test.Test_runtime.fuzz_mutator with
  | None -> { dictionary = []; max_len = None; splicing = true }
  | Some mutator ->
      { dictionary = mutator.dictionary; max_len = mutator.max_len; splicing = mutator.splicing }
