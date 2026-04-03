open Std
open Std.Data

let ( let* ) = Result.and_then

let fixture_root = Path.v "packages/riot-lsp/tests/session_fixtures"

let keep_jsonl = fun path ->
  match Path.extension path with
  | Some ".jsonl" -> `keep
  | _ -> `skip

let read_lines = fun path ->
  Fs.read path
  |> Result.map_error IO.error_message
  |> Result.map (fun source ->
    source
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter (fun line -> not (String.equal line "")))

let run_fixture = fun path ->
  let* lines = read_lines path in
  let initial : Riot_lsp.Session.outcome =
    { state = Riot_lsp.Session.empty; outbound = []; exit_code = None }
  in
  Ok
    (List.fold_left
       (fun acc line ->
         let outcome = Riot_lsp.Session.handle_payload acc.Riot_lsp.Session.state line in
         {
           Riot_lsp.Session.state = outcome.state;
           outbound = acc.outbound @ outcome.outbound;
           exit_code =
             match outcome.exit_code with
             | Some code -> Some code
             | None -> acc.exit_code;
         })
       initial lines)

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* outcome = run_fixture ctx.fixture_path in
  Test.Snapshot.assert_text
    ~ctx:ctx.test
    ~actual:(Json.to_string_pretty (Riot_lsp.Session.outcome_to_json outcome) ^ "\n")

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases ()
          ~dir:fixture_root
          ~filter:keep_jsonl
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"riot-lsp session fixtures" ~tests ~args)
    ~args:Env.args
    ()
