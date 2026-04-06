open Std
open Std.Data

let ( let* ) = Result.and_then

let fixture_root = Path.v "packages/riot-lsp/tests/session_fixtures"

let keep_jsonl = fun path ->
  match Path.extension path with
  | Some ".jsonl" -> `keep
  | _ -> `skip

let replace_all = fun text ->
  fun ~pattern ->
    fun ~with_ ->
      let pattern_len = String.length pattern in
      if Int.equal pattern_len 0 then
        text
      else
        let buffer = IO.Buffer.create (String.length text + String.length with_) in
        let starts_with_pattern offset =
          let rec loop index =
            if index >= pattern_len then
              true
            else if text.[offset + index] = pattern.[index] then
              loop (index + 1)
            else
              false
          in
          offset + pattern_len <= String.length text && loop 0
        in
        let rec loop offset =
          if offset >= String.length text then
            ()
          else if starts_with_pattern offset then
            (
              IO.Buffer.add_string buffer with_;
              loop (offset + pattern_len)
            )
          else (
            IO.Buffer.add_char buffer text.[offset];
            loop (offset + 1)
          )
        in
        let () = loop 0 in
        IO.Buffer.contents buffer

let substitute_fixture_tokens = fun line ->
  let repo_root = Env.current_dir () |> Result.unwrap_or ~default:(Path.v ".") in
  line
  |> replace_all ~pattern:"__REPO_ROOT_URI__" ~with_:(Lsp.Uri.to_string (Lsp.Uri.of_path repo_root))
  |> replace_all ~pattern:"__REPO_ROOT__" ~with_:(Path.to_string repo_root)

let normalize_snapshot_tokens = fun text ->
  let repo_root = Env.current_dir () |> Result.unwrap_or ~default:(Path.v ".") in
  text
  |> replace_all ~pattern:(Lsp.Uri.to_string (Lsp.Uri.of_path repo_root)) ~with_:"__REPO_ROOT_URI__"
  |> replace_all ~pattern:(Path.to_string repo_root) ~with_:"__REPO_ROOT__"

let read_lines = fun path ->
  Fs.read path
  |> Result.map_error IO.error_message
  |> Result.map
    (fun source ->
      source
      |> String.split_on_char '\n'
      |> List.map String.trim
      |> List.map substitute_fixture_tokens
      |> List.filter (fun line -> not (String.equal line "")))

let run_fixture = fun path ->
  let* lines = read_lines path in
  let initial: Riot_lsp.Session.outcome = {
    state = Riot_lsp.Session.empty;
    outbound = [];
    exit_code = None
  } in
  Ok (
    List.fold_left
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
      initial
      lines
  )

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* outcome = run_fixture ctx.fixture_path in
  Test.Snapshot.assert_text
    ~ctx:ctx.test
    ~actual:(normalize_snapshot_tokens (Json.to_string_pretty (Riot_lsp.Session.outcome_to_json outcome) ^ "\n"))

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixture_root
          ~filter:keep_jsonl
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"riot-lsp session fixtures" ~tests ~args)
    ~args:Env.args
    ()
