open Std
open Commonmark

let normalize_label = fun label ->
  let normalized = String.lowercase_ascii label in
  let transformed =
    String.map
      (fun ch ->
        if
          (ch >= 'a' && ch <= 'z')
          || (ch >= '0' && ch <= '9')
          || ch = '-'
          || ch = '_'
        then
          ch
        else
          '_'
      )
      normalized
  in
  let trim_underscores = fun value ->
    let len = String.length value in
    let rec skip_left idx =
      if idx >= len || not (Char.equal value.[idx] '_') then
        idx
      else
        skip_left (idx + 1)
    in
    let rec skip_right idx =
      if idx < 0 || not (Char.equal value.[idx] '_') then
        idx
      else
        skip_right (idx - 1)
    in
    let left = skip_left 0 in
    let right = skip_right (len - 1) in
    if left > right then
      "fixture"
    else
      String.sub value left (right - left + 1)
  in
  trim_underscores transformed

let fixture_test_name = fun index fixture ->
  let section =
    Option.unwrap_or ~default:"fixture" fixture.section
  in
  let example =
    Option.map Int.to_string fixture.example
    |> Option.unwrap_or ~default:(Int.to_string (index + 1))
  in
  "commonmark/" ^ normalize_label section ^ "/" ^ example ^ "_" ^ Int.to_string index

let test_fixture = fun fixture index ctx ->
  let actual = Commonmark.compile fixture.markdown in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:fixture.html

let cases = fun () ->
  all_spec_fixtures ()
  |> List.mapi (fun index fixture ->
    Test.case
      (fixture_test_name index fixture)
      (fun ctx -> test_fixture fixture index ctx))

let () =
  Actors.run
    ~main:(fun ~args ->
      Test.Cli.main
        ~name:"commonmark-spec-fixtures"
        ~tests:(cases ())
        ~args)
    ~args:Env.args
    ()
