open Std
module Test = Std.Test

let sample_benchmarks =
  let config : Bench.bench_config = { iterations = 1; warmup = 0 } in
  [
    Bench.with_config ~config "alpha_long" (fun () -> ());
    Bench.with_config ~config "beta" (fun () -> ());
    Bench.with_config ~config "middle_long_case" (fun () -> ());
  ]

let self_executable = fun () ->
  match Env.args with
  | exe :: _ -> exe
  | [] -> panic "missing argv[0] for std_bench_cli_tests"

let run_sample_capture = fun args ->
  let cmd = Command.make (self_executable ()) ~args:(("sample" :: args)) in
  Command.output cmd |> Result.expect ~msg:"failed to run sample bench cli"

let test_list_benchmarks_lists_all_cases = fun _ctx ->
  let output = run_sample_capture [ "list-benchmarks" ] in
  if not (Int.equal output.status 0) then
    Error ("expected list-benchmarks to succeed, got " ^ Int.to_string output.status)
  else
    let lines = output.stdout
    |> String.split_on_char '\n'
    |> List.filter (fun line -> not (String.equal line ""))
    |> List.sort String.compare in
    let expected = [ "alpha_long"; "beta"; "middle_long_case" ] |> List.sort String.compare in
    if lines = expected then
      Ok ()
    else
      Error ("unexpected listed benchmark names: " ^ String.concat ", " lines)

let test_run_benchmarks_pattern_matches_substring = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "_long" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered benchmark run to succeed, got " ^ Int.to_string output.status)
  else if
    String.contains output.stdout "Running 2 benchmarks"
    && String.contains output.stdout "[1] alpha_long:"
    && String.contains output.stdout "[2] middle_long_case:"
    && not (String.contains output.stdout "beta:")
  then
    Ok ()
  else
    Error "unexpected filtered benchmark output for _long"

let test_run_benchmarks_succeeds_with_zero_matches = fun _ctx ->
  let output = run_sample_capture [ "run-benchmarks"; "missing_case" ] in
  if not (Int.equal output.status 0) then
    Error ("expected filtered benchmark run with no matches to succeed, got "
    ^ Int.to_string output.status)
  else if
    String.contains output.stdout "Running 0 benchmarks" && String.contains output.stdout "Summary: 0 total"
  then
    Ok ()
  else
    Error "expected no-match benchmark run to report zero benchmarks"

let meta_tests = [
  Test.case "list-benchmarks lists all sample cases" test_list_benchmarks_lists_all_cases;
  Test.case "run-benchmarks pattern matches substring" test_run_benchmarks_pattern_matches_substring;
  Test.case "run-benchmarks succeeds when the query matches no benchmarks" test_run_benchmarks_succeeds_with_zero_matches;
]

let sample_main = fun ~args ->
  match args with
  | exe :: _sample :: rest -> Bench.Cli.main
    ~name:"sample"
    ~benchmarks:sample_benchmarks
    ~args:((exe :: rest))
  | _ -> Error (Failure "expected sample subcommand arguments")

let meta_main = fun ~args ->
  let normalize_args = function
    | [] -> [ "std_bench_cli_tests"; "run-tests" ]
    | [ exe ] -> [ exe; "run-tests" ]
    | args -> args
  in
  Test.Cli.main ~name:"std_bench_cli_tests" ~tests:meta_tests ~args:(normalize_args args)

let main = fun ~args ->
  match args with
  | _ :: "sample" :: _ -> sample_main ~args
  | _ -> meta_main ~args

let () = Miniriot.run ~main ~args:Env.args ()
