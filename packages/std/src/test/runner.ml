open Global
open Collections

let find_segment_index = fun segments needle ->
  let rec loop idx = function
    | [] -> None
    | segment :: rest ->
        if String.equal segment needle then
          Some idx
        else
          loop (idx + 1) rest
  in
  loop 0 segments

let take = fun count xs ->
  let rec loop remaining acc = function
    | _ when remaining <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: rest -> loop (remaining - 1) (x :: acc) rest
  in
  loop count [] xs

let join_path_segments = fun segments ->
  match segments with
  | [] -> "."
  | "" :: rest -> "/" ^ String.concat "/" rest
  | _ -> String.concat "/" segments

let derive_package_name = fun binary_path ->
  match binary_path with
  | None -> None
  | Some path ->
      let segments = String.split_on_char '/' path in
      match find_segment_index segments "out" with
      | Some idx when List.length segments > idx + 1 ->
          Some (List.nth segments (idx + 1))
      | _ ->
          None

let derive_workspace_root = fun ~current_dir ~binary_path ->
  match binary_path with
  | None -> current_dir
  | Some path -> (
      let segments = String.split_on_char '/' path in
      match find_segment_index segments "_build" with
      | Some 0 -> current_dir
      | Some idx -> (
          match take idx segments with
          | []
          | [ "." ] -> current_dir
          | prefix -> (
              match Path.of_string (join_path_segments prefix) with
              | Ok root -> Some root
              | Error _ -> current_dir
            )
        )
      | None -> current_dir
    )

type mode =
  Sequential
  | Shuffle

type target =
  All
  | FilterBySubstring of string

type config = {
  concurrency: int;
  reporter: (module Reporter.Intf);
  mode: mode;
  target: target;
  suite_info: Reporter.suite_info;
}

type run_summary = Test_result.summary

let make_ctx = fun ~(suite_info: Reporter.suite_info) ~index (test: Test_case.t) ->
  let current_dir = Env.current_dir () |> Result.to_option in
  Test_context.{
    suite_name = suite_info.name;
    test_name = test.name;
    test_index = index;
    source_file = suite_info.source_file;
    binary_path = suite_info.binary_path;
    workspace_root = derive_workspace_root ~current_dir ~binary_path:suite_info.binary_path;
    package_name = derive_package_name suite_info.binary_path;
    fixture = None;
  }

let filter_tests = fun target tests ->
  match target with
  | All -> tests
  | FilterBySubstring query ->
      List.filter
        (fun (test: Test_case.t) ->
          String.contains test.name query)
        tests

let shuffle_list = fun lst ->
  let arr = Array.of_list lst in
  let len = Array.length arr in
  for i = len - 1 downto 1 do
    let j = Kernel.Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

let run_single_test = fun reporter ~suite_info index (test: Test_case.t) ->
  let name = test.name in
  let test_type = test.test_type in
  let ctx = make_ctx ~suite_info ~index test in
  let result =
    if test.skip then
      Test_result.{ index; name; test_type; result = Skipped }
    else
      match test.fn ctx with
      | exception exn ->
          let result =
            let exn = Exception.to_string exn in
            let bt = Exception.get_backtrace () in
            let reason = exn ^ "\n\n" ^ bt in
            Test_result.Failed reason
          in
          Test_result.{ index; name; test_type; result }
      | Error msg ->
          Test_result.{ index; name; test_type; result = Failed msg }
      | Ok () ->
          Test_result.{ index; name; test_type; result = Passed }
  in
  let module R = (val reporter : Reporter.Intf) in
  R.on_result index result;
  result

let run_tests = fun ~config tests ->
  Exception.record_backtrace true;
  let filtered_tests = filter_tests config.target tests in
  let tests_to_run =
    match config.mode with
    | Sequential -> filtered_tests
    | Shuffle -> shuffle_list filtered_tests
  in
  let module R = (val config.reporter : Reporter.Intf) in
  R.init config.suite_info (List.length tests_to_run);
  let results =
    List.mapi
      (fun i test ->
        run_single_test config.reporter ~suite_info:config.suite_info (i + 1) test)
      tests_to_run
  in
  let summary = Test_result.make_summary results in
  R.finalize summary;
  summary
