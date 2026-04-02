open Std
open Std.Data
open Syn

let test_diagnostic = fun test_path diagnostic_path ->
  let source = Fs.read (Path.v test_path) |> Result.expect ~msg:"Failed to read test file" in
  let diagnostic_json = Fs.read (Path.v diagnostic_path) |> Result.expect ~msg:"Failed to read diagnostic file" in
  let parse_result = Syn.parse ~filename:(Path.v test_path) source in
  let actual_diagnostics = parse_result.Parser.diagnostics in
  let expected_json = Json.of_string (String.trim diagnostic_json) |> Result.expect ~msg:"Failed to parse diagnostic JSON" in
  let expected_diagnostics =
    match expected_json with
    | Json.Array items -> List.map
      (fun item -> Diagnostic.from_json item |> Result.expect ~msg:"Failed to deserialize diagnostic")
      items
    | _ -> []
  in
  let actual_json = Json.Array (List.map Diagnostic.to_json actual_diagnostics) in
  let expected_json_normalized = Json.Array (List.map Diagnostic.to_json expected_diagnostics) in
  let actual_str = Json.to_string actual_json in
  let expected_str = Json.to_string expected_json_normalized in
  if actual_str = expected_str then
    Ok ()
  else
    Error ("Diagnostics mismatch for "
    ^ test_path
    ^ "\nExpected "
    ^ Int.to_string (List.length expected_diagnostics)
    ^ " diagnostics:\n"
    ^ expected_str
    ^ "\n\nGot "
    ^ Int.to_string (List.length actual_diagnostics)
    ^ " diagnostics:\n"
    ^ actual_str
    ^ "\n")

let discover_diagnostics = fun () ->
  let diagnostics_dir = Path.v "packages/syn/tests/diagnostics" in
  let dir_exists = Fs.exists diagnostics_dir |> Result.unwrap_or ~default:false in
  if not dir_exists then
    []
  else
    let entries_iter = Fs.read_dir diagnostics_dir |> Result.expect ~msg:"Failed to read diagnostics directory" in
    let entries = Iter.MutIterator.to_list entries_iter in
    List.filter_map
      (fun entry ->
        let path = Path.to_string (Path.join diagnostics_dir entry) in
        if String.ends_with ~suffix:".ml" path then
          let diagnostic_path = path ^ ".diagnostic" in
          let exists = Fs.exists (Path.v diagnostic_path) |> Result.unwrap_or ~default:false in
          if exists then
            Some (path, diagnostic_path)
          else
            None
        else
          None)
      entries |> List.sort
      (fun ((a, _)) ((b, _)) ->
        String.compare a b)

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let diagnostics = discover_diagnostics () in
      let tests =
        List.map
          (fun ((test_path, diagnostic_path)) ->
            let name = Path.basename (Path.v test_path) in
            Test.case name (fun _ctx -> test_diagnostic test_path diagnostic_path))
          diagnostics
      in
      Test.Cli.main ~name:"syn-diagnostics" ~tests ~args)
    ~args:Env.args
    ()
