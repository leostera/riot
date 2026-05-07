open Std

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "eval"
  |> about "Evaluate Riot code in the current workspace"
  |> args
    [
      option "package"
      |> short 'p'
      |> long "package"
      |> multiple
      |> help "Build and load a package before evaluating code";
      positional "code"
      |> required false
      |> multiple
      |> help "Code to evaluate";
    ]

let parse_package_names = fun values ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | value :: rest -> (
        match Riot_model.Package_name.from_string value with
        | Ok package_name -> loop (package_name :: acc) rest
        | Error error ->
            Error (Riot_model.Package_name.error_message error)
      )
  in
  loop [] values

let run = fun ~request matches ->
  let code = ArgParser.get_many matches "code" in
  match code with
  | [] ->
      let message = "missing code to evaluate" in
      eprintln ("\027[1;31mError\027[0m: " ^ message);
      Error (Failure message)
  | code ->
      let source = String.concat " " code in
      match parse_package_names (ArgParser.get_many matches "package") with
      | Error message ->
          eprintln ("\027[1;31mError\027[0m: " ^ message);
          Error (Failure message)
      | Ok packages -> (
          match Riot_eval.run_string request ~packages ~source with
          | Ok () -> Ok ()
          | Error message ->
              eprintln ("\027[1;31mError\027[0m: " ^ message);
              Error (Failure message)
        )
