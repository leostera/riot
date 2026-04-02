open Std

let validate_source = fun ~filename source ->
  let parsed = Syn.parse ~filename source in
  match parsed.diagnostics with
  | [] -> Ok ()
  | diagnostic :: _ -> Error (Macro_error.of_parse_diagnostic diagnostic)
