open Std
open Std.Collections
open Std.Iter

type failure_stage =
  | Read
  | Parse
  | Ast
  | Typecheck

type check_failure = {
  stage: failure_stage;
  diagnostics: string list;
}

let failure_stage_to_string = function
  | Read -> "read"
  | Parse -> "parse"
  | Ast -> "AST"
  | Typecheck -> "Typ"

let has_source_extension path =
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" -> true
  | _ -> false

let is_excluded_path path =
  let parts = String.split (Path.to_string path) ~by:"/" in
  List.any parts ~fn:(fun part ->
    String.contains part "fixture"
    || String.equal part "vendor"
    || String.equal part "generated")
  || String.contains (Path.to_string path) "/tests/diagnostics/"

let rec collect_source_files path acc =
  if is_excluded_path path then
    acc
  else
    match Fs.is_dir path with
    | Ok true ->
        (match Fs.read_dir path with
        | Error _ -> acc
        | Ok entries ->
            List.fold_left (Iter.MutIterator.to_list entries) ~init:acc ~fn:(fun acc entry ->
              collect_source_files Path.(path / entry) acc))
    | Ok false ->
        if has_source_extension path && (match Fs.is_file path with Ok true -> true | _ -> false)
        then path :: acc
        else acc
    | Error _ -> acc

let render_typ_diagnostics diagnostics =
  diagnostics
  |> Vector.iter
  |> Iterator.to_list
  |> List.map ~fn:Typ.Diagnostics.Diagnostic.to_string

let check_file path =
  match Fs.read path with
  | Error error ->
      Error {
        stage = Read;
        diagnostics = [ "cannot read " ^ Path.to_string path ^ ": " ^ IO.error_message error ];
      }
  | Ok text ->
      let source_slice = IO.IoVec.IoSlice.from_string text |> Result.expect ~msg:"source slice" in
      let parse_result = Syn.parse ~filename:path source_slice in
      let parser_diagnostics =
        Vector.iter parse_result.diagnostics
        |> Iterator.to_list
        |> List.map ~fn:Syn.Diagnostic.to_string
      in
      if List.length parser_diagnostics > 0 then
        Error { stage = Parse; diagnostics = parser_diagnostics }
      else
        let source = Typ.Model.Source.make ~text in
        match Typ.Ast.from_parse_result ~source parse_result with
        | Error diagnostics ->
            Error {
              stage = Ast;
              diagnostics =
                List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string;
            }
        | Ok ast ->
            let result = Typ.Infer.check ast in
            (match render_typ_diagnostics result.diagnostics.items with
            | [] -> Ok ()
            | diagnostics -> Error { stage = Typecheck; diagnostics })

let increment counter =
  counter := Int.(!counter + 1)

let run () =
  match Env.current_dir () with
  | Error _ ->
      eprintln "cannot determine workspace root";
      2
  | Ok root ->
      let roots = [ Path.(root / Path.v "packages"); Path.(root / Path.v "compiler") ] in
      let files =
        List.fold_left roots ~init:[] ~fn:(fun files path -> collect_source_files path files)
      in
      let checked = ref 0 in
      let failures = ref 0 in
      let read_failures = ref 0 in
      let parse_failures = ref 0 in
      let ast_failures = ref 0 in
      let type_failures = ref 0 in
      let examples = ref 0 in
      List.for_each files ~fn:(fun path ->
        increment checked;
        match check_file path with
        | Ok () -> ()
        | Error failure ->
            increment failures;
            (match failure.stage with
            | Read -> increment read_failures
            | Parse -> increment parse_failures
            | Ast -> increment ast_failures
            | Typecheck -> increment type_failures);
            if !examples < 40 then (
              increment examples;
              eprintln
                (Path.to_string path ^ " [" ^ failure_stage_to_string failure.stage ^ "]: "
                ^ String.concat "\n" failure.diagnostics)));
      println
        ("Typ corpus: " ^ Int.to_string !checked ^ " files; "
        ^ Int.to_string (!checked - !failures) ^ " passed; "
        ^ Int.to_string !failures ^ " failed (parse: " ^ Int.to_string !parse_failures
        ^ ", AST: " ^ Int.to_string !ast_failures ^ ", Typ: "
        ^ Int.to_string !type_failures ^ ", read: " ^ Int.to_string !read_failures ^ ")");
      if !failures = 0 then 0 else 1

let main ~args:_ =
  match run () with
  | 0 -> Ok ()
  | code -> Error (Failure ("Typ corpus check failed (exit " ^ Int.to_string code ^ ")"))

let () = Runtime.run ~main ~args:Env.args ()
