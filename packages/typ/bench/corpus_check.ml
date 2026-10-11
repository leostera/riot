open Std
open Std.Collections
open Std.Iter

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

let render_diagnostics diagnostics =
  diagnostics
  |> Vector.iter
  |> Iterator.to_list
  |> List.map ~fn:Typ.Diagnostics.Diagnostic.to_string

let check_file path =
  match Fs.read path with
  | Error error ->
      Error ("cannot read " ^ Path.to_string path ^ ": " ^ IO.error_message error)
  | Ok text ->
      let parse_result = Syn.parse ~filename:path (IO.IoVec.IoSlice.from_string text |> Result.expect ~msg:"source slice") in
      if not (Vector.is_empty parse_result.diagnostics) then
        Error ("Syn reported parser diagnostics")
      else
        let source = Typ.Model.Source.make ~text in
        match Typ.Ast.from_parse_result ~source parse_result with
        | Error diagnostics ->
            Error
              (List.map diagnostics ~fn:Typ.Diagnostics.Diagnostic.to_string
              |> String.concat "\n")
        | Ok ast ->
            let result = Typ.Infer.check ast in
            match render_diagnostics result.diagnostics.items with
            | [] -> Ok ()
            | diagnostics -> Error (String.concat "\n" diagnostics)

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
      let examples = ref 0 in
      List.for_each files ~fn:(fun path ->
        checked := Int.(!checked + 1);
        match check_file path with
        | Ok () -> ()
        | Error diagnostic ->
            failures := Int.(!failures + 1);
            if !examples < 40 then (
              examples := Int.(!examples + 1);
              eprintln (Path.to_string path ^ ": " ^ diagnostic)));
      println
        ("Typ corpus: " ^ Int.to_string !checked ^ " files; "
        ^ Int.to_string (!checked - !failures) ^ " passed; "
        ^ Int.to_string !failures ^ " failed");
      if !failures = 0 then 0 else 1

let main ~args:_ =
  match run () with
  | 0 -> Ok ()
  | code -> Error (Failure ("Typ corpus check failed (exit " ^ Int.to_string code ^ ")"))

let () = Runtime.run ~main ~args:Env.args ()
