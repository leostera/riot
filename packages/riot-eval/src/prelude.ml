open Std
open Std.Result.Syntax

type parsed = {
  packages: Riot_model.Package_name.t list;
  body: string;
}

let is_whitespace = fun ch ->
  match ch with
  | ' '
  | '\t'
  | '\n'
  | '\r' -> true
  | _ -> false

let skip_whitespace = fun source index ->
  let len = Std.String.length source in
  let rec loop index =
    if index >= len then
      index
    else if is_whitespace (Std.String.get_unchecked source ~at:index) then
      loop (index + 1)
    else
      index
  in
  loop index

let starts_with_at = fun source ~at prefix ->
  let source_len = Std.String.length source in
  let prefix_len = Std.String.length prefix in
  if at + prefix_len > source_len then
    false
  else
    let rec loop offset =
      if offset = prefix_len then
        true
      else if Std.Char.equal
        (Std.String.get_unchecked source ~at:(at + offset))
        (Std.String.get_unchecked prefix ~at:offset)
      then
        loop (offset + 1)
      else
        false
    in
    loop 0

let find_phrase_terminator = fun source start ->
  let len = Std.String.length source in
  let rec loop index =
    if index + 1 >= len then
      None
    else if Std.Char.equal (Std.String.get_unchecked source ~at:index) ';'
            && Std.Char.equal (Std.String.get_unchecked source ~at:(index + 1)) ';'
    then
      Some index
    else
      loop (index + 1)
  in
  loop start

let parse_use_package = fun spec ->
  let spec = Std.String.trim spec in
  if Std.String.is_empty spec then
    Error "#use directive requires a package name"
  else if Std.String.contains spec " "
          || Std.String.contains spec "\t"
          || Std.String.contains spec "\n"
          || Std.String.contains spec "\r"
  then
    Error ("invalid #use directive '" ^ spec ^ "': expected one package name")
  else
    Riot_model.Package_name.from_string spec
    |> Std.Result.map_err
      ~fn:(fun error ->
        "invalid #use package '"
        ^ spec
        ^ "': "
        ^ Riot_model.Package_name.error_message error)

let parse = fun source ->
  let len = Std.String.length source in
  let rec loop index packages =
    let index = skip_whitespace source index in
    if starts_with_at source ~at:index "#use" then
      let after_directive = index + Std.String.length "#use" in
      if after_directive >= len then
        Error "#use directive requires a package name"
      else if not (is_whitespace (Std.String.get_unchecked source ~at:after_directive)) then
        let body = Std.String.sub source ~offset:index ~len:(len - index) in
        Ok { packages = Std.List.reverse packages; body }
      else
        let spec_start = skip_whitespace source after_directive in
        match find_phrase_terminator source spec_start with
        | None -> Error "unterminated #use directive: expected ';;'"
        | Some terminator ->
            let spec = Std.String.sub source ~offset:spec_start ~len:(terminator - spec_start) in
            let* package = parse_use_package spec in
            loop (terminator + 2) (package :: packages)
    else
      let body =
        if index >= len then
          ""
        else
          Std.String.sub source ~offset:index ~len:(len - index)
      in
      Ok {
        packages = Std.List.reverse packages |> Context.unique_package_names;
        body;
      }
  in
  loop 0 []

let strip_shebang = fun source ->
  if Std.String.starts_with ~prefix:"#!" source then
    match Std.String.index_of source ~char:'\n' with
    | None -> ""
    | Some index -> Std.String.sub source ~offset:(index + 1) ~len:(Std.String.length source - index - 1)
  else
    source
