open Std
open Std.Result.Syntax

type input =
  | Tty of Tty.t
  | Stdio of Std.IO.BufReader.t

let strip_trailing_newline = fun line ->
  let len = Std.String.length line in
  if len >= 2
     && Std.Char.equal (Std.String.get_unchecked line ~at:(len - 2)) '\r'
     && Std.Char.equal (Std.String.get_unchecked line ~at:(len - 1)) '\n'
  then
    Std.String.sub line ~offset:0 ~len:(len - 2)
  else if len >= 1 && Std.Char.equal (Std.String.get_unchecked line ~at:(len - 1)) '\n' then
    Std.String.sub line ~offset:0 ~len:(len - 1)
  else
    line

let phrase_complete = fun source ->
  Std.String.ends_with ~suffix:";;" (Std.String.trim source)

let strip_phrase_terminator = fun source ->
  let trimmed = Std.String.trim source in
  if Std.String.ends_with ~suffix:";;" trimmed then
    Std.String.sub trimmed ~offset:0 ~len:(Std.String.length trimmed - 2)
    |> Std.String.trim
  else
    trimmed

let directive = fun source ->
  let source = strip_phrase_terminator source in
  if Std.String.starts_with ~prefix:"#" source then
    Some source
  else
    None

let print_help = fun () ->
  println "riot repl directives:";
  println "  #help;;    show this help";
  println "  #build;;   rebuild workspace packages";
  println "  #quit;;    exit the repl";
  println "";
  println "Use #use pkg;; to build and load a package for later phrases.";
  println "End phrases with ;;. Successful phrases stay loaded for the session."

let handle_directive = fun session source ->
  let trimmed = Std.String.trim source in
  if Std.String.starts_with ~prefix:"#use " trimmed then
    `Phrase source
  else
  match directive source with
  | None -> `Phrase source
  | Some "#help" ->
      print_help ();
      `Continue
  | Some "#build" ->
      (
        match Riot_eval.Session.build_workspace session with
        | Ok () -> ()
        | Error message -> eprintln message
      );
      `Continue
  | Some "#quit"
  | Some "#exit" -> `Stop
  | Some other ->
      eprintln ("unknown directive: " ^ other);
      `Continue

let handle_directive_result = fun session source ->
  try Ok (handle_directive session source) with exn ->
    Error ("directive handling raised: " ^ Std.Exception.to_string exn)

let open_input = fun () ->
  match Tty.make
    ~fd:(Tty.stdin_fd ())
    ~stdin:(Tty.stdin_fd ())
    ~stdout:(Tty.stdout_fd ())
    ~stderr:(Tty.stderr_fd ())
    ~mode:Tty.LineBuffered
    ()
  with
  | Ok tty -> Tty tty
  | Error _ -> Stdio (Std.IO.stdin () |> Std.IO.BufReader.from_reader)

let restore_input = fun __tmp1 ->
  match __tmp1 with
  | Tty tty -> Tty.restore tty
  | Stdio _ -> ()

let input_size = fun __tmp1 ->
  match __tmp1 with
  | Tty tty ->
      let size = Tty.size tty in
      Some (Std.Int.to_string size.cols ^ "x" ^ Std.Int.to_string size.rows)
  | Stdio _ -> None

let read_line = fun input ->
  match input with
  | Tty tty -> (
      let _ = tty in
      let chunk_size = 4_096 in
      let bytes = Kernel.Bytes.create ~size:chunk_size in
      let buffer = Std.StringBuilder.create ~size:256 in
      let chunk_has_line_end = fun count ->
        let rec loop index =
          if index >= count then
            false
          else
            let ch = Kernel.Bytes.get_unchecked bytes ~at:index in
            Std.Char.equal ch '\n' || Std.Char.equal ch '\r' || loop (index + 1)
        in
        loop 0
      in
      let rec loop () =
        match Kernel.IO.Stdin.read ~pos:0 ~len:chunk_size bytes with
        | Ok 0 ->
            if Std.Int.equal (Std.StringBuilder.length buffer) 0 then
              Error `End_of_file
            else
              Ok (Std.StringBuilder.contents buffer |> strip_trailing_newline)
        | Ok count ->
            Std.StringBuilder.add_subbytes buffer bytes 0 count;
            if chunk_has_line_end count then
              Ok (Std.StringBuilder.contents buffer |> strip_trailing_newline)
            else
              loop ()
        | Error err -> Error (`Read_error (Kernel.IO.Stdin.error_to_string err))
      in
      loop ()
    )
  | Stdio reader -> (
      match Std.IO.BufReader.read_line reader with
      | Ok slice -> Ok (Std.IO.IoSlice.to_string slice |> strip_trailing_newline)
      | Error Std.IO.End_of_file -> Error `End_of_file
      | Error err -> Error (`Read_error (Std.IO.error_message err))
    )

let repl_loop = fun session ->
  let input = open_input () in
  let banner =
    match input_size input with
    | Some size -> "Riot REPL (tty " ^ size ^ ")"
    | None -> "Riot REPL"
  in
  println banner;
  println "Type #help;; for help, #build;; to rebuild, #quit;; to exit.";
  let rec loop buffered_lines =
    print (if Std.List.is_empty buffered_lines then "# " else "  ");
    match read_line input with
    | Error `End_of_file ->
        println "";
        Ok ()
    | Error (`Read_error message) -> Error ("failed to read stdin: " ^ message)
    | Ok line ->
        let buffered_lines = buffered_lines @ [ line ] in
        let source = Std.String.concat "\n" buffered_lines in
        if not (phrase_complete source) then
          loop buffered_lines
        else
          match handle_directive_result session source with
          | Error message -> Error message
          | Ok result -> (
          match result with
          | `Stop -> Ok ()
          | `Continue -> loop []
          | `Phrase phrase ->
              if Std.String.equal phrase "" then
                loop []
              else (
                match Riot_eval.Session.eval_phrase session ~source:phrase with
                | Ok () -> loop []
                | Error message ->
                    eprintln message;
                    loop []
              ))
  in
  let result =
    try loop [] with exn -> Error (Std.Exception.to_string exn)
  in
  restore_input input;
  result

let run = fun request ->
  let* session = Riot_eval.Session.create request in
  (
    match Riot_eval.Session.build_workspace session with
    | Ok () -> ()
    | Error message ->
        eprintln ("initial workspace build failed: " ^ message);
        eprintln "continuing with the base runtime; use #build;; to retry."
  );
  repl_loop session
