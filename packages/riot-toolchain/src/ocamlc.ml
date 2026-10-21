open Std
open Std.Collections
open Riot_model

(** OCaml compiler command generation and execution *)
type t = Path.t

type compiler_warning = Ocaml_compiler.warning =
  | LabelsOmitted
  | PartialMatch
  | BadModuleName
  | UnusedVariable
  | UnusedOpen
  | UnusedConstructor
  | UnusedMatch
  | NoCmiFile
  | All

type compiler_flag = Ocaml_compiler.flag =
  | NoAliasDeps
  | Open of string
  | NoStdlib
  | NoPervasives
  | Inline of int
  | NoAssert
  | Compact
  | Unsafe
  | Impl of Std.Path.t
  | Warning of compiler_warning list
  | WarnError of compiler_warning list
  | Raw of string
  | LinkAll

module Diagnostic = struct
  type severity =
    | Warning
    | Error
    | Note
    | Unknown

  type location = {
    path: string;
    line: int option;
    start_char: int option;
    end_char: int option;
    column: int option;
  }

  type render_spec =
    | Literal of string
    | Rewritable of { prefix: string; suffix: string }

  type parsed = {
    render_spec: render_spec;
    location: location option;
    severity: severity;
    code: string option;
    body: string list;
  }

  type t =
    | Parsed of parsed
    | Raw of string

  let parse_int_opt = fun value -> Int.parse value

  let rec find_substring_from = fun text ~needle ~start ->
    let text_len = String.length text in
    let needle_len = String.length needle in
    if needle_len = 0 then
      Some start
    else if start + needle_len > text_len then
      None
    else if String.equal (String.sub text ~offset:start ~len:needle_len) needle then
      Some start
    else
      find_substring_from text ~needle ~start:(start + 1)

  let find_substring = fun text needle -> find_substring_from text ~needle ~start:0

  let parse_bracket_code = fun line ->
    match find_substring line "[" with
    | None -> None
    | Some start -> (
        match find_substring_from line ~needle:"]" ~start:(start + 1) with
        | None -> None
        | Some stop -> Some (String.sub line ~offset:(start + 1) ~len:(stop - start - 1))
      )

  let strip_ansi = fun line ->
    let rec loop acc idx =
      if idx >= String.length line then
        let chars = List.reverse acc in
        let out = IO.Bytes.create ~size:(List.length chars) in
        let rec fill index = fun __tmp1 ->
          match __tmp1 with
          | [] -> IO.Bytes.to_string out
          | ch :: rest ->
              IO.Bytes.set_unchecked out ~at:index ~char:ch;
              fill (index + 1) rest
        in
        fill 0 chars
      else
        let ch = String.get_unchecked line ~at:idx in
        if ch = '\027' then
          let rec skip_escape j =
            if j >= String.length line then
              j
            else
              let escape_ch = String.get_unchecked line ~at:j in
              if (escape_ch >= 'A' && escape_ch <= 'Z') || (escape_ch >= 'a' && escape_ch <= 'z') then
                j + 1
              else
                skip_escape (j + 1)
          in
          loop acc (skip_escape (idx + 1))
        else
          loop (ch :: acc) (idx + 1)
    in
    loop [] 0

  let starts_with_ocaml_header = fun line ->
    let line = strip_ansi line in
    String.starts_with ~prefix:"File \"" line && String.contains line "\", line "

  let split_ocaml_header = fun line ->
    let line = strip_ansi line in
    match find_substring line "File \"" with
    | None -> None
    | Some marker -> (
        let path_start = marker + 6 in
        match find_substring_from line ~needle:"\", line " ~start:path_start with
        | None -> None
        | Some path_end ->
            let prefix = String.sub line ~offset:0 ~len:path_start in
            let path = String.sub line ~offset:path_start ~len:(path_end - path_start) in
            let suffix = String.sub line ~offset:path_end ~len:(String.length line - path_end) in
            let location =
              match find_substring suffix "\", line " with
              | None ->
                  {
                    path;
                    line = None;
                    start_char = None;
                    end_char = None;
                    column = None;
                  }
              | Some marker ->
                  let line_start = marker + 8 in
                  let line_stop =
                    match find_substring_from suffix ~needle:"," ~start:line_start with
                    | Some idx -> idx
                    | None -> String.length suffix
                  in
                  let line_no =
                    parse_int_opt
                      (String.sub suffix ~offset:line_start ~len:(line_stop - line_start))
                  in
                  let (start_char, end_char) =
                    match find_substring suffix ", characters " with
                    | None -> (None, None)
                    | Some chars_idx ->
                        let chars_start = chars_idx + 13 in
                        let dash_idx =
                          match find_substring_from suffix ~needle:"-" ~start:chars_start with
                          | Some idx -> idx
                          | None -> String.length suffix
                        in
                        let chars_end =
                          match find_substring_from suffix ~needle:":" ~start:dash_idx with
                          | Some idx -> idx
                          | None -> String.length suffix
                        in
                        let start_char =
                          parse_int_opt
                            (String.sub suffix ~offset:chars_start ~len:(dash_idx - chars_start))
                        in
                        let end_char =
                          if dash_idx < chars_end then
                            parse_int_opt
                              (String.sub
                                suffix
                                ~offset:(dash_idx + 1)
                                ~len:(chars_end - dash_idx - 1))
                          else
                            None
                        in
                        (start_char, end_char)
                  in
                  {
                    path;
                    line = line_no;
                    start_char;
                    end_char;
                    column = None;
                  }
            in
            Some (prefix, location, suffix)
      )

  let starts_with_c_header = fun line ->
    let line = strip_ansi line in
    let parts = String.split ~by:":" line in
    match parts with
    | path :: line_no :: column :: rest ->
        String.length path > 0 && (
          match (parse_int_opt line_no, parse_int_opt column) with
          | (Some _, Some _) -> true
          | _ -> false
        ) && List.length rest > 0
    | _ -> false

  let split_c_header = fun line ->
    let line = strip_ansi line in
    match String.split ~by:":" line with
    | path :: line_no_str :: column_str :: rest when String.length path > 0 ->
        let rest_str = String.concat ":" rest in
        let trimmed_rest = String.trim rest_str in
        (
          match (parse_int_opt line_no_str, parse_int_opt column_str) with
          | (Some line_no, Some column) when String.starts_with ~prefix:"warning:" trimmed_rest
          || String.starts_with ~prefix:"error:" trimmed_rest
          || String.starts_with ~prefix:"note:" trimmed_rest ->
              let suffix =
                String.sub
                  line
                  ~offset:(String.length path)
                  ~len:(String.length line - String.length path)
              in
              Some (
                "",
                {
                  path;
                  line = Some line_no;
                  start_char = None;
                  end_char = None;
                  column = Some column;
                },
                suffix
              )
          | _ -> None
        )
    | _ -> None

  let classify = fun lines ->
    let rec loop = fun __tmp1 ->
      match __tmp1 with
      | [] -> (Unknown, None)
      | line :: rest ->
          let trimmed =
            line
            |> strip_ansi
            |> String.trim
          in
          if String.starts_with ~prefix:"Warning " trimmed then
            (Warning, parse_bracket_code trimmed)
          else if String.contains trimmed ": warning:" then
            (Warning, parse_bracket_code trimmed)
          else if
            String.starts_with ~prefix:"Error:" trimmed || String.contains trimmed ": error:"
          then
            (Error, parse_bracket_code trimmed)
          else if
            String.starts_with ~prefix:"Note:" trimmed || String.contains trimmed ": note:"
          then
            (Note, parse_bracket_code trimmed)
          else
            loop rest
    in
    loop lines

  let make_raw = fun lines -> Raw (String.concat "\n" lines)

  let parse_block = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | first :: body ->
        let lines = first :: body in
        try
          match split_ocaml_header first with
          | Some (prefix, location, suffix) ->
              let (severity, code) = classify lines in
              Some (
                Parsed {
                  render_spec = Rewritable { prefix; suffix };
                  location = Some location;
                  severity;
                  code;
                  body;
                }
              )
          | None -> (
              match split_c_header first with
              | Some (prefix, location, suffix) ->
                  let (severity, code) = classify lines in
                  Some (
                    Parsed {
                      render_spec = Rewritable { prefix; suffix };
                      location = Some location;
                      severity;
                      code;
                      body;
                    }
                  )
              | None -> Some (make_raw lines)
            )
        with
        | _ -> Some (make_raw lines)

  let parse = fun text ->
    let trimmed = String.trim text in
    if String.equal trimmed "" then
      []
    else
      let lines = String.split ~by:"\n" trimmed in
      let flush_block acc current =
        match parse_block (List.reverse current) with
        | Some diag -> acc @ [ diag ]
        | None -> acc
      in
      let rec loop acc current = fun __tmp1 ->
        match __tmp1 with
        | [] -> flush_block acc current
        | line :: rest ->
            if starts_with_ocaml_header line || starts_with_c_header line then
              match current with
              | [] -> loop acc [ line ] rest
              | _ -> loop
                (flush_block acc current)
                [ line ]
                rest
            else
              loop acc (line :: current) rest
      in
      loop [] [] lines

  let render_header = fun diagnostic ->
    match diagnostic with
    | Raw raw -> raw
    | Parsed { render_spec; location; _ } -> (
        match (render_spec, location) with
        | (Literal line, _) -> line
        | (Rewritable { prefix; suffix }, Some location) -> prefix ^ location.path ^ suffix
        | (Rewritable { prefix; suffix }, None) -> prefix ^ suffix
      )

  let render = fun diagnostic ->
    match diagnostic with
    | Raw raw -> raw
    | Parsed { body; _ } -> String.concat "\n" (render_header diagnostic :: body)

  let render_all = fun diagnostics ->
    diagnostics
    |> List.map ~fn:render
    |> String.concat "\n"

  let map_path = fun rewrite diagnostic ->
    match diagnostic with
    | Raw _ -> diagnostic
    | Parsed parsed -> (
        match parsed.location with
        | None -> diagnostic
        | Some location -> (
            match rewrite location.path with
            | None -> diagnostic
            | Some path -> Parsed { parsed with location = Some { location with path } }
          )
      )

  let location = fun __tmp1 ->
    match __tmp1 with
    | Raw _ -> None
    | Parsed parsed -> parsed.location

  let severity = fun __tmp1 ->
    match __tmp1 with
    | Raw _ -> Unknown
    | Parsed parsed -> parsed.severity

  let is_warning = fun diagnostic ->
    match severity diagnostic with
    | Warning -> true
    | Error
    | Note
    | Unknown -> false
end

type success = {
  message: string;
  diagnostics: Diagnostic.t list;
}

type failure = {
  message: string;
  diagnostics: Diagnostic.t list;
}

type result =
  | Success of success
  | Failed of failure

type mode =
  | Compile
  | Library
  | Executable
  | CustomExe
  | SharedLibrary

type output_mode =
  | Normal
  | WriteStdoutToFile of Path.t

type invocation = {
  cwd: Path.t;
  env: (string * string) list;
  (* Directory of the compiler binary. Cross toolchains ship their binutils
     there (for example [x86_64-linux-gnu-as]) and the compiler finds them by
     name through [PATH], so [run] prepends it. It is deliberately not part of
     [to_string], so command identity does not depend on the caller's [PATH].
  *)
  tool_dir: Path.t option;
  command_string: string;
  output_mode: output_mode;
}

let make = fun path -> path

let path = fun t -> t

let run_in_dir = fun ~cwd ~env cmd_str ->
  let cmd_with_cd = "cd " ^ Path.to_string cwd ^ " && " ^ cmd_str in
  Log.debug ("  $ " ^ cmd_with_cd);
  Command.make ~env ~args:[ "-c"; cmd_with_cd ] "sh"

let base_command = fun t -> Path.to_string t

let default_disabled_warnings = [ NoCmiFile ]

let warning_code = fun __tmp1 ->
  match __tmp1 with
  | All -> "a"
  | warning ->
      Riot_model.Ocaml_compiler.warning_to_number warning
      |> Int.to_string

let render_warning_baseline = fun warnings ->
  warnings
  |> List.map ~fn:(fun warning -> "-" ^ warning_code warning)
  |> String.concat ""

let is_dev_source = fun source ->
  let path = Path.to_string source in
  String.contains path "/tests/"
  || String.contains path "/examples/"
  || String.contains path "/bench/"
  || String.starts_with ~prefix:"tests/" path
  || String.starts_with ~prefix:"examples/" path
  || String.starts_with ~prefix:"bench/" path

let warning_baseline_flags = fun source ->
  let disabled_warnings =
    if is_dev_source source then
      default_disabled_warnings @ [ BadModuleName ]
    else
      default_disabled_warnings
  in
  [ "-w"; render_warning_baseline disabled_warnings ]

let default_warning_flags = [ "-w"; render_warning_baseline default_disabled_warnings ]

let flags_to_string = Ocaml_compiler.flags_to_string

let flags_of_string = Ocaml_compiler.flags_of_string

let make_include_flags = fun dirs ->
  dirs
  |> List.map ~fn:(fun dir -> "-I " ^ dir)
  |> String.concat " "

let make_invocation = fun ?(output_mode = Normal) t ~cwd command_string ->
  {
    cwd;
    env = [ ("OCAML_COLOR", "always"); ];
    tool_dir =
      (* Only absolute compiler paths: a bare name resolved through [PATH]
         must not add the working directory to [PATH].
      *)
      if Path.is_absolute t then
        Path.parent t
      else
        None;
    command_string;
    output_mode;
  }

let build_invocation = fun
  t
  ~cwd
  ?cc
  ?(includes = [])
  ?(libs = [])
  ?(cclibs = [])
  ?(ccflags = [])
  ?(ccopt_flags = [])
  ?(cclib_flags = [])
  ?(output = None)
  ?(mode = Compile)
  ?(flags = [])
  sources ->
  let ocamlc = base_command t in
  let include_flags = make_include_flags (List.map includes ~fn:Path.to_string) in
  let cc_flag =
    match cc with
    | Some cc -> "-cc " ^ Path.to_string cc
    | None -> ""
  in
  let mode_flag =
    match mode with
    | Compile -> "-c"
    | Library -> "-a"
    | CustomExe -> ""
    | Executable -> ""
    | SharedLibrary -> "-shared"
  in
  let output_flag =
    match output with
    | Some out -> "-o " ^ Path.to_string out
    | None -> ""
  in
  let sources_str = String.concat " " sources in
  let libs_str = String.concat " " (List.map libs ~fn:Path.to_string) in
  let cclibs_str =
    if List.length cclibs > 0 then
      String.concat " " (List.map cclibs ~fn:(fun lib -> "-cclib " ^ Path.to_string lib))
    else
      ""
  in
  let ccflags_str =
    if List.length ccflags > 0 then
      String.concat " " (List.map ccflags ~fn:(fun flag -> "-ccopt \"" ^ flag ^ "\""))
    else
      ""
  in
  let ccopt_flags_str =
    if List.length ccopt_flags > 0 then
      String.concat " " (List.map ccopt_flags ~fn:(fun flag -> "-ccopt \"" ^ flag ^ "\""))
    else
      ""
  in
  let cclib_flags_str =
    if List.length cclib_flags > 0 then
      String.concat " " (List.map cclib_flags ~fn:(fun flag -> "-cclib \"" ^ flag ^ "\""))
    else
      ""
  in
  let flags_str = String.concat " " (flags_to_string flags) in
  let command_string =
    [
      ocamlc;
      cc_flag;
      "-bin-annot";
      String.concat " " default_warning_flags;
      mode_flag;
      flags_str;
      include_flags;
      output_flag;
      libs_str;
      cclibs_str;
      ccflags_str;
      ccopt_flags_str;
      cclib_flags_str;
      sources_str;
    ]
    |> List.filter ~fn:(fun s -> not (String.equal s ""))
    |> String.concat " "
  in
  if mode = SharedLibrary then (
    Log.info
      ("[OCAMLC] Building shared library with includes: "
      ^ String.concat ", " (List.map includes ~fn:Path.to_string));
    Log.info ("[OCAMLC] cclibs: " ^ String.concat ", " (List.map cclibs ~fn:Path.to_string));
    Log.info ("[OCAMLC] objects/sources: " ^ sources_str);
    Log.info ("[OCAMLC] Full command: " ^ command_string)
  );
  make_invocation t ~cwd command_string

let compile_interface = fun t ~cwd ~includes ~flags ~output source ->
  let includes_with_dot = Path.v "." :: includes in
  let has_impl_flag =
    List.any
      flags
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Impl _ -> true
        | _ -> false)
  in
  let args =
    (((([ "-bin-annot"; "-c" ] @ warning_baseline_flags source) @ flags_to_string flags)
    @ List.fold_right
      includes_with_dot
      ~init:[]
      ~fn:(fun dir acc -> [ "-I"; Path.to_string dir ] @ acc))
    @ [ "-o"; Path.to_string output ]) @ if has_impl_flag then
      []
    else
      [ Path.to_string source ]
  in
  make_invocation
    t
    ~cwd
    (String.concat " " ([ base_command t ] @ args))

let compile_impl = fun t ~cwd ~includes ~flags ?(bin_annot = true) ?cmi_file ~output source ->
  let includes_with_dot = Path.v "." :: includes in
  let has_impl_flag =
    List.any
      flags
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Impl _ -> true
        | _ -> false)
  in
  let args =
    let include_flags =
      List.fold_right
        includes_with_dot
        ~init:[]
        ~fn:(fun dir acc -> [ "-I"; Path.to_string dir ] @ acc)
    in
    let cmi_file_flags =
      match cmi_file with
      | Some path -> [ "-cmi-file"; Path.to_string path ]
      | None -> []
    in
    let source_args =
      if has_impl_flag then
        []
      else
        [ Path.to_string source ]
    in
    (
      (
        (
          (
            (
              (
                if bin_annot then
                  [ "-bin-annot"; "-c" ]
                else
                  [ "-c" ]
              ) @ warning_baseline_flags source
            ) @ flags_to_string flags
          ) @ include_flags
        ) @ cmi_file_flags
      ) @ [ "-o"; Path.to_string output ]
    ) @ source_args
  in
  make_invocation
    t
    ~cwd
    (String.concat " " ([ base_command t ] @ args))

let compile_sources = fun t ~cwd ~includes ~flags sources ->
  let includes_with_dot = Path.v "." :: includes in
  build_invocation
    t
    ~cwd
    ~includes:includes_with_dot
    ~flags
    ~mode:Compile
    (List.map sources ~fn:Path.to_string)

let generate_interface = fun t ~cwd ~includes ~flags ~output source ->
  let includes_with_dot = Path.v "." :: includes in
  let args =
    ((([ "-i" ] @ warning_baseline_flags source) @ flags_to_string flags)
    @ List.fold_right
      includes_with_dot
      ~init:[]
      ~fn:(fun dir acc -> [ "-I"; Path.to_string dir ] @ acc))
    @ [ Path.to_string source ]
  in
  make_invocation
    ~output_mode:(WriteStdoutToFile output)
    t
    ~cwd
    (String.concat " " ([ base_command t ] @ args))

let compile_c = fun t ~cwd ~includes ?cc ?(ccflags = []) ~output source ->
  build_invocation
    t
    ~cwd
    ?cc
    ~includes
    ~ccflags
    ~output:(Some output)
    ~mode:Compile
    [ Path.to_string source ]

let create_library = fun t ~cwd ~includes ~output objects ->
  build_invocation
    t
    ~cwd
    ~includes
    ~output:(Some output)
    ~mode:Library
    (List.map objects ~fn:Path.to_string)

let compile_library = fun t ~cwd ~includes ~flags ~output sources ->
  build_invocation
    t
    ~cwd
    ~includes
    ~flags
    ~output:(Some output)
    ~mode:Library
    (List.map sources ~fn:Path.to_string)

let create_executable = fun
  t
  ~cwd
  ~includes
  ~output
  ~libs
  ?cc
  ?(cclibs = [])
  ?(ccopt_flags = [])
  ?(cclib_flags = [])
  objects ->
  let includes_with_dot = Path.v "." :: includes in
  build_invocation
    t
    ~cwd
    ?cc
    ~includes:includes_with_dot
    ~libs
    ~cclibs
    ~ccopt_flags
    ~cclib_flags
    ~output:(Some output)
    ~mode:Executable
    ~flags:[ LinkAll ]
    (List.map objects ~fn:Path.to_string)

let create_shared_library = fun
  t
  ~cwd
  ~includes
  ~output
  ~libs
  ?cc
  ?(cclibs = [])
  ?(ccopt_flags = [])
  ?(cclib_flags = [])
  objects ->
  let includes_with_dot = Path.v "." :: includes in
  build_invocation
    t
    ~cwd
    ?cc
    ~includes:includes_with_dot
    ~libs
    ~cclibs
    ~ccopt_flags
    ~cclib_flags
    ~output:(Some output)
    ~mode:SharedLibrary
    ~flags:[ LinkAll ]
    (List.map objects ~fn:Path.to_string)

let create_custom_executable = fun t ~cwd ~includes ~output ~libs ?cc objects ->
  let includes_with_dot = Path.v "." :: includes in
  build_invocation
    t
    ~cwd
    ?cc
    ~includes:includes_with_dot
    ~libs
    ~output:(Some output)
    ~mode:CustomExe
    (List.map objects ~fn:Path.to_string)

let to_string = fun invocation ->
  let env_prefix =
    match invocation.env with
    | [] -> ""
    | env -> String.concat " " (List.map env ~fn:(fun (key, value) -> key ^ "=" ^ value)) ^ " "
  in
  "cd " ^ Path.to_string invocation.cwd ^ " && " ^ env_prefix ^ invocation.command_string

let run_env = fun invocation ->
  match invocation.tool_dir with
  | None -> invocation.env
  | Some tool_dir ->
      let path =
        match Env.get Env.String ~var:"PATH" with
        | Some current when not (String.equal current "") -> Path.to_string tool_dir ^ ":" ^ current
        | _ -> Path.to_string tool_dir
      in
      invocation.env @ [ ("PATH", path) ]

let run = fun invocation ->
  Log.debug ("[OCAMLC] Running command: " ^ to_string invocation);
  let cmd = run_in_dir ~cwd:invocation.cwd ~env:(run_env invocation) invocation.command_string in
  match Command.output cmd with
  | Ok output when output.Command.status = 0 -> (
      let diagnostics = Diagnostic.parse output.Command.stderr in
      match invocation.output_mode with
      | Normal -> Success { message = output.Command.stdout; diagnostics }
      | WriteStdoutToFile file -> (
          match Fs.write output.Command.stdout file with
          | Ok () ->
              Success { message = "Generated interface " ^ Path.to_string file; diagnostics }
          | Error err ->
              Failed {
                message = "Failed to write " ^ Path.to_string file ^ ": " ^ IO.error_message err;
                diagnostics = [];
              }
        )
    )
  | Ok output -> (
      let diagnostics = Diagnostic.parse output.Command.stderr in
      match invocation.output_mode with
      | Normal ->
          Failed {
            message = "Command failed with status " ^ Int.to_string output.Command.status;
            diagnostics;
          }
      | WriteStdoutToFile _ ->
          Failed {
            message = "ocamlc -i failed with exit code " ^ Int.to_string output.Command.status;
            diagnostics;
          }
    )
  | Error (Command.SystemError msg) -> Failed { message = msg; diagnostics = [] }

let is_success = fun __tmp1 ->
  match __tmp1 with
  | Success _ -> true
  | Failed _ -> false

let get_output = fun __tmp1 ->
  match __tmp1 with
  | Success { message; _ } -> message
  | Failed { message; diagnostics } ->
      let rendered = Diagnostic.render_all diagnostics in
      if String.equal rendered "" then
        message
      else
        message ^ ": " ^ rendered

let get_ocamlc_warnings = fun __tmp1 ->
  match __tmp1 with
  | Success { diagnostics; _ } ->
      diagnostics
      |> List.filter ~fn:Diagnostic.is_warning
      |> List.map ~fn:Diagnostic.render
  | Failed _ -> []
