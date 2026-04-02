open Std
module Test = Std.Test

let sample_ocaml_warning = String.concat
  "\n"
  [
    "File \"/tmp/sandbox/pkg/src/install.ml\", line 25, characters 8-26:";
    "25 |     let displayed_packages = HashMap.create () in";
    "             ^^^^^^^^^^^^^^^^^^";
    "Warning 26 [unused-var]: unused variable displayed_packages.";
  ]

let sample_c_error = "/tmp/sandbox/pkg/native/kernel_uuid.c:14:10: error: uuid/uuid.h: No such file or directory"

let sample_unparseable_c_like_line = "/tmp/sandbox/pkg/native/kernel_crypto.c:79:note this is not a structured compiler diagnostic"

let sample_colored_ocaml_warning = String.concat
  "\n"
  [
    "\027[1mFile \"/tmp/sandbox/pkg/src/install.ml\", line 25, characters 8-26\027[0m:";
    "25 |     let displayed_packages = HashMap.create () in";
    "             \027[1;35m^^^^^^^^^^^^^^^^^^\027[0m";
    "\027[1;35mWarning\027[0m 26 [unused-var]: unused variable \027[1mdisplayed_packages\027[0m.";
  ]

let test_compile_impl_disables_no_cmi_file_by_default = fun _ctx ->
  let ocamlc = Tusk_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation = Tusk_toolchain.Ocamlc.compile_impl
    ocamlc
    ~cwd:(Path.v "/tmp/work")
    ~includes:[ Path.v "src" ]
    ~flags:[]
    ~output:(Path.v "foo.cmx")
    (Path.v "src/foo.ml") in
  let command = Tusk_toolchain.Ocamlc.to_string invocation in
  if String.contains command "-w -49" then
    Ok ()
  else
    Error ("expected default warning baseline to disable warning 49, got: " ^ command)

let test_compile_impl_does_not_force_debug_symbols = fun _ctx ->
  let ocamlc = Tusk_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation = Tusk_toolchain.Ocamlc.compile_impl
    ocamlc
    ~cwd:(Path.v "/tmp/work")
    ~includes:[ Path.v "src" ]
    ~flags:[]
    ~output:(Path.v "foo.cmx")
    (Path.v "src/foo.ml") in
  let command = Tusk_toolchain.Ocamlc.to_string invocation in
  if String.contains command " -g " then
    Error ("expected compile_impl to stop hardcoding -g, got: " ^ command)
  else
    Ok ()

let test_compile_impl_renders_warn_error_and_raw_flags = fun _ctx ->
  let ocamlc = Tusk_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation = Tusk_toolchain.Ocamlc.compile_impl
    ocamlc
    ~cwd:(Path.v "/tmp/work")
    ~includes:[ Path.v "src" ]
    ~flags:[
      Tusk_toolchain.Ocamlc.WarnError [ Tusk_toolchain.Ocamlc.All ];
      Tusk_toolchain.Ocamlc.Raw "-O2";
      Tusk_toolchain.Ocamlc.Raw "-noassert";
    ]
    ~output:(Path.v "foo.cmx")
    (Path.v "src/foo.ml") in
  let command = Tusk_toolchain.Ocamlc.to_string invocation in
  if not (String.contains command "-warn-error +a") then
    Error ("expected -warn-error +a in command, got: " ^ command)
  else if not (String.contains command " -O2 ") then
    Error ("expected raw -O2 flag in command, got: " ^ command)
  else if not (String.contains command " -noassert ") then
    Error ("expected raw -noassert flag in command, got: " ^ command)
  else
    Ok ()

let test_parse_ocaml_warning_diagnostic = fun _ctx ->
  match Tusk_toolchain.Ocamlc.Diagnostic.parse sample_ocaml_warning with
  | [ diagnostic ] -> (
      match Tusk_toolchain.Ocamlc.Diagnostic.location diagnostic with
      | Some location ->
          let rendered = Tusk_toolchain.Ocamlc.Diagnostic.render diagnostic in
          if not (String.equal rendered sample_ocaml_warning) then
            Error "expected parsed warning to render back to the original block"
          else if not (String.equal location.path "/tmp/sandbox/pkg/src/install.ml") then
            Error ("unexpected parsed path: " ^ location.path)
          else if not (Tusk_toolchain.Ocamlc.Diagnostic.is_warning diagnostic) then
            Error "expected parsed diagnostic to be classified as a warning"
          else
            Ok ()
      | None -> Error "expected parsed warning to include a location"
    )
  | diagnostics -> Error ("expected exactly one parsed warning block, got "
  ^ Int.to_string (List.length diagnostics))

let test_map_path_rewrites_rendered_diagnostic = fun _ctx ->
  match Tusk_toolchain.Ocamlc.Diagnostic.parse sample_ocaml_warning with
  | [ diagnostic ] ->
      let rewritten =
        Tusk_toolchain.Ocamlc.Diagnostic.map_path
          (fun path ->
            if String.equal path "/tmp/sandbox/pkg/src/install.ml" then
              Some "./packages/tusk-cli/src/install.ml"
            else
              None)
          diagnostic
      in
      let rendered = Tusk_toolchain.Ocamlc.Diagnostic.render rewritten in
      if String.contains rendered "./packages/tusk-cli/src/install.ml" then
        Ok ()
      else
        Error ("expected rewritten diagnostic path, got: " ^ rendered)
  | _ -> Error "expected exactly one parsed warning block"

let test_parse_c_error_diagnostic = fun _ctx ->
  match Tusk_toolchain.Ocamlc.Diagnostic.parse sample_c_error with
  | [ diagnostic ] -> (
      match Tusk_toolchain.Ocamlc.Diagnostic.location diagnostic with
      | Some location when String.equal location.path "/tmp/sandbox/pkg/native/kernel_uuid.c" -> Ok ()
      | Some location -> Error ("unexpected parsed c diagnostic path: " ^ location.path)
      | None -> Error "expected parsed c diagnostic to include a location"
    )
  | _ -> Error "expected exactly one parsed c diagnostic"

let test_unparseable_c_like_line_falls_back_to_raw = fun _ctx ->
  match Tusk_toolchain.Ocamlc.Diagnostic.parse sample_unparseable_c_like_line with
  | [ diagnostic ] ->
      let rendered = Tusk_toolchain.Ocamlc.Diagnostic.render diagnostic in
      if String.equal rendered sample_unparseable_c_like_line then
        Ok ()
      else
        Error ("expected raw fallback to preserve the original text, got: " ^ rendered)
  | diagnostics -> Error ("expected exactly one raw diagnostic block, got "
  ^ Int.to_string (List.length diagnostics))

let test_parse_colored_ocaml_warning_diagnostic = fun _ctx ->
  match Tusk_toolchain.Ocamlc.Diagnostic.parse sample_colored_ocaml_warning with
  | [ diagnostic ] -> (
      match Tusk_toolchain.Ocamlc.Diagnostic.location diagnostic with
      | Some location when String.equal location.path "/tmp/sandbox/pkg/src/install.ml" ->
          if Tusk_toolchain.Ocamlc.Diagnostic.is_warning diagnostic then
            Ok ()
          else
            Error "expected colored diagnostic to still be classified as a warning"
      | Some location -> Error ("unexpected colored diagnostic path: " ^ location.path)
      | None -> Error "expected colored diagnostic to include a location"
    )
  | diagnostics -> Error ("expected exactly one parsed colored warning block, got "
  ^ Int.to_string (List.length diagnostics))

let tests =
  Test.[
    case "compile impl disables no-cmi-file by default" test_compile_impl_disables_no_cmi_file_by_default;
    case "compile impl does not force debug symbols" test_compile_impl_does_not_force_debug_symbols;
    case "compile impl renders warn-error and raw flags" test_compile_impl_renders_warn_error_and_raw_flags;
    case "parse ocaml warning diagnostic" test_parse_ocaml_warning_diagnostic;
    case "map path rewrites rendered diagnostic" test_map_path_rewrites_rendered_diagnostic;
    case "parse c error diagnostic" test_parse_c_error_diagnostic;
    case "unparseable c-like line falls back to raw" test_unparseable_c_like_line_falls_back_to_raw;
    case "parse colored ocaml warning diagnostic" test_parse_colored_ocaml_warning_diagnostic;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"toolchain_tests" ~tests ~args)
    ~args:Env.args
    ()
