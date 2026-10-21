open Std

module Test = Std.Test

let sample_ocaml_warning =
  String.concat
    "\n"
    [
      "File \"/tmp/sandbox/pkg/src/install.ml\", line 25, characters 8-26:";
      "25 |     let displayed_packages = HashMap.create () in";
      "             ^^^^^^^^^^^^^^^^^^";
      "Warning 26 [unused-var]: unused variable displayed_packages.";
    ]

let sample_c_error =
  "/tmp/sandbox/pkg/native/kernel_uuid.c:14:10: error: uuid/uuid.h: No such file or directory"

let sample_unparseable_c_like_line =
  "/tmp/sandbox/pkg/native/kernel_crypto.c:79:note this is not a structured compiler diagnostic"

let sample_colored_ocaml_warning =
  String.concat
    "\n"
    [
      "\027[1mFile \"/tmp/sandbox/pkg/src/install.ml\", line 25, characters 8-26\027[0m:";
      "25 |     let displayed_packages = HashMap.create () in";
      "             \027[1;35m^^^^^^^^^^^^^^^^^^\027[0m";
      "\027[1;35mWarning\027[0m 26 [unused-var]: unused variable \027[1mdisplayed_packages\027[0m.";
    ]

let with_temp_dir = fun label f ->
  match Fs.with_tempdir ~prefix:("riot_toolchain_" ^ label) (fun dir -> f dir) with
  | Ok result -> result
  | Error err -> Error ("failed to create temp dir: " ^ IO.error_message err)

let with_env_var = fun name value_opt f ->
  let restore_value = Env.get Env.String ~var:name in
  let restore () =
    match restore_value with
    | Some value ->
        let _ = Env.set ~var:name ~value in
        ()
    | None ->
        let _ = Env.remove ~var:name in
        ()
  in
  let () =
    match value_opt with
    | Some value ->
        let _ = Env.set ~var:name ~value in
        ()
    | None ->
        let _ = Env.remove ~var:name in
        ()
  in
  try
    let result = f () in
    restore ();
    result
  with
  | exn ->
      restore ();
      raise exn

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let target_strings = fun targets ->
  List.map targets ~fn:Riot_model.Target.to_string
  |> List.sort ~compare:String.compare

let first_distinct_target = fun host ->
  let candidates = [
    "aarch64-apple-darwin";
    "x86_64-unknown-linux-gnu";
    "aarch64-unknown-linux-gnu";
  ]
  in
  candidates
  |> List.find ~fn:(fun candidate -> not (Riot_model.Target.equal host (target candidate)))
  |> Option.map ~fn:target
  |> Option.expect ~msg:"expected at least one distinct target fixture"

let test_compile_impl_disables_no_cmi_file_by_default = fun _ctx ->
  let ocamlc = Riot_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation =
    Riot_toolchain.Ocamlc.compile_impl
      ocamlc
      ~cwd:(Path.v "/tmp/work")
      ~includes:[ Path.v "src" ]
      ~flags:[]
      ~output:(Path.v "foo.cmx")
      (Path.v "src/foo.ml")
  in
  let command = Riot_toolchain.Ocamlc.to_string invocation in
  if String.contains command "-w -49" then
    Ok ()
  else
    Error ("expected default warning baseline to disable warning 49, got: " ^ command)

let test_compile_impl_disables_bad_module_name_for_dev_sources = fun _ctx ->
  let ocamlc = Riot_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation =
    Riot_toolchain.Ocamlc.compile_impl
      ocamlc
      ~cwd:(Path.v "/tmp/work")
      ~includes:[ Path.v "examples" ]
      ~flags:[]
      ~output:(Path.v "001_hello_world.cmx")
      (Path.v "examples/001_hello_world.ml")
  in
  let command = Riot_toolchain.Ocamlc.to_string invocation in
  if String.contains command "-w -49-24" || String.contains command "-w -24-49" then
    Ok ()
  else
    Error ("expected dev-source warning baseline to disable warnings 49 and 24, got: " ^ command)

let test_compile_impl_does_not_force_debug_symbols = fun _ctx ->
  let ocamlc = Riot_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation =
    Riot_toolchain.Ocamlc.compile_impl
      ocamlc
      ~cwd:(Path.v "/tmp/work")
      ~includes:[ Path.v "src" ]
      ~flags:[]
      ~output:(Path.v "foo.cmx")
      (Path.v "src/foo.ml")
  in
  let command = Riot_toolchain.Ocamlc.to_string invocation in
  if String.contains command " -g " then
    Error ("expected compile_impl to stop hardcoding -g, got: " ^ command)
  else
    Ok ()

let test_compile_impl_can_disable_bin_annot = fun _ctx ->
  let ocamlc = Riot_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation =
    Riot_toolchain.Ocamlc.compile_impl
      ocamlc
      ~cwd:(Path.v "/tmp/work")
      ~includes:[ Path.v "src" ]
      ~flags:[]
      ~bin_annot:false
      ~output:(Path.v "foo.cmx")
      (Path.v "src/foo.ml")
  in
  let command = Riot_toolchain.Ocamlc.to_string invocation in
  if String.contains command "-bin-annot" then
    Error ("expected compile_impl to allow disabling -bin-annot, got: " ^ command)
  else
    Ok ()

let test_compile_impl_renders_warn_error_and_raw_flags = fun _ctx ->
  let ocamlc = Riot_toolchain.Ocamlc.make (Path.v "/tmp/ocamlopt.opt") in
  let invocation =
    Riot_toolchain.Ocamlc.compile_impl
      ocamlc
      ~cwd:(Path.v "/tmp/work")
      ~includes:[ Path.v "src" ]
      ~flags:[
        Riot_toolchain.Ocamlc.WarnError [ Riot_toolchain.Ocamlc.All ];
        Riot_toolchain.Ocamlc.Raw "-O2";
        Riot_toolchain.Ocamlc.Raw "-noassert";
      ]
      ~output:(Path.v "foo.cmx")
      (Path.v "src/foo.ml")
  in
  let command = Riot_toolchain.Ocamlc.to_string invocation in
  if not (String.contains command "-warn-error +a") then
    Error ("expected -warn-error +a in command, got: " ^ command)
  else if not (String.contains command " -O2 ") then
    Error ("expected raw -O2 flag in command, got: " ^ command)
  else if not (String.contains command " -noassert ") then
    Error ("expected raw -noassert flag in command, got: " ^ command)
  else
    Ok ()

let test_parse_ocaml_warning_diagnostic = fun _ctx ->
  match Riot_toolchain.Ocamlc.Diagnostic.parse sample_ocaml_warning with
  | [ diagnostic ] -> (
      match Riot_toolchain.Ocamlc.Diagnostic.location diagnostic with
      | Some location ->
          let rendered = Riot_toolchain.Ocamlc.Diagnostic.render diagnostic in
          if not (String.equal rendered sample_ocaml_warning) then
            Error "expected parsed warning to render back to the original block"
          else if not (String.equal location.path "/tmp/sandbox/pkg/src/install.ml") then
            Error ("unexpected parsed path: " ^ location.path)
          else if not (Riot_toolchain.Ocamlc.Diagnostic.is_warning diagnostic) then
            Error "expected parsed diagnostic to be classified as a warning"
          else
            Ok ()
      | None -> Error "expected parsed warning to include a location"
    )
  | diagnostics ->
      Error ("expected exactly one parsed warning block, got "
      ^ Int.to_string (List.length diagnostics))

let test_map_path_rewrites_rendered_diagnostic = fun _ctx ->
  match Riot_toolchain.Ocamlc.Diagnostic.parse sample_ocaml_warning with
  | [ diagnostic ] ->
      let rewritten =
        Riot_toolchain.Ocamlc.Diagnostic.map_path
          (fun path ->
            if String.equal path "/tmp/sandbox/pkg/src/install.ml" then
              Some "./packages/riot-cli/src/install.ml"
            else
              None)
          diagnostic
      in
      let rendered = Riot_toolchain.Ocamlc.Diagnostic.render rewritten in
      if String.contains rendered "./packages/riot-cli/src/install.ml" then
        Ok ()
      else
        Error ("expected rewritten diagnostic path, got: " ^ rendered)
  | _ -> Error "expected exactly one parsed warning block"

let test_parse_c_error_diagnostic = fun _ctx ->
  match Riot_toolchain.Ocamlc.Diagnostic.parse sample_c_error with
  | [ diagnostic ] -> (
      match Riot_toolchain.Ocamlc.Diagnostic.location diagnostic with
      | Some location when String.equal location.path "/tmp/sandbox/pkg/native/kernel_uuid.c" ->
          Ok ()
      | Some location -> Error ("unexpected parsed c diagnostic path: " ^ location.path)
      | None -> Error "expected parsed c diagnostic to include a location"
    )
  | _ -> Error "expected exactly one parsed c diagnostic"

let test_unparseable_c_like_line_falls_back_to_raw = fun _ctx ->
  match Riot_toolchain.Ocamlc.Diagnostic.parse sample_unparseable_c_like_line with
  | [ diagnostic ] ->
      let rendered = Riot_toolchain.Ocamlc.Diagnostic.render diagnostic in
      if String.equal rendered sample_unparseable_c_like_line then
        Ok ()
      else
        Error ("expected raw fallback to preserve the original text, got: " ^ rendered)
  | diagnostics ->
      Error ("expected exactly one raw diagnostic block, got "
      ^ Int.to_string (List.length diagnostics))

let test_parse_colored_ocaml_warning_diagnostic = fun _ctx ->
  match Riot_toolchain.Ocamlc.Diagnostic.parse sample_colored_ocaml_warning with
  | [ diagnostic ] -> (
      match Riot_toolchain.Ocamlc.Diagnostic.location diagnostic with
      | Some location when String.equal location.path "/tmp/sandbox/pkg/src/install.ml" ->
          if Riot_toolchain.Ocamlc.Diagnostic.is_warning diagnostic then
            Ok ()
          else
            Error "expected colored diagnostic to still be classified as a warning"
      | Some location -> Error ("unexpected colored diagnostic path: " ^ location.path)
      | None -> Error "expected colored diagnostic to include a location"
    )
  | diagnostics ->
      Error ("expected exactly one parsed colored warning block, got "
      ^ Int.to_string (List.length diagnostics))

let test_list_available_toolchains_reads_manifest = fun _ctx ->
  with_temp_dir
    "available_manifest"
    (fun temp_dir ->
      let manifest_path = Path.(temp_dir / Path.v "manifest.json") in
      let manifest =
        Data.Json.to_string_pretty
          (Data.Json.obj
            [
              ("schema_version", Data.Json.int 1);
              ("generated_at", Data.Json.string "2026-04-04T00:00:00Z");
              ("base_url", Data.Json.string "https://cdn.pkgs.ml/ocaml");
              (
                "toolchains",
                Data.Json.array
                  [
                    Data.Json.obj
                      [
                        ("version", Data.Json.string "5.5.0-riot.4");
                        ("host", Data.Json.string "aarch64-apple-darwin");
                        ("target", Data.Json.string "aarch64-apple-darwin");
                        ("artifact_target", Data.Json.string "aarch64-apple-darwin");
                        ("kind", Data.Json.string "native");
                        (
                          "artifact",
                          Data.Json.string "ocaml-5.5.0-riot.4-aarch64-apple-darwin.tar.gz"
                        );
                        (
                          "artifact_url",
                          Data.Json.string
                            "https://cdn.pkgs.ml/ocaml/ocaml-5.5.0-riot.4-aarch64-apple-darwin.tar.gz"
                        );
                        (
                          "checksum_url",
                          Data.Json.string
                            "https://cdn.pkgs.ml/ocaml/ocaml-5.5.0-riot.4-aarch64-apple-darwin.tar.gz.sha256"
                        );
                        ("size_bytes", Data.Json.int 123);
                        ("last_modified", Data.Json.string "2026-04-04T00:00:00Z");
                      ];
                    Data.Json.obj
                      [
                        ("version", Data.Json.string "5.5.0-riot.4");
                        ("host", Data.Json.string "aarch64-apple-darwin");
                        ("target", Data.Json.string "x86_64-unknown-linux-gnu");
                        (
                          "artifact_target",
                          Data.Json.string "aarch64-apple-darwin-x-x86_64-unknown-linux-gnu"
                        );
                        ("kind", Data.Json.string "cross");
                        (
                          "artifact",
                          Data.Json.string
                            "ocaml-5.5.0-riot.4-aarch64-apple-darwin-x-x86_64-unknown-linux-gnu.tar.gz"
                        );
                        (
                          "artifact_url",
                          Data.Json.string
                            "https://cdn.pkgs.ml/ocaml/ocaml-5.5.0-riot.4-aarch64-apple-darwin-x-x86_64-unknown-linux-gnu.tar.gz"
                        );
                        (
                          "checksum_url",
                          Data.Json.string
                            "https://cdn.pkgs.ml/ocaml/ocaml-5.5.0-riot.4-aarch64-apple-darwin-x-x86_64-unknown-linux-gnu.tar.gz.sha256"
                        );
                        ("size_bytes", Data.Json.int 456);
                        ("last_modified", Data.Json.string "2026-04-04T00:01:00Z");
                      ];
                  ]
              );
            ])
      in
      match Fs.write manifest manifest_path with
      | Error err -> Error ("failed to write manifest fixture: " ^ IO.error_message err)
      | Ok () ->
          with_env_var
            "RIOT_OCAML_CDN_URL"
            (Some ("file://" ^ Path.to_string temp_dir))
            (fun () ->
              match Riot_toolchain.list_available_toolchains () with
              | Error msg -> Error ("expected manifest to parse, got: " ^ msg)
              | Ok toolchains ->
                  let find_toolchain ~host ~target =
                    List.find
                      toolchains
                      ~fn:(fun (toolchain: Riot_toolchain.available_toolchain) ->
                        Riot_model.Target.equal toolchain.host host
                        && Riot_model.Target.equal toolchain.target target)
                  in
                  let native_host = target "aarch64-apple-darwin" in
                  let native_target = target "aarch64-apple-darwin" in
                  let cross_target = target "x86_64-unknown-linux-gnu" in
                  if not (Int.equal (List.length toolchains) 2) then
                    Error ("expected 2 available toolchains, got "
                    ^ Int.to_string (List.length toolchains))
                  else
                    match (
                      find_toolchain ~host:native_host ~target:native_target,
                      find_toolchain ~host:native_host ~target:cross_target
                    ) with
                    | (Some native, Some cross) -> (
                        match (native.kind, cross.kind) with
                        | (Riot_toolchain.Native, Riot_toolchain.Cross) ->
                            if not (native.size_bytes = Some 123) then
                              Error "expected native toolchain size to be parsed"
                            else if
                              not
                                (String.equal
                                  cross.artifact_target
                                  "aarch64-apple-darwin-x-x86_64-unknown-linux-gnu")
                            then
                              Error "expected cross artifact target to be parsed"
                            else
                              Ok ()
                        | _ -> Error "expected native and cross kinds to be parsed"
                      )
                    | _ -> Error "expected both native and cross toolchains to be present"))

let test_get_host_triple_matches_std_host_triple = fun _ctx ->
  if Riot_model.Target.equal (Riot_toolchain.get_host_triple ()) Std.System.host_triple then
    Ok ()
  else
    Error "expected Riot_toolchain.get_host_triple to use Std.System.host_triple"

let test_list_toolchains_returns_typed_targets = fun _ctx ->
  let host = Riot_toolchain.get_host_triple () in
  let cross = first_distinct_target host in
  let config =
    Riot_model.Toolchain_config.{
      version = "5.5.0-riot.4";
      source = Version "5.5.0-riot.4";
      targets = [ host; cross ];
    }
  in
  let toolchains = Riot_toolchain.list_toolchains ~config in
  let targets = List.map toolchains ~fn:(fun info -> info.Riot_toolchain.target) in
  let rendered = target_strings targets in
  let expected = target_strings [ host; cross ] in
  let host_info = List.find toolchains ~fn:(fun info -> Riot_model.Target.equal info.target host) in
  let cross_info =
    List.find toolchains ~fn:(fun info -> Riot_model.Target.equal info.target cross)
  in
  if not (String.equal (String.concat "," rendered) (String.concat "," expected)) then
    Error "expected list_toolchains to preserve typed target triples"
  else
    match (host_info, cross_info) with
    | (Some host_info, Some cross_info) ->
        if not host_info.is_host then
          Error "expected host target to be marked as host"
        else if cross_info.is_host then
          Error "expected non-host target to not be marked as host"
        else
          Ok ()
    | _ -> Error "expected host and cross targets to both be present"

(* Cross compilers invoke their binutils by bare name (for example
   [x86_64-linux-gnu-as]); the toolchain ships them next to the compiler.
*)

let test_run_finds_tools_beside_the_compiler = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_toolchain_tool_dir"
    (fun dir ->
      let compiler = Path.(dir / Path.v "fake-ocamlopt") in
      let tool = Path.(dir / Path.v "riot-fake-cross-as") in
      let write_script path body =
        match Fs.write ("#!/bin/sh\n" ^ body ^ "\n") path with
        | Error err -> Error (IO.error_message err)
        | Ok () ->
            Fs.set_permissions path Fs.Permissions.executable
            |> Result.map_err ~fn:IO.error_message
      in
      match write_script tool "exit 0" with
      | Error _ as err -> err
      | Ok () -> (
          match write_script compiler "exec riot-fake-cross-as \"$@\"" with
          | Error _ as err -> err
          | Ok () -> (
              let invocation =
                Riot_toolchain.Ocamlc.compile_impl
                  (Riot_toolchain.Ocamlc.make compiler)
                  ~cwd:dir
                  ~includes:[]
                  ~flags:[]
                  ~output:(Path.v "foo.cmx")
                  (Path.v "foo.ml")
              in
              if String.contains (Riot_toolchain.Ocamlc.to_string invocation) "PATH=" then
                Error "expected PATH to stay out of the invocation identity"
              else
                match Riot_toolchain.Ocamlc.run invocation with
                | Riot_toolchain.Ocamlc.Success _ -> Ok ()
                | Riot_toolchain.Ocamlc.Failed { message; _ } ->
                    Error ("expected the compiler to find its sibling tool: " ^ message)
            )
        )) with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let tests =
  Test.[
    case "ocamlc run finds tools beside the compiler" test_run_finds_tools_beside_the_compiler;
    case
      "compile impl disables no-cmi-file by default"
      test_compile_impl_disables_no_cmi_file_by_default;
    case
      "compile impl disables bad-module-name for dev sources"
      test_compile_impl_disables_bad_module_name_for_dev_sources;
    case "compile impl does not force debug symbols" test_compile_impl_does_not_force_debug_symbols;
    case "compile impl can disable bin annot" test_compile_impl_can_disable_bin_annot;
    case
      "compile impl renders warn-error and raw flags"
      test_compile_impl_renders_warn_error_and_raw_flags;
    case "parse ocaml warning diagnostic" test_parse_ocaml_warning_diagnostic;
    case "map path rewrites rendered diagnostic" test_map_path_rewrites_rendered_diagnostic;
    case "parse c error diagnostic" test_parse_c_error_diagnostic;
    case "unparseable c-like line falls back to raw" test_unparseable_c_like_line_falls_back_to_raw;
    case "parse colored ocaml warning diagnostic" test_parse_colored_ocaml_warning_diagnostic;
    case
      "get_host_triple matches Std.System.host_triple"
      test_get_host_triple_matches_std_host_triple;
    case "list_toolchains returns typed targets" test_list_toolchains_returns_typed_targets;
    case
      ~size:Large
      "list available toolchains reads manifest"
      test_list_available_toolchains_reads_manifest;
  ]

let main ~args = Test.Cli.main ~name:"toolchain_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
