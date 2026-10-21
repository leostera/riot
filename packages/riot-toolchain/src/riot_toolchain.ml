open Std

(**
   Toolchain bootstrapping and management

   This package provides an abstraction layer over OCaml compiler tooling.
   Currently shells out to ocamlc/ocamldep, but designed to support:
   - In-process RAML compiler calls via FFI
   - Multiple compiler backends
   - Easier testing and mocking
*)
type source =
  | Version of string
  | Path of Path.t
  | Url of Net.Uri.t

module Ocamldep = Ocamldep
module Ocamlc = Ocamlc
module CrossCompilingToolchain = Cross_compiling_toolchain

type t = {
  version: string;
  source: source;
  target: Riot_model.Target.t;
  (* Target triple this toolchain compiles for *)
  ocamlc: Ocamlc.t;
  ocamlc_bytecode: Ocamlc.t;
  ocamlopt: Path.t;
  ocamldep: Ocamldep.t;
}

let default_ocaml_version = "5.5.0-riot.4"

let toolchain_base_dir = Path.(Riot_model.Riot_dirs.dot_riot / Path.v "toolchains")

let target_to_string = Riot_model.Target.to_string

let get_host_triple = fun () ->
  match System.os_type with
  | "Unix" -> System.host_triple
  | _ ->
      Riot_model.Target.from_string "x86_64-unknown-linux"
      |> Result.expect ~msg:"invalid default host triple"

let get_toolchain_path = fun version ->
  let host_triple = get_host_triple () in
  Path.(toolchain_base_dir / Path.v version / Path.v (target_to_string host_triple))

let get_toolchain_path_for_target = fun version target ->
  Path.(toolchain_base_dir / Path.v version / Path.v (target_to_string target))

let local_compiler_path = fun () -> Path.v "./vendor/ocaml/compiler"

let source_from_config = fun (config: Riot_model.Toolchain_config.t) ->
  match config.source with
  | Riot_model.Toolchain_config.Version v -> Version v
  | Riot_model.Toolchain_config.Path p -> Path p
  | Riot_model.Toolchain_config.Url u -> Url u

let source_for_target = fun ~config ~target ->
  let source = source_from_config config in
  let host = get_host_triple () in
  let local_compiler = local_compiler_path () in
  if Riot_model.Target.equal target host then
    match Fs.is_dir local_compiler with
    | Ok true -> Path local_compiler
    | _ -> source
  else
    source

let make_toolchain = fun version source ~target ->
  let toolchain_path = get_toolchain_path_for_target version target in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in
  let bin_path bin = Path.(bin_dir / Path.v bin) in
  {
    version;
    source;
    target;
    ocamlc = Ocamlc.make (bin_path "ocamlopt.opt");
    ocamlc_bytecode = Ocamlc.make (bin_path "ocamlc.opt");
    ocamlopt = bin_path "ocamlopt.opt";
    ocamldep = Ocamldep.make (bin_path "ocamldep.opt");
  }

let from_config_for_target = fun ~config ~target ->
  let version = config.Riot_model.Toolchain_config.version in
  let source = source_for_target ~config ~target in
  make_toolchain version source ~target

let ocamlc = fun t -> t.ocamlc

let ocamlc_bytecode = fun t -> t.ocamlc_bytecode

let ocamlopt_path = fun t -> t.ocamlopt

let ocamldep = fun t -> t.ocamldep

let path = fun t -> get_toolchain_path_for_target t.version t.target

let c_compiler = fun t ->
  if Riot_model.Target.equal t.target (get_host_triple ()) then
    None
  else
    CrossCompilingToolchain.detect ~toolchain_root:(path t) () ~target_triplet:t.target
    |> fun detection -> detection.c_compiler

let check_binaries_exist = fun toolchain ->
  let ocamlc_path = Ocamlc.path toolchain.ocamlc in
  let ocamlc_bytecode_path = Ocamlc.path toolchain.ocamlc_bytecode in
  let ocamldep_path = Ocamldep.path toolchain.ocamldep in
  match (
    Fs.exists ocamlc_path,
    Fs.exists ocamlc_bytecode_path,
    Fs.exists toolchain.ocamlopt,
    Fs.exists ocamldep_path
  ) with
  | (Ok true, Ok true, Ok true, Ok true) -> Ok ()
  | (Ok false, _, _, _) -> Error ("ocamlc not found at " ^ Path.to_string ocamlc_path)
  | (_, Ok false, _, _) -> Error ("ocamlc.opt not found at " ^ Path.to_string ocamlc_bytecode_path)
  | (_, _, Ok false, _) -> Error ("ocamlopt not found at " ^ Path.to_string toolchain.ocamlopt)
  | (_, _, _, Ok false) -> Error ("ocamldep not found at " ^ Path.to_string ocamldep_path)
  | (Error err, _, _, _)
  | (_, Error err, _, _)
  | (_, _, Error err, _)
  | (_, _, _, Error err) -> Error ("Failed to check binaries: " ^ IO.error_message err)

let path_exists = fun path ->
  match Fs.exists path with
  | Ok true -> true
  | _ -> false

let dir_exists = fun path ->
  match Fs.is_dir path with
  | Ok true -> true
  | _ -> false

let write_path_fingerprint = fun hasher path ->
  Crypto.Sha256.write hasher (Path.to_string path);
  match Fs.metadata path with
  | Ok metadata ->
      Crypto.Sha256.write hasher (Int.to_string (Fs.Metadata.len metadata));
      Crypto.Sha256.write hasher (Float.to_string (Fs.Metadata.modified metadata))
  | Error _ -> Crypto.Sha256.write hasher "missing"

let first_existing = fun paths ->
  let rec loop remaining =
    match remaining with
    | [] -> None
    | path :: rest ->
        if path_exists path then
          Some path
        else
          loop rest
  in
  loop paths

let parse_json_field_string = fun json fields ->
  let rec resolve = fun value ->
    fun __tmp1 ->
      match __tmp1 with
      | [] -> (
          match Data.Json.get_string value with
          | Some string -> Some string
          | None -> None
        )
      | field :: rest -> (
          match Data.Json.get_field field value with
          | Some next -> resolve next rest
          | None -> None
        )
  in
  resolve json fields

let read_manifest_fingerprint = fun toolchain_path ->
  let manifest_path = Path.(toolchain_path / Path.v "manifest.json") in
  let parse_fingerprint json =
    let open Data.Json in
    parse_json_field_string json [ "toolchain_fingerprint" ]
    |> Option.or_ (parse_json_field_string json [ "fingerprint"; "value" ])
  in
  match Fs.exists manifest_path with
  | Ok true -> (
      match Fs.read manifest_path with
      | Ok raw -> (
          match Data.Json.from_string raw with
          | Ok manifest -> parse_fingerprint manifest
          | Error _ -> None
        )
      | Error _ -> None
    )
  | Ok false
  | Error _ -> None

let manifest_error = fun target manifest_path ->
  let target = target_to_string target in
  "Missing or invalid manifest.json at "
  ^ manifest_path
  ^ " for "
  ^ target
  ^ ". Reinstall this toolchain from a republished archive that includes manifest.json "
  ^ "(the release artifact must include a stable toolchain_fingerprint)."

let trim_trailing_slash = fun value ->
  if String.length value > 0 && String.get_unchecked value ~at:(String.length value - 1) = '/' then
    String.sub value ~offset:0 ~len:(String.length value - 1)
  else
    value

let ocaml_download_base_url = fun () ->
  match Env.get Env.String ~var:"RIOT_OCAML_CDN_URL" with
  | Some value when not (String.equal value "") -> trim_trailing_slash value
  | _ -> (
      match Env.get Env.String ~var:"OCAML_CDN_PUBLIC_BASE_URL" with
      | Some value when not (String.equal value "") -> trim_trailing_slash value
      | _ -> (
          match Env.get Env.String ~var:"RIOT_CDN_PUBLIC_BASE_URL" with
          | Some value when not (String.equal value "") -> trim_trailing_slash value ^ "/ocaml"
          | _ -> "https://cdn.pkgs.ml/ocaml"
        )
    )

let ensure_manifest_present = fun toolchain_path target source ->
  match source with
  | Path _ -> Ok ()
  | _ -> (
      match read_manifest_fingerprint toolchain_path with
      | Some fingerprint when not (String.equal (String.trim fingerprint) "") -> Ok ()
      | _ ->
          Error (manifest_error
            target
            (Path.to_string Path.(toolchain_path / Path.v "manifest.json")))
    )

let sysroot_candidates = fun ~toolchain_path ~target ->
  let target = target_to_string target in
  [
    Path.(toolchain_path / Path.v "sysroot");
    Path.(toolchain_path / Path.v ("sysroot-" ^ target));
    Path.(toolchain_path / Path.v "gcc" / Path.v target / Path.v "sysroot");
  ]

let bundled_sysroot_marker_paths = fun sysroot target ->
  let markers = [
    Path.(sysroot / Path.v "usr" / Path.v "include" / Path.v "uuid" / Path.v "uuid.h");
    Path.(sysroot / Path.v "usr" / Path.v "include" / Path.v "openssl" / Path.v "ssl.h");
    Path.(sysroot / Path.v "usr" / Path.v "include" / Path.v "pcre2.h");
    Path.(sysroot / Path.v "usr" / Path.v "include" / Path.v "zlib.h");
    Path.(sysroot / Path.v "usr" / Path.v "lib" / Path.v "libuuid.a");
    Path.(sysroot / Path.v "usr" / Path.v "lib" / Path.v "libssl.a");
    Path.(sysroot / Path.v "usr" / Path.v "lib" / Path.v "libcrypto.a");
    Path.(sysroot / Path.v "usr" / Path.v "lib" / Path.v "libpcre2-8.a");
  ]
  in
  let glibc_marker =
    match (target.Riot_model.Target.architecture, target.Riot_model.Target.os) with
    | ("aarch64", "linux") ->
        Some Path.(sysroot
        / Path.v "usr"
        / Path.v "include"
        / Path.v "aarch64-linux-gnu"
        / Path.v "bits"
        / Path.v "types"
        / Path.v "struct___jmp_buf_tag.h")
    | ("x86_64", "linux") ->
        Some Path.(sysroot
        / Path.v "usr"
        / Path.v "include"
        / Path.v "x86_64-linux-gnu"
        / Path.v "bits"
        / Path.v "types"
        / Path.v "struct___jmp_buf_tag.h")
    | _ -> None
  in
  match glibc_marker with
  | Some marker -> marker :: markers
  | None -> markers

let explicit_sysroot_override = fun () ->
  let present name =
    match Env.get Env.String ~var:name with
    | Some value -> not (String.equal value "")
    | None -> false
  in
  present "CROSS_SYSROOT" || present "SYSROOT"

let missing_cross_components = fun ~toolchain_path ~target ->
  let bin_prefix = CrossCompilingToolchain.bin_prefix_of_triplet target in
  let compiler_candidates = [
    Path.(toolchain_path / Path.v "bin" / Path.v (bin_prefix ^ "gcc"));
    Path.(toolchain_path / Path.v "gcc" / Path.v "bin" / Path.v (bin_prefix ^ "gcc"));
  ]
  in
  let sysroot_candidates = sysroot_candidates ~toolchain_path ~target in
  let compiler_missing =
    if List.any compiler_candidates ~fn:path_exists then
      []
    else
      [ "cross-compiler" ]
  in
  let sysroot_missing =
    match first_existing sysroot_candidates with
    | Some sysroot when List.all (bundled_sysroot_marker_paths sysroot target) ~fn:path_exists -> []
    | _ -> [ "sysroot" ]
  in
  compiler_missing @ sysroot_missing

let validate_toolchain_install = fun ~version ~target ~source ->
  let toolchain_path = get_toolchain_path_for_target version target in
  let toolchain = make_toolchain version source ~target in
  match check_binaries_exist toolchain with
  | Error _ -> Error [ "binaries" ]
  | Ok () ->
      let missing =
        if Riot_model.Target.equal target (get_host_triple ()) || explicit_sysroot_override () then
          []
        else
          missing_cross_components ~toolchain_path ~target
      in
      if List.length missing > 0 then
        Error missing
      else
        match ensure_manifest_present toolchain_path target source with
        | Error msg -> Error [ msg ]
        | Ok () -> Ok toolchain

let reset_toolchain_install = fun path ->
  match Fs.exists path with
  | Ok true -> (
      match Fs.remove_dir_all path with
      | Ok () -> Ok ()
      | Error err ->
          Error ("Failed to remove existing toolchain at "
          ^ Path.to_string path
          ^ ": "
          ^ IO.error_message err)
    )
  | Ok false -> Ok ()
  | Error err ->
      Error ("Failed to inspect existing toolchain at "
      ^ Path.to_string path
      ^ ": "
      ^ IO.error_message err)

let install_staging_path = fun toolchain_path ->
  let nonce =
    Random.bits ()
    |> Result.expect ~msg:"failed to generate toolchain staging nonce"
  in
  Path.(Path.dirname toolchain_path
  / Path.v
    ("."
    ^ Path.basename toolchain_path
    ^ ".staging-"
    ^ Int32.to_string (Process.id ())
    ^ "-"
    ^ Int.to_string nonce))

(* Replace [toolchain_path] with the verified [staging] tree. The old install
   is renamed aside first so the final path only ever holds a complete
   toolchain. When several installers race (parallel test suites, concurrent
   builds), a loser whose rename fails accepts the winner's install as long as
   it carries a manifest; callers validate the result afterwards.
*)

let swap_in_toolchain = fun ~staging ~toolchain_path ~target ->
  let retired = Path.v (Path.to_string staging ^ ".retired") in
  let installed_by_someone_else () =
    match read_manifest_fingerprint toolchain_path with
    | Some fingerprint -> not (String.equal (String.trim fingerprint) "")
    | None -> false
  in
  let move_error err =
    Error ("Failed to move toolchain into place at "
    ^ Path.to_string toolchain_path
    ^ " for "
    ^ target_to_string target
    ^ ": "
    ^ IO.error_message err)
  in
  let looks_complete () =
    installed_by_someone_else () && match Fs.is_dir Path.(toolchain_path / Path.v "bin") with
    | Ok true -> true
    | _ -> false
  in
  (* Callers only download after validation failed, so a complete-looking
     install here was finished by a concurrent installer. Keep it rather than
     yanking a toolchain that other builds may already be using.
  *)
  if looks_complete () then
    Ok ()
  else
    let _ =
      (* Missing is fine: there was no previous install, or a concurrent
         installer already retired it.
      *)
      Fs.rename ~src:toolchain_path ~dst:retired
    in
    let result =
      match Fs.rename ~src:staging ~dst:toolchain_path with
      | Ok () -> Ok ()
      | Error _ when installed_by_someone_else () -> Ok ()
      | Error err -> move_error err
    in
    let _ = reset_toolchain_install retired in
    result

let download_and_install_toolchain = fun version ~host ~target ->
  let toolchain_path = get_toolchain_path_for_target version target in
  let base_url = ocaml_download_base_url () in
  let host_name = target_to_string host in
  let target_name = target_to_string target in
  (* Determine URL pattern based on host vs cross-compilation *)
  let (binary_url, tar_filename, description) =
    if String.equal host_name target_name then
      let url = base_url ^ "/ocaml-" ^ version ^ "-" ^ host_name ^ ".tar.gz" in
      let filename = "ocaml-" ^ version ^ "-" ^ host_name ^ ".tar.gz" in
      (url, filename, "native")
    else
      (* Cross-compilation toolchain *)
      let url =
        base_url ^ "/ocaml-" ^ version ^ "-" ^ host_name ^ "-x-" ^ target_name ^ ".tar.gz"
      in
      let filename = "ocaml-" ^ version ^ "-" ^ host_name ^ "-x-" ^ target_name ^ ".tar.gz" in
      (url, filename, "cross-compilation from " ^ host_name ^ " to " ^ target_name)
  in
  println
    ("📥 Downloading OCaml " ^ version ^ " for " ^ target_name ^ " (" ^ description ^ ")...");
  match Path.parent toolchain_path with
  | None -> Error ("Toolchain path has no parent: " ^ Path.to_string toolchain_path)
  | Some install_dir -> (
      match Fs.create_dir_all install_dir with
      | Error err ->
          Error ("Failed to create toolchain directory "
          ^ Path.to_string install_dir
          ^ ": "
          ^ IO.error_message err)
      | Ok () -> (
          (* Download and extract into a per-process staging directory beside
             the final path, then rename it into place. An interrupted or
             concurrent install never leaves a half-extracted toolchain at
             [toolchain_path], and installers never share a tarball path.
          *)
          let staging = install_staging_path toolchain_path in
          let tar_path = Path.(install_dir / Path.v (Path.basename staging ^ "-" ^ tar_filename)) in
          let cleanup () =
            let _ = Fs.remove_file tar_path in
            let _ = Fs.remove_dir_all staging in
            ()
          in
          let result =
            match Fs.create_dir staging with
            | Error err ->
                Error ("Failed to create toolchain staging directory "
                ^ Path.to_string staging
                ^ ": "
                ^ IO.error_message err)
            | Ok () -> (
                (* [-f] turns HTTP errors into a failing exit status instead of
                   saving the error page as the tarball.
                *)
                let download_cmd =
                  Command.make ~args:[ "-fsSL"; "-o"; Path.to_string tar_path; binary_url; ] "curl"
                in
                match Command.output download_cmd with
                | Error (Command.SystemError msg) -> Error ("Failed to download toolchain: " ^ msg)
                | Ok output when output.Command.status != 0 ->
                    Error ("Failed to download toolchain from "
                    ^ binary_url
                    ^ ": "
                    ^ String.trim output.Command.stderr)
                | Ok _ -> (
                    println "✓ Download complete, extracting...";
                    let extract_cmd =
                      Command.make
                        ~args:[ "-xzf"; Path.to_string tar_path; "-C"; Path.to_string staging; ]
                        "tar"
                    in
                    match Command.output extract_cmd with
                    | Error (Command.SystemError msg) ->
                        Error ("Failed to extract toolchain: " ^ msg)
                    | Ok output when output.Command.status != 0 ->
                        Error ("Failed to extract toolchain tarball: "
                        ^ String.trim output.Command.stderr)
                    | Ok _ -> (
                        match ensure_manifest_present staging target (Version version) with
                        | Error msg -> Error msg
                        | Ok () -> swap_in_toolchain ~staging ~toolchain_path ~target
                      )
                  )
              )
          in
          cleanup ();
          (
            match result with
            | Ok () ->
                println ("✓ OCaml " ^ version ^ " (" ^ target_name ^ ") installed successfully")
            | Error _ -> ()
          );
          result
        )
    )

let init = fun ~config ->
  let version = config.Riot_model.Toolchain_config.version in
  let local_compiler = local_compiler_path () in
  let source =
    match config.source with
    | Riot_model.Toolchain_config.Version v -> Version v
    | Riot_model.Toolchain_config.Path p -> Path p
    | Riot_model.Toolchain_config.Url u -> Url u
  in
  let source =
    match Fs.is_dir local_compiler with
    | Ok true -> Path local_compiler
    | _ -> source
  in
  let host = get_host_triple () in
  let target = host in
  (* init always uses host target for backward compatibility *)
  let toolchain = make_toolchain version source ~target in
  let toolchain_path = get_toolchain_path version in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in
  (* Check if toolchain is already installed *)
  match Fs.is_dir bin_dir with
  | Ok true -> (
      match check_binaries_exist toolchain with
      | Ok () -> Ok toolchain
      | Error _ -> Error ("Toolchain at " ^ Path.to_string toolchain_path ^ " is incomplete")
    )
  | _ -> (
      (* Try to use ./vendor/ocaml/compiler if it exists *)
      match Fs.is_dir local_compiler with
      | Ok true -> (
          (* Create symlink from ~/.riot/toolchains/{version}/{host_triple} to ./vendor/ocaml/compiler *)
          (
            match Path.parent toolchain_path with
            | Some parent ->
                let _ = Fs.create_dir_all parent in
                ()
            | None -> ()
          );
          (* Check if symlink already exists *)
          match Fs.exists toolchain_path with
          | Ok true -> Ok (make_toolchain version source ~target)
          | _ -> (
              (* Get absolute path for local_compiler *)
              let cwd =
                Env.current_dir ()
                |> Result.expect ~msg:"Failed to get cwd"
              in
              let abs_local =
                if Path.is_absolute local_compiler then
                  local_compiler
                else
                  Path.(cwd / local_compiler)
              in
              match Fs.symlink ~src:abs_local ~dst:toolchain_path with
              | Ok () -> Ok (make_toolchain version source ~target)
              | Error err ->
                  Error ("Failed to create toolchain symlink from "
                  ^ Path.to_string toolchain_path
                  ^ " to "
                  ^ Path.to_string abs_local
                  ^ ": "
                  ^ IO.error_message err)
            )
        )
      | _ ->
          (* Try to download and install prebuilt binary *)
          println "Toolchain not found locally, attempting to download...";
          (
            match download_and_install_toolchain version ~host ~target with
            | Ok () -> (
                match check_binaries_exist toolchain with
                | Ok () -> Ok toolchain
                | Error msg -> Error ("Toolchain installed but incomplete: " ^ msg)
              )
            | Error msg ->
                let host_triple = get_host_triple () in
                Error ("Toolchain not found!\n\n\
                  Looking for: OCaml "
                ^ version
                ^ " for "
                ^ target_to_string host_triple
                ^ "\n\
                  Expected location: "
                ^ Path.to_string toolchain_path
                ^ "\n\n\
                  Download failed: "
                ^ msg)
          )
    )

let ensure_default_toolchain = fun () ->
  let default_config = Riot_model.Toolchain_config.default in
  match init ~config:default_config with
  | Ok _ -> Ok ()
  | Error msg -> Error msg

let check_health = fun toolchain ->
  match check_binaries_exist toolchain with
  | Error msg -> Error msg
  | Ok () -> (
      (* Try to execute ocamlc -version to verify it works *)
      let ocamlc_path = Ocamlc.path toolchain.ocamlc in
      let cmd = Command.make ~args:[ "-version" ] (Path.to_string ocamlc_path) in
      match Command.output cmd with
      | Ok output when output.Command.status = 0 ->
          Log.debug ("Toolchain healthy: ocamlc version = " ^ String.trim output.Command.stdout);
          Ok ()
      | Ok output ->
          Error ("ocamlc exists but failed: exit code " ^ Int.to_string output.Command.status)
      | Error (Command.SystemError msg) -> Error ("ocamlc health check failed: " ^ msg)
    )

let hash = fun t ->
  let hasher = Crypto.Sha256.create () in
  let toolchain_path = get_toolchain_path_for_target t.version t.target in
  let write_legacy_path_fingerprint paths =
    List.for_each paths ~fn:(write_path_fingerprint hasher)
  in
  Crypto.Sha256.write hasher t.version;
  Crypto.Sha256.write hasher (target_to_string t.target);
  let () =
    match t.source with
    | Path _ ->
        write_legacy_path_fingerprint
          [
            Ocamlc.path t.ocamlc;
            Ocamlc.path t.ocamlc_bytecode;
            t.ocamlopt;
            Ocamldep.path t.ocamldep;
            Path.(toolchain_path / Path.v "lib" / Path.v "ocaml" / Path.v "stdlib.cmxa");
            Path.(toolchain_path / Path.v "lib" / Path.v "ocaml" / Path.v "unix" / Path.v "unix.cmi");
            Path.(toolchain_path
            / Path.v "lib"
            / Path.v "ocaml"
            / Path.v "unix"
            / Path.v "unix.cmxa");
          ];
        if not (Riot_model.Target.equal t.target (get_host_triple ())) then (
          match first_existing (sysroot_candidates ~toolchain_path ~target:t.target) with
          | Some sysroot ->
              write_legacy_path_fingerprint (bundled_sysroot_marker_paths sysroot t.target)
          | None -> ()
        )
    | Version _
    | Url _ -> (
        match read_manifest_fingerprint toolchain_path with
        | Some fingerprint when not (String.equal (String.trim fingerprint) "") ->
            Crypto.Sha256.write hasher fingerprint
        | _ ->
            panic
              ("Toolchain manifest fingerprint is required for non-local toolchains. "
              ^ "Install from a republished archive that includes manifest.json.")
      )
  in
  Crypto.Sha256.finish hasher

(** Initialize toolchain for a specific target architecture *)
let init_for_target = fun ~config ~target ->
  let version = config.Riot_model.Toolchain_config.version in
  let source = source_for_target ~config ~target in
  let host = get_host_triple () in
  let local_compiler = local_compiler_path () in
  let toolchain_path = get_toolchain_path_for_target version target in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in
  let validate () = validate_toolchain_install ~version ~target ~source in
  let refresh () =
    match download_and_install_toolchain version ~host ~target with
    | Ok () -> (
        match validate () with
        | Ok toolchain -> Ok toolchain
        | Error missing ->
            Error ("Downloaded toolchain for "
            ^ target_to_string target
            ^ " but it is still incomplete: "
            ^ String.concat ", " missing)
      )
    | Error msg ->
        Error ("Failed to download toolchain for " ^ target_to_string target ^ ": " ^ msg)
  in
  (* Check if already installed *)
  match Fs.is_dir bin_dir with
  | Ok true -> (
      match validate () with
      | Ok toolchain -> Ok toolchain
      | Error missing when not (Riot_model.Target.equal target host) ->
          Log.info
            ("Refreshing cross toolchain for "
            ^ target_to_string target
            ^ " because it is missing: "
            ^ String.concat ", " missing);
          refresh ()
      | Error _ -> Error ("Toolchain incomplete: " ^ Path.to_string toolchain_path)
    )
  | _ ->
      (* Try local compiler if native build *)
      if Riot_model.Target.equal host target then (
        match Fs.is_dir local_compiler with
        | Ok true ->
            (* Create symlink *)
            (
              match Path.parent toolchain_path with
              | Some parent ->
                  let _ = Fs.create_dir_all parent in
                  ()
              | None -> ()
            );
            (
              match Fs.exists toolchain_path with
              | Ok true ->
                  let toolchain = make_toolchain version source ~target in
                  Ok toolchain
              | _ ->
                  let cwd =
                    Env.current_dir ()
                    |> Result.expect ~msg:"Failed to get cwd"
                  in
                  let abs_local =
                    if Path.is_absolute local_compiler then
                      local_compiler
                    else
                      Path.(cwd / local_compiler)
                  in
                  (
                    match Fs.symlink ~src:abs_local ~dst:toolchain_path with
                    | Ok () ->
                        let toolchain = make_toolchain version source ~target in
                        Ok toolchain
                    | Error err -> Error ("Failed to create symlink: " ^ IO.error_message err)
                  )
            )
        | _ -> (
            match download_and_install_toolchain version ~host ~target with
            | Ok () ->
                let toolchain = make_toolchain version source ~target in
                (
                  match check_binaries_exist toolchain with
                  | Ok () -> Ok toolchain
                  | Error msg -> Error ("Downloaded but incomplete: " ^ msg)
                )
            | Error msg ->
                Error ("Failed to download toolchain for " ^ target_to_string target ^ ": " ^ msg)
          )
      ) else
        (* Cross-compilation - download cross-toolchain *)
        refresh ()

(** Get toolchain for specific target (lazy initialization) *)
let get_for_target = fun ~config ~target -> init_for_target ~config ~target

(** Toolchain management types and functions *)
type toolchain_status =
  | Installed of {
      path: Path.t;
    }
  | NotInstalled of {
      expected_path: Path.t;
    }
  | Incomplete of {
      path: Path.t;
      missing: string list;
    }

type toolchain_info = {
  version: string;
  target: Riot_model.Target.t;
  is_host: bool;
  status: toolchain_status;
}

type available_toolchain_kind =
  | Native
  | Cross

type available_toolchain = {
  version: string;
  host: Riot_model.Target.t;
  target: Riot_model.Target.t;
  artifact_target: string;
  kind: available_toolchain_kind;
  artifact: string;
  artifact_url: string;
  checksum_url: string;
  size_bytes: int option;
  last_modified: string option;
}

let check_toolchain_status = fun ~version ~target ->
  let toolchain_path = get_toolchain_path_for_target version target in
  let source =
    if Riot_model.Target.equal target (get_host_triple ()) then
      match Fs.is_dir (local_compiler_path ()) with
      | Ok true -> Path (local_compiler_path ())
      | _ -> Version version
    else
      Version version
  in
  match Fs.is_dir toolchain_path with
  | Ok false
  | Error _ -> NotInstalled { expected_path = toolchain_path }
  | Ok true -> (
      match validate_toolchain_install ~version ~target ~source with
      | Ok _ -> Installed { path = toolchain_path }
      | Error missing -> Incomplete { path = toolchain_path; missing }
    )

let list_toolchains = fun ~config ->
  let version = config.Riot_model.Toolchain_config.version in
  let host = get_host_triple () in
  let targets =
    match config.targets with
    | [] -> [ host ]
    | ts -> ts
  in
  List.map
    targets
    ~fn:(fun target ->
      let is_host = Riot_model.Target.equal target host in
      let status = check_toolchain_status ~version ~target in
      {
        version;
        target;
        is_host;
        status;
      })

let install_all_toolchains = fun ~config ->
  let version = config.Riot_model.Toolchain_config.version in
  let toolchains = list_toolchains ~config in
  let host = get_host_triple () in
  let results =
    List.map
      toolchains
      ~fn:(fun info ->
        match info.status with
        | Installed _ ->
            println
              (
                "  ✓ " ^ target_to_string info.target ^ (
                  if info.is_host then
                    " (host)"
                  else
                    ""
                ) ^ " - already installed"
              );
            Ok `Skipped
        | NotInstalled _
        | Incomplete _ ->
            println
              (
                "  📥 " ^ target_to_string info.target ^ (
                  if info.is_host then
                    " (host)"
                  else
                    ""
                ) ^ " - downloading..."
              );
            (
              match download_and_install_toolchain version ~host ~target:info.target with
              | Ok () -> Ok `Installed
              | Error msg ->
                  println ("     ✗ Failed: " ^ msg);
                  Error (target_to_string info.target, msg)
            ))
  in
  let successes =
    List.filter
      results
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Ok _ -> true
        | Error _ -> false)
  in
  let failures =
    List.filter
      results
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Error _ -> true
        | Ok _ -> false)
  in
  if List.length failures > 0 then
    let errors =
      List.filter_map
        results
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Error (target, msg) -> Some (target ^ ": " ^ msg)
          | _ -> None)
    in
    Error ("Failed to install toolchains:\n  " ^ String.concat "\n  " errors)
  else
    let installed =
      List.filter
        successes
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Ok `Installed -> true
          | _ -> false)
      |> List.length
    in
    let skipped =
      List.filter
        successes
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Ok `Skipped -> true
          | _ -> false)
      |> List.length
    in
    Ok (installed, skipped)

let fetch_url = fun url ->
  let cmd = Command.make ~args:[ "-fsSL"; url ] "curl" in
  match Command.output cmd with
  | Error (Command.SystemError msg) -> Error ("Failed to fetch " ^ url ^ ": " ^ msg)
  | Ok output when output.Command.status != 0 ->
      let details =
        let stderr = String.trim output.Command.stderr in
        if String.equal stderr "" then
          "curl exited with " ^ Int.to_string output.Command.status
        else
          stderr
      in
      Error ("Failed to fetch " ^ url ^ ": " ^ details)
  | Ok output -> Ok output.Command.stdout

let require_json_field = fun name json ->
  match Data.Json.get_field name json with
  | Some value -> Ok value
  | None -> Error ("Toolchain manifest is missing field '" ^ name ^ "'")

let require_json_string_field = fun name json ->
  match require_json_field name json with
  | Error _ as err -> err
  | Ok value -> (
      match Data.Json.get_string value with
      | Some string -> Ok string
      | None -> Error ("Toolchain manifest field '" ^ name ^ "' must be a string")
    )

let require_json_target_field = fun name json ->
  match require_json_string_field name json with
  | Error _ as err -> err
  | Ok raw -> (
      match Riot_model.Target.from_string raw with
      | Ok target -> Ok target
      | Error msg ->
          Error ("Toolchain manifest field '"
          ^ name
          ^ "' must be a valid target triple: "
          ^ Riot_model.Target.error_message msg)
    )

let optional_json_string_field = fun name json ->
  match Data.Json.get_field name json with
  | None
  | Some Data.Json.Null -> Ok None
  | Some value -> (
      match Data.Json.get_string value with
      | Some string -> Ok (Some string)
      | None -> Error ("Toolchain manifest field '" ^ name ^ "' must be a string")
    )

let optional_json_int_field = fun name json ->
  match Data.Json.get_field name json with
  | None
  | Some Data.Json.Null -> Ok None
  | Some value -> (
      match Data.Json.get_int value with
      | Some int -> Ok (Some int)
      | None -> Error ("Toolchain manifest field '" ^ name ^ "' must be an int")
    )

let parse_available_toolchain_kind = fun __tmp1 ->
  match __tmp1 with
  | "native" -> Ok Native
  | "cross" -> Ok Cross
  | other -> Error ("Unknown toolchain manifest kind '" ^ other ^ "'")

let parse_available_toolchain = fun json ->
  match require_json_string_field "version" json with
  | Error _ as err -> err
  | Ok version -> (
      match require_json_target_field "host" json with
      | Error _ as err -> err
      | Ok host -> (
          match require_json_target_field "target" json with
          | Error _ as err -> err
          | Ok target -> (
              match require_json_string_field "artifact_target" json with
              | Error _ as err -> err
              | Ok artifact_target -> (
                  match require_json_string_field "kind" json with
                  | Error _ as err -> err
                  | Ok kind_string -> (
                      match parse_available_toolchain_kind kind_string with
                      | Error _ as err -> err
                      | Ok kind -> (
                          match require_json_string_field "artifact" json with
                          | Error _ as err -> err
                          | Ok artifact -> (
                              match require_json_string_field "artifact_url" json with
                              | Error _ as err -> err
                              | Ok artifact_url -> (
                                  match require_json_string_field "checksum_url" json with
                                  | Error _ as err -> err
                                  | Ok checksum_url -> (
                                      match optional_json_int_field "size_bytes" json with
                                      | Error _ as err -> err
                                      | Ok size_bytes -> (
                                          match optional_json_string_field "last_modified" json with
                                          | Error _ as err -> err
                                          | Ok last_modified ->
                                              Ok {
                                                version;
                                                host;
                                                target;
                                                artifact_target;
                                                kind;
                                                artifact;
                                                artifact_url;
                                                checksum_url;
                                                size_bytes;
                                                last_modified;
                                              }
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

let parse_available_toolchains_manifest = fun raw ->
  match Data.Json.from_string raw with
  | Error err -> Error ("Failed to parse toolchain manifest JSON: " ^ Data.Json.error_to_string err)
  | Ok json -> (
      match require_json_field "toolchains" json with
      | Error _ as err -> err
      | Ok toolchains_json -> (
          match Data.Json.get_array toolchains_json with
          | None -> Error "Toolchain manifest field 'toolchains' must be an array"
          | Some entries ->
              let rec loop acc = fun __tmp1 ->
                match __tmp1 with
                | [] ->
                    Ok (
                      List.reverse acc
                      |> List.sort
                        ~compare:(fun left right ->
                          let by_version = String.compare left.version right.version in
                          if by_version != Order.EQ then
                            by_version
                          else
                            let by_host =
                              String.compare
                                (Riot_model.Target.to_string left.host)
                                (Riot_model.Target.to_string right.host)
                            in
                            if by_host != Order.EQ then
                              by_host
                            else
                              let by_target =
                                String.compare
                                  (Riot_model.Target.to_string left.target)
                                  (Riot_model.Target.to_string right.target)
                              in
                              if by_target != Order.EQ then
                                by_target
                              else
                                String.compare left.artifact_target right.artifact_target)
                    )
                | entry :: rest -> (
                    match parse_available_toolchain entry with
                    | Ok toolchain -> loop (toolchain :: acc) rest
                    | Error msg -> Error msg
                  )
              in
              loop [] entries
        )
    )

let list_available_toolchains = fun () ->
  let manifest_url = ocaml_download_base_url () ^ "/manifest.json" in
  match fetch_url manifest_url with
  | Error _ as err -> err
  | Ok raw -> parse_available_toolchains_manifest raw
