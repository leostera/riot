open Std
open Std.Result.Syntax

let phrase_basename = fun (session: Context.session) phrase_id ->
  session.module_prefix ^ "_phrase_" ^ Std.Int.to_string phrase_id

let phrase_module = fun (session: Context.session) phrase_id ->
  Std.String.capitalize_ascii (phrase_basename session phrase_id)

let phrase_paths = fun (session: Context.session) phrase_id ->
  let phrase_base = phrase_basename session phrase_id in
  let source = Std.Path.join session.session_dir (Std.Path.v (phrase_base ^ ".ml")) in
  let cmx = Std.Path.join session.session_dir (Std.Path.v (phrase_base ^ ".cmx")) in
  let cmxs = Std.Path.join session.session_dir (Std.Path.v (phrase_base ^ ".cmxs")) in
  (source, cmx, cmxs)

let open_line = fun module_name -> "open " ^ module_name ^ ";;"

let persistent_open_module = fun phrase ->
  match Std.String.split (Std.String.trim phrase) ~by:" " with
  | [ "open"; module_name ] -> Some module_name
  | _ -> None

let unique_strings = fun values ->
  let rec loop seen acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Std.List.reverse acc
    | value :: rest ->
        if Std.List.any seen ~fn:(Std.String.equal value) then
          loop seen acc rest
        else
          loop (value :: seen) (value :: acc) rest
  in
  loop [] [] values

let ocaml_string_literal = fun value -> "\"" ^ Std.String.escaped value ^ "\""

let ocaml_string_list = fun values ->
  "[ "
  ^ Std.String.concat "; " (Std.List.map values ~fn:ocaml_string_literal)
  ^ " ]"

let cmi_only_dir = fun (session: Context.session) phrase_id ->
  Std.Path.(
    session.session_dir
    / Std.Path.v (session.module_prefix ^ "_phrase_" ^ Std.Int.to_string phrase_id ^ "_cmis"))

let is_special_include = fun path ->
  Std.String.starts_with ~prefix:"+" (Std.Path.to_string path)

let is_host_include = fun (session: Context.session) dir ->
  Std.List.any
    session.host_includes
    ~fn:(fun host_dir -> Std.String.equal (Std.Path.to_string host_dir) (Std.Path.to_string dir))

let host_interface_allowed = fun src ->
  let basename = Std.Path.basename src in
  Std.String.starts_with ~prefix:"Std" basename
  || Std.String.starts_with ~prefix:"Kernel" basename

let runtime_public_interface = fun src ->
  let basename = Std.Path.basename src in
  Std.String.equal basename "Std.cmi"
  || Std.String.starts_with ~prefix:"Std__" basename
  || Std.String.equal basename "Kernel.cmi"
  || Std.String.starts_with ~prefix:"Kernel__" basename

let interface_allowed = fun session dir path ->
  if is_host_include session dir then
    host_interface_allowed path
  else
    not (runtime_public_interface path)

let copy_interface_file = fun ~dst_dir src ->
  let dst = Std.Path.(dst_dir / Std.Path.v (Std.Path.basename src)) in
  match Std.Fs.exists dst with
  | Ok true -> Ok ()
  | Ok false ->
      Std.Fs.copy ~src ~dst
      |> Std.Result.map_err
        ~fn:(fun error ->
          "failed to stage eval interface "
          ^ Std.Path.to_string src
          ^ ": "
          ^ Std.IO.error_message error)
  | Error error ->
      Error (
        "failed to check staged eval interface "
        ^ Std.Path.to_string dst
        ^ ": "
        ^ Std.IO.error_message error
      )

let stage_interfaces_from_dir = fun session ~dst_dir dir ->
  if is_special_include dir then
    Ok ()
  else
    match Std.Fs.read_dir dir with
    | Error _ -> Ok ()
    | Ok entries ->
        entries
        |> Std.Iter.MutIterator.to_list
        |> Std.List.filter
          ~fn:(fun path ->
            Std.String.ends_with ~suffix:".cmi" (Std.Path.basename path)
            && interface_allowed session dir path)
        |> Std.List.fold_left
          ~init:(Ok ())
          ~fn:(fun acc path ->
            let* () = acc in
            copy_interface_file ~dst_dir (Std.Path.join dir path))

let compile_includes = fun (session: Context.session) phrase_id ->
  let dst_dir = cmi_only_dir session phrase_id in
  let* () =
    Std.Fs.create_dir_all dst_dir
    |> Std.Result.map_err
      ~fn:(fun error ->
        "failed to create eval interface directory: " ^ Std.IO.error_message error)
  in
  let* () =
    session.includes
    |> Std.List.fold_left
      ~init:(Ok ())
      ~fn:(fun acc dir ->
        let* () = acc in
        stage_interfaces_from_dir session ~dst_dir dir)
  in
  Ok (dst_dir, Std.List.filter session.includes ~fn:is_special_include)

let std_override_source = fun args ->
  [
    "module Eval_real_std = Std;;";
    "module Std = struct";
    "  include Eval_real_std";
    "  module Env = struct";
    "    include Eval_real_std.Env";
    "    let args = " ^ ocaml_string_list args;
    "  end";
    "end;;";
  ]

let alias_line = fun (alias: Context.package_alias) ->
  if Std.String.equal alias.public_root alias.compiled_root then
    None
  else
    Some ("module " ^ alias.public_root ^ " = " ^ alias.compiled_root ^ ";;")

let package_alias_lines = fun (session: Context.session) ->
  session.package_aliases
  |> Std.List.filter_map ~fn:alias_line

let phrase_source = fun (session: Context.session) phrase ->
  let std_lines =
    match session.args with
    | None -> package_alias_lines session @ [ "open Std;;" ]
    | Some args -> package_alias_lines session @ std_override_source args @ [ "open Std;;" ]
  in
  Std.String.concat
    "\n"
    (std_lines
    @ Std.List.map session.opened_modules ~fn:open_line
    @ Std.List.map session.loaded_modules ~fn:open_line
    @ [
      "";
      ";;";
      phrase;
      "";
    ])

let compile = fun (session: Context.session) phrase_id source_path cmx_path ->
  let* (cwd, includes) = compile_includes session phrase_id in
  let ocamlc = Riot_toolchain.ocamlc session.toolchain in
  let invocation =
    Riot_toolchain.Ocamlc.compile_impl
      ocamlc
      ~cwd
      ~includes
      ~flags:[ Riot_toolchain.Ocamlc.NoAliasDeps; Riot_toolchain.Ocamlc.Raw "-opaque" ]
      ~output:cmx_path
      source_path
  in
  match Riot_toolchain.Ocamlc.run invocation with
  | Riot_toolchain.Ocamlc.Success _ -> Ok ()
  | Riot_toolchain.Ocamlc.Failed _ as result ->
      Error ("phrase " ^ Std.Int.to_string phrase_id ^ " failed to compile:\n" ^ Compile.compiler_output result)

let link = fun session phrase_id cmx_path cmxs_path ->
  match Compile.link_shared_library session ~output:cmxs_path ~libs:[] [ cmx_path ] with
  | Riot_toolchain.Ocamlc.Success _ -> Ok ()
  | Riot_toolchain.Ocamlc.Failed _ as result ->
      Error ("phrase " ^ Std.Int.to_string phrase_id ^ " failed to link:\n" ^ Compile.compiler_output result)

let load = fun phrase_id cmxs_path ->
  match Loader.load_cmxs cmxs_path with
  | Ok `Loaded -> Ok ()
  | Ok (`Skipped message) ->
      Error ("phrase " ^ Std.Int.to_string phrase_id ^ " was already loaded:\n" ^ message)
  | Error message ->
      Error ("phrase " ^ Std.Int.to_string phrase_id ^ " failed to load:\n" ^ message)

let eval = fun (session: Context.session) phrase ->
  let phrase_id = session.next_phrase_id in
  let module_name = phrase_module session phrase_id in
  let (source_path, cmx_path, cmxs_path) = phrase_paths session phrase_id in
  let source = phrase_source session phrase in
  let* () =
    Std.Fs.write source source_path
    |> Std.Result.map_err ~fn:(Context.fs_error_message "failed to write eval phrase")
  in
  let* () = compile session phrase_id source_path cmx_path in
  let* () = link session phrase_id cmx_path cmxs_path in
  let* () = load phrase_id cmxs_path in
  session.next_phrase_id <- phrase_id + 1;
  (
    match persistent_open_module phrase with
    | Some opened_module ->
        session.opened_modules <- unique_strings (session.opened_modules @ [ opened_module ])
    | None -> session.loaded_modules <- session.loaded_modules @ [ module_name ]
  );
  Ok ()
