open Std
open Std.Collections
open Riot_model

type t =
  | CompileInterface of {
      source: Path.t;
      outputs: Path.t list;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileImplementation of {
      source: Path.t;
      outputs: Path.t list;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | GenerateInterface of {
      source: Path.t;
      outputs: Path.t list;
      includes: Path.t list;
      flags: Riot_toolchain.Ocamlc.compiler_flag list;
    }
  | CompileC of {
      source: Path.t;
      outputs: Path.t list;
      ccflags: string list;
    }
  | CreateLibrary of {
      outputs: Path.t list;
      objects: Path.t list;
      includes: Path.t list;
    }
  | CreateExecutable of {
      outputs: Path.t list;
      objects: Path.t list;
      libraries: Path.t list;
      includes: Path.t list;
      cclibs: Path.t list;
      (* Foreign C/Rust libraries to link with -cclib *)
      ccopt_flags: string list;
      (* cc_flags from riot.toml → passed with -ccopt *)
      cclib_flags: string list;
      (* ld_flags from riot.toml → passed with -cclib *)
    }
  | CreateSharedLibrary of {
      outputs: Path.t list;
      objects: Path.t list;
      libraries: Path.t list;
      includes: Path.t list;
      cclibs: Path.t list;
      ccopt_flags: string list;
      cclib_flags: string list;
    }
  | CopyFile of {
      source: Path.t;
      destination: Path.t;
    }
  | WriteFile of {
      destination: Path.t;
      content: string;
    }
  | BuildForeignDependency of {
      name: string;
      path: Path.t;
      build_cmd: string list;
      outputs: Path.t list;
      env: (string * string) list;
    }

(**
   Compute a deterministic content-based hash of an action.

   This hash is used for caching and must be deterministic regardless of:
   - Order of includes in the list
   - Order of outputs in the list
   - Order of compile-only flags in the list

   Link actions preserve object/library flag order because OCaml link order is
   semantically significant.
*)
let hash = fun action ->
  let open Crypto in
  let hasher = Sha256.create () in
  (* Helper to write a sorted list of paths *)
  let write_sorted_paths hasher paths =
    let sorted =
      List.sort paths ~compare:(fun a b -> String.compare (Path.to_string a) (Path.to_string b))
    in
    List.for_each sorted ~fn:(fun path -> Sha256.write hasher (Path.to_string path))
  in
  let write_paths_in_order hasher paths =
    List.for_each paths ~fn:(fun path -> Sha256.write hasher (Path.to_string path))
  in
  (* Helper to write sorted flags - use flags_to_string which returns string list *)
  let write_sorted_flags hasher flags =
    let flag_strings = Riot_toolchain.Ocamlc.flags_to_string flags in
    let sorted = List.sort flag_strings ~compare:String.compare in
    List.for_each sorted ~fn:(fun s -> Sha256.write hasher s)
  in
  let write_strings_in_order hasher values =
    List.for_each values ~fn:(fun value -> Sha256.write hasher value)
  in
  match action with
  | CompileInterface {
      source;
      outputs;
      includes;
      flags;
    } ->
      Sha256.write hasher "CompileInterface";
      Sha256.write hasher (Path.to_string source);
      write_sorted_paths hasher outputs;
      write_sorted_paths hasher includes;
      write_sorted_flags hasher flags;
      Sha256.finish hasher
  | CompileImplementation {
      source;
      outputs;
      includes;
      flags;
    } ->
      Sha256.write hasher "CompileImplementation:v2";
      Sha256.write hasher (Path.to_string source);
      write_sorted_paths hasher outputs;
      write_sorted_paths hasher includes;
      write_sorted_flags hasher flags;
      Sha256.finish hasher
  | GenerateInterface {
      source;
      outputs;
      includes;
      flags;
    } ->
      Sha256.write hasher "GenerateInterface";
      Sha256.write hasher (Path.to_string source);
      write_sorted_paths hasher outputs;
      write_sorted_paths hasher includes;
      write_sorted_flags hasher flags;
      Sha256.finish hasher
  | CompileC { source; outputs; ccflags } ->
      Sha256.write hasher "CompileC";
      Sha256.write hasher (Path.to_string source);
      write_sorted_paths hasher outputs;
      List.for_each ccflags ~fn:(Sha256.write hasher);
      Sha256.finish hasher
  | CreateLibrary { outputs; objects; includes } ->
      Sha256.write hasher "CreateLibrary";
      write_sorted_paths hasher outputs;
      write_paths_in_order hasher objects;
      write_sorted_paths hasher includes;
      Sha256.finish hasher
  | CreateExecutable {
      outputs;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      Sha256.write hasher "CreateExecutable";
      write_sorted_paths hasher outputs;
      write_paths_in_order hasher objects;
      write_paths_in_order hasher libraries;
      write_sorted_paths hasher includes;
      write_paths_in_order hasher cclibs;
      write_strings_in_order hasher ccopt_flags;
      write_strings_in_order hasher cclib_flags;
      Sha256.finish hasher
  | CreateSharedLibrary {
      outputs;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      Sha256.write hasher "CreateSharedLibrary";
      write_sorted_paths hasher outputs;
      write_paths_in_order hasher objects;
      write_paths_in_order hasher libraries;
      write_sorted_paths hasher includes;
      write_paths_in_order hasher cclibs;
      write_strings_in_order hasher ccopt_flags;
      write_strings_in_order hasher cclib_flags;
      Sha256.finish hasher
  | CopyFile { source; destination } ->
      Sha256.write hasher "CopyFile";
      Sha256.write hasher (Path.to_string source);
      Sha256.write hasher (Path.to_string destination);
      Sha256.finish hasher
  | WriteFile { destination; content } ->
      Sha256.write hasher "WriteFile";
      Sha256.write hasher (Path.to_string destination);
      Sha256.write hasher content;
      Sha256.finish hasher
  | BuildForeignDependency {
      name;
      path;
      build_cmd;
      outputs;
      env;
    } ->
      Sha256.write hasher "BuildForeignDependency";
      Sha256.write hasher name;
      Sha256.write hasher (Path.to_string path);
      let sorted_cmd = List.sort build_cmd ~compare:String.compare in
      List.for_each sorted_cmd ~fn:(Sha256.write hasher);
      write_sorted_paths hasher outputs;
      let sorted_env = List.sort env ~compare:(fun (k1, _) (k2, _) -> String.compare k1 k2) in
      List.for_each sorted_env ~fn:(fun (k, v) -> Sha256.write hasher (k ^ "=" ^ v));
      Sha256.finish hasher

let to_string = fun __tmp1 ->
  match __tmp1 with
  | CompileInterface {
      source;
      outputs;
      includes;
      flags;
    } ->
      "CompileInterface("
      ^ Path.to_string source
      ^ "->"
      ^ String.concat "," (List.map outputs ~fn:Path.to_string)
      ^ ",includes="
      ^ String.concat "," (List.map includes ~fn:Path.to_string)
      ^ ",flags="
      ^ String.concat " " (Riot_toolchain.Ocamlc.flags_to_string flags)
      ^ ")"
  | CompileImplementation {
      source;
      outputs;
      includes;
      flags;
    } ->
      "CompileImplementation("
      ^ Path.to_string source
      ^ "->"
      ^ String.concat "," (List.map outputs ~fn:Path.to_string)
      ^ ",includes="
      ^ String.concat "," (List.map includes ~fn:Path.to_string)
      ^ ",flags="
      ^ String.concat " " (Riot_toolchain.Ocamlc.flags_to_string flags)
      ^ ")"
  | GenerateInterface {
      source;
      outputs;
      includes;
      flags;
    } ->
      "GenerateInterface("
      ^ Path.to_string source
      ^ "->"
      ^ String.concat "," (List.map outputs ~fn:Path.to_string)
      ^ ",includes="
      ^ String.concat "," (List.map includes ~fn:Path.to_string)
      ^ ",flags="
      ^ String.concat " " (Riot_toolchain.Ocamlc.flags_to_string flags)
      ^ ")"
  | CompileC { source; outputs; ccflags } ->
      "CompileC("
      ^ Path.to_string source
      ^ "->"
      ^ String.concat "," (List.map outputs ~fn:Path.to_string)
      ^ ",ccflags="
      ^ String.concat " " ccflags
      ^ ")"
  | CreateLibrary { outputs; objects; includes } ->
      "CreateLibrary("
      ^ String.concat "," (List.map outputs ~fn:Path.to_string)
      ^ ",objects="
      ^ String.concat "," (List.map objects ~fn:Path.to_string)
      ^ ",includes="
      ^ String.concat "," (List.map includes ~fn:Path.to_string)
      ^ ")"
  | CreateExecutable {
      outputs;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      "CreateExecutable("
      ^ String.concat "," (List.map outputs ~fn:Path.to_string)
      ^ ",objects="
      ^ String.concat "," (List.map objects ~fn:Path.to_string)
      ^ ",libraries="
      ^ String.concat "," (List.map libraries ~fn:Path.to_string)
      ^ ",includes="
      ^ String.concat "," (List.map includes ~fn:Path.to_string)
      ^ ",cclibs="
      ^ String.concat "," (List.map cclibs ~fn:Path.to_string)
      ^ ",ccopt_flags="
      ^ String.concat " " ccopt_flags
      ^ ",cclib_flags="
      ^ String.concat " " cclib_flags
      ^ ")"
  | CreateSharedLibrary {
      outputs;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      "CreateSharedLibrary("
      ^ String.concat "," (List.map outputs ~fn:Path.to_string)
      ^ ",objects="
      ^ String.concat "," (List.map objects ~fn:Path.to_string)
      ^ ",libraries="
      ^ String.concat "," (List.map libraries ~fn:Path.to_string)
      ^ ",includes="
      ^ String.concat "," (List.map includes ~fn:Path.to_string)
      ^ ",cclibs="
      ^ String.concat "," (List.map cclibs ~fn:Path.to_string)
      ^ ",ccopt_flags="
      ^ String.concat " " ccopt_flags
      ^ ",cclib_flags="
      ^ String.concat " " cclib_flags
      ^ ")"
  | CopyFile { source; destination } ->
      "CopyFile(" ^ Path.to_string source ^ "->" ^ Path.to_string destination ^ ")"
  | WriteFile { destination; content } ->
      "WriteFile("
      ^ Path.to_string destination
      ^ ","
      ^ Int.to_string (String.length content)
      ^ " bytes)"
  | BuildForeignDependency {
      name;
      path;
      build_cmd;
      outputs;
      _;
    } ->
      "BuildForeignDependency("
      ^ name
      ^ ",path="
      ^ Path.to_string path
      ^ ",cmd="
      ^ String.concat " " build_cmd
      ^ ",outputs="
      ^ String.concat "," (List.map outputs ~fn:Path.to_string)
      ^ ")"

let to_json = fun action ->
  let open Data.Json in
  match action with
  | CompileInterface {
      source;
      outputs;
      includes;
      flags;
    } ->
      obj
        [
          ("type", string "CompileInterface");
          ("source", string (Path.to_string source));
          ("outputs", array (List.map outputs ~fn:(fun p -> string (Path.to_string p))));
          ("includes", array (List.map includes ~fn:(fun p -> string (Path.to_string p))));
          ("flags", array (List.map (Riot_toolchain.Ocamlc.flags_to_string flags) ~fn:string));
        ]
  | CompileImplementation {
      source;
      outputs;
      includes;
      flags;
    } ->
      obj
        [
          ("type", string "CompileImplementation");
          ("source", string (Path.to_string source));
          ("outputs", array (List.map outputs ~fn:(fun p -> string (Path.to_string p))));
          ("includes", array (List.map includes ~fn:(fun p -> string (Path.to_string p))));
          ("flags", array (List.map (Riot_toolchain.Ocamlc.flags_to_string flags) ~fn:string));
        ]
  | GenerateInterface {
      source;
      outputs;
      includes;
      flags;
    } ->
      obj
        [
          ("type", string "GenerateInterface");
          ("source", string (Path.to_string source));
          ("outputs", array (List.map outputs ~fn:(fun p -> string (Path.to_string p))));
          ("includes", array (List.map includes ~fn:(fun p -> string (Path.to_string p))));
          ("flags", array (List.map (Riot_toolchain.Ocamlc.flags_to_string flags) ~fn:string));
        ]
  | CompileC { source; outputs; ccflags } ->
      obj
        [
          ("type", string "CompileC");
          ("source", string (Path.to_string source));
          ("outputs", array (List.map outputs ~fn:(fun p -> string (Path.to_string p))));
          ("ccflags", array (List.map ccflags ~fn:string));
        ]
  | CreateLibrary { outputs; objects; includes } ->
      obj
        [
          ("type", string "CreateLibrary");
          ("outputs", array (List.map outputs ~fn:(fun p -> string (Path.to_string p))));
          ("objects", array (List.map objects ~fn:(fun p -> string (Path.to_string p))));
          ("includes", array (List.map includes ~fn:(fun p -> string (Path.to_string p))));
        ]
  | CreateExecutable {
      outputs;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      obj
        [
          ("outputs", array (List.map outputs ~fn:(fun p -> string (Path.to_string p))));
          ("type", string "CreateExecutable");
          ("objects", array (List.map objects ~fn:(fun p -> string (Path.to_string p))));
          ("libraries", array (List.map libraries ~fn:(fun p -> string (Path.to_string p))));
          ("includes", array (List.map includes ~fn:(fun p -> string (Path.to_string p))));
          ("cclibs", array (List.map cclibs ~fn:(fun p -> string (Path.to_string p))));
          ("ccopt_flags", array (List.map ccopt_flags ~fn:string));
          ("cclib_flags", array (List.map cclib_flags ~fn:string));
        ]
  | CreateSharedLibrary {
      outputs;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      obj
        [
          ("outputs", array (List.map outputs ~fn:(fun p -> string (Path.to_string p))));
          ("type", string "CreateSharedLibrary");
          ("objects", array (List.map objects ~fn:(fun p -> string (Path.to_string p))));
          ("libraries", array (List.map libraries ~fn:(fun p -> string (Path.to_string p))));
          ("includes", array (List.map includes ~fn:(fun p -> string (Path.to_string p))));
          ("cclibs", array (List.map cclibs ~fn:(fun p -> string (Path.to_string p))));
          ("ccopt_flags", array (List.map ccopt_flags ~fn:string));
          ("cclib_flags", array (List.map cclib_flags ~fn:string));
        ]
  | CopyFile { source; destination } ->
      obj
        [
          ("type", string "CopyFile");
          ("source", string (Path.to_string source));
          ("destination", string (Path.to_string destination));
        ]
  | WriteFile { destination; content } ->
      obj
        [
          ("type", string "WriteFile");
          ("destination", string (Path.to_string destination));
          ("content", string content);
        ]
  | BuildForeignDependency {
      name;
      path;
      build_cmd;
      outputs;
      env;
    } ->
      obj
        [
          ("type", string "BuildForeignDependency");
          ("name", string name);
          ("path", string (Path.to_string path));
          ("build_cmd", array (List.map build_cmd ~fn:string));
          ("outputs", array (List.map outputs ~fn:(fun p -> string (Path.to_string p))));
          ("env", obj (List.map env ~fn:(fun (k, v) -> (k, string v))));
        ]

let from_json = fun json ->
  let open Data.Json in
  let parse_path_list field json =
    match get_field field json with
    | Some (Array arr) ->
        Some (
          List.filter_map
            arr
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | String s -> Some (Path.v s)
              | _ -> None)
        )
    | _ -> None
  in
  let parse_string_list field json =
    match get_field field json with
    | Some (Array arr) ->
        Some (
          List.filter_map
            arr
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | String s -> Some s
              | _ -> None)
        )
    | _ -> None
  in
  let parse_flags json =
    match parse_string_list "flags" json with
    | Some raw -> Some (Riot_toolchain.Ocamlc.flags_of_string raw)
    | None -> Some []
  in
  match get_field "type" json with
  | None -> Error "Missing type field"
  | Some (String "CompileInterface") -> (
      match (
        get_field "source" json,
        parse_path_list "outputs" json,
        parse_path_list "includes" json,
        parse_flags json
      ) with
      | (Some (String src), Some outs, Some includes, Some flags) ->
          Ok (
            CompileInterface {
              source = Path.v src;
              outputs = outs;
              includes;
              flags;
            }
          )
      | _ -> Error "Invalid CompileInterface"
    )
  | Some (String "CompileImplementation") -> (
      match (
        get_field "source" json,
        parse_path_list "outputs" json,
        parse_path_list "includes" json,
        parse_flags json
      ) with
      | (Some (String src), Some outs, Some includes, Some flags) ->
          Ok (
            CompileImplementation {
              source = Path.v src;
              outputs = outs;
              includes;
              flags;
            }
          )
      | _ -> Error "Invalid CompileImplementation"
    )
  | Some (String "GenerateInterface") -> (
      match (
        get_field "source" json,
        parse_path_list "outputs" json,
        parse_path_list "includes" json,
        parse_flags json
      ) with
      | (Some (String src), Some outs, Some includes, Some flags) ->
          Ok (
            GenerateInterface {
              source = Path.v src;
              outputs = outs;
              includes;
              flags;
            }
          )
      | _ -> Error "Invalid GenerateInterface"
    )
  | Some (String "CompileC") -> (
      match (get_field "source" json, parse_path_list "outputs" json) with
      | (Some (String src), Some outs) ->
          let ccflags =
            match parse_string_list "ccflags" json with
            | Some flags -> flags
            | None -> []
          in
          Ok (CompileC { source = Path.v src; outputs = outs; ccflags })
      | _ -> Error "Invalid CompileC"
    )
  | Some (String "CreateLibrary") -> (
      match (
        parse_path_list "outputs" json,
        parse_path_list "objects" json,
        parse_path_list "includes" json
      ) with
      | (Some outs, Some objects, Some includes) ->
          Ok (CreateLibrary { outputs = outs; objects; includes })
      | _ -> Error "Invalid CreateLibrary"
    )
  | Some (String "CreateExecutable") -> (
      match (
        parse_path_list "outputs" json,
        parse_path_list "objects" json,
        parse_path_list "libraries" json,
        parse_path_list "includes" json,
        parse_path_list "cclibs" json,
        parse_string_list "ccopt_flags" json,
        parse_string_list "cclib_flags" json
      ) with
      | (
          Some outs,
          Some objects,
          Some libraries,
          Some includes,
          Some cclibs,
          Some ccopt_flags,
          Some cclib_flags
        ) ->
          Ok (
            CreateExecutable {
              outputs = outs;
              objects;
              libraries;
              includes;
              cclibs;
              ccopt_flags;
              cclib_flags;
            }
          )
      | _ -> Error "Invalid CreateExecutable"
    )
  | Some (String "CreateSharedLibrary") -> (
      match (
        parse_path_list "outputs" json,
        parse_path_list "objects" json,
        parse_path_list "libraries" json,
        parse_path_list "includes" json,
        parse_path_list "cclibs" json,
        parse_string_list "ccopt_flags" json,
        parse_string_list "cclib_flags" json
      ) with
      | (
          Some outs,
          Some objects,
          Some libraries,
          Some includes,
          Some cclibs,
          Some ccopt_flags,
          Some cclib_flags
        ) ->
          Ok (
            CreateSharedLibrary {
              outputs = outs;
              objects;
              libraries;
              includes;
              cclibs;
              ccopt_flags;
              cclib_flags;
            }
          )
      | _ -> Error "Invalid CreateSharedLibrary"
    )
  | Some (String "CopyFile") -> (
      match (get_field "source" json, get_field "destination" json) with
      | (Some (String src), Some (String dst)) ->
          Ok (CopyFile { source = Path.v src; destination = Path.v dst })
      | _ -> Error "Invalid CopyFile"
    )
  | Some (String "WriteFile") -> (
      match (get_field "destination" json, get_field "content" json) with
      | (Some (String dst), Some (String content)) ->
          Ok (WriteFile { destination = Path.v dst; content })
      | _ -> Error "Invalid WriteFile"
    )
  | Some (String "BuildForeignDependency") -> (
      let parse_build_cmd json =
        match get_field "build_cmd" json with
        | Some (Array arr) ->
            Some (
              List.filter_map
                arr
                ~fn:(fun __tmp1 ->
                  match __tmp1 with
                  | String s -> Some s
                  | _ -> None)
            )
        | _ -> None
      in
      let parse_env json =
        match get_field "env" json with
        | Some (Object fields) ->
            List.filter_map
              fields
              ~fn:(fun (k, v) ->
                match v with
                | String s -> Some (k, s)
                | _ -> None)
        | _ -> []
      in
      match (
        get_field "name" json,
        get_field "path" json,
        parse_build_cmd json,
        parse_path_list "outputs" json
      ) with
      | (Some (String name), Some (String path), Some build_cmd, Some outs) ->
          let env = parse_env json in
          Ok (
            BuildForeignDependency {
              name;
              path = Path.v path;
              build_cmd;
              outputs = outs;
              env;
            }
          )
      | _ -> Error "Invalid BuildForeignDependency"
    )
  | Some (String _) -> Error "Unknown action type"
  | _ -> Error "type must be string"

let equal = fun a1 a2 ->
  let list_all2 left right ~fn =
    List.compare_lengths ~left ~right = 0
    && List.all (List.zip left right) ~fn:(fun (left, right) -> fn left right)
  in
  match (a1, a2) with
  | (CompileInterface r1, CompileInterface r2) ->
      Path.equal r1.source r2.source
      && list_all2 r1.outputs r2.outputs ~fn:Path.equal
      && list_all2 r1.includes r2.includes ~fn:Path.equal
      && r1.flags = r2.flags
  | (CompileImplementation r1, CompileImplementation r2) ->
      Path.equal r1.source r2.source
      && list_all2 r1.outputs r2.outputs ~fn:Path.equal
      && list_all2 r1.includes r2.includes ~fn:Path.equal
      && r1.flags = r2.flags
  | (GenerateInterface r1, GenerateInterface r2) ->
      Path.equal r1.source r2.source
      && list_all2 r1.outputs r2.outputs ~fn:Path.equal
      && list_all2 r1.includes r2.includes ~fn:Path.equal
      && r1.flags = r2.flags
  | (CompileC r1, CompileC r2) ->
      Path.equal r1.source r2.source && list_all2 r1.outputs r2.outputs ~fn:Path.equal
  | (CreateLibrary r1, CreateLibrary r2) ->
      list_all2 r1.outputs r2.outputs ~fn:Path.equal
      && list_all2 r1.objects r2.objects ~fn:Path.equal
      && list_all2 r1.includes r2.includes ~fn:Path.equal
  | (CreateExecutable r1, CreateExecutable r2) ->
      list_all2 r1.outputs r2.outputs ~fn:Path.equal
      && list_all2 r1.objects r2.objects ~fn:Path.equal
      && list_all2 r1.libraries r2.libraries ~fn:Path.equal
      && list_all2 r1.includes r2.includes ~fn:Path.equal
      && list_all2 r1.cclibs r2.cclibs ~fn:Path.equal
      && list_all2 r1.ccopt_flags r2.ccopt_flags ~fn:String.equal
      && list_all2 r1.cclib_flags r2.cclib_flags ~fn:String.equal
  | (CreateSharedLibrary r1, CreateSharedLibrary r2) ->
      list_all2 r1.outputs r2.outputs ~fn:Path.equal
      && list_all2 r1.objects r2.objects ~fn:Path.equal
      && list_all2 r1.libraries r2.libraries ~fn:Path.equal
      && list_all2 r1.includes r2.includes ~fn:Path.equal
      && list_all2 r1.cclibs r2.cclibs ~fn:Path.equal
      && list_all2 r1.ccopt_flags r2.ccopt_flags ~fn:String.equal
      && list_all2 r1.cclib_flags r2.cclib_flags ~fn:String.equal
  | (CopyFile r1, CopyFile r2) ->
      Path.equal r1.source r2.source && Path.equal r1.destination r2.destination
  | (WriteFile r1, WriteFile r2) ->
      Path.equal r1.destination r2.destination && String.equal r1.content r2.content
  | (BuildForeignDependency r1, BuildForeignDependency r2) ->
      r1.name = r2.name
      && Path.equal r1.path r2.path
      && r1.build_cmd = r2.build_cmd
      && list_all2 r1.outputs r2.outputs ~fn:Path.equal
      && r1.env = r2.env
  | _ -> false

let outputs = fun __tmp1 ->
  match __tmp1 with
  | CompileInterface { outputs; _ } -> outputs
  | CompileImplementation { outputs; _ } -> outputs
  | GenerateInterface { outputs; _ } -> outputs
  | CompileC { outputs; _ } -> outputs
  | CreateLibrary { outputs; _ } -> outputs
  | CreateExecutable { outputs; _ } -> outputs
  | CreateSharedLibrary { outputs; _ } -> outputs
  | CopyFile { destination; _ } -> [ destination ]
  | WriteFile { destination; _ } -> [ destination ]
  | BuildForeignDependency { outputs; _ } -> outputs

let kind = fun __tmp1 ->
  match __tmp1 with
  | CompileInterface _ -> "CompileInterface"
  | CompileImplementation _ -> "CompileImplementation"
  | GenerateInterface _ -> "GenerateInterface"
  | CompileC _ -> "CompileC"
  | CreateLibrary _ -> "CreateLibrary"
  | CreateExecutable _ -> "CreateExecutable"
  | CreateSharedLibrary _ -> "CreateSharedLibrary"
  | CopyFile _ -> "CopyFile"
  | WriteFile _ -> "WriteFile"
  | BuildForeignDependency _ -> "BuildForeignDependency"
