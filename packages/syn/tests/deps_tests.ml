open Std
open Syn

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create deps test source slice"

let parse_modules = fun ~env ~filename source ->
  let parse_result = Syn.parse ~filename:(Path.v filename) (source_slice source) in
  match Syn.Deps.from_parse_result ~env parse_result with
  | Ok deps -> Ok (Syn.Deps.modules deps)
  | Error (Syn.Deps.Parse_diagnostics diagnostics) ->
      Error ("parse diagnostics: "
      ^ String.concat "; " (List.map diagnostics ~fn:Syn.Diagnostic.to_string))

let alias_exports = fun names ->
  List.fold_left
    names
    ~init:Syn.Deps.Env.empty
    ~fn:(fun exports name ->
      Syn.Deps.Env.add_path exports ~path:[ name ] ~free_names:[ name ])

let alias_exports_with_super = fun ~alias_module names ->
  let exports = alias_exports names in
  Syn.Deps.Env.add_scoped_binding exports ~path:[ "Super" ] ~free_names:[ alias_module ] ~exports

let open_paths = fun paths env ->
  List.fold_left
    paths
    ~init:env
    ~fn:(fun env path -> Syn.Deps.Env.open_path env ~path)

let test_deps_collect_value_declaration_modules_from_implicit_alias_opens = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Aliases" ]
      ~free_names:[ "Kernel__Aliases" ]
      ~exports:(alias_exports [ "Result" ])
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Net"; "Aliases" ]
      ~free_names:[ "Kernel__Net__Aliases" ]
      ~exports:(alias_exports [ "Socket_addr" ])
    |> open_paths [ [ "Kernel"; "Aliases" ]; [ "Kernel"; "Net"; "Aliases" ] ]
  in
  match parse_modules
    ~env
    ~filename:"unix.mli"
    "val resolve_stream: host:string -> port:int -> (Socket_addr.t array, error) Result.t\n" with
  | Ok modules when modules = [ "Result"; "Socket_addr" ] -> Ok ()
  | Ok modules ->
      Error ("expected deps [Result, Socket_addr], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_opened_public_root_module_instead_of_child_module = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_binding
      ~path:[ "Syn" ]
      ~free_names:[ "Syn" ]
      ~exports:(alias_exports [ "Token" ])
  in
  match parse_modules
    ~env
    ~filename:"main.ml"
    "open Syn\n\nlet token_kind = fun (token: Token.t) -> token.kind\n" with
  | Ok modules when modules = [ "Syn" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Syn], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_opaque_opened_public_root_for_child_module = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_path ~path:[ "Std" ] ~free_names:[ "Std" ]
  in
  let source =
    {ocaml|open Std

let cwd = Env.current_dir ()

let value = Env.get Env.String ~var:"RIOT_ENV"
|ocaml}
  in
  match parse_modules ~env ~filename:"environment.ml" source with
  | Ok modules when modules = [ "Std" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Std], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_opaque_opened_public_root_for_qualified_child_module = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_path ~path:[ "Std" ] ~free_names:[ "Std" ]
  in
  let source =
    {ocaml|open Std

let render = fun () ->
  let buffer = IO.Buffer.create ~size:256 in
  IO.Buffer.contents buffer
|ocaml}
  in
  match parse_modules ~env ~filename:"pretext.ml" source with
  | Ok modules when modules = [ "Std" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Std], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_qualified_public_root_module_instead_of_child_module = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_binding
      ~path:[ "Syn" ]
      ~free_names:[ "Syn" ]
      ~exports:(alias_exports [ "Token" ])
  in
  match parse_modules
    ~env
    ~filename:"main.ml"
    "let token_kind = fun (token: Syn.Token.t) -> token.kind\n" with
  | Ok modules when modules = [ "Syn" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Syn], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_do_not_collect_exported_children_from_module_alias = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_binding
      ~path:[ "Kernel" ]
      ~free_names:[ "Kernel" ]
      ~exports:(alias_exports [ "Array"; "Result" ])
  in
  match parse_modules
    ~env
    ~filename:"kernel_new_addr_tests.ml"
    "module Kernel = Kernel\nlet len = Kernel.Array.length values\n" with
  | Ok modules when modules = [ "Kernel" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Kernel], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_do_not_collect_scoped_exported_children_from_module_alias = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel" ]
      ~free_names:[ "Kernel" ]
      ~exports:(alias_exports [ "Array"; "Result" ])
  in
  match parse_modules
    ~env
    ~filename:"kernel_new_addr_tests.ml"
    "module Kernel = Kernel\nlet len = Kernel.Array.length values\n" with
  | Ok modules when modules = [ "Kernel" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Kernel], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_do_not_collect_exported_children_from_facade_alias = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Syntax_tree" ]
      ~free_names:[ "Syntax_tree" ]
      ~exports:(alias_exports [ "Builder" ])
  in
  match parse_modules ~env ~filename:"syn.ml" "module SyntaxTree = Syntax_tree\n" with
  | Ok modules when modules = [ "Syntax_tree" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Syntax_tree], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_open_scoped_root_keeps_child_modules_on_opened_root = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Std" ]
      ~free_names:[ "Std" ]
      ~exports:(alias_exports [ "Array" ])
  in
  let source = {ocaml|open Std

let length = fun values -> Array.length values
|ocaml}
  in
  match parse_modules ~env ~filename:"stdin_bench.ml" source with
  | Ok modules when modules = [ "Std" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Std], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_opened_child_named_like_current_file_stays_on_opened_root = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Std" ]
      ~free_names:[ "Std" ]
      ~exports:(alias_exports [ "Config" ])
  in
  let source = {ocaml|open Std

let load = fun path -> Config.load_file path
|ocaml}
  in
  match parse_modules ~env ~filename:"config.ml" source with
  | Ok modules when modules = [ "Std" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Std], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_keep_unknown_qualified_roots_after_open = fun _ctx ->
  let source =
    {ocaml|open Prelude

type t = Regex_stubs.compiled

let compile = Regex_stubs.compile
|ocaml}
  in
  match parse_modules ~env:Syn.Deps.Env.empty ~filename:"regex.ml" source with
  | Ok modules when modules = [ "Prelude"; "Regex_stubs" ] -> Ok ()
  | Ok modules ->
      Error ("expected deps [Prelude, Regex_stubs], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_opened_module_root_for_exported_child = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_binding
      ~path:[ "Iter" ]
      ~free_names:[ "Iter" ]
      ~exports:(alias_exports [ "Iterator"; "MutIterator" ])
  in
  match parse_modules
    ~env
    ~filename:"string.mli"
    "open Iter\nval into_iter: string -> char Iterator.t\nval into_mut_iter: string -> char MutIterator.t\n" with
  | Ok modules when modules = [ "Iter" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Iter], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_known_root_for_opaque_qualified_open = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_path ~path:[ "Kernel" ] ~free_names:[ "Kernel" ]
  in
  match parse_modules
    ~env
    ~filename:"tcp_listener.ml"
    "open Kernel.Async\nlet readable = Interest.readable\n" with
  | Ok modules when modules = [ "Kernel" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Kernel], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_super_alias_child_module = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Gooey"; "Aliases" ]
      ~free_names:[ "Gooey__Aliases" ]
      ~exports:(alias_exports_with_super ~alias_module:"Gooey__Aliases" [ "Config"; "Viewport" ])
    |> open_paths [ [ "Gooey"; "Aliases" ] ]
  in
  let source =
    {ocaml|type t = {
  measure: (constraints:Super.Config.constraints -> Viewport.t);
}
|ocaml}
  in
  match parse_modules ~env ~filename:"element.mli" source with
  | Ok modules when modules = [ "Config"; "Gooey__Aliases"; "Viewport" ] -> Ok ()
  | Ok modules ->
      Error ("expected deps [Config, Gooey__Aliases, Viewport], got ["
      ^ String.concat ", " modules
      ^ "]")
  | Error err -> Error err

let test_deps_ignore_lowercase_field_access_roots = fun _ctx ->
  let source =
    {ocaml|type opts = { follow_symlinks: bool }
type iterator_state = { roots: int list; opts: opts }
let update = fun state -> { state.opts with follow_symlinks = false }
let size = fun state -> List.length state.roots
let make =
let module Base = struct
  type state = iterator_state
  let size = fun state -> List.length state.roots
end in
()
|ocaml}
  in
  match parse_modules ~env:Syn.Deps.Env.empty ~filename:"walker.ml" source with
  | Ok modules when modules = [ "List" ] -> Ok ()
  | Ok modules -> Error ("expected deps [List], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_manifest_type_modules_from_implicit_alias_opens = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Fs"; "Aliases" ]
      ~free_names:[ "Kernel__Fs__Aliases" ]
      ~exports:(alias_exports [ "File" ])
    |> open_paths [ [ "Kernel"; "Fs"; "Aliases" ] ]
  in
  match parse_modules ~env ~filename:"read_dir.mli" "type kind = File.kind =\n  | RegularFile\n" with
  | Ok modules when modules = [ "File" ] -> Ok ()
  | Ok modules -> Error ("expected deps [File], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_qualified_public_root_from_implicit_root_alias_open = fun _ctx ->
  let fs_exports =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_binding
      ~path:[ "Fs" ]
      ~free_names:[ "Fs" ]
      ~exports:(alias_exports [ "File" ])
  in
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Aliases" ]
      ~free_names:[ "Kernel__Aliases" ]
      ~exports:fs_exports
    |> open_paths [ [ "Kernel"; "Aliases" ] ]
  in
  match parse_modules ~env ~filename:"process.mli" "type error =\n  | File of Fs.File.error\n" with
  | Ok modules when modules = [ "Fs" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Fs], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_variant_payload_modules_from_implicit_alias_opens = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Aliases" ]
      ~free_names:[ "Kernel__Aliases" ]
      ~exports:(alias_exports [ "System_error" ])
    |> open_paths [ [ "Kernel"; "Aliases" ] ]
  in
  match parse_modules ~env ~filename:"unix.mli" "type error =\n  | System of System_error.t\n" with
  | Ok modules when modules = [ "System_error" ] -> Ok ()
  | Ok modules -> Error ("expected deps [System_error], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_collect_field_access_modules_from_implicit_alias_opens = fun _ctx ->
  let env =
    Syn.Deps.Env.empty
    |> Syn.Deps.Env.add_scoped_binding
      ~path:[ "Kernel"; "Async"; "Aliases" ]
      ~free_names:[ "Kernel__Async__Aliases" ]
      ~exports:(alias_exports [ "Libc" ])
    |> open_paths [ [ "Kernel"; "Async"; "Aliases" ] ]
  in
  match parse_modules
    ~env
    ~filename:"unix.ml"
    "let is_error = fun event -> event.flags land Libc.ev_error != 0\n" with
  | Ok modules when modules = [ "Libc" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Libc], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_ignore_polymorphic_variant_tags = fun _ctx ->
  let source =
    {ocaml|type t = {
  mutable active: [
    `Data
    | `Control
  ];
}
let set_data = fun t -> t.active <- `Data
|ocaml}
  in
  match parse_modules ~env:Syn.Deps.Env.empty ~filename:"tags.ml" source with
  | Ok [] -> Ok ()
  | Ok modules -> Error ("expected deps [], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_ignore_local_module_type_in_first_class_module = fun _ctx ->
  let source =
    {ocaml|module type Intf = sig
  type state
  type item
end

type ('item, 'state) iter = (module Intf with type item = 'item and type state = External.state)
|ocaml}
  in
  match parse_modules ~env:Syn.Deps.Env.empty ~filename:"iter.ml" source with
  | Ok modules when modules = [ "External" ] -> Ok ()
  | Ok modules -> Error ("expected deps [External], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_bind_first_class_module_function_parameter = fun _ctx ->
  let source =
    {ocaml|module type ConfigSpec = sig
  type t
  val spec: t
  val get: t -> string
end

let get (type a) ((module M : ConfigSpec with type t = a)) =
  M.get M.spec
|ocaml}
  in
  match parse_modules ~env:Syn.Deps.Env.empty ~filename:"config.ml" source with
  | Ok [] -> Ok ()
  | Ok modules -> Error ("expected deps [], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_bind_first_class_module_match_pattern = fun _ctx ->
  let source =
    {ocaml|module type Intf = sig
  val render: int -> string
end

type t =
  | Pack: (module Intf) * int -> t

let render = function
  | Pack ((module I), state) -> I.render state
|ocaml}
  in
  match parse_modules ~env:Syn.Deps.Env.empty ~filename:"packed.ml" source with
  | Ok [] -> Ok ()
  | Ok modules -> Error ("expected deps [], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_local_open_keeps_child_modules_on_opened_root = fun _ctx ->
  let source = {ocaml|let output = fun mod_ ->
  let open Dep_graph in
  Module.cmi mod_
|ocaml}
  in
  match parse_modules ~env:Syn.Deps.Env.empty ~filename:"action.ml" source with
  | Ok modules when modules = [ "Dep_graph"; "Module" ] -> Ok ()
  | Ok modules ->
      Error ("expected deps [Dep_graph, Module], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let test_deps_open_keeps_child_modules_on_opened_root = fun _ctx ->
  let source = {ocaml|open Std

let length = fun values -> Array.length values
|ocaml}
  in
  match parse_modules ~env:Syn.Deps.Env.empty ~filename:"stdin_bench.ml" source with
  | Ok modules when modules = [ "Array"; "Std" ] -> Ok ()
  | Ok modules -> Error ("expected deps [Array, Std], got [" ^ String.concat ", " modules ^ "]")
  | Error err -> Error err

let name = "syn-deps"

let tests =
  Test.[
    case
      "deps collect value declaration modules from implicit alias opens"
      test_deps_collect_value_declaration_modules_from_implicit_alias_opens;
    case
      "deps collect manifest type modules from implicit alias opens"
      test_deps_collect_manifest_type_modules_from_implicit_alias_opens;
    case
      "deps collect qualified public root from implicit root alias open"
      test_deps_collect_qualified_public_root_from_implicit_root_alias_open;
    case
      "deps collect opened public root module instead of child module"
      test_deps_collect_opened_public_root_module_instead_of_child_module;
    case
      "deps collect opaque opened public root for child module"
      test_deps_collect_opaque_opened_public_root_for_child_module;
    case
      "deps collect opaque opened public root for qualified child module"
      test_deps_collect_opaque_opened_public_root_for_qualified_child_module;
    case
      "deps collect qualified public root module instead of child module"
      test_deps_collect_qualified_public_root_module_instead_of_child_module;
    case
      "deps do not collect exported children from module alias"
      test_deps_do_not_collect_exported_children_from_module_alias;
    case
      "deps do not collect scoped exported children from module alias"
      test_deps_do_not_collect_scoped_exported_children_from_module_alias;
    case
      "deps do not collect exported children from facade alias"
      test_deps_do_not_collect_exported_children_from_facade_alias;
    case
      "deps open scoped root keeps child modules on opened root"
      test_deps_open_scoped_root_keeps_child_modules_on_opened_root;
    case
      "deps opened child named like current file stays on opened root"
      test_deps_opened_child_named_like_current_file_stays_on_opened_root;
    case
      "deps keep unknown qualified roots after open"
      test_deps_keep_unknown_qualified_roots_after_open;
    case
      "deps collect opened module root for exported child"
      test_deps_collect_opened_module_root_for_exported_child;
    case
      "deps collect known root for opaque qualified open"
      test_deps_collect_known_root_for_opaque_qualified_open;
    case "deps collect Super alias child module" test_deps_collect_super_alias_child_module;
    case "deps ignore lowercase field access roots" test_deps_ignore_lowercase_field_access_roots;
    case
      "deps collect variant payload modules from implicit alias opens"
      test_deps_collect_variant_payload_modules_from_implicit_alias_opens;
    case
      "deps collect field access modules from implicit alias opens"
      test_deps_collect_field_access_modules_from_implicit_alias_opens;
    case "deps ignore polymorphic variant tags" test_deps_ignore_polymorphic_variant_tags;
    case
      "deps ignore local module type in first class module"
      test_deps_ignore_local_module_type_in_first_class_module;
    case
      "deps bind first class module function parameter"
      test_deps_bind_first_class_module_function_parameter;
    case
      "deps bind first class module match pattern"
      test_deps_bind_first_class_module_match_pattern;
    case
      "deps local open keeps child modules on opened root"
      test_deps_local_open_keeps_child_modules_on_opened_root;
    case
      "deps open keeps child modules on opened root"
      test_deps_open_keeps_child_modules_on_opened_root;
  ]

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
