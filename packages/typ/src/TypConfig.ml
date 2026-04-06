open Std

type env = (string * TypeScheme.t) list

type t = {
  prelude: env;
  loaded_modules: ModuleTypings.t list;
  store: Store.t option;
  capture_traces: bool;
  ambient: env;
  ambient_type_decls: FileSummary.type_decl list;
}

let default = {
  prelude = LanguagePrelude.bindings;
  loaded_modules = BootstrapModules.summaries;
  store = None;
  capture_traces = true;
  ambient = [];
  ambient_type_decls = [];
}

let with_ambient = fun config ~ambient -> { config with ambient }

let with_ambient_type_decls = fun config ~ambient_type_decls -> { config with ambient_type_decls }

let with_loaded_modules = fun config ~loaded_modules -> { config with loaded_modules }

let with_store = fun config ~store -> { config with store }

let with_capture_traces = fun config ~capture_traces -> { config with capture_traces }
