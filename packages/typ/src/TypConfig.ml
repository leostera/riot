open Std

type env = (string * TypeScheme.t) list

type t = {
  prelude: env;
  loaded_modules: ModuleTypings.t list;
  ambient: env;
  ambient_type_decls: FileSummary.type_decl list;
}

let default = {
  prelude = LanguagePrelude.bindings;
  loaded_modules = BootstrapModules.summaries;
  ambient = [];
  ambient_type_decls = [];
}

let with_ambient = fun config ~ambient -> { config with ambient }

let with_ambient_type_decls = fun config ~ambient_type_decls -> { config with ambient_type_decls }

let with_loaded_modules = fun config ~loaded_modules -> { config with loaded_modules }
