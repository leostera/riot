open Std

type macro_fn = Macro_token_stream.t -> Macro_result.t

type exported_macro = {
  name: string;
  expand: macro_fn;
}

type t = {
  module_path: string list;
  macros: exported_macro list;
}

let fn = fun name expand -> { name; expand }

let v = fun ~module_path macros -> { module_path; macros }

let module_path = fun provider -> provider.module_path

let macros = fun provider -> provider.macros

let find_macro = fun provider name ->
  List.find_opt
    (fun macro_ ->
      String.equal macro_.name name)
    provider.macros
