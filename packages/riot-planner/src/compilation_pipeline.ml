open Std
open Riot_model

type planned_source = {
  actions: Action.t list;
  compile_source: Path.t;
  copied_sources: Path.t list;
}

let resolve_concrete_source = fun ~(package: Package.t) path ->
  if Path.is_absolute path then
    path
  else
    Path.join package.path path

module Stage = struct
  type source = {
    path: Path.t;
    source: string;
  }

  type parsed = {
    source: source;
    env: Macro.Environment.t;
  }

  type expanded = {
    parsed: parsed;
    result: Macro.expansion;
  }

  let from_source_file = fun ~(package: Package.t) path ->
    let readable_path = resolve_concrete_source ~package path in
    match Fs.read readable_path with
    | Error err ->
        Error
          ("failed to read source for macro expansion: "
          ^ Path.to_string readable_path
          ^ " ("
          ^ IO.error_message err
          ^ ")")
    | Ok source -> Ok { path; source }

  let syn_parse = fun source ->
    {
      source;
      env = Macro.Environment.create ~filename:source.path source.source;
    }

  let macro_expand = fun ?providers parsed ->
    match Macro.Expander.expand_environment ?providers parsed.env with
    | Ok result -> Ok { parsed; result }
    | Error err ->
        Error
          ("macro expansion failed for "
          ^ Path.to_string parsed.source.path
          ^ ": "
          ^ Macro.error_message err)

  let to_planned_source = fun expanded ->
    match expanded.result with
    | { changed = false; _ } ->
        {
          actions = [];
          compile_source = expanded.parsed.source.path;
          copied_sources = [ expanded.parsed.source.path ];
        }
    | { source; changed = true } ->
        {
          actions = [ Action.WriteFile { destination = expanded.parsed.source.path; content = source } ];
          compile_source = expanded.parsed.source.path;
          copied_sources = [ expanded.parsed.source.path ];
        }
end

let plan_concrete_source = fun ?providers ~(package: Package.t) path ->
  match Stage.from_source_file ~package path with
  | Error _ as err -> err
  | Ok source ->
      let parsed = Stage.syn_parse source in
      (
        match Stage.macro_expand ?providers parsed with
        | Error _ as err -> err
        | Ok expanded -> Ok (Stage.to_planned_source expanded)
      )
