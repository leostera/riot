open Std
open Riot_model

type planned_source = {
  actions: Action.t list;
  compile_source: Path.t;
  copied_sources: Path.t list;
}

let provider_path_string = fun (provider: Riot_model.Macro_provider.t) ->
  match provider.module_path with
  | [] -> "<root>"
  | module_path -> String.concat "." module_path

let qualified_macro_name = fun (provider: Riot_model.Macro_provider.t) macro_name ->
  let provider_path = provider_path_string provider in
  if String.equal provider_path "<root>" then
    macro_name ^ "!"
  else
    provider_path ^ "." ^ macro_name ^ "!"

let sort_uniq_strings = fun values ->
  List.sort_uniq String.compare values

let available_provider_paths = fun providers ->
  providers
  |> List.map provider_path_string
  |> List.filter (fun path -> not (String.equal path "<root>"))
  |> sort_uniq_strings

let provider_exported_macros = fun (provider: Riot_model.Macro_provider.t) ->
  provider.macros
  |> List.map (qualified_macro_name provider)
  |> sort_uniq_strings

let with_list_suffix = fun label values ->
  match values with
  | [] -> ""
  | _ -> "; " ^ label ^ ": " ^ String.concat ", " values

let validate_macro_invocation = fun ~providers (invocation: Macro.Parser.invocation) ->
  let qualified_name = String.concat "." invocation.callee_path in
  match List.rev invocation.callee_path with
  | [] -> Error "macro invocation is missing a callee path"
  | macro_name :: rev_module_path ->
      let module_path = List.rev rev_module_path in
      if module_path = [] then
        Error ("macro invocation must be qualified: "
        ^ qualified_name
        ^ "!"
        ^ with_list_suffix "reachable providers" (available_provider_paths providers))
      else
        let matching_providers =
          List.filter
            (fun (provider: Riot_model.Macro_provider.t) -> provider.module_path = module_path)
            providers
        in
        match matching_providers with
        | [] ->
            Error ("unsupported macro invocation: "
            ^ qualified_name
            ^ "!"
            ^ with_list_suffix "reachable providers" (available_provider_paths providers))
        | [ provider ] ->
            if List.mem macro_name provider.macros then
              Ok ()
            else
              Error ("unsupported macro invocation: "
              ^ qualified_name
              ^ "!"
              ^ with_list_suffix
                ("provider " ^ provider_path_string provider ^ " exports")
                (provider_exported_macros provider))
        | _ ->
            Error ("ambiguous qualified macro invocation: "
            ^ qualified_name
            ^ "!"
            ^ "; multiple providers export module path "
            ^ String.concat "." module_path)

let validate_macro_invocations = fun ~providers invocations ->
  let rec loop = function
    | [] -> Ok ()
    | invocation :: rest -> (
        match validate_macro_invocation ~providers invocation with
        | Ok () -> loop rest
        | Error _ as err -> err)
  in
  loop invocations

let resolve_concrete_source = fun ~(package:Package.t) path ->
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

  type runner_expansion = {
    parsed: parsed;
    workspace_root: Path.t;
    target_dir_root: Path.t;
    providers: Riot_model.Macro_provider.t list;
    provider_hash: string;
  }

  let from_source_file = fun ~(package:Package.t) path ->
    let readable_path = resolve_concrete_source ~package path in
    match Fs.read readable_path with
    | Error err -> Error ("failed to read source for macro expansion: "
    ^ Path.to_string readable_path
    ^ " ("
    ^ IO.error_message err
    ^ ")")
    | Ok source -> Ok { path; source }

  let syn_parse = fun source ->
    { source; env = Macro.Environment.create ~filename:source.path source.source }

  let macro_expand = fun ?providers parsed ->
    match Macro.Expander.expand_environment ?providers parsed.env with
    | Ok result -> Ok { parsed; result }
    | Error err -> Error ("macro expansion failed for "
    ^ Path.to_string parsed.source.path
    ^ ": "
    ^ Macro.error_message err)

  let plan_runner_expansion = fun ~workspace_root ~target_dir_root ~providers parsed ->
    match Macro.Parser.collect_invocations parsed.env with
    | Error err -> Error ("macro expansion failed for "
    ^ Path.to_string parsed.source.path
    ^ ": "
    ^ Macro.error_message err)
    | Ok [] -> Ok None
    | Ok _invocations ->
        if (Macro.Environment.parsed parsed.env).diagnostics != [] then
          Error ("macro expansion failed for " ^ Path.to_string parsed.source.path ^ ": macro expansion requires a parse-clean source file")
        else if providers = [] then
          Error ("macro expansion failed for " ^ Path.to_string parsed.source.path ^ ": explicit macro expansion requires at least one reachable macro provider")
        else
          match validate_macro_invocations ~providers _invocations with
          | Error message ->
              Error ("macro expansion failed for " ^ Path.to_string parsed.source.path ^ ": " ^ message)
          | Ok () -> (
              match Macro.Runner.validate_providers providers with
              | Error err ->
                  Error ("macro expansion failed for "
                  ^ Path.to_string parsed.source.path
                  ^ ": "
                  ^ Macro.error_message err)
              | Ok () ->
                  Ok (
                    Some {
                      parsed;
                      workspace_root;
                      target_dir_root;
                      providers;
                      provider_hash = Macro.Runner.providers_hash ~workspace_root providers;
                    }
                  )
            )

  let to_planned_source = fun expanded ->
    match expanded.result with
    | { changed=false; _ } -> {
      actions = [];
      compile_source = expanded.parsed.source.path;
      copied_sources = [ expanded.parsed.source.path ]
    }
    | { source; changed=true } -> {
      actions = [ Action.WriteFile { destination = expanded.parsed.source.path; content = source } ];
      compile_source = expanded.parsed.source.path;
      copied_sources = [ expanded.parsed.source.path ]
    }

  let runner_expansion_to_planned_source = fun expansion ->
    {
      actions =
        [ Action.RunMacroExpansion {
            source = expansion.parsed.source.path;
            destination = expansion.parsed.source.path;
            workspace_root = expansion.workspace_root;
            target_dir_root = expansion.target_dir_root;
            providers = expansion.providers;
            provider_hash = expansion.provider_hash;
          }; ];
      compile_source = expansion.parsed.source.path;
      copied_sources = [ expansion.parsed.source.path ];
    }
end

let plan_concrete_source = fun ?providers ?(macro_providers = []) ?workspace_root ?target_dir_root ~(package:Package.t) path ->
  match Stage.from_source_file ~package path with
  | Error _ as err -> err
  | Ok source ->
      let parsed = Stage.syn_parse source in
      if macro_providers = [] then
        (
          match Stage.macro_expand ?providers parsed with
          | Error _ as err -> err
          | Ok expanded -> Ok (Stage.to_planned_source expanded)
        )
      else
        (
          match (workspace_root, target_dir_root) with
          | Some workspace_root, Some target_dir_root -> (
              match Stage.plan_runner_expansion
                ~workspace_root
                ~target_dir_root
                ~providers:macro_providers
                parsed with
              | Error _ as err -> err
              | Ok (Some expansion) -> Ok (Stage.runner_expansion_to_planned_source expansion)
              | Ok None -> Ok { actions = []; compile_source = path; copied_sources = [ path ] }
            )
          | _ -> Error "macro expansion planning requires workspace_root and target_dir_root when explicit macro providers are present"
        )
