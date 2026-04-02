open Std

let builtin_providers = fun () -> [ Macro_format.provider (); ]

let module_path_string = fun provider ->
  match Macro_provider.module_path provider with
  | [] -> "<root>"
  | module_path -> String.concat "." module_path

let qualified_macro_name = fun provider macro_name ->
  let provider_path = module_path_string provider in
  if String.equal provider_path "<root>" then
    macro_name ^ "!"
  else
    provider_path ^ "." ^ macro_name ^ "!"

let sort_uniq_strings = fun values ->
  List.sort_uniq String.compare values

let available_provider_paths = fun providers ->
  providers
  |> List.map module_path_string
  |> List.filter (fun path -> not (String.equal path "<root>"))
  |> sort_uniq_strings

let available_qualified_macros = fun providers ->
  providers
  |> List.concat_map (fun provider ->
    Macro_provider.macros provider
    |> List.map (fun (macro_: Macro_provider.exported_macro) -> qualified_macro_name provider macro_.name))
  |> sort_uniq_strings

let provider_exported_macros = fun provider ->
  Macro_provider.macros provider
  |> List.map (fun (macro_: Macro_provider.exported_macro) -> qualified_macro_name provider macro_.name)
  |> sort_uniq_strings

let with_list_suffix = fun label values ->
  match values with
  | [] -> ""
  | _ -> "; " ^ label ^ ": " ^ String.concat ", " values

let resolve = fun ?providers invocation ->
  let explicit_providers, providers =
    match providers with
    | Some providers -> (true, providers)
    | None -> (false, builtin_providers ())
  in
  let callee_path = invocation.Macro_parser.callee_path in
  let qualified_name = String.concat "." callee_path in
  match List.rev callee_path with
  | [] -> Error (Macro_error.make ~span:invocation.span "macro invocation is missing a callee path")
  | macro_name :: rev_module_path ->
      let module_path = List.rev rev_module_path in
      if module_path = [] then
        if explicit_providers then
          Error (Macro_error.make
            ~span:invocation.span
            ("macro invocation must be qualified: "
            ^ qualified_name
            ^ "!"
            ^ with_list_suffix "reachable providers" (available_provider_paths providers)))
        else
          let matches = providers
          |> List.filter_map
            (fun provider ->
              Macro_provider.find_macro provider macro_name
              |> Option.map (fun macro_ -> (provider, macro_))) in
          (
            match matches with
            | [] ->
                Error (Macro_error.make
                  ~span:invocation.span
                  ("unsupported macro invocation: "
                  ^ qualified_name
                  ^ "!"
                  ^ with_list_suffix "reachable qualified macros" (available_qualified_macros providers)))
            | [ (_, macro_) ] -> Ok macro_
            | _ ->
                let matching_qualified_macros =
                  matches
                  |> List.map
                    (fun (provider, (macro_: Macro_provider.exported_macro)) ->
                      qualified_macro_name provider macro_.name)
                  |> sort_uniq_strings
                in
                Error (Macro_error.make
                  ~span:invocation.span
                  ("ambiguous bare macro invocation: "
                  ^ qualified_name
                  ^ "!"
                  ^ with_list_suffix "matching qualified macros" matching_qualified_macros))
          )
      else
        let matching_providers =
          List.filter
            (fun provider -> Macro_provider.module_path provider = module_path)
            providers
        in
        match matching_providers with
        | [] ->
            Error (Macro_error.make
              ~span:invocation.span
              ("unsupported macro invocation: "
              ^ qualified_name
              ^ "!"
              ^ with_list_suffix "reachable providers" (available_provider_paths providers)))
        | [ provider ] -> (
            match Macro_provider.find_macro provider macro_name with
            | Some macro_ -> Ok macro_
            | None ->
                Error (Macro_error.make
                  ~span:invocation.span
                  ("unsupported macro invocation: "
                  ^ qualified_name
                  ^ "!"
                  ^ with_list_suffix
                    ("provider " ^ module_path_string provider ^ " exports")
                    (provider_exported_macros provider)))
          )
        | _ ->
            Error (Macro_error.make
              ~span:invocation.span
              ("ambiguous qualified macro invocation: "
              ^ qualified_name
              ^ "!"
              ^ "; multiple providers export module path "
              ^ String.concat "." module_path))
