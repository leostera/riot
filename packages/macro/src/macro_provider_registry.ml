open Std

let builtin_providers = fun () ->
  [
    Macro_format.provider ();
  ]

let resolve = fun ?providers invocation ->
  let explicit_providers, providers =
    match providers with
    | Some providers -> (true, providers)
    | None -> (false, builtin_providers ())
  in
  let callee_path = invocation.Macro_parser.callee_path in
  let qualified_name = String.concat "." callee_path in
  match List.rev callee_path with
  | [] ->
      Error
        (Macro_error.make
           ~span:invocation.span
           "macro invocation is missing a callee path")
  | macro_name :: rev_module_path ->
      let module_path = List.rev rev_module_path in
      if module_path = [] then
        if explicit_providers then
          Error
            (Macro_error.make
               ~span:invocation.span
               ("macro invocation must be qualified: " ^ qualified_name ^ "!"))
        else
          let matches =
            providers |> List.filter_map
              (fun provider ->
                Macro_provider.find_macro provider macro_name |> Option.map
                  (fun macro_ -> (provider, macro_)))
          in
          (
            match matches with
            | [] ->
                Error
                  (Macro_error.make
                     ~span:invocation.span
                     ("unsupported macro invocation: " ^ qualified_name ^ "!"))
            | [ (_, macro_) ] -> Ok macro_
            | _ ->
                Error
                  (Macro_error.make
                     ~span:invocation.span
                     ("ambiguous bare macro invocation: " ^ qualified_name ^ "!"))
          )
      else
        let provider =
          List.find_opt
            (fun provider -> Macro_provider.module_path provider = module_path)
            providers
        in
        match provider with
        | None ->
            Error
              (Macro_error.make
                 ~span:invocation.span
                 ("unsupported macro invocation: " ^ qualified_name ^ "!"))
        | Some provider -> (
            match Macro_provider.find_macro provider macro_name with
            | Some macro_ -> Ok macro_
            | None ->
                Error
                  (Macro_error.make
                     ~span:invocation.span
                     ("unsupported macro invocation: " ^ qualified_name ^ "!"))
          )
