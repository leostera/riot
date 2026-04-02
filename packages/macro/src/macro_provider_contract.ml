open Std

let provider_source_path = fun (provider: Riot_model.Macro_provider.t) ->
  if Path.is_absolute provider.source_path then
    provider.source_path
  else
    Path.(provider.package_path / provider.source_path)

let display_source_path = fun (provider: Riot_model.Macro_provider.t) ->
  if Path.is_absolute provider.source_path then
    match Path.strip_prefix provider.source_path ~prefix:provider.package_path with
    | Ok relative -> Path.to_string relative
    | Error _ -> Path.to_string provider.source_path
  else
    Path.to_string provider.source_path

let read_provider_source = fun (provider: Riot_model.Macro_provider.t) ->
  let source_path = provider_source_path provider in
  match Fs.read source_path with
  | Ok source -> Ok (source_path, source)
  | Error err ->
      Error (Macro_error.make
        ("failed to read macro provider source for package '"
        ^ provider.package_name
        ^ "': "
        ^ IO.error_message err))

let parse_provider_source = fun (provider: Riot_model.Macro_provider.t) source_path source ->
  let parsed = Syn.parse ~filename:source_path source in
  match parsed.diagnostics with
  | [] -> Ok parsed
  | diagnostic :: _ ->
      Error (Macro_error.make
        ~span:diagnostic.Syn.Diagnostic.span
        ("macro package '"
        ^ provider.package_name
        ^ "' provider source must parse cleanly: "
        ^ Syn.Diagnostic.main_message diagnostic))

let provider_binding_exists = fun source_file ->
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.exists
    (function
      | Syn.Cst.StructureItem.LetBinding binding ->
          String.equal (Syn.Cst.LetBinding.name binding) "provider"
      | _ -> false)

let validate = fun (provider: Riot_model.Macro_provider.t) ->
  match read_provider_source provider with
  | Error _ as err -> err
  | Ok (source_path, source) -> (
      match parse_provider_source provider source_path source with
      | Error _ as err -> err
      | Ok parsed -> (
          match Syn.build_cst parsed with
          | Error (Syn.Parse_diagnostics diagnostics) -> (
              match diagnostics with
              | diagnostic :: _ ->
                  Error (Macro_error.make
                    ~span:diagnostic.Syn.Diagnostic.span
                    ("macro package '"
                    ^ provider.package_name
                    ^ "' provider source must parse cleanly: "
                    ^ Syn.Diagnostic.main_message diagnostic))
              | [] ->
                  Error (Macro_error.make
                    ("macro package '"
                    ^ provider.package_name
                    ^ "' provider source could not be validated")))
          | Error (Syn.Cst_builder_error err) ->
              Error (Macro_error.make
                ~span:err.Syn.CstBuilder.span
                ("macro package '"
                ^ provider.package_name
                ^ "' provider source could not be lifted to the typed syntax tree: "
                ^ err.message))
          | Ok source_file ->
              if provider_binding_exists source_file then
                Ok ()
              else
                Error (Macro_error.make
                  ("macro package '"
                  ^ provider.package_name
                  ^ "' must expose a top-level `let provider` entrypoint in "
                  ^ display_source_path provider))
        ))

let validate_all = fun providers ->
  let rec loop = function
    | [] -> Ok ()
    | provider :: rest -> (
        match validate provider with
        | Ok () -> loop rest
        | Error _ as err -> err)
  in
  loop providers
