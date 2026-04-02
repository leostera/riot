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

let find_provider_binding = fun source_file ->
  let rec loop = function
    | [] -> None
    | Syn.Cst.StructureItem.LetBinding binding :: rest ->
        if String.equal (Syn.Cst.LetBinding.name binding) "provider" then
          Some binding
        else
          loop rest
    | _ :: rest -> loop rest
  in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> loop

let rec is_unit_pattern = function
  | Syn.Cst.Pattern.Literal { literal=Syn.Cst.PatternLiteral.Unit _; _ } -> true
  | Syn.Cst.Pattern.Parenthesized pattern -> is_unit_pattern pattern.inner
  | _ -> false

let is_unit_parameter = function
  | Syn.Cst.Parameter.Positional parameter -> is_unit_pattern parameter.pattern
  | _ -> false

let has_single_unit_parameter = fun parameters ->
  match parameters with
  | [ parameter ] -> is_unit_parameter parameter
  | _ -> false

let rec unwrap_parenthesized_expression = function
  | Syn.Cst.Expression.Parenthesized expr -> unwrap_parenthesized_expression expr.inner
  | expr -> expr

let binding_is_provider_thunk = fun binding ->
  if has_single_unit_parameter (Syn.Cst.LetBinding.parameters binding) then
    true
  else
    match unwrap_parenthesized_expression (Syn.Cst.LetBinding.value binding) with
    | Syn.Cst.Expression.Fun expr -> has_single_unit_parameter expr.parameters
    | _ -> false

let validate_provider_binding_shape = fun (provider: Riot_model.Macro_provider.t) binding ->
  if binding_is_provider_thunk binding then
    Ok ()
  else
    Error (Macro_error.make
      ("macro package '"
      ^ provider.package_name
      ^ "' must expose `let provider () = ...` in "
      ^ display_source_path provider))

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
              match find_provider_binding source_file with
              | Some binding -> validate_provider_binding_shape provider binding
              | None ->
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
