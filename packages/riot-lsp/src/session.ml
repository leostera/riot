open Std
open Std.Data
open Std.Result.Syntax
open Typ.Diagnostics
open Typ.Model

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Syn.Ast.Continue ())

module Vector = Std.Collections.Vector

let lint_rules = Riot_fix.Pipeline.default_rules ()

type document = {
  uri: Lsp.Uri.t;
  version: int;
  text: string;
  path: Path.t option;
}

type fixable_lint_diagnostic = {
  diagnostic: Riot_fix.Diagnostic.t;
  lsp_diagnostic: Lsp.Diagnostic.t;
  fix: Riot_fix.Fix.fix;
}

type typ_query_context = unit

type t = {
  initialized: bool;
  shutdown_requested: bool;
  documents: document list;
  workspace_manager: Riot_model.Workspace_manager.t;
}

type outcome = {
  state: t;
  outbound: Json.t list;
  exit_code: int option;
}

let empty = {
  initialized = false;
  shutdown_requested = false;
  documents = [];
  workspace_manager = Riot_model.Workspace_manager.create ();
}

let uri_equal = fun left ->
  fun right -> String.equal (Lsp.Uri.to_string left) (Lsp.Uri.to_string right)

let upsert_document = fun state document -> {
  state with
  documents = document
  :: List.filter state.documents ~fn:(fun existing -> not (uri_equal existing.uri document.uri));
}

let remove_document = fun state uri -> {
  state with
  documents = List.filter state.documents ~fn:(fun document -> not (uri_equal document.uri uri));
}

let find_document = fun state uri ->
  List.find
    state.documents
    ~fn:(fun document -> uri_equal document.uri uri)

let response_error = fun ~id ~code ~message ?data () ->
  Lsp.error_response_to_json
    ~id
    Lsp.{ code; message; data }

let ok = fun state ?exit_code outbound -> { state; outbound; exit_code }

let filename_of_uri = fun uri ->
  match Lsp.Uri.to_path uri with
  | Ok path -> path
  | Error _ -> Path.v "buffer.ml"

let compare_paths = fun left right -> String.compare (Path.to_string left) (Path.to_string right)

let dedupe_paths = fun paths ->
  let sorted = List.sort paths ~compare:compare_paths in
  List.unique sorted ~compare:compare_paths

let document_path_key = fun (document: document) ->
  match document.path with
  | None -> None
  | Some path ->
      Some (
        Path.normalize path
        |> Path.to_string
      )

let document_in_root = fun root ->
  fun (document: document) ->
    match document.path with
    | None -> false
    | Some path ->
        let root = Path.normalize root in
        let path = Path.normalize path in
        Path.equal path root || match Path.strip_prefix path ~prefix:root with
        | Ok _ -> true
        | Error _ -> false

let filename_of_document = fun (document: document) ->
  match document.path with
  | Some path -> path
  | None -> filename_of_uri document.uri

let source_slice = fun text ->
  IO.IoVec.IoSlice.from_string text
  |> Result.expect ~msg:"failed to create LSP parser source slice"

let add_prepared_typ_source = fun session ->
  fun ~kind:_ ->
    fun ~origin:_ ->
      fun ~revision:_ -> fun ~text:_ -> fun ~parse_result:_ -> fun ~cst:_ -> (session, (), ())

let typ_source_origin_of_document = fun (document: document) ->
  match document.path with
  | Some _path -> None
  | None -> None

let package_scope_for_file = fun state path ->
  let start_dir = Path.dirname path in
  match Riot_model.Workspace_manager.scan state.workspace_manager start_dir with
  | Error _ -> None
  | Ok (workspace, _errors) -> (
      match Riot_model.Workspace_manifest.find_package_for_path workspace ~path with
      | None -> None
      | Some manifest ->
          Some (Riot_model.Workspace_manifest.realize_package
            ~intent:Riot_model.Package.Runtime
            manifest)
    )

let package_source_files = fun (pkg: Riot_model.Package.t) ->
  ((pkg.sources.src @ pkg.sources.tests) @ pkg.sources.examples) @ pkg.sources.bench
  |> List.map ~fn:(fun relative -> Path.(pkg.path / relative))
  |> dedupe_paths

let package_typ_summary_source_files = fun (pkg: Riot_model.Package.t) ->
  pkg.sources.src
  |> List.map ~fn:(fun relative -> Path.(pkg.path / relative))
  |> dedupe_paths

let package_library_typ_source_files = fun (pkg: Riot_model.Package.t) ->
  match pkg.library with
  | None -> package_typ_summary_source_files pkg
  | Some { path = library_path } ->
      let interface_path = Path.(add_extension (remove_extension library_path) ~ext:"mli") in
      let files =
        match Fs.exists interface_path with
        | Ok true -> [ interface_path; library_path ]
        | Ok false
        | Error _ -> [ library_path ]
      in
      files
      |> dedupe_paths

let typ_target_files = fun state ->
  fun (document: document) ->
    match document.path with
    | None -> []
    | Some path -> (
        match package_scope_for_file state path with
        | Some pkg -> (
            let package_root = pkg.path in
            let package_files = package_source_files pkg in
            let open_documents =
              state.documents
              |> List.filter ~fn:(document_in_root package_root)
              |> List.filter_map ~fn:(fun document -> document.path)
            in
            dedupe_paths ((package_files @ open_documents) @ [ path ])
          )
        | None -> [ path ]
      )

let text_for_path = fun state path ->
  let key =
    Path.normalize path
    |> Path.to_string
  in
  state.documents
  |> List.find
    ~fn:(fun document ->
      match document_path_key document with
      | Some candidate -> String.equal candidate key
      | None -> false)
  |> function
    | Some document ->
        Some document.text
    | None -> (
        match Fs.read path with
        | Ok text -> Some text
        | Error _ -> None
      )

let typ_config_for_document = fun _state -> fun (_document: document) -> ()

let typ_snapshot_for_document = fun _state -> fun (_document: document) -> None

let typ_query_context_for_document = fun _state -> fun _document -> None

let typ_analysis_for_document = fun _state -> fun _document -> None

let diagnostic_to_lsp = fun text ->
  fun (diagnostic: Syn.Diagnostic.t) ->
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:diagnostic.span.start
        ~end_offset:diagnostic.span.end_;
      severity = Some Lsp.Diagnostic.Error;
      code = Some (Syn.Diagnostic.id diagnostic);
      source = Some "syn";
      message = Syn.Diagnostic.main_message diagnostic;
      tags = None;
      data = Some (Syn.Diagnostic.to_json diagnostic);
    }

let lint_diagnostic_severity = fun severity ->
  match severity with
  | Riot_fix.Diagnostic.Error -> Lsp.Diagnostic.Error
  | Riot_fix.Diagnostic.Warning -> Lsp.Diagnostic.Warning
  | Riot_fix.Diagnostic.Info -> Lsp.Diagnostic.Information
  | Riot_fix.Diagnostic.Hint -> Lsp.Diagnostic.Hint

let lint_diagnostic_to_lsp = fun text ->
  fun (diagnostic: Riot_fix.Diagnostic.t) ->
    let span = Riot_fix.Diagnostic.span diagnostic in
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:span.start
        ~end_offset:span.end_;
      severity = Some (lint_diagnostic_severity (Riot_fix.Diagnostic.severity diagnostic));
      code = Some (Riot_fix.Rule_id.to_string (Riot_fix.Diagnostic.rule_id diagnostic));
      source = Some "riot-fix";
      message = Riot_fix.Diagnostic.message diagnostic;
      tags = None;
      data = Some (Riot_fix.Diagnostic.to_json diagnostic);
    }

let typ_diagnostic_severity = fun severity ->
  match severity with
  | Typ.Diagnostics.Diagnostic.UnsupportedSyntax _
  | Typ.Diagnostics.Diagnostic.UnsupportedType _
  | Typ.Diagnostics.Diagnostic.AnnotationMismatch _
  | Typ.Diagnostics.Diagnostic.InfiniteSubstitution _
  | Typ.Diagnostics.Diagnostic.TypeMismatch _ -> Lsp.Diagnostic.Error

let typ_diagnostic_to_lsp = fun text ->
  fun (diagnostic: Typ.Diagnostics.Diagnostic.t) ->
    let (span, message) =
      match diagnostic with
      | Typ.Diagnostics.Diagnostic.UnsupportedSyntax unsupported -> (
        unsupported.span,
        "Unsupported syntax: "
        ^ Syn.SyntaxKind.to_string unsupported.kind
        ^ " - "
        ^ unsupported.summary
      )
      | Typ.Diagnostics.Diagnostic.UnsupportedType unsupported -> (
        unsupported.span,
        "Unsupported type: " ^ unsupported.summary
      )
      | Typ.Diagnostics.Diagnostic.AnnotationMismatch mismatch -> (
        mismatch.span,
        Typ.Diagnostics.Diagnostic.to_string diagnostic
      )
      | Typ.Diagnostics.Diagnostic.InfiniteSubstitution substitution -> (
        substitution.span,
        Typ.Diagnostics.Diagnostic.to_string diagnostic
      )
      | Typ.Diagnostics.Diagnostic.TypeMismatch mismatch -> (
        mismatch.span,
        Typ.Diagnostics.Diagnostic.to_string diagnostic
      )
    in
    {
      Lsp.Diagnostic.range = Lsp.Utf16.range_of_offsets
        text
        ~start_offset:span.start
        ~end_offset:span.end_;
      severity = Some (typ_diagnostic_severity diagnostic);
      code = Some "typ";
      source = Some "typ";
      message;
      tags = None;
      data = None;
    }

let analyze_document = fun document ->
  Riot_fix.Source_runner.run
    ~rules:lint_rules
    ~filename:(filename_of_document document)
    document.text

let compare_position = fun (left: Lsp.Position.t) ->
  fun (right: Lsp.Position.t) ->
    match Int.compare left.line right.line with
    | Order.EQ -> Int.compare left.character right.character
    | n -> n

let ranges_overlap = fun (left: Lsp.Range.t) ->
  fun (right: Lsp.Range.t) ->
    compare_position left.end_ right.start_ != Order.LT
    && compare_position right.end_ left.start_ != Order.LT

let same_range = fun (left: Lsp.Range.t) ->
  fun (right: Lsp.Range.t) ->
    compare_position left.start_ right.start_ = Order.EQ
    && compare_position left.end_ right.end_ = Order.EQ

let same_lsp_diagnostic = fun (left: Lsp.Diagnostic.t) ->
  fun (right: Lsp.Diagnostic.t) ->
    same_range left.range right.range
    && Option.equal left.code right.code ~fn:String.equal
    && Option.equal left.source right.source ~fn:String.equal
    && String.equal left.message right.message

let action_kind_allowed = fun only actual ->
  let actual_name = Lsp.Action_kind.to_string actual in
  match only with
  | None -> true
  | Some requested ->
      List.exists
        (fun requested_kind ->
          let requested_name = Lsp.Action_kind.to_string requested_kind in
          String.equal requested_name actual_name
          || String.starts_with ~prefix:(requested_name ^ ".") actual_name)
        requested

let lint_diagnostic_requested = fun context range diagnostic ->
  if List.is_empty context.Lsp.Text_document_methods.Code_action.diagnostics then
    ranges_overlap diagnostic.Lsp.Diagnostic.range range
  else
    List.exists (same_lsp_diagnostic diagnostic) context.diagnostics

let document_range = fun text ->
  Lsp.Utf16.range_of_offsets
    text
    ~start_offset:0
    ~end_offset:(String.length text)

let workspace_edit_of_text = fun document text -> {
  Lsp.Workspace_edit.changes = [
    (document.uri, [ { Lsp.Text_edit.range = document_range document.text; new_text = text } ]);
  ];
}

let maybe_format_text = fun document text ->
  let parse_result = Syn.parse ~filename:(filename_of_uri document.uri) (source_slice text) in
  if Vector.length parse_result.diagnostics > 0 then
    text
  else
    match Krasny.format parse_result with
    | Ok formatted -> formatted
    | Error _ -> text

let finalized_workspace_edit_of_text = fun document text ->
  let text = maybe_format_text document text in
  workspace_edit_of_text document text

let fixable_lint_diagnostics = fun document result ->
  result.Riot_fix.Source_runner.diagnostics
  |> List.filter_map
    ~fn:(fun diagnostic ->
      match Riot_fix.Diagnostic.fix diagnostic with
      | None -> None
      | Some fix ->
          Some {
            diagnostic;
            lsp_diagnostic = lint_diagnostic_to_lsp document.text diagnostic;
            fix;
          })

let quickfix_action_of_entry = fun document entry ->
  match Riot_fix.Fix.apply_fix ~source:document.text entry.fix with
  | Error _ -> None
  | Ok text ->
      Some (
        Lsp.Code_action_or_command.Action {
          Lsp.Code_action.title = Riot_fix.Fix.title entry.fix;
          kind = Some Lsp.Action_kind.Quick_fix;
          diagnostics = Some [ entry.lsp_diagnostic ];
          is_preferred = Some true;
          edit = Some (finalized_workspace_edit_of_text document text);
          command = None;
          data = None;
        }
      )

let fix_all_action = fun document entries ->
  match Riot_fix.Fix.apply_fixes
    ~source:document.text
    (List.map entries ~fn:(fun entry -> entry.fix)) with
  | Error _ -> None
  | Ok text ->
      Some (
        Lsp.Code_action_or_command.Action {
          Lsp.Code_action.title = "Fix all auto-fixable Riot diagnostics";
          kind = Some Lsp.Action_kind.Source_fix_all;
          diagnostics = Some (List.map entries ~fn:(fun entry -> entry.lsp_diagnostic));
          is_preferred = None;
          edit = Some (finalized_workspace_edit_of_text document text);
          command = None;
          data = None;
        }
      )

let typ_diagnostics = fun state ->
  fun document ->
    ignore state;
    ignore document;
    []

let publish_diagnostics = fun state ->
  fun document ->
    let result = analyze_document document in
    let diagnostics =
      (List.map result.parse_diagnostics ~fn:(diagnostic_to_lsp document.text)
      @ List.map result.diagnostics ~fn:(lint_diagnostic_to_lsp document.text))
      @ typ_diagnostics state document
    in
    let params: Lsp.Text_document_methods.Publish_diagnostics.params = {
      uri = document.uri;
      version = Some document.version;
      diagnostics;
    }
    in
    Lsp.notification_to_json Lsp.Text_document_methods.Publish_diagnostics.notification params

let query_position_of_lsp_position = fun text position ->
  match Lsp.Utf16.offset_of_position text position with
  | Error _ -> None
  | Ok offset -> Some offset

let range_of_span = fun text (span: Syn.Ceibo.Span.t) ->
  Lsp.Utf16.range_of_offsets
    text
    ~start_offset:span.start
    ~end_offset:span.end_

let range_of_node = fun text node ->
  range_of_span
    text
    (Syn.Ceibo.Span.make ~start:(Syn.Ast.Node.span_start node) ~end_:(Syn.Ast.Node.span_end node))

let range_of_token = fun text token ->
  range_of_span
    text
    (Syn.Ceibo.Span.make
      ~start:(Syn.Ast.Token.span_start token)
      ~end_:(Syn.Ast.Token.span_end token))

let range_of_tokens = fun text tokens ->
  match tokens with
  | [] -> Lsp.Utf16.range_of_offsets text ~start_offset:0 ~end_offset:0
  | first :: rest ->
      let last = List.fold_left rest ~init:first ~fn:(fun _ token -> token) in
      let start = Syn.Ast.Token.span_start first in
      let end_ = Syn.Ast.Token.span_end last in
      Lsp.Utf16.range_of_offsets text ~start_offset:start ~end_offset:end_

let text_of_name_tokens = fun tokens ->
  tokens
  |> List.map ~fn:Syn.Ast.Token.text
  |> String.concat ""

let vector_to_list = fun vector ->
  Vector.to_array vector
  |> Array.to_list

let collect_tokens = fun ~size collect ->
  let tokens = Vector.with_capacity ~size in
  collect ~fn:(fun token -> Vector.push tokens ~value:token);
  vector_to_list tokens

let rec binding_name_tokens_of_parameter = fun parameter ->
  match Syn.Ast.Parameter.view parameter with
  | Syn.Ast.Parameter.Param { pattern = Some pattern; _ } -> binding_name_tokens_of_pattern pattern
  | Syn.Ast.Parameter.Param { label = Syn.Ast.Parameter.Labeled { name = Some label }; _ }
  | Syn.Ast.Parameter.Param { label = Syn.Ast.Parameter.Optional { name = Some label; _ }; _ } ->
      Some [ label ]
  | Syn.Ast.Parameter.Param _
  | Syn.Ast.Parameter.Unknown _ -> None

and binding_name_tokens_of_pattern = fun pattern ->
  match Syn.Ast.Pattern.view pattern with
  | Syn.Ast.Pattern.Ident { ident } -> (
      match Syn.Ast.Ident.last_segment ident with
      | Some token -> Some [ token ]
      | None -> None
    )
  | Syn.Ast.Pattern.Alias { alias; _ }
  | Syn.Ast.Pattern.Constraint { pattern = alias; _ }
  | Syn.Ast.Pattern.Lazy { pattern = alias }
  | Syn.Ast.Pattern.Exception { pattern = alias } -> binding_name_tokens_of_pattern alias
  | Syn.Ast.Pattern.Unit
  | Syn.Ast.Pattern.Wildcard
  | Syn.Ast.Pattern.Literal _
  | Syn.Ast.Pattern.Construct _
  | Syn.Ast.Pattern.Tuple _
  | Syn.Ast.Pattern.List _
  | Syn.Ast.Pattern.Array _
  | Syn.Ast.Pattern.Record _
  | Syn.Ast.Pattern.PolyVariant _
  | Syn.Ast.Pattern.FirstClassModule _
  | Syn.Ast.Pattern.Interval _
  | Syn.Ast.Pattern.Or _
  | Syn.Ast.Pattern.Cons _
  | Syn.Ast.Pattern.Error _
  | Syn.Ast.Pattern.Unknown _ -> None

let rec value_like_type_is_function = fun type_ ->
  match Syn.Ast.TypeExpr.view type_ with
  | Syn.Ast.TypeExpr.Arrow _ -> true
  | Syn.Ast.TypeExpr.Poly { body; _ } -> value_like_type_is_function body
  | _ -> false

let symbol_kind_of_type_member = fun member ->
  if Option.is_some (Syn.Ast.TypeDeclaration.Member.variant_type member) then
    Lsp.Symbol_kind.Enum
  else
    Lsp.Symbol_kind.Struct

let symbol_children = function
  | [] -> None
  | children -> Some children

let document_symbol_of_named_item = fun
  ~text ~name ~kind ~syntax_node ~selection_range ?detail ?children () ->
  {
    Lsp.Document_symbol_item.name;
    detail;
    kind;
    range = range_of_node text syntax_node;
    selection_range;
    children;
  }

let let_binding_symbol = fun text ~range_node binding ->
  match Syn.Ast.LetBinding.pattern binding
  |> Option.and_then ~fn:binding_name_tokens_of_pattern with
  | None -> []
  | Some name_tokens ->
      let has_parameters = ref false in
      iter_fold Syn.Ast.LetBinding.fold_parameter binding ~fn:(fun _ -> has_parameters := true);
      let kind =
        match Syn.Ast.LetBinding.type_annotation binding with
        | Some type_ when value_like_type_is_function type_ -> Lsp.Symbol_kind.Function
        | _ when !has_parameters -> Lsp.Symbol_kind.Function
        | _ -> Lsp.Symbol_kind.Variable
      in
      [
        document_symbol_of_named_item
          ~text
          ~name:(text_of_name_tokens name_tokens)
          ~kind
          ~syntax_node:range_node
          ~selection_range:(range_of_tokens text name_tokens)
          ();
      ]

let let_declaration_symbols = fun text declaration ->
  let symbols = Vector.with_capacity ~size:2 in
  iter_fold
    Syn.Ast.LetDeclaration.fold_binding
    declaration
    ~fn:(fun binding ->
      let_binding_symbol text ~range_node:(Syn.Ast.LetDeclaration.as_node declaration) binding
      |> List.for_each ~fn:(fun symbol -> Vector.push symbols ~value:symbol));
  vector_to_list symbols

let type_declaration_symbols = fun text declaration ->
  Syn.Ast.TypeDeclaration.fold_members
    declaration
    []
    (fun acc member ->
      match Syn.Ast.TypeDeclaration.Member.name member with
      | None -> acc
      | Some name ->
          document_symbol_of_named_item
            ~text
            ~name:(Syn.Ast.Token.text name)
            ~kind:(symbol_kind_of_type_member member)
            ~syntax_node:(Syn.Ast.TypeDeclaration.as_node declaration)
            ~selection_range:(range_of_token text name)
            ()
          :: acc)
  |> List.reverse

let type_extension_symbols = fun text declaration ->
  match Syn.Ast.TypeExtensionDeclaration.name declaration with
  | None -> []
  | Some name ->
      [
        document_symbol_of_named_item
          ~text
          ~name:(Syn.Ast.Token.text name)
          ~kind:Lsp.Symbol_kind.Enum
          ~syntax_node:(Syn.Ast.TypeExtensionDeclaration.as_node declaration)
          ~selection_range:(range_of_token text name)
          ();
      ]

let rec collect_structure_symbols = fun text collect ->
  let items = Vector.with_capacity ~size:8 in
  collect ~fn:(fun item -> Vector.push items ~value:item);
  structure_item_symbols text (vector_to_list items)

and collect_signature_symbols = fun text collect ->
  let items = Vector.with_capacity ~size:8 in
  collect ~fn:(fun item -> Vector.push items ~value:item);
  signature_item_symbols text (vector_to_list items)

and module_declaration_children = fun text declaration ->
  match Syn.Ast.ModuleDeclaration.body declaration with
  | Syn.Ast.ModuleDeclaration.Expr { body } -> (
      match Syn.Ast.ModuleExpr.view body with
      | Syn.Ast.ModuleExpr.Structure _ ->
          collect_structure_symbols
            text
            (iter_fold Syn.Ast.ModuleDeclaration.fold_structure_item declaration)
      | Syn.Ast.ModuleExpr.Ident _
      | Syn.Ast.ModuleExpr.Functor _
      | Syn.Ast.ModuleExpr.Apply _
      | Syn.Ast.ModuleExpr.Constraint _
      | Syn.Ast.ModuleExpr.Opaque _
      | Syn.Ast.ModuleExpr.Error _
      | Syn.Ast.ModuleExpr.Unknown _ -> []
    )
  | Syn.Ast.ModuleDeclaration.Type { body } -> (
      match Syn.Ast.ModuleTypeExpr.view body with
      | Syn.Ast.ModuleTypeExpr.Signature _ ->
          collect_signature_symbols
            text
            (iter_fold Syn.Ast.ModuleDeclaration.fold_signature_item declaration)
      | Syn.Ast.ModuleTypeExpr.Ident _
      | Syn.Ast.ModuleTypeExpr.With _
      | Syn.Ast.ModuleTypeExpr.Typeof _
      | Syn.Ast.ModuleTypeExpr.Functor _
      | Syn.Ast.ModuleTypeExpr.Error _
      | Syn.Ast.ModuleTypeExpr.Unknown _ -> []
    )
  | Syn.Ast.ModuleDeclaration.Unsupported _ -> []

and module_declaration_symbols = fun text declaration ->
  Syn.Ast.ModuleDeclaration.fold_members
    declaration
    []
    (fun acc member ->
      match Syn.Ast.ModuleDeclaration.Member.name member with
      | None -> acc
      | Some name ->
          let children = module_declaration_children text declaration in
          document_symbol_of_named_item
            ~text
            ~name:(Syn.Ast.Token.text name)
            ~kind:Lsp.Symbol_kind.Module
            ~syntax_node:(Syn.Ast.ModuleDeclaration.as_node declaration)
            ~selection_range:(range_of_token text name)
            ?children:(symbol_children children)
            ()
          :: acc)
  |> List.reverse

and module_type_declaration_symbols = fun text declaration ->
  match Syn.Ast.ModuleTypeDeclaration.name declaration with
  | None -> []
  | Some name ->
      let children =
        match Syn.Ast.ModuleTypeDeclaration.body declaration with
        | Syn.Ast.ModuleTypeDeclaration.Abstract
        | Syn.Ast.ModuleTypeDeclaration.Unsupported _ -> []
        | Syn.Ast.ModuleTypeDeclaration.Manifest { body } -> (
            match Syn.Ast.ModuleTypeExpr.view body with
            | Syn.Ast.ModuleTypeExpr.Signature _ ->
                collect_signature_symbols
                  text
                  (iter_fold Syn.Ast.ModuleTypeDeclaration.fold_signature_item declaration)
            | Syn.Ast.ModuleTypeExpr.Ident _
            | Syn.Ast.ModuleTypeExpr.With _
            | Syn.Ast.ModuleTypeExpr.Typeof _
            | Syn.Ast.ModuleTypeExpr.Functor _
            | Syn.Ast.ModuleTypeExpr.Error _
            | Syn.Ast.ModuleTypeExpr.Unknown _ -> []
          )
      in
      [
        document_symbol_of_named_item
          ~text
          ~name:(Syn.Ast.Token.text name)
          ~kind:Lsp.Symbol_kind.Interface
          ~syntax_node:(Syn.Ast.ModuleTypeDeclaration.as_node declaration)
          ~selection_range:(range_of_token text name)
          ?children:(symbol_children children)
          ();
      ]

and exception_symbols = fun text declaration ->
  match Syn.Ast.ExceptionDeclaration.name declaration with
  | None -> []
  | Some name ->
      [
        document_symbol_of_named_item
          ~text
          ~name:(Syn.Ast.Token.text name)
          ~kind:Lsp.Symbol_kind.Event
          ~syntax_node:(Syn.Ast.ExceptionDeclaration.as_node declaration)
          ~selection_range:(range_of_token text name)
          ();
      ]

and value_like_declaration_symbols = fun text ~syntax_node ~name_tokens ~type_annotation ->
  match name_tokens with
  | [] -> []
  | _ ->
      let kind =
        match type_annotation with
        | Some type_ when value_like_type_is_function type_ -> Lsp.Symbol_kind.Function
        | _ -> Lsp.Symbol_kind.Variable
      in
      [
        document_symbol_of_named_item
          ~text
          ~name:(text_of_name_tokens name_tokens)
          ~kind
          ~syntax_node
          ~selection_range:(range_of_tokens text name_tokens)
          ();
      ]

and value_declaration_symbols = fun text declaration ->
  let name_tokens =
    collect_tokens ~size:2 (iter_fold Syn.Ast.ValueDeclaration.fold_name_token declaration)
  in
  value_like_declaration_symbols
    text
    ~syntax_node:(Syn.Ast.ValueDeclaration.as_node declaration)
    ~name_tokens
    ~type_annotation:(Syn.Ast.ValueDeclaration.type_annotation declaration)

and external_declaration_symbols = fun text declaration ->
  let name_tokens =
    collect_tokens ~size:2 (iter_fold Syn.Ast.ExternalDeclaration.fold_name_token declaration)
  in
  value_like_declaration_symbols
    text
    ~syntax_node:(Syn.Ast.ExternalDeclaration.as_node declaration)
    ~name_tokens
    ~type_annotation:(Syn.Ast.ExternalDeclaration.type_annotation declaration)

and structure_item_symbols = fun text items ->
  items
  |> List.map
    ~fn:(fun item ->
      match Syn.Ast.StructureItem.view item with
      | Syn.Ast.StructureItem.Let declaration -> let_declaration_symbols text declaration
      | Syn.Ast.StructureItem.Type (
        Syn.Ast.TypeDeclarationItem declaration
      ) ->
          type_declaration_symbols text declaration
      | Syn.Ast.StructureItem.Type (
        Syn.Ast.TypeExtensionItem declaration
      ) ->
          type_extension_symbols text declaration
      | Syn.Ast.StructureItem.Module declaration -> module_declaration_symbols text declaration
      | Syn.Ast.StructureItem.ModuleType declaration ->
          module_type_declaration_symbols text declaration
      | Syn.Ast.StructureItem.External declaration -> external_declaration_symbols text declaration
      | Syn.Ast.StructureItem.Exception declaration -> exception_symbols text declaration
      | Syn.Ast.StructureItem.Open _
      | Syn.Ast.StructureItem.Include _
      | Syn.Ast.StructureItem.Extension _
      | Syn.Ast.StructureItem.Attribute _
      | Syn.Ast.StructureItem.Expr _
      | Syn.Ast.StructureItem.Error _
      | Syn.Ast.StructureItem.Unknown _ -> [])
  |> List.concat

and signature_item_symbols = fun text items ->
  items
  |> List.map
    ~fn:(fun item ->
      match Syn.Ast.SignatureItem.view item with
      | Syn.Ast.SignatureItem.Value declaration -> value_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.Type (
        Syn.Ast.TypeDeclarationItem declaration
      ) ->
          type_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.Type (
        Syn.Ast.TypeExtensionItem declaration
      ) ->
          type_extension_symbols text declaration
      | Syn.Ast.SignatureItem.Module declaration -> module_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.ModuleType declaration ->
          module_type_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.External declaration -> external_declaration_symbols text declaration
      | Syn.Ast.SignatureItem.Exception declaration -> exception_symbols text declaration
      | Syn.Ast.SignatureItem.Open _
      | Syn.Ast.SignatureItem.Include _
      | Syn.Ast.SignatureItem.Extension _
      | Syn.Ast.SignatureItem.Attribute _
      | Syn.Ast.SignatureItem.Error _
      | Syn.Ast.SignatureItem.Unknown _ -> [])
  |> List.concat

let document_symbols_for_document = fun document ->
  let parsed = Syn.parse ~filename:(filename_of_document document) (source_slice document.text) in
  if Vector.length parsed.Syn.Parser.diagnostics > 0 then
    Some []
  else
    let root = Syn.Ast.SourceFile.make parsed.Syn.Parser.tree in
    let symbols =
      match Syn.Ast.SourceFile.view root with
      | Syn.Ast.SourceFile.Implementation implementation ->
          let items = Vector.with_capacity ~size:16 in
          iter_fold
            Syn.Ast.Implementation.fold_item
            implementation
            ~fn:(fun item -> Vector.push items ~value:item);
          structure_item_symbols document.text (vector_to_list items)
      | Syn.Ast.SourceFile.Interface interface ->
          let items = Vector.with_capacity ~size:16 in
          iter_fold
            Syn.Ast.Interface.fold_item
            interface
            ~fn:(fun item -> Vector.push items ~value:item);
          signature_item_symbols document.text (vector_to_list items)
    in
    Some symbols

let hover_for_document = fun state ->
  fun document ->
    fun position ->
      ignore state;
      ignore position;
      ignore document;
      None

let document_source_for_origin = fun state origin ->
  ignore state;
  ignore origin;
  None

let location_for_definition_site = fun state definition ->
  ignore state;
  ignore definition;
  None

let definition_for_document = fun state ->
  fun document ->
    fun position ->
      ignore state;
      ignore document;
      ignore position;
      None

let document_symbol_for_document = fun document -> document_symbols_for_document document

let clear_diagnostics = fun uri ->
  let params: Lsp.Text_document_methods.Publish_diagnostics.params = {
    uri;
    version = None;
    diagnostics = [];
  }
  in
  Lsp.notification_to_json Lsp.Text_document_methods.Publish_diagnostics.notification params

let splice_text = fun text ->
  fun range ->
    fun replacement ->
      let* start_offset = Lsp.Utf16.offset_of_position text range.Lsp.Range.start_ in
      let* end_offset = Lsp.Utf16.offset_of_position text range.end_ in
      if start_offset > end_offset then
        Error "invalid text edit range"
      else
        let prefix = String.sub text ~offset:0 ~len:start_offset in
        let suffix = String.sub text ~offset:end_offset ~len:(String.length text - end_offset) in
        Ok (prefix ^ replacement ^ suffix)

let apply_change = fun text ->
  fun (change: Lsp.Text_document.content_change_event) ->
    match change.range with
    | None -> Ok change.text
    | Some range -> splice_text text range change.text

let apply_changes = fun text ->
  fun changes ->
    List.fold_left
      changes
      ~init:(Ok text)
      ~fn:(fun acc change ->
        let* current = acc in
        apply_change current change)

let capabilities = {
  Lsp.Initialize.Server_capabilities.position_encoding = Some "utf-16";
  text_document_sync = Some (Lsp.Initialize.Server_capabilities.Sync_options {
    open_close = Some true;
    change = Some Lsp.Text_document.Sync_kind.Full;
    save = None;
  });
  document_formatting_provider = Some true;
  definition_provider = Some true;
  hover_provider = Some true;
  document_symbol_provider = Some true;
  code_action_provider = Some (Lsp.Initialize.Server_capabilities.Provider_options {
    code_action_kinds = Some [ Lsp.Action_kind.Quick_fix; Source_fix_all ];
    resolve_provider = Some false;
  });
  experimental = None;
}

let initialize_result: Lsp.Initialize.result = {
  capabilities;
  server_info = Some { Lsp.Server_info.name = "riot-lsp"; version = None };
}

let debug_json = fun state ->
  let documents =
    state.documents
    |> List.sort
      ~compare:(fun left right ->
        String.compare
          (Lsp.Uri.to_string left.uri)
          (Lsp.Uri.to_string right.uri))
    |> List.map
      ~fn:(fun document ->
        Json.obj
          [ ("uri", Lsp.Uri.to_json document.uri); ("version", Json.int document.version); ])
  in
  Json.obj
    [
      ("initialized", Json.bool state.initialized);
      ("shutdownRequested", Json.bool state.shutdown_requested);
      ("documents", Json.array documents);
    ]

let outcome_to_json = fun outcome ->
  Json.obj
    [
      ("outbound", Json.array outcome.outbound);
      ("exitCode", match outcome.exit_code with
      | None -> Json.Null
      | Some code -> Json.int code);
      ("state", debug_json outcome.state);
    ]

let handle_initialize = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Initialize.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, _params) ->
        if state.initialized then
          ok
            state
            [
              response_error
                ~id
                ~code:Lsp.Error_code.invalid_request
                ~message:"initialize was already called"
                ();
            ]
        else
          let state = { state with initialized = true; shutdown_requested = false } in
          ok state [ Lsp.response_to_json ~id Lsp.Initialize.request initialize_result ]

let handle_shutdown = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Shutdown.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, ()) ->
        let state = { state with shutdown_requested = true } in
        ok state [ Lsp.response_to_json ~id Lsp.Shutdown.request () ]

let handle_formatting = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Formatting.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"formatting requested for a document that is not open"
                  ();
              ]
        | Some document ->
            let parse_result =
              Syn.parse ~filename:(filename_of_uri document.uri) (source_slice document.text)
            in
            if Vector.length parse_result.diagnostics > 0 then
              ok
                state
                [ Lsp.response_to_json ~id Lsp.Text_document_methods.Formatting.request None ]
            else
              match Krasny.format parse_result with
              | Ok formatted ->
                  let result =
                    if String.equal formatted document.text then
                      Some []
                    else
                      Some [
                        {
                          Lsp.Text_edit.range = document_range document.text;
                          new_text = formatted;
                        };
                      ]
                  in
                  ok
                    state
                    [ Lsp.response_to_json ~id Lsp.Text_document_methods.Formatting.request result ]
              | Error error ->
                  ok
                    state
                    [
                      response_error
                        ~id
                        ~code:Lsp.Error_code.internal_error
                        ~message:(Krasny.format_error_to_string error)
                        ();
                    ]
      )

let handle_hover = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Hover.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"hover requested for a document that is not open"
                  ();
              ]
        | Some document ->
            ok
              state
              [
                Lsp.response_to_json
                  ~id
                  Lsp.Text_document_methods.Hover.request
                  (hover_for_document state document params.position);
              ]
      )

let handle_definition = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Definition.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"definition requested for a document that is not open"
                  ();
              ]
        | Some document ->
            ok
              state
              [
                Lsp.response_to_json
                  ~id
                  Lsp.Text_document_methods.Definition.request
                  (definition_for_document state document params.position);
              ]
      )

let handle_document_symbol = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Document_symbol.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"document symbols requested for a document that is not open"
                  ();
              ]
        | Some document ->
            ok
              state
              [
                Lsp.response_to_json
                  ~id
                  Lsp.Text_document_methods.Document_symbol.request
                  (document_symbol_for_document document);
              ]
      )

let handle_code_action = fun state ->
  fun payload ->
    match Lsp.request_of_json Lsp.Text_document_methods.Code_action.request payload with
    | Error reason ->
        ok
          state
          [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
    | Ok (id, params) -> (
        match find_document state params.text_document.uri with
        | None ->
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.invalid_params
                  ~message:"code actions requested for a document that is not open"
                  ();
              ]
        | Some document ->
            let analysis = analyze_document document in
            let fixable = fixable_lint_diagnostics document analysis in
            let actions = [] in
            let actions =
              if action_kind_allowed params.context.only Lsp.Action_kind.Quick_fix then
                actions
                @ (
                  fixable
                  |> List.filter
                    ~fn:(fun entry ->
                      lint_diagnostic_requested
                        params.context
                        params.range
                        entry.lsp_diagnostic)
                  |> List.filter_map ~fn:(quickfix_action_of_entry document)
                )
              else
                actions
            in
            let actions =
              if action_kind_allowed params.context.only Lsp.Action_kind.Source_fix_all then
                match fixable with
                | [] -> actions
                | _ -> (
                    match fix_all_action document fixable with
                    | Some action -> actions @ [ action ]
                    | None -> actions
                  )
              else
                actions
            in
            let result =
              match actions with
              | [] -> None
              | _ -> Some actions
            in
            ok
              state
              [ Lsp.response_to_json ~id Lsp.Text_document_methods.Code_action.request result ]
      )

let handle_request = fun state ->
  fun request ->
    fun payload ->
      if (not state.initialized) && not (String.equal request.Jsonrpc.method_ "initialize") then
        let id = Option.unwrap_or request.Jsonrpc.id ~default:Jsonrpc.Null in
        ok
          state
          [
            response_error
              ~id
              ~code:Lsp.Error_code.server_not_initialized
              ~message:"server not initialized"
              ();
          ]
      else
        match request.Jsonrpc.method_ with
        | "initialize" -> handle_initialize state payload
        | "shutdown" -> handle_shutdown state payload
        | "textDocument/definition" -> handle_definition state payload
        | "textDocument/documentSymbol" -> handle_document_symbol state payload
        | "textDocument/hover" -> handle_hover state payload
        | "textDocument/formatting" -> handle_formatting state payload
        | "textDocument/codeAction" -> handle_code_action state payload
        | method_ ->
            let id = Option.unwrap_or request.Jsonrpc.id ~default:Jsonrpc.Null in
            ok
              state
              [
                response_error
                  ~id
                  ~code:Lsp.Error_code.method_not_found
                  ~message:("unknown method `" ^ method_ ^ "`")
                  ();
              ]

let handle_did_open = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_open.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | Some existing ->
            let document = {
              uri = params.text_document.uri;
              version = params.text_document.version;
              text = params.text_document.text;
              path = existing.path;
            }
            in
            let state = upsert_document state document in
            ok state [ publish_diagnostics state document ]
        | None ->
            let document = {
              uri = params.text_document.uri;
              version = params.text_document.version;
              text = params.text_document.text;
              path =
                match Lsp.Uri.to_path params.text_document.uri with
                | Ok path -> Some path
                | Error _ ->
                    None;
            }
            in
            let state = upsert_document state document in
            ok state [ publish_diagnostics state document ]
      )

let handle_did_change = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_change.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | None -> ok state []
        | Some document -> (
            match apply_changes document.text params.content_changes with
            | Error _ -> ok state []
            | Ok text ->
                let document = {
                  uri = document.uri;
                  version = params.text_document.version;
                  text;
                  path = document.path;
                }
                in
                let state = upsert_document state document in
                ok state [ publish_diagnostics state document ]
          )
      )

let handle_did_close = fun state ->
  fun payload ->
    match Lsp.notification_of_json Lsp.Text_document_methods.Did_close.notification payload with
    | Error _reason -> ok state []
    | Ok params -> (
        match find_document state params.text_document.uri with
        | None ->
            let state = remove_document state params.text_document.uri in
            ok state [ clear_diagnostics params.text_document.uri ]
        | Some _document ->
            let state = remove_document state params.text_document.uri in
            ok state [ clear_diagnostics params.text_document.uri ]
      )

let handle_notification = fun state ->
  fun request ->
    fun payload ->
      if not state.initialized then
        match request.Jsonrpc.method_ with
        | "exit" -> ok state ~exit_code:1 []
        | _ -> ok state []
      else
        match request.Jsonrpc.method_ with
        | "initialized" -> ok state []
        | "textDocument/didOpen" -> handle_did_open state payload
        | "textDocument/didChange" -> handle_did_change state payload
        | "textDocument/didClose" -> handle_did_close state payload
        | "exit" ->
            let exit_code =
              if state.shutdown_requested then
                0
              else
                1
            in
            ok state ~exit_code []
        | _ -> ok state []

let handle_payload = fun state ->
  fun payload ->
    match Json.of_string payload with
    | Error error ->
        ok
          state
          [
            response_error
              ~id:Jsonrpc.Null
              ~code:Lsp.Error_code.parse_error
              ~message:(Json.error_to_string error)
              ();
          ]
    | Ok json -> (
        match Jsonrpc.request_of_json json with
        | Error reason ->
            ok
              state
              [
                response_error
                  ~id:Jsonrpc.Null
                  ~code:Lsp.Error_code.invalid_request
                  ~message:reason
                  ();
              ]
        | Ok request -> (
            match request.Jsonrpc.id with
            | Some _ -> handle_request state request json
            | None -> handle_notification state request json
          )
      )
