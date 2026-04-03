open Std
open Std.Data

let ( let* ) = Result.and_then

type document = {
  uri: Lsp.Uri.t;
  version: int;
  text: string;
}

type t = {
  initialized: bool;
  shutdown_requested: bool;
  documents: document list;
}

type outcome = {
  state: t;
  outbound: Json.t list;
  exit_code: int option;
}

let empty = { initialized = false; shutdown_requested = false; documents = [] }

let uri_equal = fun left -> fun right ->
  String.equal (Lsp.Uri.to_string left) (Lsp.Uri.to_string right)

let upsert_document = fun state document ->
  {
    state with
    documents = document :: List.filter (fun existing -> not (uri_equal existing.uri document.uri)) state.documents;
  }

let remove_document = fun state uri ->
  { state with documents = List.filter (fun document -> not (uri_equal document.uri uri)) state.documents }

let find_document = fun state uri ->
  List.find_opt (fun document -> uri_equal document.uri uri) state.documents

let response_error = fun ~id ~code ~message ?data () ->
  Lsp.error_response_to_json ~id Lsp.{ code; message; data }

let ok = fun state ?exit_code outbound ->
  { state; outbound; exit_code }

let filename_of_uri = fun uri ->
  match Lsp.Uri.to_path uri with
  | Ok path -> path
  | Error _ -> Path.v "buffer.ml"

let diagnostic_to_lsp = fun text -> fun (diagnostic : Syn.Diagnostic.t) ->
  {
    Lsp.Diagnostic.range =
      Lsp.Utf16.range_of_offsets text ~start_offset:diagnostic.span.start ~end_offset:diagnostic.span.end_;
    severity = Some Lsp.Diagnostic.Error;
    code = Some (Syn.Diagnostic.id diagnostic);
    source = Some "syn";
    message = Syn.Diagnostic.main_message diagnostic;
    tags = None;
    data = Some (Syn.Diagnostic.to_json diagnostic);
  }

let publish_diagnostics = fun document ->
  let diagnostics =
    Syn.parse ~filename:(filename_of_uri document.uri) document.text
    |> fun result -> result.diagnostics
    |> List.map (diagnostic_to_lsp document.text)
  in
  let params : Lsp.Text_document_methods.Publish_diagnostics.params = {
    uri = document.uri;
    version = Some document.version;
    diagnostics;
  } in
  Lsp.notification_to_json Lsp.Text_document_methods.Publish_diagnostics.notification params

let clear_diagnostics = fun uri ->
  let params : Lsp.Text_document_methods.Publish_diagnostics.params = {
    uri;
    version = None;
    diagnostics = [];
  } in
  Lsp.notification_to_json Lsp.Text_document_methods.Publish_diagnostics.notification params

let splice_text = fun text -> fun range -> fun replacement ->
  let* start_offset = Lsp.Utf16.offset_of_position text range.Lsp.Range.start_ in
  let* end_offset = Lsp.Utf16.offset_of_position text range.end_ in
  if start_offset > end_offset then
    Error "invalid text edit range"
  else
    let prefix = String.sub text 0 start_offset in
    let suffix = String.sub text end_offset (String.length text - end_offset) in
    Ok (prefix ^ replacement ^ suffix)

let apply_change = fun text -> fun (change : Lsp.Text_document.content_change_event) ->
  match change.range with
  | None -> Ok change.text
  | Some range -> splice_text text range change.text

let apply_changes = fun text -> fun changes ->
  List.fold_left
    (fun acc change ->
      let* current = acc in
      apply_change current change)
    (Ok text) changes

let capabilities =
  {
    Lsp.Initialize.Server_capabilities.position_encoding = Some "utf-16";
    text_document_sync =
      Some
        (Lsp.Initialize.Server_capabilities.Sync_options
           { open_close = Some true; change = Some Lsp.Text_document.Sync_kind.Full; save = None });
    document_formatting_provider = Some false;
    code_action_provider = Some (Lsp.Initialize.Server_capabilities.Bool false);
    experimental = None;
  }

let initialize_result : Lsp.Initialize.result =
  { capabilities; server_info = Some { Lsp.Server_info.name = "riot-lsp"; version = None } }

let debug_json = fun state ->
  let documents =
    state.documents
    |> List.sort (fun left right -> String.compare (Lsp.Uri.to_string left.uri) (Lsp.Uri.to_string right.uri))
    |> List.map (fun document ->
      Json.obj
        [
          ("uri", Lsp.Uri.to_json document.uri);
          ("version", Json.int document.version);
        ])
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
      ( "exitCode",
        match outcome.exit_code with
        | None -> Json.Null
        | Some code -> Json.int code );
      ("state", debug_json outcome.state);
    ]

let handle_initialize = fun state -> fun payload ->
  match Lsp.request_of_json Lsp.Initialize.request payload with
  | Error reason ->
      ok state [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
  | Ok (id, _params) ->
      if state.initialized then
        ok state [ response_error ~id ~code:Lsp.Error_code.invalid_request ~message:"initialize was already called" () ]
      else
        let state = { state with initialized = true; shutdown_requested = false } in
        ok state [ Lsp.response_to_json ~id Lsp.Initialize.request initialize_result ]

let handle_shutdown = fun state -> fun payload ->
  match Lsp.request_of_json Lsp.Shutdown.request payload with
  | Error reason ->
      ok state [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_params ~message:reason () ]
  | Ok (id, ()) ->
      let state = { state with shutdown_requested = true } in
      ok state [ Lsp.response_to_json ~id Lsp.Shutdown.request () ]

let handle_request = fun state -> fun request -> fun payload ->
  if (not state.initialized) && not (String.equal request.Jsonrpc.method_ "initialize") then
    let id = Option.unwrap_or request.Jsonrpc.id ~default:Jsonrpc.Null in
    ok state
      [ response_error ~id ~code:Lsp.Error_code.server_not_initialized ~message:"server not initialized" () ]
  else
    match request.Jsonrpc.method_ with
    | "initialize" -> handle_initialize state payload
    | "shutdown" -> handle_shutdown state payload
    | method_ ->
        let id = Option.unwrap_or request.Jsonrpc.id ~default:Jsonrpc.Null in
        ok state [ response_error ~id ~code:Lsp.Error_code.method_not_found ~message:("unknown method `" ^ method_ ^ "`") () ]

let handle_did_open = fun state -> fun payload ->
  match Lsp.notification_of_json Lsp.Text_document_methods.Did_open.notification payload with
  | Error _reason -> ok state []
  | Ok params ->
      let document =
        {
          uri = params.text_document.uri;
          version = params.text_document.version;
          text = params.text_document.text;
        }
      in
      let state = upsert_document state document in
      ok state [ publish_diagnostics document ]

let handle_did_change = fun state -> fun payload ->
  match Lsp.notification_of_json Lsp.Text_document_methods.Did_change.notification payload with
  | Error _reason -> ok state []
  | Ok params -> (
      match find_document state params.text_document.uri with
      | None -> ok state []
      | Some document -> (
          match apply_changes document.text params.content_changes with
          | Error _ -> ok state []
          | Ok text ->
              let document = { uri = document.uri; version = params.text_document.version; text } in
              let state = upsert_document state document in
              ok state [ publish_diagnostics document ]
        )
    )

let handle_did_close = fun state -> fun payload ->
  match Lsp.notification_of_json Lsp.Text_document_methods.Did_close.notification payload with
  | Error _reason -> ok state []
  | Ok params ->
      let state = remove_document state params.text_document.uri in
      ok state [ clear_diagnostics params.text_document.uri ]

let handle_notification = fun state -> fun request -> fun payload ->
  if not state.initialized then
    match request.Jsonrpc.method_ with
    | "exit" ->
        ok state ~exit_code:1 []
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

let handle_payload = fun state -> fun payload ->
  match Json.of_string payload with
  | Error error ->
      ok state
        [
          response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.parse_error
            ~message:(Json.error_to_string error) ();
        ]
  | Ok json -> (
      match Jsonrpc.request_of_json json with
      | Error reason ->
          ok state [ response_error ~id:Jsonrpc.Null ~code:Lsp.Error_code.invalid_request ~message:reason () ]
      | Ok request -> (
          match request.Jsonrpc.id with
          | Some _ -> handle_request state request json
          | None -> handle_notification state request json
        )
    )
