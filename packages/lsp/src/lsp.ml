open Std
open Std.Data

type json = Json.t
type request_id = Jsonrpc.id

type response_error = {
  code: int;
  message: string;
  data: json option;
}

let ( let* ) = Result.and_then

let ignore_context = fun decode -> fun _ -> decode

let string_contains_char = fun s -> fun ch ->
  match String.index_opt s ch with
  | Some _ -> true
  | None -> false

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } ->
      "system call '" ^ syscall ^ "' returned invalid UTF-8 path: " ^ path
  | Path.SystemError error -> error

let unsupported_json_kind = fun expected -> function
  | Json.Null -> expected ^ " must not be null"
  | Json.Bool _ -> expected ^ " must be a JSON " ^ expected ^ ", found bool"
  | Json.Int _ -> expected ^ " must be a JSON " ^ expected ^ ", found int"
  | Json.Float _ -> expected ^ " must be a JSON " ^ expected ^ ", found float"
  | Json.String _ -> expected ^ " must be a JSON " ^ expected ^ ", found string"
  | Json.Array _ -> expected ^ " must be a JSON " ^ expected ^ ", found array"
  | Json.Object _ -> expected ^ " must be a JSON " ^ expected ^ ", found object"

module Decode = struct
  let field = fun name -> fun fields -> List.assoc_opt name fields

  let object_fields = fun context -> function
    | Json.Object fields -> Ok fields
    | value -> Error (unsupported_json_kind (context ^ " object") value)

  let string = fun context -> function
    | Json.String value -> Ok value
    | value -> Error (unsupported_json_kind (context ^ " string") value)

  let int = fun context -> function
    | Json.Int value -> Ok value
    | value -> Error (unsupported_json_kind (context ^ " int") value)

  let bool = fun context -> function
    | Json.Bool value -> Ok value
    | value -> Error (unsupported_json_kind (context ^ " bool") value)

  let list = fun context -> fun decode -> function
    | Json.Array items ->
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | item :: rest ->
              let* decoded = decode item in
              loop (decoded :: acc) rest
        in
        loop [] items
    | value -> Error (unsupported_json_kind (context ^ " array") value)

  let required = fun context -> fun name -> fun decode -> fun fields ->
    match field name fields with
    | Some value -> decode (context ^ "." ^ name) value
    | None -> Error ("missing required field `" ^ context ^ "." ^ name ^ "`")

  let optional = fun context -> fun name -> fun decode -> fun fields ->
    match field name fields with
    | None -> Ok None
    | Some value ->
        let* decoded = decode (context ^ "." ^ name) value in
        Ok (Some decoded)
end

module Encode = struct
  let field_opt = fun name -> fun encode -> fun value -> fun fields ->
    match value with
    | None -> fields
    | Some value -> (name, encode value) :: fields
end

module Error_code = struct
  let parse_error = -32_700
  let invalid_request = -32_600
  let method_not_found = -32_601
  let invalid_params = -32_602
  let internal_error = -32_603
  let server_not_initialized = -32_002
  let request_cancelled = -32_800
  let content_modified = -32_801
end

module Uri = struct
  type t = string

  let of_string = fun value -> value
  let to_string = fun value -> value

  let encode_path = fun path ->
    Path.to_string path |> String.split_on_char '/' |> List.map Net.Uri.percent_encode |> String.concat "/"

  let of_path = fun path ->
    "file://" ^ encode_path path

  let to_path = fun value ->
    match Net.Uri.of_string value with
    | Error _ -> Error ("invalid URI: " ^ value)
    | Ok uri -> (
        match Net.Uri.scheme uri with
        | Some "file" -> (
            match Net.Uri.authority uri with
            | None
            | Some ""
            | Some "localhost" ->
                let path = Net.Uri.path uri |> Net.Uri.percent_decode in
                Path.of_string path |> Result.map_error path_error_message
            | Some authority ->
                Error ("unsupported file URI authority: " ^ authority)
          )
        | Some scheme -> Error ("unsupported URI scheme: " ^ scheme)
        | None -> Error "URI is missing a scheme"
      )

  let to_json = fun value -> Json.String value

  let of_json = fun value -> Decode.string "uri" value
end

module Position = struct
  type t = {
    line: int;
    character: int;
  }

  let to_json = fun { line; character } ->
    Json.obj [ ("line", Json.int line); ("character", Json.int character) ]

  let of_json = fun value ->
    let* fields = Decode.object_fields "position" value in
    let* line = Decode.required "position" "line" Decode.int fields in
    let* character = Decode.required "position" "character" Decode.int fields in
    Ok { line; character }
end

module Range = struct
  type t = {
    start_: Position.t;
    end_: Position.t;
  }

  let to_json = fun { start_; end_ } ->
    Json.obj [ ("start", Position.to_json start_); ("end", Position.to_json end_) ]

  let of_json = fun value ->
    let* fields = Decode.object_fields "range" value in
    let* start_ = Decode.required "range" "start" (ignore_context Position.of_json) fields in
    let* end_ = Decode.required "range" "end" (ignore_context Position.of_json) fields in
    Ok { start_; end_ }
end

module Diagnostic = struct
  type severity =
    | Error
    | Warning
    | Information
    | Hint

  type tag =
    | Unnecessary
    | Deprecated

  type t = {
    range: Range.t;
    severity: severity option;
    code: string option;
    source: string option;
    message: string;
    tags: tag list option;
    data: json option;
  }

  let severity_to_int = function
    | Error -> 1
    | Warning -> 2
    | Information -> 3
    | Hint -> 4

  let severity_of_int = function
    | 1 -> Ok Error
    | 2 -> Ok Warning
    | 3 -> Ok Information
    | 4 -> Ok Hint
    | value -> Error ("invalid diagnostic severity: " ^ Int.to_string value)

  let tag_to_int = function
    | Unnecessary -> 1
    | Deprecated -> 2

  let tag_of_int = function
    | 1 -> Ok Unnecessary
    | 2 -> Ok Deprecated
    | value -> Error ("invalid diagnostic tag: " ^ Int.to_string value)

  let to_json = fun { range; severity; code; source; message; tags; data } ->
    let fields = [ ("range", Range.to_json range); ("message", Json.string message) ] in
    let fields = Encode.field_opt "severity" (fun severity -> Json.int (severity_to_int severity)) severity fields in
    let fields = Encode.field_opt "code" Json.string code fields in
    let fields = Encode.field_opt "source" Json.string source fields in
    let fields =
      Encode.field_opt "tags" (fun tags -> Json.array (List.map (fun tag -> Json.int (tag_to_int tag)) tags)) tags
        fields
    in
    let fields = Encode.field_opt "data" (fun value -> value) data fields in
    Json.obj (List.rev fields)

  let of_json = fun value ->
    let* fields = Decode.object_fields "diagnostic" value in
    let* range = Decode.required "diagnostic" "range" (ignore_context Range.of_json) fields in
    let* severity =
      Decode.optional "diagnostic" "severity" (ignore_context (fun value ->
        let* value = Decode.int "diagnostic.severity" value in
        severity_of_int value)) fields
    in
    let* code = Decode.optional "diagnostic" "code" Decode.string fields in
    let* source = Decode.optional "diagnostic" "source" Decode.string fields in
    let* message = Decode.required "diagnostic" "message" Decode.string fields in
    let* tags =
      Decode.optional "diagnostic" "tags" (ignore_context (fun value ->
        Decode.list "diagnostic.tags" (fun value ->
          let* value = Decode.int "diagnostic.tags[]" value in
          tag_of_int value) value)) fields
    in
    let data = Decode.field "data" fields in
    Ok { range; severity; code; source; message; tags; data }
end

module Text_edit = struct
  type t = {
    range: Range.t;
    new_text: string;
  }

  let to_json = fun { range; new_text } ->
    Json.obj [ ("range", Range.to_json range); ("newText", Json.string new_text) ]

  let of_json = fun value ->
    let* fields = Decode.object_fields "textEdit" value in
    let* range = Decode.required "textEdit" "range" (ignore_context Range.of_json) fields in
    let* new_text = Decode.required "textEdit" "newText" Decode.string fields in
    Ok { range; new_text }
end

module Workspace_edit = struct
  type t = {
    changes: (Uri.t * Text_edit.t list) list;
  }

  let to_json = fun { changes } ->
    let changes =
      changes |> List.map (fun (uri, edits) -> (Uri.to_string uri, Json.array (List.map Text_edit.to_json edits)))
    in
    Json.obj [ ("changes", Json.obj changes) ]

  let of_json = fun value ->
    let* fields = Decode.object_fields "workspaceEdit" value in
    let* changes_fields =
      match Decode.field "changes" fields with
      | Some (Json.Object changes) -> Ok changes
      | Some value -> Error (unsupported_json_kind "workspaceEdit.changes object" value)
      | None -> Error "missing required field `workspaceEdit.changes`"
    in
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | (uri, edits_json) :: rest ->
          let* edits = Decode.list "workspaceEdit.changes" Text_edit.of_json edits_json in
          loop ((Uri.of_string uri, edits) :: acc) rest
    in
    let* changes = loop [] changes_fields in
    Ok { changes }
end

module Command = struct
  type t = {
    title: string;
    command: string;
    arguments: json list option;
  }

  let to_json = fun { title; command; arguments } ->
    let fields = [ ("title", Json.string title); ("command", Json.string command) ] in
    let fields = Encode.field_opt "arguments" Json.array arguments fields in
    Json.obj (List.rev fields)

  let of_json = fun value ->
    let* fields = Decode.object_fields "command" value in
    let* title = Decode.required "command" "title" Decode.string fields in
    let* command = Decode.required "command" "command" Decode.string fields in
    let* arguments =
      match Decode.field "arguments" fields with
      | None -> Ok None
      | Some (Json.Array items) -> Ok (Some items)
      | Some value -> Error (unsupported_json_kind "command.arguments array" value)
    in
    Ok { title; command; arguments }
end

module Action_kind = struct
  type t =
    | Quick_fix
    | Refactor
    | Refactor_extract
    | Refactor_inline
    | Refactor_rewrite
    | Source
    | Source_fix_all
    | Custom of string

  let to_string = function
    | Quick_fix -> "quickfix"
    | Refactor -> "refactor"
    | Refactor_extract -> "refactor.extract"
    | Refactor_inline -> "refactor.inline"
    | Refactor_rewrite -> "refactor.rewrite"
    | Source -> "source"
    | Source_fix_all -> "source.fixAll"
    | Custom value -> value

  let of_string = function
    | "quickfix" -> Quick_fix
    | "refactor" -> Refactor
    | "refactor.extract" -> Refactor_extract
    | "refactor.inline" -> Refactor_inline
    | "refactor.rewrite" -> Refactor_rewrite
    | "source" -> Source
    | "source.fixAll" -> Source_fix_all
    | value -> Custom value

  let to_json = fun value -> Json.string (to_string value)

  let of_json = fun value ->
    let* value = Decode.string "actionKind" value in
    Ok (of_string value)
end

module Code_action = struct
  type t = {
    title: string;
    kind: Action_kind.t option;
    diagnostics: Diagnostic.t list option;
    is_preferred: bool option;
    edit: Workspace_edit.t option;
    command: Command.t option;
    data: json option;
  }

  let to_json = fun { title; kind; diagnostics; is_preferred; edit; command; data } ->
    let fields = [ ("title", Json.string title) ] in
    let fields = Encode.field_opt "kind" Action_kind.to_json kind fields in
    let fields =
      Encode.field_opt "diagnostics" (fun diagnostics -> Json.array (List.map Diagnostic.to_json diagnostics))
        diagnostics fields
    in
    let fields = Encode.field_opt "isPreferred" Json.bool is_preferred fields in
    let fields = Encode.field_opt "edit" Workspace_edit.to_json edit fields in
    let fields = Encode.field_opt "command" Command.to_json command fields in
    let fields = Encode.field_opt "data" (fun value -> value) data fields in
    Json.obj (List.rev fields)

  let of_json = fun value ->
    let* fields = Decode.object_fields "codeAction" value in
    let* title = Decode.required "codeAction" "title" Decode.string fields in
    let* kind = Decode.optional "codeAction" "kind" (ignore_context Action_kind.of_json) fields in
    let* diagnostics =
      Decode.optional "codeAction" "diagnostics" (ignore_context (Decode.list "codeAction.diagnostics" Diagnostic.of_json))
        fields
    in
    let* is_preferred = Decode.optional "codeAction" "isPreferred" Decode.bool fields in
    let* edit = Decode.optional "codeAction" "edit" (ignore_context Workspace_edit.of_json) fields in
    let* command = Decode.optional "codeAction" "command" (ignore_context Command.of_json) fields in
    let data = Decode.field "data" fields in
    Ok { title; kind; diagnostics; is_preferred; edit; command; data }
end

module Code_action_or_command = struct
  type t =
    | Action of Code_action.t
    | Command of Command.t

  let to_json = function
    | Action action -> Code_action.to_json action
    | Command command -> Command.to_json command

  let of_json = fun value ->
    let* fields = Decode.object_fields "codeActionOrCommand" value in
    let is_command_only =
      Option.is_some (Decode.field "command" fields)
      && Decode.field "edit" fields = None
      && Decode.field "diagnostics" fields = None
      && Decode.field "kind" fields = None
      && Decode.field "isPreferred" fields = None
      && Decode.field "data" fields = None
    in
    if is_command_only then
      Command.of_json value |> Result.map (fun command -> Command command)
    else
      Code_action.of_json value |> Result.map (fun action -> Action action)
end

module Client_info = struct
  type t = {
    name: string;
    version: string option;
  }

  let to_json = fun { name; version } ->
    let fields = [ ("name", Json.string name) ] in
    let fields = Encode.field_opt "version" Json.string version fields in
    Json.obj (List.rev fields)

  let of_json = fun value ->
    let* fields = Decode.object_fields "clientInfo" value in
    let* name = Decode.required "clientInfo" "name" Decode.string fields in
    let* version = Decode.optional "clientInfo" "version" Decode.string fields in
    Ok { name; version }
end

module Server_info = struct
  type t = {
    name: string;
    version: string option;
  }

  let to_json = fun { name; version } ->
    let fields = [ ("name", Json.string name) ] in
    let fields = Encode.field_opt "version" Json.string version fields in
    Json.obj (List.rev fields)

  let of_json = fun value ->
    let* fields = Decode.object_fields "serverInfo" value in
    let* name = Decode.required "serverInfo" "name" Decode.string fields in
    let* version = Decode.optional "serverInfo" "version" Decode.string fields in
    Ok { name; version }
end

module Workspace_folder = struct
  type t = {
    uri: Uri.t;
    name: string;
  }

  let to_json = fun { uri; name } ->
    Json.obj [ ("uri", Uri.to_json uri); ("name", Json.string name) ]

  let of_json = fun value ->
    let* fields = Decode.object_fields "workspaceFolder" value in
    let* uri = Decode.required "workspaceFolder" "uri" (ignore_context Uri.of_json) fields in
    let* name = Decode.required "workspaceFolder" "name" Decode.string fields in
    Ok { uri; name }
end

module Text_document = struct
  type identifier = {
    uri: Uri.t;
  }

  type versioned_identifier = {
    uri: Uri.t;
    version: int;
  }

  type item = {
    uri: Uri.t;
    language_id: string;
    version: int;
    text: string;
  }

  module Sync_kind = struct
    type t =
      | None_
      | Full
      | Incremental

    let to_int = function
      | None_ -> 0
      | Full -> 1
      | Incremental -> 2

    let of_int = function
      | 0 -> Ok None_
      | 1 -> Ok Full
      | 2 -> Ok Incremental
      | value -> Error ("invalid text document sync kind: " ^ Int.to_string value)

    let to_json = fun value -> Json.int (to_int value)

    let of_json = fun value ->
      let* value = Decode.int "textDocumentSyncKind" value in
      of_int value
  end

  type sync_options = {
    open_close: bool option;
    change: Sync_kind.t option;
    save: bool option;
  }

  type content_change_event = {
    range: Range.t option;
    range_length: int option;
    text: string;
  }

  type formatting_options = {
    tab_size: int;
    insert_spaces: bool;
    trim_trailing_whitespace: bool option;
    insert_final_newline: bool option;
    trim_final_newlines: bool option;
    extra: (string * json) list;
  }

  module Identifier = struct
    type t = identifier

    let to_json = fun ({ uri } : identifier) -> Json.obj [ ("uri", Uri.to_json uri) ]

    let of_json = fun value ->
      let* fields = Decode.object_fields "textDocumentIdentifier" value in
      let* uri = Decode.required "textDocumentIdentifier" "uri" (ignore_context Uri.of_json) fields in
      Ok { uri }
  end

  module Versioned_identifier = struct
    type t = versioned_identifier

    let to_json = fun ({ uri; version } : versioned_identifier) ->
      Json.obj [ ("uri", Uri.to_json uri); ("version", Json.int version) ]

    let of_json = fun value ->
      let* fields = Decode.object_fields "versionedTextDocumentIdentifier" value in
      let* uri =
        Decode.required "versionedTextDocumentIdentifier" "uri" (ignore_context Uri.of_json) fields
      in
      let* version = Decode.required "versionedTextDocumentIdentifier" "version" Decode.int fields in
      Ok { uri; version }
  end

  module Item = struct
    type t = item

    let to_json = fun ({ uri; language_id; version; text } : item) ->
      Json.obj [
        ("uri", Uri.to_json uri);
        ("languageId", Json.string language_id);
        ("version", Json.int version);
        ("text", Json.string text);
      ]

    let of_json = fun value ->
      let* fields = Decode.object_fields "textDocumentItem" value in
      let* uri = Decode.required "textDocumentItem" "uri" (ignore_context Uri.of_json) fields in
      let* language_id = Decode.required "textDocumentItem" "languageId" Decode.string fields in
      let* version = Decode.required "textDocumentItem" "version" Decode.int fields in
      let* text = Decode.required "textDocumentItem" "text" Decode.string fields in
      Ok { uri; language_id; version; text }
  end

  let sync_options_to_json = fun { open_close; change; save } ->
    let fields = [] in
    let fields = Encode.field_opt "openClose" Json.bool open_close fields in
    let fields = Encode.field_opt "change" Sync_kind.to_json change fields in
    let fields = Encode.field_opt "save" Json.bool save fields in
    Json.obj (List.rev fields)

  let sync_options_of_json = fun value ->
    let* fields = Decode.object_fields "textDocumentSync" value in
    let* open_close = Decode.optional "textDocumentSync" "openClose" Decode.bool fields in
    let* change = Decode.optional "textDocumentSync" "change" (ignore_context Sync_kind.of_json) fields in
    let* save = Decode.optional "textDocumentSync" "save" Decode.bool fields in
    Ok { open_close; change; save }

  let content_change_event_to_json = fun { range; range_length; text } ->
    let fields = [ ("text", Json.string text) ] in
    let fields = Encode.field_opt "range" Range.to_json range fields in
    let fields = Encode.field_opt "rangeLength" Json.int range_length fields in
    Json.obj (List.rev fields)

  let content_change_event_of_json = fun value ->
    let* fields = Decode.object_fields "contentChangeEvent" value in
    let* range = Decode.optional "contentChangeEvent" "range" (ignore_context Range.of_json) fields in
    let* range_length = Decode.optional "contentChangeEvent" "rangeLength" Decode.int fields in
    let* text = Decode.required "contentChangeEvent" "text" Decode.string fields in
    Ok { range; range_length; text }

  let formatting_options_to_json = fun options ->
    let fields = List.rev options.extra in
    let fields = ("tabSize", Json.int options.tab_size) :: fields in
    let fields = ("insertSpaces", Json.bool options.insert_spaces) :: fields in
    let fields =
      Encode.field_opt "trimTrailingWhitespace" Json.bool options.trim_trailing_whitespace fields
    in
    let fields = Encode.field_opt "insertFinalNewline" Json.bool options.insert_final_newline fields in
    let fields = Encode.field_opt "trimFinalNewlines" Json.bool options.trim_final_newlines fields in
    Json.obj (List.rev fields)

  let formatting_options_of_json = fun value ->
    let* fields = Decode.object_fields "formattingOptions" value in
    let* tab_size = Decode.required "formattingOptions" "tabSize" Decode.int fields in
    let* insert_spaces = Decode.required "formattingOptions" "insertSpaces" Decode.bool fields in
    let* trim_trailing_whitespace =
      Decode.optional "formattingOptions" "trimTrailingWhitespace" Decode.bool fields
    in
    let* insert_final_newline =
      Decode.optional "formattingOptions" "insertFinalNewline" Decode.bool fields
    in
    let* trim_final_newlines = Decode.optional "formattingOptions" "trimFinalNewlines" Decode.bool fields in
    let is_known = function
      | "tabSize"
      | "insertSpaces"
      | "trimTrailingWhitespace"
      | "insertFinalNewline"
      | "trimFinalNewlines" -> true
      | _ -> false
    in
    let extra = List.filter (fun (name, _) -> not (is_known name)) fields in
    Ok {
      tab_size;
      insert_spaces;
      trim_trailing_whitespace;
      insert_final_newline;
      trim_final_newlines;
      extra;
    }
end

module Method = struct
  type ('params, 'result) request = {
    name: string;
    params_of_jsonrpc: Jsonrpc.params -> ('params, string) result;
    params_to_jsonrpc: 'params -> Jsonrpc.params;
    result_of_json: Json.t -> ('result, string) result;
    result_to_json: 'result -> Json.t;
  }

  type 'params notification = {
    name: string;
    params_of_jsonrpc: Jsonrpc.params -> ('params, string) result;
    params_to_jsonrpc: 'params -> Jsonrpc.params;
  }

  let request =
    fun ~name -> fun ~params_of_jsonrpc -> fun ~params_to_jsonrpc -> fun ~result_of_json -> fun ~result_to_json ->
    { name; params_of_jsonrpc; params_to_jsonrpc; result_of_json; result_to_json }

  let notification = fun ~name -> fun ~params_of_jsonrpc -> fun ~params_to_jsonrpc ->
    { name; params_of_jsonrpc; params_to_jsonrpc }
end

module Params = struct
  let object_params = fun context -> fun decode -> function
    | Jsonrpc.Named fields -> decode fields
    | Jsonrpc.NoParams -> decode []
    | Jsonrpc.Positional _ ->
        Error (context ^ " expects named object params")

  let no_params = fun context -> function
    | Jsonrpc.NoParams
    | Jsonrpc.Named [] -> Ok ()
    | Jsonrpc.Named _ -> Error (context ^ " expects no params")
    | Jsonrpc.Positional [] -> Ok ()
    | Jsonrpc.Positional _ -> Error (context ^ " expects no params")

  let named = fun fields ->
    if List.is_empty fields then
      Jsonrpc.Named []
    else
      Jsonrpc.Named fields
end

module Initialize = struct
  module Server_capabilities = struct
    type text_document_sync =
      | Kind of Text_document.Sync_kind.t
      | Sync_options of Text_document.sync_options

    type code_action_options = {
      code_action_kinds: Action_kind.t list option;
      resolve_provider: bool option;
    }

    type code_action_provider =
      | Bool of bool
      | Provider_options of code_action_options

    type t = {
      position_encoding: string option;
      text_document_sync: text_document_sync option;
      document_formatting_provider: bool option;
      code_action_provider: code_action_provider option;
      experimental: json option;
    }

    let code_action_options_to_json = fun { code_action_kinds; resolve_provider } ->
      let fields = [] in
      let fields =
        Encode.field_opt "codeActionKinds"
          (fun kinds -> Json.array (List.map Action_kind.to_json kinds))
          code_action_kinds fields
      in
      let fields = Encode.field_opt "resolveProvider" Json.bool resolve_provider fields in
      Json.obj (List.rev fields)

    let code_action_options_of_json = fun value ->
      let* fields = Decode.object_fields "serverCapabilities.codeActionProvider" value in
      let* code_action_kinds =
        Decode.optional "serverCapabilities.codeActionProvider" "codeActionKinds"
          (ignore_context (Decode.list "serverCapabilities.codeActionProvider.codeActionKinds" Action_kind.of_json))
          fields
      in
      let* resolve_provider =
        Decode.optional "serverCapabilities.codeActionProvider" "resolveProvider" Decode.bool fields
      in
      Ok { code_action_kinds; resolve_provider }

    let text_document_sync_to_json = function
      | Kind kind -> Text_document.Sync_kind.to_json kind
      | Sync_options options -> Text_document.sync_options_to_json options

    let text_document_sync_of_json = function
      | Json.Int _ as value ->
          let* kind = Text_document.Sync_kind.of_json value in
          Ok (Kind kind)
      | Json.Object _ as value ->
          let* options = Text_document.sync_options_of_json value in
          Ok (Sync_options options)
      | value -> Error (unsupported_json_kind "serverCapabilities.textDocumentSync value" value)

    let code_action_provider_to_json = function
      | Bool value -> Json.bool value
      | Provider_options value -> code_action_options_to_json value

    let code_action_provider_of_json = function
      | Json.Bool value -> Ok (Bool value)
      | Json.Object _ as value ->
          let* options = code_action_options_of_json value in
          Ok (Provider_options options)
      | value -> Error (unsupported_json_kind "serverCapabilities.codeActionProvider value" value)

    let to_json = fun capabilities ->
      let fields = [] in
      let fields = Encode.field_opt "positionEncoding" Json.string capabilities.position_encoding fields in
      let fields =
        Encode.field_opt "textDocumentSync" text_document_sync_to_json capabilities.text_document_sync fields
      in
      let fields =
        Encode.field_opt "documentFormattingProvider" Json.bool capabilities.document_formatting_provider fields
      in
      let fields =
        Encode.field_opt "codeActionProvider" code_action_provider_to_json capabilities.code_action_provider fields
      in
      let fields = Encode.field_opt "experimental" (fun value -> value) capabilities.experimental fields in
      Json.obj (List.rev fields)

    let of_json = fun value ->
      let* fields = Decode.object_fields "serverCapabilities" value in
      let* position_encoding = Decode.optional "serverCapabilities" "positionEncoding" Decode.string fields in
      let* text_document_sync =
        Decode.optional "serverCapabilities" "textDocumentSync" (ignore_context text_document_sync_of_json) fields
      in
      let* document_formatting_provider =
        Decode.optional "serverCapabilities" "documentFormattingProvider" Decode.bool fields
      in
      let* code_action_provider =
        Decode.optional "serverCapabilities" "codeActionProvider" (ignore_context code_action_provider_of_json) fields
      in
      let experimental = Decode.field "experimental" fields in
      Ok {
        position_encoding;
        text_document_sync;
        document_formatting_provider;
        code_action_provider;
        experimental;
      }
  end

  type params = {
    process_id: int option;
    client_info: Client_info.t option;
    root_uri: Uri.t option;
    capabilities: json;
    initialization_options: json option;
    trace: string option;
    workspace_folders: Workspace_folder.t list option;
  }

  type result = {
    capabilities: Server_capabilities.t;
    server_info: Server_info.t option;
  }

  let params_to_jsonrpc = fun (params : params) ->
    let fields = [ ("capabilities", params.capabilities) ] in
    let fields = Encode.field_opt "processId" Json.int params.process_id fields in
    let fields = Encode.field_opt "clientInfo" Client_info.to_json params.client_info fields in
    let fields = Encode.field_opt "rootUri" Uri.to_json params.root_uri fields in
    let fields = Encode.field_opt "initializationOptions" (fun value -> value) params.initialization_options fields in
    let fields = Encode.field_opt "trace" Json.string params.trace fields in
    let fields =
      Encode.field_opt "workspaceFolders"
        (fun folders -> Json.array (List.map Workspace_folder.to_json folders))
        params.workspace_folders fields
    in
    Params.named (List.rev fields)

  let params_of_jsonrpc = Params.object_params "initialize" (fun fields ->
    let* process_id = Decode.optional "initialize" "processId" Decode.int fields in
    let* client_info = Decode.optional "initialize" "clientInfo" (ignore_context Client_info.of_json) fields in
    let* root_uri = Decode.optional "initialize" "rootUri" (ignore_context Uri.of_json) fields in
    let capabilities = Option.unwrap_or (Decode.field "capabilities" fields) ~default:(Json.obj []) in
    let* initialization_options = Ok (Decode.field "initializationOptions" fields) in
    let* trace = Decode.optional "initialize" "trace" Decode.string fields in
    let* workspace_folders =
      Decode.optional "initialize" "workspaceFolders"
        (ignore_context (Decode.list "initialize.workspaceFolders" Workspace_folder.of_json))
        fields
    in
    Ok {
      process_id;
      client_info;
      root_uri;
      capabilities;
      initialization_options;
      trace;
      workspace_folders;
    })

  let result_to_json = fun ({ capabilities; server_info } : result) ->
    let fields = [ ("capabilities", Server_capabilities.to_json capabilities) ] in
    let fields = Encode.field_opt "serverInfo" Server_info.to_json server_info fields in
    Json.obj (List.rev fields)

  let result_of_json = fun value ->
    let* fields = Decode.object_fields "initializeResult" value in
    let* capabilities =
      Decode.required "initializeResult" "capabilities" (ignore_context Server_capabilities.of_json) fields
    in
    let* server_info =
      Decode.optional "initializeResult" "serverInfo" (ignore_context Server_info.of_json) fields
    in
    Ok { capabilities; server_info }

  let request =
    Method.request
      ~name:"initialize"
      ~params_of_jsonrpc
      ~params_to_jsonrpc
      ~result_of_json
      ~result_to_json
end

module Shutdown = struct
  type params = unit
  type result = unit

  let request =
    Method.request
      ~name:"shutdown"
      ~params_of_jsonrpc:(Params.no_params "shutdown")
      ~params_to_jsonrpc:(fun () -> Jsonrpc.NoParams)
      ~result_of_json:(fun value ->
        match value with
        | Json.Null -> Ok ()
        | value -> Error (unsupported_json_kind "shutdown result null" value))
      ~result_to_json:(fun () -> Json.Null)
end

module Initialized = struct
  type params = unit

  let notification =
    Method.notification
      ~name:"initialized"
      ~params_of_jsonrpc:(Params.no_params "initialized")
      ~params_to_jsonrpc:(fun () -> Params.named [])
end

module Exit = struct
  type params = unit

  let notification =
    Method.notification
      ~name:"exit"
      ~params_of_jsonrpc:(Params.no_params "exit")
      ~params_to_jsonrpc:(fun () -> Jsonrpc.NoParams)
end

module Text_document_requests = struct
  module Did_open = struct
    type params = {
      text_document: Text_document.item;
    }

    let params_to_jsonrpc = fun { text_document } ->
      Params.named [ ("textDocument", Text_document.Item.to_json text_document) ]

    let params_of_jsonrpc = Params.object_params "textDocument/didOpen" (fun fields ->
      let* text_document =
        Decode.required "textDocument/didOpen" "textDocument" (ignore_context Text_document.Item.of_json) fields
      in
      Ok { text_document })

    let notification =
      Method.notification
        ~name:"textDocument/didOpen"
        ~params_of_jsonrpc
        ~params_to_jsonrpc
  end

  module Did_change = struct
    type params = {
      text_document: Text_document.versioned_identifier;
      content_changes: Text_document.content_change_event list;
    }

    let params_to_jsonrpc = fun { text_document; content_changes } ->
      Params.named
        [
          ("textDocument", Text_document.Versioned_identifier.to_json text_document);
          ("contentChanges", Json.array (List.map Text_document.content_change_event_to_json content_changes));
        ]

    let params_of_jsonrpc = Params.object_params "textDocument/didChange" (fun fields ->
      let* text_document =
        Decode.required "textDocument/didChange" "textDocument"
          (ignore_context Text_document.Versioned_identifier.of_json)
          fields
      in
      let* content_changes =
        Decode.required "textDocument/didChange" "contentChanges"
          (ignore_context
             (Decode.list "textDocument/didChange.contentChanges" Text_document.content_change_event_of_json))
          fields
      in
      Ok { text_document; content_changes })

    let notification =
      Method.notification
        ~name:"textDocument/didChange"
        ~params_of_jsonrpc
        ~params_to_jsonrpc
  end

  module Did_close = struct
    type params = {
      text_document: Text_document.identifier;
    }

    let params_to_jsonrpc = fun { text_document } ->
      Params.named [ ("textDocument", Text_document.Identifier.to_json text_document) ]

    let params_of_jsonrpc = Params.object_params "textDocument/didClose" (fun fields ->
      let* text_document =
        Decode.required "textDocument/didClose" "textDocument"
          (ignore_context Text_document.Identifier.of_json)
          fields
      in
      Ok { text_document })

    let notification =
      Method.notification
        ~name:"textDocument/didClose"
        ~params_of_jsonrpc
        ~params_to_jsonrpc
  end

  module Publish_diagnostics = struct
    type params = {
      uri: Uri.t;
      version: int option;
      diagnostics: Diagnostic.t list;
    }

    let params_to_jsonrpc = fun { uri; version; diagnostics } ->
      let fields = [
        ("uri", Uri.to_json uri);
        ("diagnostics", Json.array (List.map Diagnostic.to_json diagnostics));
      ] in
      let fields = Encode.field_opt "version" Json.int version fields in
      Params.named (List.rev fields)

    let params_of_jsonrpc = Params.object_params "textDocument/publishDiagnostics" (fun fields ->
      let* uri = Decode.required "textDocument/publishDiagnostics" "uri" (ignore_context Uri.of_json) fields in
      let* version = Decode.optional "textDocument/publishDiagnostics" "version" Decode.int fields in
      let* diagnostics =
        Decode.required "textDocument/publishDiagnostics" "diagnostics"
          (ignore_context (Decode.list "textDocument/publishDiagnostics.diagnostics" Diagnostic.of_json))
          fields
      in
      Ok { uri; version; diagnostics })

    let notification =
      Method.notification
        ~name:"textDocument/publishDiagnostics"
        ~params_of_jsonrpc
        ~params_to_jsonrpc
  end

  module Formatting = struct
    type params = {
      text_document: Text_document.identifier;
      options: Text_document.formatting_options;
    }

    type result = Text_edit.t list option

    let params_to_jsonrpc = fun { text_document; options } ->
      Params.named
        [
          ("textDocument", Text_document.Identifier.to_json text_document);
          ("options", Text_document.formatting_options_to_json options);
        ]

    let params_of_jsonrpc = Params.object_params "textDocument/formatting" (fun fields ->
      let* text_document =
        Decode.required "textDocument/formatting" "textDocument"
          (ignore_context Text_document.Identifier.of_json)
          fields
      in
      let* options =
        Decode.required "textDocument/formatting" "options"
          (ignore_context Text_document.formatting_options_of_json)
          fields
      in
      Ok { text_document; options })

    let result_to_json = function
      | None -> Json.Null
      | Some edits -> Json.array (List.map Text_edit.to_json edits)

    let result_of_json = function
      | Json.Null -> Ok None
      | Json.Array edits as value ->
          let* edits = Decode.list "textDocument/formatting result" Text_edit.of_json value in
          Ok (Some edits)
      | value -> Error (unsupported_json_kind "textDocument/formatting result array" value)

    let request =
      Method.request
        ~name:"textDocument/formatting"
        ~params_of_jsonrpc
        ~params_to_jsonrpc
        ~result_of_json
        ~result_to_json
  end

  module Code_action = struct
    type context = {
      diagnostics: Diagnostic.t list;
      only: Action_kind.t list option;
      trigger_kind: int option;
    }

    type params = {
      text_document: Text_document.identifier;
      range: Range.t;
      context: context;
    }

    type result = Code_action_or_command.t list option

    let context_to_json = fun { diagnostics; only; trigger_kind } ->
      let fields = [ ("diagnostics", Json.array (List.map Diagnostic.to_json diagnostics)) ] in
      let fields =
        Encode.field_opt "only" (fun kinds -> Json.array (List.map Action_kind.to_json kinds)) only fields
      in
      let fields = Encode.field_opt "triggerKind" Json.int trigger_kind fields in
      Json.obj (List.rev fields)

    let context_of_json = fun value ->
      let* fields = Decode.object_fields "codeActionContext" value in
      let* diagnostics =
        Decode.required "codeActionContext" "diagnostics"
          (ignore_context (Decode.list "codeActionContext.diagnostics" Diagnostic.of_json))
          fields
      in
      let* only =
        Decode.optional "codeActionContext" "only"
          (ignore_context (Decode.list "codeActionContext.only" Action_kind.of_json))
          fields
      in
      let* trigger_kind = Decode.optional "codeActionContext" "triggerKind" Decode.int fields in
      Ok { diagnostics; only; trigger_kind }

    let params_to_jsonrpc = fun { text_document; range; context } ->
      Params.named
        [
          ("textDocument", Text_document.Identifier.to_json text_document);
          ("range", Range.to_json range);
          ("context", context_to_json context);
        ]

    let params_of_jsonrpc = Params.object_params "textDocument/codeAction" (fun fields ->
      let* text_document =
        Decode.required "textDocument/codeAction" "textDocument"
          (ignore_context Text_document.Identifier.of_json)
          fields
      in
      let* range = Decode.required "textDocument/codeAction" "range" (ignore_context Range.of_json) fields in
      let* context =
        Decode.required "textDocument/codeAction" "context" (ignore_context context_of_json) fields
      in
      Ok { text_document; range; context })

    let result_to_json = function
      | None -> Json.Null
      | Some actions -> Json.array (List.map Code_action_or_command.to_json actions)

    let result_of_json = function
      | Json.Null -> Ok None
      | Json.Array _ as value ->
          let* actions = Decode.list "textDocument/codeAction result" Code_action_or_command.of_json value in
          Ok (Some actions)
      | value -> Error (unsupported_json_kind "textDocument/codeAction result array" value)

    let request =
      Method.request
        ~name:"textDocument/codeAction"
        ~params_of_jsonrpc
        ~params_to_jsonrpc
        ~result_of_json
        ~result_to_json
  end
end

module Text_document_methods = Text_document_requests

let request_to_json:
  type params res.
  id:Jsonrpc.id -> (params, res) Method.request -> params -> Json.t =
  fun ~id -> fun method_ -> fun params ->
  Jsonrpc.request
    ~method_:method_.Method.name
    ~params:(method_.Method.params_to_jsonrpc params)
    ~id
    ()
  |> Jsonrpc.request_to_json

let request_of_json:
  type params res.
  (params, res) Method.request -> Json.t -> ((Jsonrpc.id * params), string) result =
  fun method_ -> fun json ->
  let* request = Jsonrpc.request_of_json json in
  if not (String.equal request.method_ method_.Method.name) then
    Error ("expected request method `" ^ method_.Method.name ^ "`, found `" ^ request.method_ ^ "`")
  else
    match request.id with
    | None -> Error ("request `" ^ method_.Method.name ^ "` is missing an id")
    | Some id ->
        let* params = method_.Method.params_of_jsonrpc request.params in
        Ok (id, params)

let response_to_json:
  type params res.
  id:Jsonrpc.id -> (params, res) Method.request -> res -> Json.t =
  fun ~id -> fun method_ -> fun result ->
  Json.obj
    [
      ("jsonrpc", Json.string Jsonrpc.version);
      ("id", Jsonrpc.id_to_json id);
      ("result", method_.Method.result_to_json result);
    ]

let response_of_json:
  type params res.
  (params, res) Method.request -> Json.t -> ((Jsonrpc.id * res), string) result =
  fun method_ -> fun json ->
  let* fields = Decode.object_fields "response" json in
  let* jsonrpc = Decode.required "response" "jsonrpc" Decode.string fields in
  if not (String.equal jsonrpc Jsonrpc.version) then
    Error ("unsupported JSON-RPC version `" ^ jsonrpc ^ "`")
  else
    let* id =
      match Decode.field "id" fields with
      | Some value -> Jsonrpc.id_of_json value
      | None -> Error "missing required field `response.id`"
    in
    match Decode.field "error" fields with
    | Some _ -> Error "expected successful response, found error object"
    | None ->
        let* result_json =
          match Decode.field "result" fields with
          | Some value -> Ok value
          | None -> Error "missing required field `response.result`"
        in
        let* result = method_.Method.result_of_json result_json in
        Ok (id, result)

let notification_to_json:
  type params.
  params Method.notification -> params -> Json.t =
  fun method_ -> fun params ->
  Jsonrpc.notification
    ~method_:method_.Method.name
    ~params:(method_.Method.params_to_jsonrpc params)
    ()
  |> Jsonrpc.request_to_json

let notification_of_json:
  type params.
  params Method.notification -> Json.t -> (params, string) result =
  fun method_ -> fun json ->
  let* request = Jsonrpc.request_of_json json in
  if not (String.equal request.method_ method_.Method.name) then
    Error ("expected notification method `" ^ method_.Method.name ^ "`, found `" ^ request.method_ ^ "`")
  else
    match request.id with
    | Some _ -> Error ("notification `" ^ method_.Method.name ^ "` must not include an id")
    | None -> method_.Method.params_of_jsonrpc request.params

let response_error_to_json = fun { code; message; data } ->
  let fields = [ ("code", Json.int code); ("message", Json.string message) ] in
  let fields = Encode.field_opt "data" (fun value -> value) data fields in
  Json.obj (List.rev fields)

let response_error_of_json = fun value ->
  let* fields = Decode.object_fields "responseError" value in
  let* code = Decode.required "responseError" "code" Decode.int fields in
  let* message = Decode.required "responseError" "message" Decode.string fields in
  let data = Decode.field "data" fields in
  Ok { code; message; data }

let error_response_to_json = fun ~id -> fun error ->
  Json.obj
    [
      ("jsonrpc", Json.string Jsonrpc.version);
      ("id", Jsonrpc.id_to_json id);
      ("error", response_error_to_json error);
    ]

let error_response_of_json = fun json ->
  let* fields = Decode.object_fields "errorResponse" json in
  let* jsonrpc = Decode.required "errorResponse" "jsonrpc" Decode.string fields in
  if not (String.equal jsonrpc Jsonrpc.version) then
    Error ("unsupported JSON-RPC version `" ^ jsonrpc ^ "`")
  else
    let* id =
      match Decode.field "id" fields with
      | Some value -> Jsonrpc.id_of_json value
      | None -> Error "missing required field `errorResponse.id`"
    in
    let* error =
      match Decode.field "error" fields with
      | Some value -> response_error_of_json value
      | None -> Error "missing required field `errorResponse.error`"
    in
    Ok (id, error)

module Utf16 = struct
  let position_of_offset = fun text ~offset ->
    let position = Unicode.Utf16.position_of_offset text ~offset in
    let line = position.Unicode.Utf16.line in
    let character = position.Unicode.Utf16.character in
    { Position.line = line; character }

  let offset_of_position = fun text -> fun position ->
    let utf16_position : Unicode.Utf16.position =
      { line = position.Position.line; character = position.character }
    in
    Unicode.Utf16.offset_of_position text utf16_position

  let range_of_offsets = fun text -> fun ~start_offset -> fun ~end_offset ->
    {
      Range.start_ = position_of_offset text ~offset:start_offset;
      end_ = position_of_offset text ~offset:end_offset;
    }
end
