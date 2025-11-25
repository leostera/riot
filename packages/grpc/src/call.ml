open Std

type method_type =
  | Unary
  | ServerStreaming
  | ClientStreaming
  | BidiStreaming

type method_def = {
  service : string;
  method_ : string;
  method_type : method_type;
}

type call_config = {
  timeout : Metadata.timeout option;
  metadata : Metadata.t;
  max_message_size : int option;
  compression : Metadata.encoding option;
}

let unary_method ~service ~method_ =
  { service; method_; method_type = Unary }

let server_streaming_method ~service ~method_ =
  { service; method_; method_type = ServerStreaming }

let client_streaming_method ~service ~method_ =
  { service; method_; method_type = ClientStreaming }

let bidi_streaming_method ~service ~method_ =
  { service; method_; method_type = BidiStreaming }

let is_request_streaming method_def =
  match method_def.method_type with
  | Unary | ServerStreaming -> false
  | ClientStreaming | BidiStreaming -> true

let is_response_streaming method_def =
  match method_def.method_type with
  | Unary | ClientStreaming -> false
  | ServerStreaming | BidiStreaming -> true

let default_config =
  {
    timeout = None;
    metadata = Metadata.empty;
    max_message_size = None;
    compression = None;
  }

let with_timeout config ~timeout = { config with timeout = Some timeout }

let with_metadata config ~metadata =
  { config with metadata = Metadata.add_all config.metadata metadata }

let with_max_message_size config ~max_message_size =
  { config with max_message_size = Some max_message_size }

let with_compression config ~compression =
  { config with compression = Some compression }

let method_path method_def = format "/%s/%s" method_def.service method_def.method_
