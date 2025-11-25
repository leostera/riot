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
  request_streaming : bool;
  response_streaming : bool;
}

type call_config = {
  timeout : Metadata.timeout option;
  metadata : Metadata.t;
  max_message_size : int option;
  compression : Metadata.encoding option;
}

let unary_method ~service ~method_ =
  {
    service;
    method_;
    method_type = Unary;
    request_streaming = false;
    response_streaming = false;
  }

let server_streaming_method ~service ~method_ =
  {
    service;
    method_;
    method_type = ServerStreaming;
    request_streaming = false;
    response_streaming = true;
  }

let client_streaming_method ~service ~method_ =
  {
    service;
    method_;
    method_type = ClientStreaming;
    request_streaming = true;
    response_streaming = false;
  }

let bidi_streaming_method ~service ~method_ =
  {
    service;
    method_;
    method_type = BidiStreaming;
    request_streaming = true;
    response_streaming = true;
  }

let default_config =
  {
    timeout = None;
    metadata = Metadata.empty;
    max_message_size = None;
    compression = None;
  }

let with_timeout config timeout = { config with timeout = Some timeout }

let with_metadata config metadata =
  { config with metadata = Metadata.add_all config.metadata metadata }

let with_max_message_size config size =
  { config with max_message_size = Some size }

let with_compression config compression =
  { config with compression = Some compression }

let method_path method_def = format "/%s/%s" method_def.service method_def.method_
