open Std
module Token_stream = Macro_token_stream
module Result = Macro_result
module Error = Macro_error
module Environment = Macro_environment
module Parser = Macro_parser
module Parse = Macro_parse
module Provider = Macro_provider
module Provider_contract = Macro_provider_contract
module Runner = Macro_runner
module Format = Macro_format
module Format_parser = Macro_format_parser
module Validator = Macro_validator
module Expander = Macro_expander

type error = Error.t = {
  message: string;
  span: Syn.Ceibo.Span.t option;
}

type expansion = Expander.expansion = {
  source: string;
  changed: bool;
}

let error_message = Error.message

let builtin_providers = Expander.builtin_providers

let expand_source = Expander.expand_source
