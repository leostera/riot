open Std

module Error = Macro_error
module Environment = Macro_environment
module Parser = Macro_parser
module Format = Macro_format
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

let expand_source = Expander.expand_source
