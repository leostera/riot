open Std

let render_literal_source = fun content -> "\"" ^ content ^ "\""

let buffer_name = fun stream ->
  match Macro_token_stream.span stream with
  | Some span ->
      "__riot_macro_format_buffer_" ^ string_of_int span.start ^ "_" ^ string_of_int span.end_
  | None -> "__riot_macro_format_buffer"

let render_builder_program = fun ~buffer_name ~capacity format_items values ->
  let append statement_source statements_rev =
    ("Stdlib.Buffer.add_string " ^ buffer_name ^ " " ^ statement_source) :: statements_rev in
  let rec collect statements_rev remaining_values =
    function
    | [] ->
        if remaining_values = [] then
          Ok (List.rev statements_rev)
        else
          Error "format! placeholder count does not match the number of supplied arguments"
    | Macro_format_parser.String text :: rest ->
        collect
          (append (render_literal_source text) statements_rev)
          remaining_values
          rest
    | Macro_format_parser.Hole Macro_format_parser.Next_arg_to_string :: rest -> (
        match remaining_values with
        | value :: remaining ->
            collect
              (append value statements_rev)
              remaining
              rest
        | [] ->
            Error "format! placeholder count does not match the number of supplied arguments"
      )
    | Macro_format_parser.Hole (Macro_format_parser.Var_to_string name) :: rest ->
        collect
          (append ("(" ^ name ^ ")") statements_rev)
          remaining_values
          rest
  in
  match collect [] values format_items with
  | Error message -> Error message
  | Ok statements ->
      Ok
        ("(let "
        ^ buffer_name
        ^ " = Stdlib.Buffer.create "
        ^ string_of_int capacity
        ^ " in "
        ^ String.concat "; " (statements @ [ "Stdlib.Buffer.contents " ^ buffer_name ])
        ^ ")")

let render_format_program = fun body literal_text format_items rendered_args ->
  match render_builder_program
    ~buffer_name:(buffer_name body)
    ~capacity:(String.length literal_text)
    format_items
    rendered_args with
  | Error message ->
      Macro_result.error
        (Macro_error.make
           ?span:(Macro_token_stream.span body)
           message)
  | Ok source ->
      Macro_result.ok_source source

let expand = fun body ->
  match Macro_parse.expr_arguments body with
  | Error err -> Macro_result.error err
  | Ok [] ->
      Macro_result.error
        (Macro_error.make
           ?span:(Macro_token_stream.span body)
           "format! requires a format string argument")
  | Ok (format_arg :: value_args) ->
      let format_arg = Macro_parse.unwrap_grouping format_arg in
      if Syn.Ceibo.Red.SyntaxNode.kind format_arg != Syn.SyntaxKind.STRING_LITERAL then
        Macro_result.error
          (Macro_error.make
             ?span:(Macro_token_stream.span_of_node format_arg)
             "format! currently requires a string literal as its first argument")
      else
        let literal_text = Macro_token_stream.source_of_node body format_arg in
        match Macro_token_stream.span_of_node format_arg with
        | None ->
            Macro_result.error
              (Macro_error.make
                 ?span:(Macro_token_stream.span body)
                 "format! could not recover the format string span")
        | Some span -> (
            match Macro_format_parser.parse_literal ~literal_text ~span with
            | Error err -> Macro_result.error err
            | Ok format_items ->
                let rendered_args = List.map
                  (fun arg ->
                    let arg = Macro_parse.unwrap_grouping arg in
                    "(" ^ Macro_token_stream.source_of_node body arg ^ ")")
                  value_args in
                render_format_program body literal_text format_items rendered_args
          )

let provider = fun () ->
  Macro_provider.v
    ~module_path:[ "Macro" ]
    [
      Macro_provider.fn "format" expand;
    ]
