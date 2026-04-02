open Std

let rewrite_rust_format_string = fun literal_text ~span ->
  let literal_len = String.length literal_text in
  if literal_len < 2 || literal_text.[0] != '"' || literal_text.[literal_len - 1] != '"' then
    Error
      (Macro_error.make
         ~span
         "format! currently requires an ordinary string literal format string")
  else
    let fragments = ref [ "\"" ] in
    let push_fragment = fun fragment -> fragments := fragment :: !fragments in
    let rec loop index placeholder_count =
      if index >= literal_len - 1 then (
        push_fragment "\"";
        Ok (String.concat "" (List.rev !fragments), placeholder_count)
      ) else
        match literal_text.[index] with
        | '\\' ->
            if index + 1 < literal_len - 1 then (
              push_fragment "\\";
              push_fragment (String.make 1 literal_text.[index + 1]);
              loop (index + 2) placeholder_count
            ) else (
              push_fragment "\\";
              loop (index + 1) placeholder_count
            )
        | '%' ->
            push_fragment "%%";
            loop (index + 1) placeholder_count
        | '{' ->
            if index + 1 >= literal_len - 1 then
              Error
                (Macro_error.make
                   ~span
                   "format! found an unmatched '{' in the format string")
            else
              (
                match literal_text.[index + 1] with
                | '{' ->
                    push_fragment "{";
                    loop (index + 2) placeholder_count
                | '}' ->
                    push_fragment "%s";
                    loop (index + 2) (placeholder_count + 1)
                | _ ->
                    Error
                      (Macro_error.make
                         ~span
                         "format! currently supports only bare {} placeholders and escaped {{ / }} braces")
              )
        | '}' ->
            if index + 1 < literal_len - 1 && literal_text.[index + 1] = '}' then (
              push_fragment "}";
              loop (index + 2) placeholder_count
            ) else
              Error
                (Macro_error.make
                   ~span
                   "format! found an unmatched '}' in the format string")
        | ch ->
            push_fragment (String.make 1 ch);
            loop (index + 1) placeholder_count
    in
    loop 1 0

let expand = fun ~env invocation ->
  match Macro_parser.body_arguments invocation with
  | [] ->
      Error
        (Macro_error.make
           ~span:invocation.span
           "format! requires a format string argument")
  | format_arg :: value_args ->
      let format_arg = Macro_parser.unwrap_grouping format_arg in
      if Syn.Ceibo.Red.SyntaxNode.kind format_arg != Syn.SyntaxKind.STRING_LITERAL then
        Error
          (Macro_error.make
             ?span:(Macro_environment.span_of_node format_arg)
             "format! currently requires a string literal as its first argument")
      else
        let literal_text = Macro_environment.source_of_node env format_arg in
        match Macro_environment.span_of_node format_arg with
        | None ->
            Error
              (Macro_error.make
                 ~span:invocation.span
                 "format! could not recover the format string span")
        | Some span -> (
            match rewrite_rust_format_string literal_text ~span with
            | Error err -> Error err
            | Ok (printf_format, placeholder_count) ->
                if placeholder_count != List.length value_args then
                  Error
                    (Macro_error.make
                       ~span:invocation.span
                       "format! placeholder count does not match the number of supplied arguments")
                else
                  let rendered_args = List.map
                    (fun arg ->
                      let arg = Macro_parser.unwrap_grouping arg in
                      "(" ^ Macro_environment.source_of_node_with_leading_trivia env arg ^ ")")
                    value_args in
                  Ok
                    ("("
                     ^ String.concat " "
                       (("Stdlib.Printf.sprintf " ^ printf_format) :: rendered_args)
                     ^ ")")
          )
