open Std
open Std.Collections

type error = {
  message: string;
  span: Syn.Ceibo.Span.t option;
}

type expansion = {
  source: string;
  changed: bool;
}

type invocation = {
  name: string;
  span: Syn.Ceibo.Span.t;
  open_span: Syn.Ceibo.Span.t;
  close_span: Syn.Ceibo.Span.t;
}

type replacement = {
  span: Syn.Ceibo.Span.t;
  source: string;
}

let error_message = fun error -> error.message

let source_of_span = fun source (span: Syn.Ceibo.Span.t) ->
  let width = span.end_ - span.start in
  if width <= 0 then
    ""
  else
    String.sub source span.start width

let token_body_span = fun first_token last_token ->
  let first_span = first_token.Syn.Token.span in
  let last_span = last_token.Syn.Token.span in
  Syn.Ceibo.Span.make ~start:first_span.start ~end_:last_span.end_

let token_slice_span = fun (tokens: Syn.Token.t list) ->
  match tokens, List.rev tokens with
  | first :: _, last :: _ -> Some (token_body_span first last)
  | _ -> None

let token_slice_with_leading_trivia_span = fun (tokens: Syn.Token.t list) ->
  match tokens, List.rev tokens with
  | first :: _, last :: _ ->
      let start =
        match first.Syn.Token.leading_trivia with
        | trivia :: _ -> trivia.Syn.Token.span.start
        | [] -> first.Syn.Token.span.start
      in
      Some (Syn.Ceibo.Span.make ~start ~end_:last.Syn.Token.span.end_)
  | _ -> None

let source_of_token_slice = fun source tokens ->
  match token_slice_with_leading_trivia_span tokens with
  | Some span -> source_of_span source span
  | None -> ""

let source_of_token_body_slice = fun source tokens ->
  match token_slice_span tokens with
  | Some span -> source_of_span source span
  | None -> ""

let split_top_level_arguments = fun (tokens: Syn.Token.t list) ->
  let args = ref [] in
  let current = ref [] in
  let expected_closers = ref [] in
  let flush_current () =
    match List.rev !current with
    | [] -> ()
    | current_tokens ->
        args := current_tokens :: !args;
        current := []
  in
  List.iter
    (fun token ->
      match token.Syn.Token.kind, !expected_closers with
      | Syn.Token.Comma, [] ->
          flush_current ()
      | Syn.Token.OpenDelim delimiter, _ ->
          expected_closers := Syn.Token.CloseDelim delimiter :: !expected_closers;
          current := token :: !current
      | Syn.Token.CloseDelim _, expected :: rest when token.Syn.Token.kind = expected ->
          expected_closers := rest;
          current := token :: !current
      | _ ->
          current := token :: !current)
    tokens;
  flush_current ();
  List.rev !args

let rewrite_rust_format_string = fun literal_text ~span ->
  let literal_len = String.length literal_text in
  if literal_len < 2 || literal_text.[0] != '"' || literal_text.[literal_len - 1] != '"' then
    Error {
      message = "format! currently requires an ordinary string literal format string";
      span = Some span;
    }
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
              Error {
                message = "format! found an unmatched '{' in the format string";
                span = Some span;
              }
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
                    Error {
                      message =
                        "format! currently supports only bare {} placeholders and escaped {{ / }} braces";
                      span = Some span;
                    }
              )
        | '}' ->
            if index + 1 < literal_len - 1 && literal_text.[index + 1] = '}' then (
              push_fragment "}";
              loop (index + 2) placeholder_count
            ) else
              Error {
                message = "format! found an unmatched '}' in the format string";
                span = Some span;
              }
        | ch ->
            push_fragment (String.make 1 ch);
            loop (index + 1) placeholder_count
    in
    loop 1 0

let invocation_of_node = fun source (node: (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node) ->
  let children = Syn.Ceibo.Red.SyntaxNode.children_list node in
  match children with
  | Syn.Ceibo.Red.Node callee :: Syn.Ceibo.Red.Token _bang :: Syn.Ceibo.Red.Token open_token :: rest -> (
      match List.rev rest with
      | Syn.Ceibo.Red.Token close_token :: _payload ->
          let name =
            match Syn.Ceibo.Red.SyntaxNode.first_token callee, Syn.Ceibo.Red.SyntaxNode.last_token callee with
            | Some first_token, Some last_token ->
                source_of_span
                  source
                  (Syn.Ceibo.Span.make
                     ~start:(Syn.Ceibo.Red.SyntaxToken.span first_token).start
                     ~end_:(Syn.Ceibo.Red.SyntaxToken.span last_token).end_)
            | _ -> ""
          in
          (
            match Syn.Ceibo.Red.SyntaxNode.first_token node, Syn.Ceibo.Red.SyntaxNode.last_token node with
            | Some first_token, Some last_token -> Ok {
              name;
              span = Syn.Ceibo.Span.make
                ~start:(Syn.Ceibo.Red.SyntaxToken.span first_token).start
                ~end_:(Syn.Ceibo.Red.SyntaxToken.span last_token).end_;
              open_span = Syn.Ceibo.Red.SyntaxToken.span open_token;
              close_span = Syn.Ceibo.Red.SyntaxToken.span close_token;
            }
            | _ -> Error {
              message = "macro expansion could not recover the macro token span";
              span = Some (Syn.Ceibo.Red.SyntaxNode.span node);
            }
          )
      | _ -> Error {
        message = "macro expansion expected a closing delimiter";
        span = Some (Syn.Ceibo.Red.SyntaxNode.span node);
      }
    )
  | _ -> Error {
    message = "macro expansion expected a function-like macro shape";
    span = Some (Syn.Ceibo.Red.SyntaxNode.span node);
  }

let payload_tokens_for_invocation = fun tokens (invocation: invocation) ->
  List.filter
    (fun (token: Syn.Token.t) ->
      token.span.start >= invocation.open_span.end_ && token.span.end_ <= invocation.close_span.start)
    tokens

let expand_format_invocation = fun ~source ~tokens invocation ->
  let args = payload_tokens_for_invocation tokens invocation |> split_top_level_arguments in
  match args with
  | [] ->
      Error {
        message = "format! requires a format string argument";
        span = Some invocation.span;
      }
  | format_arg :: value_args ->
      (
        match format_arg with
        | [ format_token ] -> (
            match format_token.Syn.Token.kind with
            | Syn.Token.Literal (Syn.Token.String _) ->
                let literal_text = source_of_span source format_token.span in
                (
                  match rewrite_rust_format_string literal_text ~span:format_token.span with
                  | Error err -> Error err
                  | Ok (printf_format, placeholder_count) ->
                      if placeholder_count != List.length value_args then
                        Error {
                          message =
                            "format! placeholder count does not match the number of supplied arguments";
                          span = Some invocation.span;
                        }
                      else
                        let rendered_args = List.map
                          (fun arg_tokens -> "(" ^ source_of_token_body_slice source arg_tokens ^ ")")
                          value_args in
                        let replacement =
                          "("
                          ^ String.concat " " (("Stdlib.Printf.sprintf " ^ printf_format) :: rendered_args)
                          ^ ")"
                        in
                        Ok { span = invocation.span; source = replacement }
                )
            | _ ->
                Error {
                  message = "format! currently requires a string literal as its first argument";
                  span = token_slice_span format_arg;
                }
          )
        | _ ->
            Error {
              message = "format! currently requires a string literal as its first argument";
              span = token_slice_span format_arg;
            }
      )

let replacement_of_invocation = fun ~source ~tokens invocation ->
  match invocation.name with
  | "format" -> expand_format_invocation ~source ~tokens invocation
  | _ ->
      Error {
        message = "unsupported macro invocation: " ^ invocation.name ^ "!";
        span = Some invocation.span;
      }

let apply_replacements = fun source replacements ->
  let sorted = List.sort
    (fun left right -> Int.compare right.span.start left.span.start)
    replacements in
  List.fold_left
    (fun current_source replacement ->
      let before = String.sub current_source 0 replacement.span.start in
      let after_start = replacement.span.end_ in
      let after_len = String.length current_source - after_start in
      let after =
        if after_len <= 0 then
          ""
        else
          String.sub current_source after_start after_len
      in
      before ^ replacement.source ^ after)
    source
    sorted

let collect_macro_invocations = fun source (result: Syn.Parser.parse_result) ->
  let root = Syn.Ceibo.Red.new_root result.tree in
  let nodes = ref [] in
  Syn.Ceibo.Red.SyntaxNode.preorder root
    (fun element ->
      match element with
      | Syn.Ceibo.Red.Node node when Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.MACRO_EXPR ->
          nodes := node :: !nodes
      | _ -> ());
  let rec lift acc = function
    | [] -> Ok (List.rev acc)
    | node :: rest -> (
        match invocation_of_node source node with
        | Ok invocation -> lift (invocation :: acc) rest
        | Error err -> Error err
      )
  in
  lift [] (List.rev !nodes)

let has_invocations = fun invocations ->
  match invocations with
  | _ :: _ -> true
  | [] -> false

let expand_once = fun ~filename source ->
  let parsed = Syn.parse ~filename source in
  match collect_macro_invocations source parsed with
  | Error err -> Error err
  | Ok invocations ->
      if not (has_invocations invocations) then
        Ok { source; changed = false }
      else if parsed.diagnostics != [] then
        Error {
          message = "macro expansion requires a parse-clean source file";
          span = None;
        }
      else
        let rec build_replacements acc = function
          | [] -> Ok (List.rev acc)
          | invocation :: rest -> (
              match replacement_of_invocation ~source ~tokens:parsed.tokens invocation with
              | Ok replacement -> build_replacements (replacement :: acc) rest
              | Error err -> Error err
            )
        in
        (
          match build_replacements [] invocations with
          | Error err -> Error err
          | Ok replacements -> Ok {
            source = apply_replacements source replacements;
            changed = true;
          }
        )

let expand_source = fun ~filename source ->
  let rec loop current_source changed_any remaining_passes =
    if remaining_passes = 0 then
      Error {
        message = "macro expansion reached the recursive expansion limit";
        span = None;
      }
    else
      match expand_once ~filename current_source with
      | Error err -> Error err
      | Ok { source = next_source; changed = changed_this_pass } ->
          if changed_this_pass then
            loop next_source true (remaining_passes - 1)
          else
            Ok { source = next_source; changed = changed_any }
  in
  loop source false 16
