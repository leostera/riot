open Std

type expansion = {
  source: string;
  changed: bool;
}

type replacement = {
  span: Syn.Ceibo.Span.t;
  source: string;
}

let has_invocations = fun invocations ->
  match invocations with
  | _ :: _ -> true
  | [] -> false

let builtin_providers = Macro_provider_registry.builtin_providers

let replacement_of_invocation = fun ?providers invocation ->
  match Macro_provider_registry.resolve ?providers invocation with
  | Error err -> Error err
  | Ok macro_ ->
      let result = macro_.expand invocation.body in
      match result.diagnostics with
      | diagnostic :: _ -> Error diagnostic
      | [] -> Ok { span = invocation.span; source = Macro_token_stream.source result.output }

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

let expand_environment = fun ?providers env ->
  let source = Macro_environment.source env in
  let filename = Macro_environment.filename env in
  match Macro_parser.collect_invocations env with
  | Error err -> Error err
  | Ok invocations ->
      if not (has_invocations invocations) then
        Ok { source; changed = false }
      else if (Macro_environment.parsed env).diagnostics != [] then
        Error
          (Macro_error.make "macro expansion requires a parse-clean source file")
      else
        let rec build_replacements acc = function
          | [] -> Ok (List.rev acc)
          | invocation :: rest -> (
              match replacement_of_invocation ?providers invocation with
              | Ok replacement -> build_replacements (replacement :: acc) rest
              | Error err -> Error err
            )
        in
        (
          match build_replacements [] invocations with
          | Error err -> Error err
          | Ok replacements ->
              let expanded_source = apply_replacements source replacements in
              (
                match Macro_validator.validate_source ~filename expanded_source with
                | Ok () -> Ok { source = expanded_source; changed = true }
                | Error err -> Error err
              )
        )

let expand_once = fun ?providers ~filename source ->
  let env = Macro_environment.create ~filename source in
  expand_environment ?providers env

let expand_source = fun ?providers ~filename source ->
  let rec loop current_source changed_any remaining_passes =
    if remaining_passes = 0 then
      Error
        (Macro_error.make "macro expansion reached the recursive expansion limit")
    else
      match expand_once ?providers ~filename current_source with
      | Error err -> Error err
      | Ok { source = next_source; changed = changed_this_pass } ->
          if changed_this_pass then
            loop next_source true (remaining_passes - 1)
          else
            Ok { source = next_source; changed = changed_any }
  in
  loop source false 16
