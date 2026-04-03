open Std
open Std.Collections

let rule_id = "class-case-module-names"

let rule_description = "Module names should use ClassCase instead of underscores"

let rule_explain = {|
Module names are one of the main visual cues in OCaml code. `ClassCase` tells the
reader that an identifier names a module or constructor-sized thing, while
`snake_case` is reserved for values and fields.

Names like `Foo_bar` blur that boundary. They are neither normal module names nor
normal value names, so they read like a typo every time they appear.

Prefer `FooBar`, `HttpClient`, or `ParserState`. Save underscores for value-level
identifiers.
|}

let contains_underscore = fun text ->
  String.exists (fun ch -> ch = '_') text

let starts_upper = fun text ->
  String.length text > 0 && let ch = String.get text 0 in
  ch >= 'A' && ch <= 'Z'

let capitalize_piece = fun piece ->
  if String.equal piece "" then
    ""
  else
    let first = String.get piece 0 |> Char.uppercase_ascii |> String.make 1 in
    let rest =
      if String.length piece = 1 then
        ""
      else
        String.sub piece 1 (String.length piece - 1)
    in
    first ^ rest

let to_class_case = fun text ->
  text
  |> String.split_on_char '_'
  |> List.filter (fun piece -> not (String.equal piece ""))
  |> List.map capitalize_piece
  |> String.concat ""

let should_flag_module_name = fun text -> contains_underscore text || not (starts_upper text)

let make_diagnostic = fun token ->
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  let replacement = to_class_case original in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:(("Rename " ^ original ^ " to " ^ replacement))
    ()

let diagnostic_for_module_structure = fun decl ->
  let name = Syn.Cst.ModuleStructure.name decl in
  if should_flag_module_name name then
    let token = Syn.Cst.ModuleStructure.module_name_token decl |> Syn.Cst.Token.syntax_token in
    Some (make_diagnostic token)
  else
    None

let diagnostic_for_module_signature = fun decl ->
  let name = Syn.Cst.ModuleSignature.name decl in
  if should_flag_module_name name then
    let token = Syn.Cst.ModuleSignature.module_name_token decl |> Syn.Cst.Token.syntax_token in
    Some (make_diagnostic token)
  else
    None

let diagnostic_for_module_type_decl = fun decl ->
  let name = Syn.Cst.ModuleTypeDeclaration.name decl in
  if should_flag_module_name name then
    let token = Syn.Cst.ModuleTypeDeclaration.module_type_name_token decl |> Syn.Cst.Token.syntax_token in
    Some (make_diagnostic token)
  else
    None

let rec diagnostics_for_module_structure = fun decl ->
  let rest =
    match Syn.Cst.ModuleStructure.next_and_declaration decl with
    | Some next -> diagnostics_for_module_structure next
    | None -> []
  in
  Option.to_list (diagnostic_for_module_structure decl) @ rest

let rec diagnostics_for_module_signature = fun decl ->
  let rest =
    match Syn.Cst.ModuleSignature.next_and_declaration decl with
    | Some next -> diagnostics_for_module_signature next
    | None -> []
  in
  Option.to_list (diagnostic_for_module_signature decl) @ rest

let diagnostics_for_items = fun source_file ->
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items |> List.concat_map
        (
          function
          | Syn.Cst.StructureItem.ModuleDeclaration decl -> diagnostics_for_module_structure decl
          | Syn.Cst.StructureItem.ModuleTypeDeclaration decl -> Option.to_list
            (diagnostic_for_module_type_decl decl)
          | _ -> []
        )
  | Syn.Cst.Interface { items; _ } ->
      items |> List.concat_map
        (
          function
          | Syn.Cst.SignatureItem.ModuleDeclaration decl -> diagnostics_for_module_signature decl
          | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl -> Option.to_list
            (diagnostic_for_module_type_decl decl)
          | _ -> []
        )

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  diagnostics_for_items source_file

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
