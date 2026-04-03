open Std
open Std.Collections

let rule_id = "prefer-t-for-single-type-modules"

let rule_description = "Modules with a single type definition should usually call it t"

let rule_explain = {|
When a module has one obvious main type, naming it `t` makes the surrounding code read
naturally. `User.t`, `Decoder.t`, and `Cache.t` are standard OCaml shapes that
communicate "the primary type owned by this module" without forcing the name to repeat
itself.

If that same module writes `type user`, call sites end up with `User.user`, which is
usually redundant. The module name has already supplied the context.

This rule is intentionally narrow. It only applies when the module clearly exposes one
primary type. Modules with several important types should name them explicitly.
|}

let single_type_decl_token = fun (decl: Syn.Cst.TypeDeclaration.t) ->
  match Syn.Cst.Ident.name (Syn.Cst.TypeDeclaration.type_name decl) with
  | Some _ -> (
      match Syn.Cst.TypeDeclaration.next_and_declaration decl with
      | None ->
          Some (Syn.Cst.TypeDeclaration.name_token decl |> Syn.Cst.Token.syntax_token)
      | Some _ -> None
    )
  | _ -> None

let single_structure_type_decl_token = fun items ->
  match
    List.filter_map
      (
        function
        | Syn.Cst.StructureItem.TypeDeclaration decl -> Some decl
        | _ -> None
      )
      items
  with
  | [ decl ] -> single_type_decl_token decl
  | _ -> None

let single_signature_type_decl_token = fun items ->
  match
    List.filter_map
      (
        function
        | Syn.Cst.SignatureItem.TypeDeclaration decl -> Some decl
        | _ -> None
      )
      items
  with
  | [ decl ] -> single_type_decl_token decl
  | _ -> None

let make_diagnostic = fun token ->
  let original = Syn.Ceibo.Red.SyntaxToken.text token in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:(("Rename " ^ original ^ " to t"))
    ()

let diagnostic_for_module_structure = fun decl ->
  match Syn.CstBuilder.structure_items_of_module_expression
    (Syn.Cst.ModuleStructure.module_expression decl) with
  | Ok items -> (
      match single_structure_type_decl_token items with
      | Some token when Syn.Ceibo.Red.SyntaxToken.text token != "t" -> Some (make_diagnostic token)
      | _ -> None
    )
  | Error _ -> None

let diagnostic_for_module_type_decl = fun decl ->
  match Syn.Cst.ModuleTypeDeclaration.module_type decl with
  | Some module_type -> (
      match Syn.CstBuilder.signature_items_of_module_type module_type with
      | Ok items -> (
          match single_signature_type_decl_token items with
          | Some token when Syn.Ceibo.Red.SyntaxToken.text token != "t" -> Some (make_diagnostic token)
          | _ -> None
        )
      | Error _ -> None
    )
  | None -> None

let diagnostics_for_items = fun source_file ->
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items |> List.filter_map
        (
          function
          | Syn.Cst.StructureItem.ModuleDeclaration decl -> diagnostic_for_module_structure decl
          | Syn.Cst.StructureItem.ModuleTypeDeclaration decl -> diagnostic_for_module_type_decl decl
          | _ -> None
        )
  | Syn.Cst.Interface { items; _ } ->
      items |> List.filter_map
        (
          function
          | Syn.Cst.SignatureItem.ModuleDeclaration decl -> (
              match Syn.Cst.ModuleSignature.definition decl with
              | Syn.Cst.ModuleSignature.Signature module_type -> (
                  match Syn.CstBuilder.signature_items_of_module_type module_type with
                  | Ok items -> (
                      match single_signature_type_decl_token items with
                      | Some token when Syn.Ceibo.Red.SyntaxToken.text token != "t" -> Some (make_diagnostic
                        token)
                      | _ -> None
                    )
                  | Error _ -> None
                )
              | Syn.Cst.ModuleSignature.Alias _ -> None
            )
          | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl ->
              diagnostic_for_module_type_decl decl
          | _ ->
              None
        )

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:(fun ctx _red_root ->
      let source_file = ctx.cst in
      diagnostics_for_items source_file)
    ()
