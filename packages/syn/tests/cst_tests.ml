open Std
open Std.Collections
open Syn

let expect_some = fun value ~msg ->
  match value with
  | Some value -> Ok value
  | None -> Error msg

let declaration_name_text = fun tokens -> tokens |> List.map Syn.Cst.Token.text |> String.concat ""

let token_sequence_text = fun tokens -> tokens |> List.map Syn.Cst.Token.text |> String.concat ""

let attribute_sigil_text = fun (attribute: Syn.Cst.attribute) -> token_sequence_text attribute.sigil_tokens

let extension_sigil_text = fun (extension: Syn.Cst.extension) -> token_sequence_text extension.sigil_tokens

let sample_ml = Path.v "sample.ml"

let sample_mli = Path.v "sample.mli"

type parsed = {
  tokens: Syn.Token.t list;
  tree: (Syn.SyntaxKind.t, string) Ceibo.Green.node;
  diagnostics: Syn.Diagnostic.t list;
  cst: Syn.Cst.source_file option;
}

let with_optional_cst = fun result ->
  let cst =
    match Syn.build_cst result with
    | Ok cst -> Some cst
    | Error _ -> None
  in
  {
    tokens = result.Syn.Parser.tokens;
    tree = result.Syn.Parser.tree;
    diagnostics = result.Syn.Parser.diagnostics;
    cst
  }

let parse_ml = fun source -> Syn.parse ~filename:sample_ml source |> with_optional_cst

let parse_mli = fun source -> Syn.parse ~filename:sample_mli source |> with_optional_cst

let read_file = fun path -> Fs.read_to_string path |> Result.expect ~msg:"failed to read file"

let assert_cst_json_snapshot = fun ~ctx parsed ->
  let cst = expect_some parsed.cst ~msg:"expected CST for diagnostics-free parse"
  |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
  Test.Snapshot.assert_json ~ctx ~actual:(Syn.CstJson.of_source_file cst)

let assert_ml_cst_snapshot = fun ~ctx source -> assert_cst_json_snapshot ~ctx (parse_ml source)

let assert_mli_cst_snapshot = fun ~ctx source -> assert_cst_json_snapshot ~ctx (parse_mli source)

let structure_items = function
  | Syn.Cst.Implementation { items; _ } -> items
  | Syn.Cst.Interface _ -> []

let ident_text = fun ident ->
  Syn.Cst.Ident.last_segment ident |> Option.map Syn.Cst.Token.text |> Option.unwrap_or ~default:""

let signature_items = function
  | Syn.Cst.Interface { items; _ } -> items
  | Syn.Cst.Implementation _ -> []

let node_leading_trivia = fun syntax_node -> Syn.Cst.leading_trivia_before_node ~after:0 syntax_node

let node_leading_trivia_texts = fun syntax_node ->
  node_leading_trivia syntax_node |> List.map Syn.Cst.Trivia.text

let top_level_let_bindings = fun cst ->
  structure_items cst |> List.filter_map
    (
      function
      | Syn.Cst.StructureItem.LetBinding binding -> Some binding
      | _ -> None
    )

let token_trivia_kinds = fun token ->
  token.Syn.Token.leading_trivia
  |> List.map (fun (trivia: Syn.Token.trivia) -> trivia.Syn.Token.kind)

let green_token_kinds = fun node ->
  let rec loop = fun acc ->
    function
    | Ceibo.Green.Token token -> token.kind :: acc
    | Ceibo.Green.Node node -> Ceibo.Green.children node |> Array.to_list |> List.fold_left loop acc
  in
  loop [] (Ceibo.Green.Node node) |> List.rev

let tests = [
  Test.case "ceibo tokens preserve leading trivia separately from token body width"
    (fun _ctx ->
      let comment = Ceibo.Green.make_trivia ~kind:Syn.SyntaxKind.COMMENT ~text:"(* hi *)" ~width:8 in
      let space = Ceibo.Green.make_trivia ~kind:Syn.SyntaxKind.WHITESPACE ~text:" " ~width:1 in
      let token = Ceibo.Green.make_token
        ~leading_trivia:[ comment; space ]
        ~kind:Syn.SyntaxKind.IDENT_EXPR
        ~text:"x"
        ~width:1 in
      Test.assert_equal ~expected:1 ~actual:(Ceibo.Green.token_width token);
      Test.assert_equal ~expected:10 ~actual:(Ceibo.Green.token_full_width token);
      Test.assert_equal ~expected:10 ~actual:(Ceibo.Green.width (Ceibo.Green.Token token));
      Test.assert_equal ~expected:2 ~actual:(List.length (Ceibo.Green.leading_trivia token));
      Ok ());
  Test.case "ceibo red tokens derive leading trivia spans before the token body"
    (fun _ctx ->
      let comment = Ceibo.Green.make_trivia ~kind:Syn.SyntaxKind.COMMENT ~text:"(*hi*)" ~width:6 in
      let space = Ceibo.Green.make_trivia ~kind:Syn.SyntaxKind.WHITESPACE ~text:" " ~width:1 in
      let token = Ceibo.Green.make_token
        ~leading_trivia:[ comment; space ]
        ~kind:Syn.SyntaxKind.IDENT_EXPR
        ~text:"x"
        ~width:1 in
      let syntax_token = Ceibo.Red.new_token token (Ceibo.Span.make ~start:7 ~end_:8) in
      let leading = Ceibo.Red.SyntaxToken.leading_trivia syntax_token in
      match leading with
      | [comment;space] ->
          Test.assert_equal
            ~expected:(Ceibo.Span.make ~start:0 ~end_:6)
            ~actual:(Ceibo.Red.SyntaxTrivia.span comment);
          Test.assert_equal
            ~expected:(Ceibo.Span.make ~start:6 ~end_:7)
            ~actual:(Ceibo.Red.SyntaxTrivia.span space);
          Test.assert_equal
            ~expected:(Ceibo.Span.make ~start:7 ~end_:8)
            ~actual:(Ceibo.Red.SyntaxToken.span syntax_token);
          Ok ()
      | _ -> Error "expected two trivia entries");
  Test.case "ceibo builder helpers can construct tokens with leading trivia"
    (fun _ctx ->
      let comment = Ceibo.Green.make_trivia ~kind:Syn.SyntaxKind.COMMENT ~text:"(*hi*)" ~width:6 in
      match Ceibo.Builder.make_token_with_leading_trivia
        ~leading_trivia:[ comment ]
        ~kind:Syn.SyntaxKind.IDENT_EXPR
        ~text:"x"
        ~width:1 with
      | Ceibo.Green.Token token ->
          Test.assert_equal ~expected:1 ~actual:(List.length (Ceibo.Green.leading_trivia token));
          Ok ()
      | Ceibo.Green.Node _ -> Error "expected token element");
  Test.case "lexer attaches trailing file trivia to EOF leading trivia"
    (fun _ctx ->
      let tokens = Syn.Lexer.tokenize "let x = 1\n(* tail *)\n" in
      match List.rev tokens with
      | eof :: _ ->
          Test.assert_equal ~expected:Syn.Token.EOF ~actual:eof.Syn.Token.kind;
          (
            match token_trivia_kinds eof with
            | [
              Syn.Token.WhitespaceTrivia;
              Syn.Token.CommentTrivia { terminated=true; _ };
              Syn.Token.WhitespaceTrivia;

            ] -> Ok ()
            | _ -> Error "expected EOF to own trailing whitespace/comment trivia"
          )
      | [] -> Error "expected token stream to end with EOF");
  Test.case "lexer attaches mixed leading trivia onto the next real token"
    (fun _ctx ->
      let tokens = Syn.Lexer.tokenize "(** doc *)\n(* comment *)\nlet x = 1" in
      match tokens with
      | let_kw :: ident :: eq :: int_literal :: eof :: [] ->
          Test.assert_equal ~expected:(Syn.Token.Keyword Syn.Keyword.Let) ~actual:let_kw.Syn.Token.kind;
          Test.assert_equal ~expected:(Syn.Token.Ident "x") ~actual:ident.Syn.Token.kind;
          Test.assert_equal ~expected:Syn.Token.Eq ~actual:eq.Syn.Token.kind;
          Test.assert_equal
            ~expected:(Syn.Token.Literal (Syn.Token.Int 1))
            ~actual:int_literal.Syn.Token.kind;
          Test.assert_equal ~expected:Syn.Token.EOF ~actual:eof.Syn.Token.kind;
          (
            match token_trivia_kinds let_kw with
            | [
              Syn.Token.DocstringTrivia { terminated=true; _ };
              Syn.Token.WhitespaceTrivia;
              Syn.Token.CommentTrivia { terminated=true; _ };
              Syn.Token.WhitespaceTrivia;

            ] -> Ok ()
            | _ -> Error "expected let token to own docstring/comment leading trivia"
          )
      | _ -> Error "expected lexer to emit only real tokens plus EOF");
  Test.case "parser consumes real-token streams without reintroducing trivia tokens"
    (fun _ctx ->
      let source = "(** doc *)\n(* comment *)\nlet x = 1" in
      let result = parse_ml source in
      Test.assert_equal ~expected:[] ~actual:result.diagnostics;
      (
        match List.map (fun token -> token.Syn.Token.kind) result.tokens with
        | [
          Syn.Token.Keyword Syn.Keyword.Let;
          Syn.Token.Ident "x";
          Syn.Token.Eq;
          Syn.Token.Literal (Syn.Token.Int 1);
          Syn.Token.EOF;

        ] -> Ok ()
        | _ -> Error "expected parser to preserve the lexer real-token stream"
      ));
  Test.case "parser compatibility keeps trailing file comments visible"
    (fun _ctx ->
      let result = parse_ml "let x = 1\n(* tail *)\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding _ :: Syn.Cst.StructureItem.Comment comment :: _ ->
          Test.assert_equal ~expected:"(* tail *)" ~actual:(Syn.Cst.Comment.text comment);
          Ok ()
      | _ -> Error "expected trailing file comment after let binding");
  Test.case "build_cst keeps trailing file docstrings visible via EOF trivia"
    (fun _ctx ->
      let result = parse_ml "let x = 1\n(** tail doc *)\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding _ :: Syn.Cst.StructureItem.Docstring docstring :: _ ->
          Test.assert_equal ~expected:"(** tail doc *)" ~actual:(Syn.Cst.Docstring.text docstring);
          Ok ()
      | _ -> Error "expected trailing file docstring after let binding");
  Test.case "visit traverses recursive declaration chains once"
    (fun _ctx ->
      let result = parse_ml
        "let rec a = 1 and b = 2 and c = 3\n\
         type a = A and b = B and c = C\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let let_binding_count = Syn.Visit.source_file
        {
          Syn.Visit.default
          with visit_let_binding =
            (fun count walk binding ->
              walk.descend_let_binding (count + 1) binding);
        }
        0
        cst
      in
      let type_declaration_count = Syn.Visit.source_file
        {
          Syn.Visit.default
          with visit_type_declaration =
            (fun count walk declaration ->
              walk.descend_type_declaration (count + 1) declaration);
        }
        0
        cst
      in
      Test.assert_equal ~expected:3 ~actual:let_binding_count;
      Test.assert_equal ~expected:3 ~actual:type_declaration_count;
      Ok ());
  Test.case "parse results retain original tokens with EOF-owned trailing trivia"
    (fun _ctx ->
      let result = parse_ml "let x = 1\n(* tail *)\n" in
      match List.rev result.tokens with
      | eof :: _ ->
          Test.assert_equal ~expected:Syn.Token.EOF ~actual:eof.Syn.Token.kind;
          (
            match token_trivia_kinds eof with
            | [
              Syn.Token.WhitespaceTrivia;
              Syn.Token.CommentTrivia { terminated=true; _ };
              Syn.Token.WhitespaceTrivia;

            ] -> Ok ()
            | _ -> Error "expected parse result tokens to preserve EOF-owned trailing trivia"
          )
      | [] -> Error "expected token stream to end with EOF");
  Test.case "green tree no longer materializes standalone trivia tokens"
    (fun _ctx ->
      let source = "(* lead *)\n\
           let x =\n\
             (* inner *)\n\
             1\n"
      in
      let result = parse_ml source in
      let token_kinds = green_token_kinds result.tree in
      Test.assert_false
        (
          List.exists
            (fun kind ->
              match kind with
              | Syn.SyntaxKind.WHITESPACE
              | Syn.SyntaxKind.COMMENT
              | Syn.SyntaxKind.DOCSTRING -> true
              | _ -> false)
            token_kinds
        );
      Test.assert_equal
        ~expected:((String.length source - 1))
        ~actual:(Ceibo.Green.width (Ceibo.Green.Node result.tree));
      Ok ());
  Test.case "red tree traversal stays trivia-free while first token keeps leading trivia"
    (fun _ctx ->
      let source = "(* lead *)\n\
           let x =\n\
             (* inner *)\n\
             1\n"
      in
      let result = parse_ml source in
      let root = Ceibo.Red.new_root result.tree in
      let token_kinds = Ceibo.Red.SyntaxNode.tokens root |> List.map Ceibo.Red.SyntaxToken.kind in
      Test.assert_false
        (
          List.exists
            (fun kind ->
              match kind with
              | Syn.SyntaxKind.WHITESPACE
              | Syn.SyntaxKind.COMMENT
              | Syn.SyntaxKind.DOCSTRING -> true
              | _ -> false)
            token_kinds
        );
      match Ceibo.Red.SyntaxNode.first_token root with
      | Some token -> (
          match Ceibo.Red.SyntaxToken.leading_trivia token |> List.map Ceibo.Red.SyntaxTrivia.kind with
          | [Syn.SyntaxKind.COMMENT;Syn.SyntaxKind.WHITESPACE;] -> Ok ()
          | _ -> Error "expected first token to own leading comment and whitespace trivia"
        )
      | None -> Error "expected root to have a first token");
  Test.case "cst exists for diagnostics-free parse"
    (fun _ctx ->
      let result = parse_ml "type userProfile = int\n" in
      Test.assert_equal ~expected:0 ~actual:(List.length result.diagnostics);
      Test.assert_true (Option.is_some result.cst);
      (
        match result.cst with
        | Some cst -> Test.assert_equal
          ~expected:`Implementation
          ~actual:(Syn.Cst.SourceFile.kind cst)
        | None -> ()
      );
      Ok ());
  Test.case "cst root distinguishes interfaces from implementations"
    (fun _ctx ->
      let result = parse_mli "val create : int -> int\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      Test.assert_equal ~expected:`Interface ~actual:(Syn.Cst.SourceFile.kind cst);
      Ok ());
  Test.case "cst source files expose top-level phrase separator tokens"
    (fun _ctx ->
      let result = parse_ml
        "let first = 1;;\n\
             let second = 2;;\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      Test.assert_equal
        ~expected:[ ";"; ";" ]
        ~actual:((Syn.Cst.SourceFile.phrase_separator_tokens cst |> List.map Syn.Cst.Token.text));
      Ok ());
  Test.case "cst polymorphic-variant inherit patterns keep the identifier after #"
    (fun _ctx ->
      let result = parse_ml
        "let x =\n\
             match y with\n\
             | #color -> 0\n\
             | #theme -> 1\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding { value=Match { cases; _ }; _ } :: _ -> (
          match cases with
          | { pattern=PolyVariantInherit first; _ } :: { pattern=PolyVariantInherit second; _ } :: _ ->
              Test.assert_equal
                ~expected:[ "color" ]
                ~actual:((Syn.Cst.Ident.segments first.type_path |> List.map Syn.Cst.Token.text));
              Test.assert_equal
                ~expected:[ "theme" ]
                ~actual:((Syn.Cst.Ident.segments second.type_path |> List.map Syn.Cst.Token.text));
              Ok ()
          | _ -> Error "expected polymorphic-variant inherit match cases"
        )
      | _ -> Error "expected let binding with match expression");
  Test.case "cst is absent when parse diagnostics exist"
    (fun _ctx ->
      let result = parse_ml "let x =\n" in
      Test.assert_true (List.length result.diagnostics > 0);
      Test.assert_true (Option.is_none result.cst);
      Ok ());
  Test.case "cst ident helpers compare paths structurally"
    (fun _ctx ->
      let left = Syn.Cst.Ident.from_string "List.rev" in
      let right = Syn.Cst.Ident.from_string "List.rev" in
      let wrong = Syn.Cst.Ident.from_string "List.map" in
      Test.assert_true (Syn.Cst.Ident.equal left right);
      Test.assert_false (Syn.Cst.Ident.equal left wrong);
      Test.assert_equal
        ~expected:[ "List"; "rev" ]
        ~actual:((Syn.Cst.Ident.segments left |> List.map Syn.Cst.Token.text));
      Ok ());
  Test.case "cst type extensions keep last module-path segment as name"
    (fun _ctx ->
      let result = parse_ml "type Message.t += Added\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.TypeExtension decl :: _ ->
          Test.assert_equal
            ~expected:"t"
            ~actual:(Syn.Cst.Token.text (Syn.Cst.TypeExtension.name_token decl));
          Ok ()
      | _ -> Error "expected first item to be a type extension");
  Test.case "cst type extensions preserve module-path names across inline comments"
    (fun _ctx ->
      let result = parse_ml "type Message (* c *).t += Added\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeExtension decl :: _ ->
          Test.assert_equal
            ~expected:"t"
            ~actual:(Syn.Cst.Token.text (Syn.Cst.TypeExtension.name_token decl));
          Ok ()
      | _ -> Error "expected commented type extension");
  Test.case "cst type extensions are preserved in interfaces"
    (fun _ctx ->
      let result = parse_mli "type Message.t += Added\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.TypeExtension decl :: _ ->
          let constructors = Syn.Cst.TypeExtension.constructors decl
          |> List.map Syn.Cst.VariantConstructor.name in
          Test.assert_equal ~expected:[ "Added" ] ~actual:constructors;
          Ok ()
      | _ -> Error "expected first item to be a type extension");
  Test.case "cst interface type declarations preserve abstract and manifest forms"
    (fun _ctx ->
      let result = parse_mli
        "type t\n\
             type alias = int\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.TypeDeclaration { type_definition=Syn.Cst.TypeDefinition.Abstract; _ } :: Syn.Cst.SignatureItem.TypeDeclaration {
        type_definition=Syn.Cst.TypeDefinition.Alias {
          manifest=Syn.Cst.CoreType.Constr { constructor_path; _ };
          _;

        };
        destructive_substitution_token=None;
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "int") ~actual:(Syn.Cst.Ident.name constructor_path);
          Ok ()
      | _ -> Error "expected interface type declarations");
  Test.case "cst interface type declarations distinguish destructive substitutions"
    (fun _ctx ->
      let result = parse_mli "type view := string\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.TypeDeclaration {
        type_name;
        type_definition=Syn.Cst.TypeDefinition.Alias {
          manifest=Syn.Cst.CoreType.Constr { constructor_path; _ };
          _;

        };
        destructive_substitution_token=Some _;
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "view") ~actual:(Syn.Cst.Ident.name type_name);
          Test.assert_equal ~expected:(Some "string") ~actual:(Syn.Cst.Ident.name constructor_path);
          Ok ()
      | _ -> Error "expected destructive type substitution");
  Test.case "cst type declarations preserve private flags structurally"
    (fun _ctx ->
      let result = parse_ml
        "type visible = int\n\
             type hidden_record = private { value : int }\n\
             type hidden_abstract = private\n\
             type hidden_variant = private Left | Right\n\
             type hidden_alias = private int\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let declarations =
        structure_items cst
        |> List.filter_map
          (
            function
            | Syn.Cst.StructureItem.TypeDeclaration decl -> Some decl
            | _ -> None
          )
      in
      match declarations with
      | visible :: hidden_record :: hidden_abstract :: hidden_variant :: hidden_alias :: _ ->
          Test.assert_false (Syn.Cst.TypeDeclaration.is_private visible);
          Test.assert_true (Syn.Cst.TypeDeclaration.is_private hidden_record);
          Test.assert_true (Syn.Cst.TypeDeclaration.is_private hidden_abstract);
          Test.assert_true (Syn.Cst.TypeDeclaration.is_private hidden_variant);
          Test.assert_true (Syn.Cst.TypeDeclaration.is_private hidden_alias);
          Test.assert_equal
            ~expected:(Some "private")
            ~actual:((Syn.Cst.TypeDeclaration.private_flag hidden_record
            |> Syn.Cst.PrivateFlag.private_token
            |> Option.map Syn.Cst.Token.text));
          Ok ()
      | _ -> Error "expected private and public type declarations");
  Test.case "cst type declarations preserve nonrec and manifest aliases"
    (fun _ctx ->
      let result = parse_ml
        "type nonrec 'a option = 'a option = None | Some of 'a\n\
             type point = Base.point = private { x : int; y : int }\n\
             type color = Tty.Color.t = private RGB | No_color\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let declarations =
        structure_items cst
        |> List.filter_map
          (
            function
            | Syn.Cst.StructureItem.TypeDeclaration decl -> Some decl
            | _ -> None
          )
      in
      match declarations with
      | option_decl :: point_decl :: color_decl :: _ -> (
          Test.assert_true (Option.is_some (Syn.Cst.TypeDeclaration.nonrec_token option_decl));
          (
            match Syn.Cst.TypeDeclaration.manifest_alias option_decl with
            | Some (Syn.Cst.CoreType.Constr { constructor_path; arguments; _ }) ->
                Test.assert_equal
                  ~expected:(Some "option")
                  ~actual:(Syn.Cst.Ident.name constructor_path);
                Test.assert_equal ~expected:1 ~actual:(List.length arguments)
            | _ -> panic "expected option manifest alias"
          );
          (
            match Syn.Cst.TypeDeclaration.type_definition option_decl with
            | Syn.Cst.TypeDefinition.Variant _ -> ()
            | _ -> panic "expected variant body for option alias"
          );
          (
            match Syn.Cst.TypeDeclaration.manifest_alias point_decl with
            | Some (Syn.Cst.CoreType.Constr { constructor_path; _ }) -> Test.assert_true
              (Syn.Cst.Ident.equal constructor_path (Syn.Cst.Ident.from_string "Base.point"))
            | _ -> panic "expected point manifest alias"
          );
          (
            match Syn.Cst.TypeDeclaration.type_definition point_decl with
            | Syn.Cst.TypeDefinition.Record _ -> ()
            | _ -> panic "expected record body for point alias"
          );
          Test.assert_true (Syn.Cst.TypeDeclaration.is_private point_decl);
          (
            match Syn.Cst.TypeDeclaration.manifest_alias color_decl with
            | Some (Syn.Cst.CoreType.Constr { constructor_path; _ }) -> Test.assert_true
              (Syn.Cst.Ident.equal constructor_path (Syn.Cst.Ident.from_string "Tty.Color.t"))
            | _ -> panic "expected color manifest alias"
          );
          (
            match Syn.Cst.TypeDeclaration.type_definition color_decl with
            | Syn.Cst.TypeDefinition.Variant _ -> ()
            | _ -> panic "expected variant body for color alias"
          );
          Test.assert_true (Syn.Cst.TypeDeclaration.is_private color_decl);
          Ok ()
        )
      | _ -> Error "expected manifest alias declarations");
  Test.case "cst mutual type declarations preserve grouped bindings"
    (fun _ctx ->
      let result = parse_ml "type node = File of string\nand forest = node list\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.and_declarations decl with
          | [ second ] ->
              Test.assert_equal
                ~expected:(Some "node")
                ~actual:(Syn.Cst.Ident.name (Syn.Cst.TypeDeclaration.type_name decl));
              Test.assert_equal
                ~expected:(Some "forest")
                ~actual:(Syn.Cst.Ident.name (Syn.Cst.TypeDeclaration.type_name second));
              Ok ()
          | _ -> Error "expected two grouped type declarations"
        )
      | _ -> Error "expected grouped type declaration");
  Test.case "cst mutual type declarations keep uppercase and-declarations on the variant path"
    (fun _ctx ->
      let result = parse_ml
        "type head = A\n\
             and alias = Outer (* c *).Inner.t\n\
             and variant = B\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.type_definition decl, Syn.Cst.TypeDeclaration.and_declarations
            decl with
          | Syn.Cst.TypeDefinition.Variant { constructors=[ head_constr ]; _ }, [
            alias_decl;
            variant_decl
          ] ->
              Test.assert_equal ~expected:"A" ~actual:(Syn.Cst.VariantConstructor.name head_constr);
              (
                match Syn.Cst.TypeDeclaration.type_definition alias_decl with
                | Syn.Cst.TypeDefinition.Alias {
                  manifest=Syn.Cst.CoreType.Constr { constructor_path; _ };
                  _
                } -> Test.assert_equal
                  ~expected:[ "Outer"; "Inner"; "t" ]
                  ~actual:((Syn.Cst.Ident.segments constructor_path |> List.map Syn.Cst.Token.text))
                | _ -> raise
                  (Failure "expected grouped alias declaration to keep the qualified type path")
              );
              (
                match Syn.Cst.TypeDeclaration.type_definition variant_decl with
                | Syn.Cst.TypeDefinition.Variant { constructors=[ variant_constr ]; _ } ->
                    Test.assert_equal
                      ~expected:"B"
                      ~actual:(Syn.Cst.VariantConstructor.name variant_constr);
                    Ok ()
                | _ -> Error "expected trailing grouped declaration to stay a bare variant"
              )
          | _ -> Error "expected grouped type declaration with alias and trailing variant"
        )
      | _ -> Error "expected grouped type declaration");
  Test.case "cst interface grouped type declarations keep uppercase GADT and-declarations on the variant path"
    (fun _ctx ->
      let result = parse_mli
        "type _ expr = Int : int expr\n\
             and packed = Packed (* c *) : int expr -> packed\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.and_declarations decl with
          | [ packed_decl ] -> (
              match Syn.Cst.TypeDeclaration.type_definition packed_decl with
              | Syn.Cst.TypeDefinition.Variant { constructors=[ packed_constr ]; _ } ->
                  Test.assert_equal
                    ~expected:"Packed"
                    ~actual:(Syn.Cst.VariantConstructor.name packed_constr);
                  (
                    match Syn.Cst.VariantConstructor.arguments packed_constr with
                    | Some (Syn.Cst.ConstructorArguments.Tuple [ _ ]) -> ()
                    | _ -> raise
                      (Failure "expected grouped GADT constructor to expose its arrow parameter")
                  );
                  (
                    match Syn.Cst.VariantConstructor.result_type packed_constr with
                    | Some (Syn.Cst.CoreType.Constr { constructor_path; arguments=[]; _ }) ->
                        Test.assert_equal
                          ~expected:(Some "packed")
                          ~actual:(Syn.Cst.Ident.name constructor_path);
                        Ok ()
                    | _ -> Error "expected grouped GADT constructor to expose its result type"
                  )
              | _ -> Error "expected grouped interface declaration to stay a GADT variant"
            )
          | _ -> Error "expected one grouped interface type declaration"
        )
      | _ -> Error "expected grouped interface type declaration");
  Test.case "cst grouped type declarations keep and-member docs and following section headings in order"
    (fun _ctx ->
      let result = parse_mli
        "type node = unit\n\
             (** Node type doc *)\n\
             (** Element type doc *)\n\
             and element = unit\n\
             (** {2 Next} *)\n\
             type next = unit\n"
      in
      assert_cst_json_snapshot ~ctx:_ctx result);
  Test.case "cst grouped type declarations keep docstrings on trailing and-members at eof"
    (fun _ctx ->
      let result = parse_mli
        "type a = unit\n\
             (** doc for b *)\n\
             and b = unit\n"
      in
      assert_cst_json_snapshot ~ctx:_ctx result);
  Test.case "cst grouped type declarations keep comments on trailing and-members at eof"
    (fun _ctx ->
      let result = parse_mli
        "type a = unit\n\
             (* comment for b *)\n\
             and b = unit\n"
      in
      assert_cst_json_snapshot ~ctx:_ctx result);
  Test.case "cst type declarations expose direct type parameters"
    (fun _ctx ->
      let result = parse_ml "type ('a, 'error) resultish = int\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ ->
          let params = Syn.Cst.TypeDeclaration.type_params decl
          |> List.filter_map Syn.Cst.TypeParameter.type_variable
          |> List.map Syn.Cst.TypeVariable.text in
          Test.assert_equal ~expected:[ "'a"; "'error" ] ~actual:params;
          Ok ()
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst type declarations expose parameter variance and injectivity"
    (fun _ctx ->
      let result = parse_ml "type (+!'a, -'b, !'c, 'd) descriptor = int\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ ->
          let params = Syn.Cst.TypeDeclaration.type_params decl in
          let variances =
            params
            |> List.map
              (fun param ->
                match Syn.Cst.TypeParameter.variance param with
                | Some (Syn.Cst.TypeParameterVariance.Covariant _) -> Some "covariant"
                | Some (Syn.Cst.TypeParameterVariance.Contravariant _) -> Some "contravariant"
                | None -> None)
          in
          let injectivity = params
          |> List.map (fun param -> Option.is_some (Syn.Cst.TypeParameter.injectivity_token param)) in
          let names = params
          |> List.map
            (fun param ->
              Syn.Cst.TypeParameter.type_variable param |> Option.map Syn.Cst.TypeVariable.text) in
          Test.assert_equal
            ~expected:[ Some "covariant"; Some "contravariant"; None; None ]
            ~actual:variances;
          Test.assert_equal ~expected:[ true; false; true; false ] ~actual:injectivity;
          Test.assert_equal ~expected:[ Some "'a"; Some "'b"; Some "'c"; Some "'d" ] ~actual:names;
          Ok ()
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst type declarations expose declaration constraints"
    (fun _ctx ->
      let result = parse_ml "type ('a, 'b) pair = 'a * 'b constraint 'a = int constraint 'b = string\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ ->
          let constraints = Syn.Cst.TypeDeclaration.constraints decl in
          let sides =
            constraints
            |> List.map
              (fun ({ left; right; _ }: Syn.Cst.TypeConstraint.t) ->
                let left_name =
                  match left with
                  | Syn.Cst.CoreType.Var { name_token; _ } -> Syn.Cst.Token.text name_token
                  | _ -> "<unexpected-left>"
                in
                let right_name =
                  match right with
                  | Syn.Cst.CoreType.Constr { constructor_path; _ } -> (
                      match Syn.Cst.Ident.name constructor_path with
                      | Some name -> name
                      | None -> "<missing-right>"
                    )
                  | _ -> "<unexpected-right>"
                in
                (left_name, right_name))
          in
          Test.assert_equal ~expected:[ ("a", "int"); ("b", "string") ] ~actual:sides;
          Ok ()
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst type declarations expose record fields structurally"
    (fun _ctx ->
      let result = parse_ml "type user = { mutable userName : string; created_at : int }\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.type_definition decl with
          | Syn.Cst.TypeDefinition.Record { fields; _ } ->
              let names = fields |> List.map Syn.Cst.RecordField.name in
              let mutability = fields
              |> List.map (fun field -> Option.is_some (Syn.Cst.RecordField.mutable_token field)) in
              Test.assert_equal ~expected:[ "userName"; "created_at" ] ~actual:names;
              Test.assert_equal ~expected:[ true; false ] ~actual:mutability;
              Ok ()
          | _ -> Error "expected record type definition"
        )
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst record fields separate field attributes from field types"
    (fun _ctx ->
      let result = parse_ml "type user = { name : int [@deprecated]; code : (string [@boxed]) }\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.type_definition decl with
          | Syn.Cst.TypeDefinition.Record { fields=[name_field;code_field]; _ } ->
              let attribute_name ({ name; _ }: Syn.Cst.attribute) = Syn.Cst.Ident.name name in
              let attribute_names field = Syn.Cst.RecordField.attributes field |> List.filter_map attribute_name in
              Test.assert_equal ~expected:[ "deprecated" ] ~actual:(attribute_names name_field);
              Test.assert_equal ~expected:[] ~actual:(attribute_names code_field);
              (
                match Syn.Cst.RecordField.field_type name_field with
                | Syn.Cst.CoreType.Constr { constructor_path; _ } -> Test.assert_equal
                  ~expected:(Some "int")
                  ~actual:(Syn.Cst.Ident.name constructor_path)
                | _ -> raise (Failure "expected attributed record field type to unwrap to int")
              );
              (
                match Syn.Cst.RecordField.field_type code_field with
                | Syn.Cst.CoreType.Parenthesized {
                  inner=Syn.Cst.CoreType.Attribute {
                    attribute;
                    type_=Syn.Cst.CoreType.Constr { constructor_path; _ };
                    _;

                  };
                  _;

                } ->
                    Test.assert_equal ~expected:(Some "boxed") ~actual:(attribute_name attribute);
                    Test.assert_equal
                      ~expected:(Some "string")
                      ~actual:(Syn.Cst.Ident.name constructor_path);
                    Ok ()
                | _ -> Error "expected parenthesized type attribute to remain on the field type"
              )
          | _ -> Error "expected record type definition"
        )
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst type declarations expose variant constructors structurally"
    (fun _ctx ->
      let result = parse_ml "type user = | Guest_user | RegisteredUser of int\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.type_definition decl with
          | Syn.Cst.TypeDefinition.Variant { constructors; _ } ->
              let names = constructors
              |> List.map Syn.Cst.VariantConstructor.name
              |> List.sort String.compare in
              Test.assert_equal ~expected:[ "Guest_user"; "RegisteredUser" ] ~actual:names;
              Ok ()
          | _ -> Error "expected variant type definition"
        )
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst variant constructors preserve tuple argument lists"
    (fun _ctx ->
      let result = parse_ml "type coord = Point2D of int * int | Wrapped of (int * int)\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.type_definition decl with
          | Syn.Cst.TypeDefinition.Variant { constructors=[point2d;wrapped]; _ } ->
              (
                match Syn.Cst.VariantConstructor.arguments point2d with
                | Some (Syn.Cst.ConstructorArguments.Tuple elements) -> Test.assert_equal
                  ~expected:2
                  ~actual:(List.length elements)
                | _ -> raise (Failure "expected Point2D constructor to expose two tuple arguments")
              );
              (
                match Syn.Cst.VariantConstructor.arguments wrapped with
                | Some (Syn.Cst.ConstructorArguments.Tuple [ wrapped_type ]) -> (
                    match wrapped_type with
                    | Syn.Cst.CoreType.Parenthesized {
                      inner=Syn.Cst.CoreType.Tuple { elements=[_;_]; _ };
                      _;

                    } -> Ok ()
                    | _ -> Error "expected Wrapped constructor to keep the parenthesized tuple as one argument"
                  )
                | _ -> Error "expected Wrapped constructor to expose a single tuple argument"
              )
          | _ -> Error "expected variant type definition"
        )
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst variant constructors preserve inline record arguments"
    (fun _ctx ->
      let result = parse_ml "type person = Person of { name : string; age : int } | Anonymous\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.type_definition decl with
          | Syn.Cst.TypeDefinition.Variant { constructors=[person;anonymous]; _ } ->
              (
                match Syn.Cst.VariantConstructor.arguments person with
                | Some (Syn.Cst.ConstructorArguments.Record { fields=[name_field;age_field]; _ }) ->
                    Test.assert_equal ~expected:"name" ~actual:(Syn.Cst.RecordField.name name_field);
                    Test.assert_equal ~expected:"age" ~actual:(Syn.Cst.RecordField.name age_field)
                | Some (Syn.Cst.ConstructorArguments.Record _) ->
                    raise (Failure "expected Person constructor inline record to have two fields")
                | _ ->
                    raise (Failure "expected Person constructor to expose inline record fields")
              );
              Test.assert_equal
                ~expected:None
                ~actual:(Syn.Cst.VariantConstructor.arguments anonymous);
              Ok ()
          | _ -> Error "expected variant type definition"
        )
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst GADT constructors expose argument and result structure"
    (fun _ctx ->
      let result = parse_ml "type _ expr = Int : int expr | Val : 'a -> 'a expr\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.type_definition decl with
          | Syn.Cst.TypeDefinition.Variant { constructors=[int_constr;val_constr]; _ } ->
              Test.assert_equal
                ~expected:None
                ~actual:(Syn.Cst.VariantConstructor.arguments int_constr);
              (
                match Syn.Cst.VariantConstructor.result_type int_constr with
                | Some (Syn.Cst.CoreType.Constr { constructor_path; arguments=[ _ ]; _ }) -> Test.assert_equal
                  ~expected:(Some "expr")
                  ~actual:(Syn.Cst.Ident.name constructor_path)
                | _ -> raise (Failure "expected Int GADT constructor to expose its result type")
              );
              (
                match Syn.Cst.VariantConstructor.arguments val_constr with
                | Some (Syn.Cst.ConstructorArguments.Tuple [ Syn.Cst.CoreType.Var _ ]) -> ()
                | _ -> raise (Failure "expected Val GADT constructor to expose its arrow parameter")
              );
              (
                match Syn.Cst.VariantConstructor.result_type val_constr with
                | Some (Syn.Cst.CoreType.Constr {
                  constructor_path;
                  arguments=[ Syn.Cst.CoreType.Var { name_token; _ } ];
                  _;

                }) ->
                    Test.assert_equal
                      ~expected:(Some "expr")
                      ~actual:(Syn.Cst.Ident.name constructor_path);
                    Test.assert_equal ~expected:"a" ~actual:(Syn.Cst.Token.text name_token);
                    Ok ()
                | _ -> Error "expected Val GADT constructor to expose its result type"
              )
          | _ -> Error "expected variant type definition"
        )
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst type extensions expose GADT constructor result types"
    (fun _ctx ->
      let result = parse_ml "type _ Effect.t += Yield : unit Effect.t\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeExtension decl :: _ -> (
          match Syn.Cst.TypeExtension.constructors decl with
          | [ yield ] ->
              Test.assert_equal ~expected:None ~actual:(Syn.Cst.VariantConstructor.arguments yield);
              (
                match Syn.Cst.VariantConstructor.result_type yield with
                | Some (Syn.Cst.CoreType.Constr { constructor_path; arguments=[ _ ]; _ }) ->
                    Test.assert_equal
                      ~expected:(Some "t")
                      ~actual:(Syn.Cst.Ident.name constructor_path);
                    Ok ()
                | _ -> Error "expected extension constructor to expose its result type"
              )
          | _ -> Error "expected one type-extension constructor"
        )
      | _ -> Error "expected type extension item");
  Test.case "cst type declarations expose polyvariant tags structurally"
    (fun _ctx ->
      let result = parse_ml "type user = [ `guest_user | `RegisteredUser of int ]\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.TypeDeclaration decl :: _ -> (
          match Syn.Cst.TypeDeclaration.type_definition decl with
          | Syn.Cst.TypeDefinition.PolyVariant poly_variant ->
              let names = Syn.Cst.PolyVariant.tags poly_variant |> List.map Syn.Cst.PolyVariantTag.name in
              Test.assert_equal ~expected:[ "guest_user"; "RegisteredUser" ] ~actual:names;
              Ok ()
          | _ -> Error "expected polyvariant type definition"
        )
      | _ -> Error "expected first item to be a type declaration");
  Test.case "cst polyvariant rows preserve inherited fields and bounds"
    (fun _ctx ->
      let result = parse_mli "val cast : [> base | `Ready ] -> unit\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration {
        type_=Syn.Cst.CoreType.Arrow {
          parameter_type=Syn.Cst.CoreType.PolyVariant poly_variant;
          result_type=Syn.Cst.CoreType.Constr _;
          _;

        };
        _;

      } :: _ -> (
          let fields = Syn.Cst.PolyVariant.fields poly_variant in
          match Syn.Cst.PolyVariant.kind poly_variant, fields with
          | Syn.Cst.PolyVariantBound.LowerBound _, [
            Syn.Cst.RowField.Inherit { type_=Syn.Cst.CoreType.Constr { constructor_path; _ }; _ };
            Syn.Cst.RowField.Tag tag;

          ] ->
              let inherited_name =
                match Syn.Cst.Ident.name constructor_path with
                | Some name -> name
                | None -> ""
              in
              Test.assert_equal ~expected:"base" ~actual:inherited_name;
              Test.assert_equal ~expected:"Ready" ~actual:(Syn.Cst.PolyVariantTag.name tag);
              Ok ()
          | _ -> Error "expected lower-bounded polyvariant row with inherited type"
        )
      | _ -> Error "expected first item to be a value declaration");
  Test.case "cst let bindings expose function binding names"
    (fun _ctx ->
      let result = parse_ml "let userProfile x = x\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.LetBinding binding :: _ ->
          Test.assert_equal ~expected:"userProfile" ~actual:(Syn.Cst.LetBinding.name binding);
          Test.assert_true (Syn.Cst.LetBinding.is_function binding);
          Ok ()
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst let bindings distinguish value bindings from function bindings"
    (fun _ctx ->
      let result = parse_ml "let userProfile = 42\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.LetBinding binding :: _ ->
          Test.assert_false (Syn.Cst.LetBinding.is_function binding);
          Ok ()
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst module declarations preserve structure module expressions"
    (fun _ctx ->
      let result = parse_ml "module Foo_bar = struct let answer = 42 end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.ModuleDeclaration ({
        module_name;
        functor_parameters=[];
        module_type=None;
        module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes=[ item_node ]; _ };
        _;

      } as decl) :: _ ->
          Test.assert_equal ~expected:"Foo_bar" ~actual:(Syn.Cst.Token.text module_name);
          Test.assert_equal
            ~expected:"LET_BINDING"
            ~actual:(SyntaxKind.to_string (Ceibo.Red.SyntaxNode.kind item_node));
          Test.assert_false (Syn.Cst.ModuleStructure.is_recursive decl);
          Ok ()
      | _ -> Error "expected module declaration with structure module expression");
  Test.case "cst builder can reify nested structure items from module expressions"
    (fun _ctx ->
      let result = parse_ml "module Foo_bar = struct let answer = 42 end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=((Syn.Cst.ModuleExpression.Structure _) as module_expression);
        _;

      } :: _ -> (
          match Syn.CstBuilder.structure_items_of_module_expression module_expression with
          | Ok [ Syn.Cst.StructureItem.LetBinding binding ] ->
              Test.assert_equal ~expected:"answer" ~actual:(Syn.Cst.LetBinding.name binding);
              Ok ()
          | Ok _ ->
              Error "expected nested structure items to lift a let binding"
          | Error _ ->
              Error "expected nested structure items to reify successfully"
        )
      | _ -> Error "expected module declaration with structure module expression");
  Test.case "cst builder keeps terminal nested structure comments standalone before end"
    (fun _ctx ->
      let result = parse_ml
        "module Foo_bar = struct\n\
             \  let answer = 42\n\
             \  (* keep me *)\n\
             end\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=((Syn.Cst.ModuleExpression.Structure _) as module_expression);
        _;

      } :: _ -> (
          match Syn.CstBuilder.structure_items_of_module_expression module_expression with
          | Ok [Syn.Cst.StructureItem.LetBinding binding;Syn.Cst.StructureItem.Comment comment;] ->
              Test.assert_equal ~expected:"answer" ~actual:(Syn.Cst.LetBinding.name binding);
              Test.assert_equal ~expected:"(* keep me *)" ~actual:(Syn.Cst.Comment.text comment);
              Ok ()
          | Ok _ ->
              Error "expected nested structure terminal comment to stay standalone"
          | Error _ ->
              Error "expected nested structure terminal comment to reify successfully"
        )
      | _ -> Error "expected module declaration with structure module expression");
  Test.case "cst builder normalizes nested structure grouped type docs and headings"
    (fun ctx ->
      assert_ml_cst_snapshot ~ctx
        "module Green = struct\n\
             \  (** ## Types *)\n\
             \n\
             \  type ('kind, 'text) token = { kind : 'kind; text : 'text; width : int }\n\
             \  (** Green token - leaf node containing source text. *)\n\
             \n\
             \  type ('kind, 'text) node = {\n\
             \    kind : 'kind;\n\
             \    children : ('kind, 'text) element array;\n\
             \  }\n\
             \  (** Green node - interior node with children. *)\n\
             \n\
             \  (** Element can be either a token or a node. *)\n\
             \  and ('kind, 'text) element =\n\
             \    | Token of ('kind, 'text) token\n\
             \    | Node of ('kind, 'text) node\n\
             \n\
             \  (** ## Construction *)\n\
             \n\
             \  let make_token ~kind ~text ~width = { kind; text; width }\n\
             end\n");
  Test.case "cst module declarations preserve constrained module expressions"
    (fun _ctx ->
      let result = parse_ml "module M : S with type t = int = struct type t = int end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_type=Some (Syn.Cst.ModuleType.With {
          base=Syn.Cst.ModuleType.Path outer_base;
          constraints=[ outer_constraint ];
          _;

        });
        module_expression=Syn.Cst.ModuleExpression.Constraint {
          module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes=[ item_node ]; _ };
          module_type=Syn.Cst.ModuleType.With {
            base=Syn.Cst.ModuleType.Path inner_base;
            constraints=[ inner_constraint ];
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name outer_base);
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name inner_base);
          (
            match (outer_constraint.constrained_type, inner_constraint.constrained_type) with
            | (Syn.Cst.CoreType.Constr { constructor_path=outer_path; arguments=outer_args; _ }, Syn.Cst.CoreType.Constr {
              constructor_path=inner_path;
              arguments=inner_args;
              _
            }) ->
                Test.assert_equal ~expected:(Some "t") ~actual:(Syn.Cst.Ident.name outer_path);
                Test.assert_equal ~expected:0 ~actual:(List.length outer_args);
                Test.assert_equal ~expected:(Some "t") ~actual:(Syn.Cst.Ident.name inner_path);
                Test.assert_equal ~expected:0 ~actual:(List.length inner_args);
                Test.assert_equal
                  ~expected:"TYPE_DECL"
                  ~actual:(SyntaxKind.to_string (Ceibo.Red.SyntaxNode.kind item_node));
                Ok ()
            | _ -> Error "expected constrained module-type targets"
          );
      | _ -> Error "expected constrained module declaration");
  Test.case "cst module declarations preserve identifier module expressions"
    (fun _ctx ->
      let result = parse_ml "module Alias = Source\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=Syn.Cst.ModuleExpression.Path path;
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "Source") ~actual:(Syn.Cst.Ident.name path);
          Ok ()
      | _ -> Error "expected module declaration with identifier module expression");
  Test.case "cst module declarations preserve functor module expressions"
    (fun _ctx ->
      let result = parse_ml "module F = functor (X : S) -> X\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=Syn.Cst.ModuleExpression.Functor {
          parameters=[ { name_token; module_type=Syn.Cst.ModuleType.Path param_type; _ } ];
          body=Syn.Cst.ModuleExpression.Path body_path;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"X" ~actual:(Syn.Cst.Token.text name_token);
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name param_type);
          Test.assert_equal ~expected:(Some "X") ~actual:(Syn.Cst.Ident.name body_path);
          Ok ()
      | _ -> Error "expected module declaration with functor module expression");
  Test.case "cst module declarations preserve functor applications structurally"
    (fun _ctx ->
      let result = parse_ml "module M = F(X)(Y)\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=Syn.Cst.ModuleExpression.Apply {
          callee=Syn.Cst.ModuleExpression.Apply {
            callee=Syn.Cst.ModuleExpression.Path functor_path;
            argument=Syn.Cst.ModuleExpression.Path first_arg;
            _;

          };
          argument=Syn.Cst.ModuleExpression.Path second_arg;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "F") ~actual:(Syn.Cst.Ident.name functor_path);
          Test.assert_equal ~expected:(Some "X") ~actual:(Syn.Cst.Ident.name first_arg);
          Test.assert_equal ~expected:(Some "Y") ~actual:(Syn.Cst.Ident.name second_arg);
          Ok ()
      | _ -> Error "expected module declaration with functor application");
  Test.case "cst module declarations preserve functor applications across inline comments"
    (fun _ctx ->
      let result = parse_ml "module M = F (* c *) (X)\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=Syn.Cst.ModuleExpression.Apply {
          callee=Syn.Cst.ModuleExpression.Path functor_path;
          argument=Syn.Cst.ModuleExpression.Path arg_path;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "F") ~actual:(Syn.Cst.Ident.name functor_path);
          Test.assert_equal ~expected:(Some "X") ~actual:(Syn.Cst.Ident.name arg_path);
          Ok ()
      | _ -> Error "expected commented module application");
  Test.case "cst module declarations preserve unit functor applications"
    (fun _ctx ->
      let result = parse_ml "module M = F()\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=Syn.Cst.ModuleExpression.ApplyUnit {
          callee=Syn.Cst.ModuleExpression.Path functor_path;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "F") ~actual:(Syn.Cst.Ident.name functor_path);
          Ok ()
      | _ -> Error "expected module declaration with unit functor application");
  Test.case "cst module declarations preserve extension module expressions"
    (fun _ctx ->
      let result = parse_ml "module M = [%driver]\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=Syn.Cst.ModuleExpression.Extension extension;
        _;

      } :: _ ->
          Test.assert_equal ~expected:"%" ~actual:(extension_sigil_text extension);
          Test.assert_equal ~expected:(Some "driver") ~actual:(Syn.Cst.Ident.name extension.name);
          Ok ()
      | _ -> Error "expected module declaration with extension module expression");
  Test.case "cst module declarations preserve unpacked first-class modules"
    (fun _ctx ->
      let result = parse_ml "module M = (val packed : S)\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=Syn.Cst.ModuleExpression.Parenthesized {
          inner=Syn.Cst.ModuleExpression.ModuleUnpack {
            expression=Syn.Cst.Expression.Path { path=module_path; _ };
            package_type=Some { module_type_path; constraints=[]; _ };
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "packed") ~actual:(Syn.Cst.Ident.name module_path);
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name module_type_path);
          Ok ()
      | _ -> Error "expected module declaration with unpacked first-class module");
  Test.case "cst module declarations preserve parenthesized module-type-of lookahead with comments"
    (fun _ctx ->
      let result = parse_ml "module M : ((module (* c *) type of N)) = N\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_type=Some Syn.Cst.ModuleType.Parenthesized {
          inner=Syn.Cst.ModuleType.Parenthesized {
            inner=Syn.Cst.ModuleType.TypeOf { module_path; _ };
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "N") ~actual:(Syn.Cst.Ident.name module_path);
          Ok ()
      | _ -> Error "expected module declaration with parenthesized module-type-of body");
  Test.case
    "cst recursive module items preserve grouped bindings"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "module rec A : sig val x : int end = struct let x = B.y end\nand B : sig val y : int end = struct let y = 1 end\n");
  Test.case "cst interface module declarations preserve signature-only bindings"
    (fun _ctx ->
      let result = parse_mli "module M : sig val x : int end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ModuleDeclaration ({
        module_name;
        definition=Syn.Cst.ModuleSignature.Signature (Syn.Cst.ModuleType.Signature _);
        _;

      } as decl) :: _ ->
          Test.assert_equal ~expected:"M" ~actual:(Syn.Cst.Token.text module_name);
          Test.assert_false (Syn.Cst.ModuleSignature.is_recursive decl);
          Ok ()
      | _ -> Error "expected first item to be an interface module declaration");
  Test.case
    "cst interface recursive modules preserve grouped signatures"
    (fun ctx -> assert_mli_cst_snapshot ~ctx "module rec A : sig val x : int end\nand B : sig val y : int end\n");
  Test.case "cst module type declarations expose declared names"
    (fun _ctx ->
      let result = parse_ml "module type Foo_bar = sig end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration decl :: _ ->
          Test.assert_equal ~expected:"Foo_bar" ~actual:(Syn.Cst.ModuleTypeDeclaration.name decl);
          Ok ()
      | _ -> Error "expected first item to be a module type declaration");
  Test.case "cst module type lookahead survives inline comments"
    (fun _ctx ->
      let result = parse_ml "module (* c *) type S = sig end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = structure_items cst in
      match items with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration decl :: _ ->
          Test.assert_equal ~expected:"S" ~actual:(Syn.Cst.ModuleTypeDeclaration.name decl);
          Ok ()
      | _ -> Error "expected first item to be a commented module type declaration");
  Test.case "cst interface module type declarations expose declared names"
    (fun _ctx ->
      let result = parse_mli "module type Foo_bar = sig end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let items = signature_items cst in
      match items with
      | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl :: _ ->
          Test.assert_equal ~expected:"Foo_bar" ~actual:(Syn.Cst.ModuleTypeDeclaration.name decl);
          Ok ()
      | _ -> Error "expected first item to be an interface module type declaration");
  Test.case "cst module type declarations preserve identifier module type bodies"
    (fun _ctx ->
      let result = parse_ml "module type Alias = Source\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.Path path);
        _
      } :: _ ->
          Test.assert_equal ~expected:(Some "Source") ~actual:(Syn.Cst.Ident.name path);
          Ok ()
      | _ -> Error "expected module type declaration with identifier body");
  Test.case "cst interface module type declarations preserve identifier module type bodies"
    (fun _ctx ->
      let result = parse_mli "module type Alias = Source\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.Path path);
        _
      } :: _ ->
          Test.assert_equal ~expected:(Some "Source") ~actual:(Syn.Cst.Ident.name path);
          Ok ()
      | _ -> Error "expected interface module type declaration with identifier body");
  Test.case "cst module type declarations preserve signature module type bodies"
    (fun _ctx ->
      let result = parse_ml "module type S = sig val x : int end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.Signature _);
        _
      } :: _ -> Ok ()
      | _ -> Error "expected module type declaration with signature body");
  Test.case "cst builder can reify nested signature items from module types"
    (fun _ctx ->
      let result = parse_ml "module type S = sig val x : int end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some ((Syn.Cst.ModuleType.Signature { signature_syntax_node; _ }) as module_type);
        _;

      } :: _ ->
          Test.assert_equal
            ~expected:"SIG_EXPR"
            ~actual:(SyntaxKind.to_string (Ceibo.Red.SyntaxNode.kind signature_syntax_node));
          (
            match Syn.CstBuilder.signature_items_of_module_type module_type with
            | Ok [ Syn.Cst.SignatureItem.ValueDeclaration decl ] ->
                Test.assert_equal ~expected:"x" ~actual:(declaration_name_text decl.name_tokens);
                Ok ()
            | Ok _ ->
                Error "expected nested signature items to lift a val declaration"
            | Error _ ->
                Error "expected nested signature items to reify successfully"
          )
      | _ -> Error "expected module type declaration with signature body");
  Test.case "cst builder keeps terminal nested signature docstrings standalone before end"
    (fun _ctx ->
      let result = parse_ml
        "module type S = sig\n\
             \  val x : int\n\
             \  (** keep me *)\n\
             end\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some ((Syn.Cst.ModuleType.Signature _) as module_type);
        _;

      } :: _ -> (
          match Syn.CstBuilder.signature_items_of_module_type module_type with
          | Ok [
            Syn.Cst.SignatureItem.ValueDeclaration decl;
            Syn.Cst.SignatureItem.Docstring docstring;

          ] ->
              Test.assert_equal ~expected:"x" ~actual:(declaration_name_text decl.name_tokens);
              Test.assert_equal ~expected:"(** keep me *)" ~actual:(Syn.Cst.Docstring.text docstring);
              Ok ()
          | Ok _ ->
              Error "expected nested signature terminal docstring to stay standalone"
          | Error _ ->
              Error "expected nested signature terminal docstring to reify successfully"
        )
      | _ -> Error "expected module type declaration with signature body");
  Test.case "cst builder keeps terminal nested signature comments standalone before end"
    (fun _ctx ->
      let result = parse_ml
        "module type S = sig\n\
             \  val x : int\n\
             \  (* keep me *)\n\
             end\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some ((Syn.Cst.ModuleType.Signature _) as module_type);
        _;

      } :: _ -> (
          match Syn.CstBuilder.signature_items_of_module_type module_type with
          | Ok [Syn.Cst.SignatureItem.ValueDeclaration decl;Syn.Cst.SignatureItem.Comment comment;] ->
              Test.assert_equal ~expected:"x" ~actual:(declaration_name_text decl.name_tokens);
              Test.assert_equal
                ~expected:None
                ~actual:(Syn.Cst.ValueDeclaration.trailing_comment decl);
              Test.assert_equal ~expected:"(* keep me *)" ~actual:(Syn.Cst.Comment.text comment);
              Ok ()
          | Ok _ ->
              Error "expected nested signature terminal comment to stay standalone"
          | Error _ ->
              Error "expected nested signature terminal comment to reify successfully"
        )
      | _ -> Error "expected module type declaration with signature body");
  Test.case "cst builder normalizes nested signature grouped type docs and headings"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        "module Green : sig\n\
             \  (** ## Types *)\n\
             \n\
             \  type ('kind, 'text) token = { kind : 'kind; text : 'text; width : int }\n\
             \  (** Green token - leaf node containing source text. *)\n\
             \n\
             \  type ('kind, 'text) node = {\n\
             \    kind : 'kind;\n\
             \    children : ('kind, 'text) element array;\n\
             \  }\n\
             \  (** Green node - interior node with children. *)\n\
             \n\
             \  (** Element can be either a token or a node. *)\n\
             \  and ('kind, 'text) element =\n\
             \    | Token of ('kind, 'text) token\n\
             \    | Node of ('kind, 'text) node\n\
             \n\
             \  (** ## Construction *)\n\
             \n\
             \  val make_token : kind:'kind -> text:'text -> width:int -> ('kind, 'text) token\n\
             end\n");
  Test.case "cst builder keeps nested signature prefix docs before declarations"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        {|
module Span : sig
  (** Span type representing a range in source text. *)
  type t
  (** `make` creates a span. *)
  val make : unit -> t
end
|});
  Test.case "cst builder normalizes repeated nested signature type docs"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        "module Capabilities : sig\n\
             \  (** {2 Capabilities} *)\n\
             \n\
             \  (** Empty for now, can be extended *)\n\
             \  type tool_capability = unit\n\
             \  type resource_capability = { subscribe : bool option }\n\
             \  (** Empty for now, can be extended *)\n\
             \  type prompt_capability = unit\n\
             \  (** Empty for now, can be extended *)\n\
             \  type sampling_capability = unit\n\
             end\n");
  Test.case "cst builder keeps ceibo nested signature headings stable"
    (fun ctx ->
      let source = read_file
        Path.(Path.v "packages" / Path.v "ceibo" / Path.v "src" / Path.v "ceibo.mli") in
      assert_mli_cst_snapshot ~ctx source);
  Test.case "cst builder preserves nested signature standalone docs and comments after open"
    (fun _ctx ->
      let result = parse_mli
        "module Nested : sig\n\
             \  open Std\n\
             \  (** Nested overview *)\n\
             \  (* plain note *)\n\
             \  val x : int\n\
             end\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [
        Syn.Cst.SignatureItem.ModuleDeclaration {
          definition=Syn.Cst.ModuleSignature.Signature ((Syn.Cst.ModuleType.Signature _) as module_type);
          _;

        }
      ] -> (
          match Syn.CstBuilder.signature_items_of_module_type module_type with
          | Ok [
            Syn.Cst.SignatureItem.OpenStatement _;
            Syn.Cst.SignatureItem.Docstring docstring;
            Syn.Cst.SignatureItem.Comment comment;
            Syn.Cst.SignatureItem.ValueDeclaration decl
          ] ->
              Test.assert_equal
                ~expected:"(** Nested overview *)"
                ~actual:(Syn.Cst.Docstring.text docstring);
              Test.assert_equal ~expected:"(* plain note *)" ~actual:(Syn.Cst.Comment.text comment);
              Test.assert_equal ~expected:"x" ~actual:(declaration_name_text decl.name_tokens);
              Ok ()
          | Ok _ ->
              Error "expected nested signature open/doc/comment/value ordering"
          | Error _ ->
              Error "expected nested signature standalone docs/comments to normalize successfully"
        )
      | _ -> Error "expected module declaration with nested signature body");
  Test.case "cst module type declarations preserve with-constraint bodies"
    (fun _ctx ->
      let result = parse_ml "module type S = Driver with type config = int\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.With {
          base=Syn.Cst.ModuleType.Path base_path;
          constraints=[ constraint_ ];
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "Driver") ~actual:(Syn.Cst.Ident.name base_path);
          (
            match constraint_.constrained_type with
            | Syn.Cst.CoreType.Constr { constructor_path; arguments; _ } ->
                Test.assert_equal
                  ~expected:(Some "config")
                  ~actual:(Syn.Cst.Ident.name constructor_path);
                Test.assert_equal ~expected:0 ~actual:(List.length arguments);
                Ok ()
            | _ -> Error "expected constrained module-type target"
          )
      | _ -> Error "expected module type declaration with constrained body");
  Test.case "cst module type declarations preserve module-type-of bodies"
    (fun _ctx ->
      let result = parse_ml "module type S = module type of Stdlib.Array\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.TypeOf {
          module_path=Syn.Cst.Ident.Qualified { name_token; _ };
          _
        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Array" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected module type declaration with module-type-of body");
  Test.case "cst module type declarations keep module-type-of lookahead stable across inline comments"
    (fun _ctx ->
      let result = parse_ml "module type S = module (* c1 *) type (* c2 *) of Stdlib.Array\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.TypeOf {
          module_path=Syn.Cst.Ident.Qualified { name_token; _ };
          _
        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Array" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected commented module type declaration with module-type-of body");
  Test.case "cst module type declarations preserve extension module type bodies"
    (fun _ctx ->
      let result = parse_ml "module type S = [%sig_ext]\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.Extension extension);
        _;

      } :: _ ->
          Test.assert_equal ~expected:"%" ~actual:(extension_sigil_text extension);
          Test.assert_equal ~expected:(Some "sig_ext") ~actual:(Syn.Cst.Ident.name extension.name);
          Ok ()
      | _ -> Error "expected module type declaration with extension body");
  Test.case "cst interface class declarations preserve typed class-type annotations"
    (fun _ctx ->
      let result = parse_mli "class c : object method x : int end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ClassDeclaration {
        class_name;
        class_type=Syn.Cst.ClassType.Signature {
          syntax_node=class_type_syntax_node;
          fields=[
            Syn.Cst.ClassTypeField.Method {
              name_token=class_type_method_name;
              type_=Syn.Cst.CoreType.Constr { constructor_path=declared_type; _;  };
              _;

            };

          ];

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"c" ~actual:(Syn.Cst.Token.text class_name);
          Test.assert_equal
            ~expected:"OBJECT_EXPR"
            ~actual:(SyntaxKind.to_string (Ceibo.Red.SyntaxNode.kind class_type_syntax_node));
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text class_type_method_name);
          Test.assert_equal ~expected:(Some "int") ~actual:(Syn.Cst.Ident.name declared_type);
          Ok ()
      | _ -> Error "expected interface class declaration");
  Test.case "cst implementation class declarations preserve declaration-site class-type annotations"
    (fun _ctx ->
      let result = parse_ml "class service : object method run : response end = object method run = value end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassDeclaration {
        class_name;
        class_type=Some (Syn.Cst.ClassType.Signature {
          fields=[
            Syn.Cst.ClassTypeField.Method {
              name_token=method_name;
              type_=Syn.Cst.CoreType.Constr { constructor_path=response_type; _ };
              _;

            };

          ];
          _;

        });
        class_body=Syn.Cst.ClassExpression.Structure {
          fields=[
            Syn.Cst.ClassField.Method {
              name_token=body_method_name;
              definition=Syn.Cst.ConcreteMethod {
                body=Syn.Cst.Expression.Path { path=body_value_path; _ };
                type_=None;

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"service" ~actual:(Syn.Cst.Token.text class_name);
          Test.assert_equal ~expected:"run" ~actual:(Syn.Cst.Token.text method_name);
          Test.assert_equal ~expected:(Some "response") ~actual:(Syn.Cst.Ident.name response_type);
          Test.assert_equal ~expected:"run" ~actual:(Syn.Cst.Token.text body_method_name);
          Test.assert_equal ~expected:(Some "value") ~actual:(Syn.Cst.Ident.name body_value_path);
          Ok ()
      | _ -> Error "expected implementation class declaration");
  Test.case "cst class declarations preserve typed class-expression forms"
    (fun _ctx ->
      let result = parse_ml
        "class direct = builder\n\
             class applied = builder arg\n\
             class factory = fun x -> object method run = x end\n\
             class local = let helper = 1 in object method run = helper end\n\
             class opened = M.(builder)\n\
             class generated = [%class_body]\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassDeclaration {
        class_name=direct_name;
        class_body=Syn.Cst.ClassExpression.Path direct_path;
        _;

      } :: Syn.Cst.StructureItem.ClassDeclaration {
        class_name=applied_name;
        class_body=Syn.Cst.ClassExpression.Apply {
          callee=Syn.Cst.ClassExpression.Path applied_callee;
          argument=Syn.Cst.Positional (Syn.Cst.Expression.Path { path=applied_arg; _ });
          _;

        };
        _;

      } :: Syn.Cst.StructureItem.ClassDeclaration {
        class_name=factory_name;
        class_body=Syn.Cst.ClassExpression.Fun {
          parameters=[ Syn.Cst.Parameter.Positional { name_token=Some factory_param; _ };  ];
          body=Syn.Cst.ClassExpression.Structure {
            fields=[
              Syn.Cst.ClassField.Method {
                name_token=factory_method_name;
                definition=Syn.Cst.ConcreteMethod {
                  body=Syn.Cst.Expression.Path { path=factory_body_path; _ };
                  type_=None;

                };
                _;

              };

            ];
            _;

          };
          _;

        };
        _;

      } :: Syn.Cst.StructureItem.ClassDeclaration {
        class_name=local_name;
        class_body=Syn.Cst.ClassExpression.Let {
          binding_pattern=Syn.Cst.Pattern.Identifier { name_token=helper_name; _ };
          bound_value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int { literal_token; _ });
          body=Syn.Cst.ClassExpression.Structure {
            fields=[
              Syn.Cst.ClassField.Method {
                definition=Syn.Cst.ConcreteMethod {
                  body=Syn.Cst.Expression.Path { path=helper_path; _ };
                  type_=None;

                };
                _;

              };

            ];
            _;

          };
          _;

        };
        _;

      } :: Syn.Cst.StructureItem.ClassDeclaration {
        class_name=opened_name;
        class_body=Syn.Cst.ClassExpression.LocalOpen (Syn.Cst.Delimited {
          module_path=opened_module_path;
          body=Syn.Cst.ClassExpression.Path opened_body_path;
          _;

        });
        _;

      } :: Syn.Cst.StructureItem.ClassDeclaration {
        class_name=generated_name;
        class_body=Syn.Cst.ClassExpression.Extension extension;
        _;

      } :: _ ->
          Test.assert_equal ~expected:"direct" ~actual:(Syn.Cst.Token.text direct_name);
          Test.assert_equal ~expected:(Some "builder") ~actual:(Syn.Cst.Ident.name direct_path);
          Test.assert_equal ~expected:"applied" ~actual:(Syn.Cst.Token.text applied_name);
          Test.assert_equal ~expected:(Some "builder") ~actual:(Syn.Cst.Ident.name applied_callee);
          Test.assert_equal ~expected:(Some "arg") ~actual:(Syn.Cst.Ident.name applied_arg);
          Test.assert_equal ~expected:"factory" ~actual:(Syn.Cst.Token.text factory_name);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text factory_param);
          Test.assert_equal ~expected:"run" ~actual:(Syn.Cst.Token.text factory_method_name);
          Test.assert_equal ~expected:(Some "x") ~actual:(Syn.Cst.Ident.name factory_body_path);
          Test.assert_equal ~expected:"local" ~actual:(Syn.Cst.Token.text local_name);
          Test.assert_equal ~expected:"helper" ~actual:(Syn.Cst.Token.text helper_name);
          Test.assert_equal ~expected:"1" ~actual:(Syn.Cst.Token.text literal_token);
          Test.assert_equal ~expected:(Some "helper") ~actual:(Syn.Cst.Ident.name helper_path);
          Test.assert_equal ~expected:"opened" ~actual:(Syn.Cst.Token.text opened_name);
          Test.assert_equal ~expected:(Some "M") ~actual:(Syn.Cst.Ident.name opened_module_path);
          Test.assert_equal ~expected:(Some "builder") ~actual:(Syn.Cst.Ident.name opened_body_path);
          Test.assert_equal ~expected:"generated" ~actual:(Syn.Cst.Token.text generated_name);
          Test.assert_equal ~expected:(Some "class_body") ~actual:(Syn.Cst.Ident.name extension.name);
          Ok ()
      | _ -> Error "expected typed class-expression declarations");
  Test.case "cst class declarations preserve constrained class-expression bodies"
    (fun _ctx ->
      let result = parse_ml "class constrained = (builder : service)\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassDeclaration {
        class_name;
        class_body=Syn.Cst.ClassExpression.Constraint {
          class_expression=Syn.Cst.ClassExpression.Path builder_path;
          class_type=Syn.Cst.ClassType.Path service_path;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"constrained" ~actual:(Syn.Cst.Token.text class_name);
          Test.assert_equal ~expected:(Some "builder") ~actual:(Syn.Cst.Ident.name builder_path);
          Test.assert_equal ~expected:(Some "service") ~actual:(Syn.Cst.Ident.name service_path);
          Ok ()
      | _ -> Error "expected constrained class-expression declaration");
  Test.case "cst class structures preserve field attributes"
    (fun ctx ->
      assert_ml_cst_snapshot ~ctx
        "class c = object\n\
             \  inherit builder [@@inh]\n\
             \  val state = seed [@@tracked]\n\
             \  method run = state [@@trace]\n\
             \  constraint t = int [@@eq]\n\
             \  initializer ignore state [@@init]\n\
             end\n");
  Test.case "cst class structures preserve class fields, constraints, and extensions"
    (fun _ctx ->
      let result = parse_ml
        "class c = object\n\
             \  val mutable state = seed\n\
             \  inherit (builder arg)\n\
             \  method private run = state\n\
             \  constraint t = int\n\
             \  [%%field]\n\
             \  initializer ignore state\n\
             end\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassDeclaration {
        class_body=Syn.Cst.ClassExpression.Structure {
          fields=[
            Syn.Cst.ClassField.Value {
              name_token=state_name;
              definition=Syn.Cst.ConcreteValue {
                value=Syn.Cst.Expression.Path { path=state_value; _ };
                type_=None;

              };
              modifier_tokens=[ state_mutable_token ];
              _;

            };
            Syn.Cst.ClassField.Inherit {
              class_expression=Syn.Cst.ClassExpression.Parenthesized {
                inner=Syn.Cst.ClassExpression.Apply {
                  callee=Syn.Cst.ClassExpression.Path inherit_callee;
                  argument=Syn.Cst.Positional (Syn.Cst.Expression.Path { path=inherit_arg; _ });
                  _;

                };
                _;

              };
              _;

            };
            Syn.Cst.ClassField.Method {
              name_token=method_name;
              definition=Syn.Cst.ConcreteMethod {
                body=Syn.Cst.Expression.Path { path=method_body; _ };
                type_=None;

              };
              modifier_tokens=[ run_private_token ];
              _;

            };
            Syn.Cst.ClassField.Constraint {
              left=Syn.Cst.CoreType.Constr { constructor_path=left_type; _ };
              right=Syn.Cst.CoreType.Constr { constructor_path=right_type; _ };
              _;

            };
            Syn.Cst.ClassField.Extension extension;
            Syn.Cst.ClassField.Initializer {
              body=Syn.Cst.Expression.Apply {
                callee=Syn.Cst.Expression.Path { path=init_callee; _ };
                argument=Syn.Cst.Positional (Syn.Cst.Expression.Path { path=init_arg; _ });
                _;

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"state" ~actual:(Syn.Cst.Token.text state_name);
          Test.assert_equal ~expected:"mutable" ~actual:(Syn.Cst.Token.text state_mutable_token);
          Test.assert_equal ~expected:(Some "seed") ~actual:(Syn.Cst.Ident.name state_value);
          Test.assert_equal ~expected:(Some "builder") ~actual:(Syn.Cst.Ident.name inherit_callee);
          Test.assert_equal ~expected:(Some "arg") ~actual:(Syn.Cst.Ident.name inherit_arg);
          Test.assert_equal ~expected:"run" ~actual:(Syn.Cst.Token.text method_name);
          Test.assert_equal ~expected:"private" ~actual:(Syn.Cst.Token.text run_private_token);
          Test.assert_equal ~expected:(Some "state") ~actual:(Syn.Cst.Ident.name method_body);
          Test.assert_equal ~expected:(Some "t") ~actual:(Syn.Cst.Ident.name left_type);
          Test.assert_equal ~expected:(Some "int") ~actual:(Syn.Cst.Ident.name right_type);
          Test.assert_equal ~expected:(Some "field") ~actual:(Syn.Cst.Ident.name extension.name);
          Test.assert_equal ~expected:(Some "ignore") ~actual:(Syn.Cst.Ident.name init_callee);
          Test.assert_equal ~expected:(Some "state") ~actual:(Syn.Cst.Ident.name init_arg);
          Ok ()
      | _ -> Error "expected structured class fields");
  Test.case "cst class structures distinguish concrete and virtual members"
    (fun _ctx ->
      let result = parse_ml
        "class c = object\n\
             \  val mutable state = seed\n\
             \  val virtual name : string\n\
             \  method run = state\n\
             \  method virtual reset : unit\n\
             end\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassDeclaration {
        class_body=Syn.Cst.ClassExpression.Structure {
          fields=[
            Syn.Cst.ClassField.Value {
              definition=Syn.Cst.ConcreteValue {
                value=Syn.Cst.Expression.Path { path=state_value; _ };
                type_=None;

              };
              modifier_tokens=[ state_mutable_token ];
              _;

            };
            Syn.Cst.ClassField.Value {
              name_token=virtual_value_name;
              definition=Syn.Cst.VirtualValue {
                virtual_token=virtual_value_token;
                type_=Syn.Cst.CoreType.Constr { constructor_path=virtual_value_type; _ };

              };
              _;

            };
            Syn.Cst.ClassField.Method {
              definition=Syn.Cst.ConcreteMethod {
                body=Syn.Cst.Expression.Path { path=method_body; _ };
                type_=None;

              };
              _;

            };
            Syn.Cst.ClassField.Method {
              name_token=virtual_method_name;
              definition=Syn.Cst.VirtualMethod {
                virtual_token=virtual_method_token;
                type_=Syn.Cst.CoreType.Constr { constructor_path=virtual_method_type; _ };

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "seed") ~actual:(Syn.Cst.Ident.name state_value);
          Test.assert_equal ~expected:"mutable" ~actual:(Syn.Cst.Token.text state_mutable_token);
          Test.assert_equal ~expected:"name" ~actual:(Syn.Cst.Token.text virtual_value_name);
          Test.assert_equal ~expected:"virtual" ~actual:(Syn.Cst.Token.text virtual_value_token);
          Test.assert_equal ~expected:(Some "string") ~actual:(Syn.Cst.Ident.name virtual_value_type);
          Test.assert_equal ~expected:(Some "state") ~actual:(Syn.Cst.Ident.name method_body);
          Test.assert_equal ~expected:"reset" ~actual:(Syn.Cst.Token.text virtual_method_name);
          Test.assert_equal ~expected:"virtual" ~actual:(Syn.Cst.Token.text virtual_method_token);
          Test.assert_equal ~expected:(Some "unit") ~actual:(Syn.Cst.Ident.name virtual_method_type);
          Ok ()
      | _ -> Error "expected concrete and virtual class member definitions");
  Test.case "cst builder keeps class trailing trivia as explicit body entries"
    (fun _ctx ->
      let source = "class c = object\n\
           \  method run = 1\n\
           \  (** body doc *)\n\
           \  (* body comment *)\n\
           end\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassDeclaration {
        class_body=Syn.Cst.ClassExpression.Structure { syntax_node; fields; _;  };
        _;

      } :: _ ->
          let body_items = Syn.CstBuilder.class_field_items_of_fields ~source_node:syntax_node fields in
          (
            match body_items with
            | [
              Syn.CstBuilder.ClassField (Syn.Cst.ClassField.Method _);
              Syn.CstBuilder.Docstring docstring;
              Syn.CstBuilder.Comment comment;

            ] ->
                Test.assert_equal
                  ~expected:"(** body doc *)"
                  ~actual:(Syn.Cst.Docstring.text docstring);
                Test.assert_equal
                  ~expected:"(* body comment *)"
                  ~actual:(Syn.Cst.Comment.text comment);
                Ok ()
            | _ -> Error "expected class field stream to include method and body docstring/comment"
          )
      | _ -> Error "expected class declaration with structure body");
  Test.case "cst interface class type declarations preserve structured signatures"
    (fun _ctx ->
      let result = parse_mli "class type ct = object method x : int end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ClassTypeDeclaration {
        class_type_name;
        class_type_body=Syn.Cst.ClassType.Signature {
          syntax_node=class_type_body_syntax_node;
          fields=[
            Syn.Cst.ClassTypeField.Method {
              name_token;
              type_=Syn.Cst.CoreType.Constr { constructor_path; _ };
              _;

            };

          ];

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"ct" ~actual:(Syn.Cst.Token.text class_type_name);
          Test.assert_equal
            ~expected:"OBJECT_EXPR"
            ~actual:(SyntaxKind.to_string (Ceibo.Red.SyntaxNode.kind class_type_body_syntax_node));
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text name_token);
          Test.assert_equal ~expected:(Some "int") ~actual:(Syn.Cst.Ident.name constructor_path);
          Ok ()
      | _ -> Error "expected interface class type declaration");
  Test.case "cst interface class type lookahead survives inline comments"
    (fun _ctx ->
      let result = parse_mli "class (* c *) type ct = object method x : int end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ClassTypeDeclaration {
        class_type_name;
        class_type_body=Syn.Cst.ClassType.Signature {
          fields=[
            Syn.Cst.ClassTypeField.Method {
              name_token;
              type_=Syn.Cst.CoreType.Constr { constructor_path; _ };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"ct" ~actual:(Syn.Cst.Token.text class_type_name);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text name_token);
          Test.assert_equal ~expected:(Some "int") ~actual:(Syn.Cst.Ident.name constructor_path);
          Ok ()
      | _ -> Error "expected commented interface class type declaration");
  Test.case "cst rejects locally opened class types"
    (fun _ctx ->
      let result = parse_ml
        {|
class type direct = C
class type opened = M.(C)
class type generated = ([%ct])
|}
      in
      Test.assert_equal ~expected:[] ~actual:result.diagnostics;
      Test.assert_equal ~expected:None ~actual:result.cst;
      Ok ());
  Test.case "cst interface class declarations preserve arrow-style class types"
    (fun _ctx ->
      let result = parse_mli "class service : request -> object method run : int end\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ClassDeclaration {
        class_name;
        class_type=Syn.Cst.ClassType.Arrow {
          parameter_type=Syn.Cst.CoreType.Constr { constructor_path=request_type; _ };
          result_type=Syn.Cst.ClassType.Signature {
            fields=[
              Syn.Cst.ClassTypeField.Method {
                name_token=method_name;
                type_=Syn.Cst.CoreType.Constr { constructor_path=method_type; _ };
                _;

              };

            ];
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"service" ~actual:(Syn.Cst.Token.text class_name);
          Test.assert_equal ~expected:(Some "request") ~actual:(Syn.Cst.Ident.name request_type);
          Test.assert_equal ~expected:"run" ~actual:(Syn.Cst.Token.text method_name);
          Test.assert_equal ~expected:(Some "int") ~actual:(Syn.Cst.Ident.name method_type);
          Ok ()
      | _ -> Error "expected interface class declaration with arrow class type");
  Test.case "cst class type declarations preserve arrow and parenthesized arrow bodies"
    (fun _ctx ->
      let result = parse_ml
        "class type factory = request -> response -> service\n\
             class type grouped = (request -> service)\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassTypeDeclaration {
        class_type_name=factory_name;
        class_type_body=Syn.Cst.ClassType.Arrow {
          parameter_type=Syn.Cst.CoreType.Constr { constructor_path=request_type; _ };
          result_type=Syn.Cst.ClassType.Arrow {
            parameter_type=Syn.Cst.CoreType.Constr { constructor_path=response_type; _ };
            result_type=Syn.Cst.ClassType.Path service_type;
            _;

          };
          _;

        };
        _;

      } :: Syn.Cst.StructureItem.ClassTypeDeclaration {
        class_type_name=grouped_name;
        class_type_body=Syn.Cst.ClassType.Parenthesized {
          inner=Syn.Cst.ClassType.Arrow {
            parameter_type=Syn.Cst.CoreType.Constr { constructor_path=grouped_request; _ };
            result_type=Syn.Cst.ClassType.Path grouped_service;
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"factory" ~actual:(Syn.Cst.Token.text factory_name);
          Test.assert_equal ~expected:(Some "request") ~actual:(Syn.Cst.Ident.name request_type);
          Test.assert_equal ~expected:(Some "response") ~actual:(Syn.Cst.Ident.name response_type);
          Test.assert_equal ~expected:(Some "service") ~actual:(Syn.Cst.Ident.name service_type);
          Test.assert_equal ~expected:"grouped" ~actual:(Syn.Cst.Token.text grouped_name);
          Test.assert_equal ~expected:(Some "request") ~actual:(Syn.Cst.Ident.name grouped_request);
          Test.assert_equal ~expected:(Some "service") ~actual:(Syn.Cst.Ident.name grouped_service);
          Ok ()
      | _ -> Error "expected arrow-style class type declarations");
  Test.case "cst class type signatures preserve field structure and field attributes"
    (fun ctx ->
      assert_ml_cst_snapshot ~ctx
        "class type ct = object\n\
             \  inherit base [@@foo]\n\
             \  val mutable state : int [@@foo]\n\
             \  method private close : string [@@foo]\n\
             \  constraint t = int [@@foo]\n\
             end\n");
  Test.case "cst class type signatures preserve extension fields"
    (fun ctx ->
      assert_ml_cst_snapshot ~ctx
        "class type ct = object\n\
             \  [%%foo]\n\
             \  method run : int\n\
             end\n");
  Test.case "cst builder keeps class-type trailing trivia as explicit body entries"
    (fun _ctx ->
      let source = "class type ct = object\n\
           \  method run : int\n\
           \  (** body doc *)\n\
           \  (* body comment *)\n\
           end\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassTypeDeclaration {
        class_type_body=Syn.Cst.ClassType.Signature { syntax_node; fields;  };
        _;

      } :: _ ->
          let body_items = Syn.CstBuilder.class_type_field_items_of_fields ~source_node:syntax_node fields in
          (
            match body_items with
            | [
              Syn.CstBuilder.ClassTypeField (Syn.Cst.ClassTypeField.Method _);
              Syn.CstBuilder.Docstring docstring;
              Syn.CstBuilder.Comment comment;

            ] ->
                Test.assert_equal
                  ~expected:"(** body doc *)"
                  ~actual:(Syn.Cst.Docstring.text docstring);
                Test.assert_equal
                  ~expected:"(* body comment *)"
                  ~actual:(Syn.Cst.Comment.text comment);
                Ok ()
            | _ -> Error "expected class type field stream to include method and body docstring/comment"
          )
      | _ -> Error "expected class type declaration with signature body");
  Test.case "cst value declarations preserve names and type nodes"
    (fun _ctx ->
      let result = parse_mli "val create : name:string -> person\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration {
        name_tokens=[ name_token ];
        type_=Syn.Cst.CoreType.Arrow {
          label=Some (Syn.Cst.ArrowLabel.Named { sigil_token=None; label_token;  });
          parameter_type=Syn.Cst.CoreType.Constr { constructor_path=parameter_path; _ };
          result_type=Syn.Cst.CoreType.Constr { constructor_path=result_path; _ };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"create" ~actual:(Syn.Cst.Token.text name_token);
          Test.assert_equal ~expected:"name" ~actual:(Syn.Cst.Token.text label_token);
          Test.assert_equal ~expected:(Some "string") ~actual:(Syn.Cst.Ident.name parameter_path);
          Test.assert_equal ~expected:(Some "person") ~actual:(Syn.Cst.Ident.name result_path);
          Ok ()
      | _ -> Error "expected first item to be a value declaration");
  Test.case "cst value declarations preserve optional arrow labels"
    (fun _ctx ->
      let result = parse_mli "val create : ?state:string -> person\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration {
        type_=Syn.Cst.CoreType.Arrow {
          label=Some (Syn.Cst.ArrowLabel.OptionalNamed { sigil_token; label_token;  });
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"?" ~actual:(Syn.Cst.Token.text sigil_token);
          Test.assert_equal ~expected:"state" ~actual:(Syn.Cst.Token.text label_token);
          Ok ()
      | _ -> Error "expected optional labeled value declaration");
  Test.case "cst value declarations lift explicit polymorphic core types"
    (fun _ctx ->
      let result = parse_mli "val id : 'a 'b. 'a -> 'b -> 'a\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration {
        type_=Syn.Cst.CoreType.Poly {
          type_keyword_token=None;
          binders;
          body=Syn.Cst.CoreType.Arrow { result_type=Syn.Cst.CoreType.Arrow _; _;  };
          _;

        };
        _;

      } :: _ ->
          let binder_text = binders |> List.map Syn.Cst.TypeBinder.text in
          let quoted = binders |> List.map Syn.Cst.TypeBinder.is_quoted in
          Test.assert_equal ~expected:[ "'a"; "'b" ] ~actual:binder_text;
          Test.assert_equal ~expected:[ true; true ] ~actual:quoted;
          Ok ()
      | _ -> Error "expected explicitly polymorphic value declaration");
  Test.case "cst value declarations preserve quoted core type variable sigils"
    (fun _ctx ->
      let result = parse_mli "val id : 'a -> 'a\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration {
        type_=Syn.Cst.CoreType.Arrow {
          parameter_type=Syn.Cst.CoreType.Var {
            sigil_token=Some parameter_sigil_token;
            name_token=parameter_name_token;
            _;

          };
          result_type=Syn.Cst.CoreType.Var {
            sigil_token=Some result_sigil_token;
            name_token=result_name_token;
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"'" ~actual:(Syn.Cst.Token.text parameter_sigil_token);
          Test.assert_equal ~expected:"'" ~actual:(Syn.Cst.Token.text result_sigil_token);
          Test.assert_equal ~expected:"a" ~actual:(Syn.Cst.Token.text parameter_name_token);
          Test.assert_equal ~expected:"a" ~actual:(Syn.Cst.Token.text result_name_token);
          Ok ()
      | _ -> Error "expected quoted core type variables");
  Test.case "cst value declarations preserve core type alias binder sigils"
    (fun _ctx ->
      let result = parse_mli "val cast : ('a list as 'whole) -> 'whole\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration {
        type_=Syn.Cst.CoreType.Arrow {
          parameter_type=Syn.Cst.CoreType.Parenthesized {
            inner=Syn.Cst.CoreType.Alias {
              sigil_token=Some alias_sigil_token;
              name_token=alias_name_token;
              _;

            };
            _;

          };
          result_type=Syn.Cst.CoreType.Var {
            sigil_token=Some result_sigil_token;
            name_token=result_name_token;
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"'" ~actual:(Syn.Cst.Token.text alias_sigil_token);
          Test.assert_equal ~expected:"whole" ~actual:(Syn.Cst.Token.text alias_name_token);
          Test.assert_equal ~expected:"'" ~actual:(Syn.Cst.Token.text result_sigil_token);
          Test.assert_equal ~expected:"whole" ~actual:(Syn.Cst.Token.text result_name_token);
          Ok ()
      | _ -> Error "expected aliased core type binder with explicit sigil");
  Test.case "cst tokens expose operator-like names and fixed operators"
    (fun _ctx ->
      let result = parse_mli "val ( mod ) : int -> int -> int\nval ( mod ) : int -> int -> int\nval ( |> ) : 'a -> ('a -> 'b) -> 'b\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration { name_tokens=[ mod_name_token ]; _ } :: Syn.Cst.SignatureItem.ValueDeclaration {
        name_tokens=[ second_mod_name_token ];
        _
      } :: Syn.Cst.SignatureItem.ValueDeclaration { name_tokens=[ pipe_name_token ]; _ } :: _ ->
          Test.assert_equal
            ~expected:true
            ~actual:(Syn.Cst.Token.is_operator_like_name mod_name_token);
          Test.assert_equal
            ~expected:false
            ~actual:(Syn.Cst.Token.is_identifier_like_name mod_name_token);
          Test.assert_equal
            ~expected:true
            ~actual:(Syn.Cst.Token.same_text mod_name_token second_mod_name_token);
          Test.assert_equal
            ~expected:false
            ~actual:(Syn.Cst.Token.same_text mod_name_token pipe_name_token);
          (
            match Syn.Cst.Token.fixed_operator pipe_name_token with
            | Some Syn.Cst.Token.PipeForward -> Ok ()
            | _ -> Error "expected |> name token to expose PipeForward fixed-operator kind"
          )
      | _ -> Error "expected operator value declarations");
  Test.case "cst value declarations lift explicit polymorphic core types with inline comments"
    (fun _ctx ->
      let result = parse_mli "val id : 'a (* c *) 'b (* d *) . 'a -> 'b -> 'a\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration {
        type_=Syn.Cst.CoreType.Poly {
          binders;
          body=Syn.Cst.CoreType.Arrow { result_type=Syn.Cst.CoreType.Arrow _; _;  };
          _;

        };
        _;

      } :: _ ->
          let binder_text = binders |> List.map Syn.Cst.TypeBinder.text in
          let quoted = binders |> List.map Syn.Cst.TypeBinder.is_quoted in
          Test.assert_equal ~expected:[ "'a"; "'b" ] ~actual:binder_text;
          Test.assert_equal ~expected:[ true; true ] ~actual:quoted;
          Ok ()
      | _ -> Error "expected commented explicitly polymorphic value declaration");
  Test.case "cst value declarations preserve package core types"
    (fun _ctx ->
      let result = parse_mli "val driver : (module Driver with type config = int)\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ValueDeclaration {
        type_=Syn.Cst.CoreType.FirstClassModule {
          package_type={ module_type_path=base_path; constraints; _ };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "Driver") ~actual:(Syn.Cst.Ident.name base_path);
          Test.assert_equal ~expected:1 ~actual:(List.length constraints);
          Ok ()
      | _ -> Error "expected package core type");
  Test.case "cst rejects locally opened core types"
    (fun _ctx ->
      let result = parse_mli
        {|
val decode : Outer.Inner.(request -> response)
|}
      in
      Test.assert_true (List.length result.diagnostics > 0);
      Test.assert_equal ~expected:None ~actual:result.cst;
      Ok ());
  Test.case "cst rejects locally opened core types with inline comments"
    (fun _ctx ->
      let result = parse_mli
        {|
val decode : Outer.Inner. (* c *) (request -> response)
|}
      in
      Test.assert_true (List.length result.diagnostics > 0);
      Test.assert_equal ~expected:None ~actual:result.cst;
      Ok ());
  Test.case "cst rejects locally opened core types with comments before dots"
    (fun _ctx ->
      let result = parse_mli
        {|
val decode : Outer.Inner (* c *).(request -> response)
|}
      in
      Test.assert_true (List.length result.diagnostics > 0);
      Test.assert_equal ~expected:None ~actual:result.cst;
      Ok ());
  Test.case "cst external declarations preserve primitive names"
    (fun _ctx ->
      let result = parse_ml "external sqrt : float -> float = \"caml_sqrt_float\"\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ExternalDeclaration {
        name_tokens=[ name_token ];
        primitive_name_tokens;
        _
      } :: _ ->
          Test.assert_equal ~expected:"sqrt" ~actual:(Syn.Cst.Token.text name_token);
          Test.assert_equal
            ~expected:[ "\"caml_sqrt_float\"" ]
            ~actual:(List.map Syn.Cst.Token.text primitive_name_tokens);
          Ok ()
      | _ -> Error "expected first item to be an external declaration");
  Test.case "cst external declarations preserve item attributes"
    (fun _ctx ->
      let result = parse_ml "external caml_hash : int -> int = \"caml_hash\" [@@noalloc]\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ExternalDeclaration { attributes; _ } :: _ ->
          Test.assert_equal ~expected:1 ~actual:(List.length attributes);
          (
            match attributes with
            | [ attribute ] ->
                let attribute_name =
                  match Syn.Cst.Ident.name attribute.name with
                  | Some name -> name
                  | None -> ""
                in
                Test.assert_equal ~expected:"noalloc" ~actual:attribute_name;
                Ok ()
            | _ -> Error "expected one external declaration attribute"
          )
      | _ -> Error "expected first item to be an external declaration");
  Test.case "cst shortcut extension declarations preserve declared names"
    (fun _ctx ->
      let result = parse_ml
        "module%foo [@foo] M = M\n\
             module type%foo [@foo] S = S\n\
             external%foo [@foo] x : _ = \"\"\n\
             exception%foo [@foo] X\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration module_decl :: Syn.Cst.StructureItem.ModuleTypeDeclaration module_type_decl :: Syn.Cst.StructureItem.ExternalDeclaration external_decl :: Syn.Cst.StructureItem.ExceptionDeclaration exception_decl :: _ ->
          Test.assert_equal ~expected:"M" ~actual:(Syn.Cst.ModuleStructure.name module_decl);
          Test.assert_equal
            ~expected:"S"
            ~actual:(Syn.Cst.ModuleTypeDeclaration.name module_type_decl);
          Test.assert_equal ~expected:"x" ~actual:(declaration_name_text external_decl.name_tokens);
          Test.assert_equal ~expected:"X" ~actual:(Syn.Cst.Token.text exception_decl.name_token);
          Ok ()
      | _ -> Error "expected shortcut extension declaration items");
  Test.case "cst include statements preserve typed include targets"
    (fun _ctx ->
      let result = parse_mli "include module type of Stdlib.Array\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.IncludeStatement {
        target=Syn.Cst.ModuleType (Syn.Cst.ModuleType.TypeOf { module_path; _ });
        _
      } :: _ ->
          Test.assert_equal ~expected:(Some "Array") ~actual:(Syn.Cst.Ident.name module_path);
          Ok ()
      | _ -> Error "expected first item to be an include statement");
  Test.case "cst include module type of lookahead survives inline comments"
    (fun _ctx ->
      let result = parse_mli "include module (* c *) type of Stdlib.Array\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.IncludeStatement {
        target=Syn.Cst.ModuleType (Syn.Cst.ModuleType.TypeOf { module_path; _ });
        _
      } :: _ ->
          Test.assert_equal ~expected:(Some "Array") ~actual:(Syn.Cst.Ident.name module_path);
          Ok ()
      | _ -> Error "expected commented include module type of statement");
  Test.case "cst implementation includes preserve module-expression targets"
    (fun _ctx ->
      let result = parse_ml "include Std.List\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.IncludeStatement {
        target=Syn.Cst.ModuleExpression (Syn.Cst.ModuleExpression.Path (Syn.Cst.Ident.Qualified {
          prefix=Syn.Cst.Ident.Ident { name_token=root; _ };
          name_token=leaf;
          _;

        }));
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Std" ~actual:(Syn.Cst.Token.text root);
          Test.assert_equal ~expected:"List" ~actual:(Syn.Cst.Token.text leaf);
          Ok ()
      | _ -> Error "expected first item to be an include statement");
  Test.case
    "cst source files distinguish standalone attribute items"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "[@@@attr]\n");
  Test.case
    "cst source files distinguish standalone extension items"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "[%%toplevel_eval 42]\n");
  Test.case "cst interfaces distinguish standalone attribute items"
    (fun _ctx ->
      let result = parse_mli "[@@@attr]\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.Attribute attribute :: _ ->
          Test.assert_equal ~expected:(Some "attr") ~actual:(Syn.Cst.Ident.name attribute.name);
          Ok ()
      | _ -> Error "expected first item to be an interface attribute item");
  Test.case "cst interfaces distinguish standalone extension items"
    (fun _ctx ->
      let result = parse_mli "[%%signature_item]\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.Extension extension :: _ ->
          Test.assert_equal
            ~expected:(Some "signature_item")
            ~actual:(Syn.Cst.Ident.name extension.name);
          Ok ()
      | _ -> Error "expected first item to be an interface extension item");
  Test.case
    "cst interface extension payloads default to opaque tokens"
    (fun ctx -> assert_mli_cst_snapshot ~ctx "[%%signature_item val x : int]\n");
  Test.case "cst attributed types keep attribute names and payload nodes"
    (fun _ctx ->
      let result = parse_ml "type t = int [@foo]\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeDeclaration {
        type_definition=Syn.Cst.TypeDefinition.Alias {
          manifest=Syn.Cst.CoreType.Attribute { attribute; _ };
          _
        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"@" ~actual:(attribute_sigil_text attribute);
          Test.assert_equal ~expected:(Some "foo") ~actual:(Syn.Cst.Ident.name attribute.name);
          Test.assert_equal ~expected:None ~actual:attribute.payload;
          Ok ()
      | _ -> Error "expected attributed type alias");
  Test.case "cst pattern attributes attach orthogonally to the lifted pattern"
    (fun _ctx ->
      let result = parse_ml "let (x [@foo]) = value\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        binding_pattern=Syn.Cst.Pattern.Parenthesized { inner; _ };
        _;

      } :: _ -> (
          match inner with
          | Syn.Cst.Pattern.Identifier { name_token; _ } ->
              let attributes = Syn.Cst.Pattern.attributes inner in
              Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text name_token);
              Test.assert_equal ~expected:1 ~actual:(List.length attributes);
              Test.assert_equal
                ~expected:[ Some "foo" ]
                ~actual:((attributes
                |> List.map (fun ({ name; _ }: Syn.Cst.attribute) -> Syn.Cst.Ident.name name)));
              Ok ()
          | _ -> Error "expected parenthesized identifier pattern"
        )
      | _ -> Error "expected let binding with parenthesized pattern");
  Test.case
    "cst expression attributes keep opaque payload tokens"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "let _ = value [@foo 1 + 2]\n");
  Test.case "cst expression attributes survive inline comments after bracket"
    (fun _ctx ->
      let result = parse_ml "let _ = value [ (* c *) @foo 1 + 2]\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding { value; _ } :: _ ->
          let attributes = Syn.Cst.Expression.attributes value in
          Test.assert_equal ~expected:1 ~actual:(List.length attributes);
          (
            match attributes with
            | { name; payload=Some (Syn.Cst.Payload.Opaque _); _ } :: _ ->
                Test.assert_equal ~expected:(Some "foo") ~actual:(Syn.Cst.Ident.name name);
                Ok ()
            | _ -> Error "expected commented expression attribute payload"
          )
      | _ -> Error "expected let binding with commented expression attribute");
  Test.case
    "cst extensions keep typed `:` payloads opaque by default"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "let _ = [%foo: int -> string]\n");
  Test.case
    "cst extensions survive inline comments after bracket"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "let _ = [ (* c *) %foo: int -> string]\n");
  Test.case "cst exception declarations preserve declared names"
    (fun _ctx ->
      let result = parse_ml "exception Not_found\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ExceptionDeclaration { name_token; _ } :: _ ->
          Test.assert_equal ~expected:"Not_found" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected first item to be an exception declaration");
  Test.case "cst interface exception declarations preserve declared names"
    (fun _ctx ->
      let result = parse_mli "exception Not_found\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.ExceptionDeclaration { name_token; _ } :: _ ->
          Test.assert_equal ~expected:"Not_found" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected first item to be an interface exception declaration");
  Test.case "cst type declarations preserve first-class module type definitions"
    (fun _ctx ->
      let result = parse_ml
        "type transport = (module Transport)\n\
             type driver = (module Driver with type config = int)\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | _first_decl :: Syn.Cst.StructureItem.TypeDeclaration {
        type_definition=Syn.Cst.TypeDefinition.FirstClassModule {
          package_type={ module_type_path=base_path; constraints; _ };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "Driver") ~actual:(Syn.Cst.Ident.name base_path);
          Test.assert_equal ~expected:1 ~actual:(List.length constraints);
          Ok ()
      | _ -> Error "expected first-class module type definition");
  Test.case "cst type declarations distinguish class types from constructors"
    (fun _ctx ->
      let result = parse_ml
        "type bare = #list\n\
             type applied = int #list\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.TypeDeclaration {
        type_definition=Syn.Cst.TypeDefinition.Alias {
          manifest=Syn.Cst.CoreType.Class { hash_token; class_path=bare_path; arguments=[]; _;  };
          _;

        };
        _;

      } :: Syn.Cst.StructureItem.TypeDeclaration {
        type_definition=Syn.Cst.TypeDefinition.Alias {
          manifest=Syn.Cst.CoreType.Class {
            class_path=applied_path;
            arguments=[
              Syn.Cst.CoreType.Constr { constructor_path=applied_argument; arguments=[]; _;  };

            ];
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"#" ~actual:(Syn.Cst.Token.text hash_token);
          Test.assert_equal ~expected:(Some "list") ~actual:(Syn.Cst.Ident.name bare_path);
          Test.assert_equal ~expected:(Some "list") ~actual:(Syn.Cst.Ident.name applied_path);
          Test.assert_equal ~expected:(Some "int") ~actual:(Syn.Cst.Ident.name applied_argument);
          Ok ()
      | _ -> Error "expected class-type aliases");
  Test.case "cst open statements preserve open! structurally"
    (fun _ctx ->
      let result = parse_ml "open! Std.List\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.OpenStatement stmt :: _ ->
          Test.assert_true (Syn.Cst.OpenStatement.has_bang stmt);
          (
            match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.ModuleExpression (Syn.Cst.ModuleExpression.Path path) -> Test.assert_equal
              ~expected:(Some "List")
              ~actual:(Syn.Cst.Ident.name path)
            | _ -> ()
          );
          Test.assert_equal ~expected:(Some "List")
            ~actual:((
              match Syn.Cst.OpenStatement.module_path stmt with
              | Some module_path -> Syn.Cst.Ident.name module_path
              | None -> None
            ));
          (
            match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.ModuleExpression (Syn.Cst.ModuleExpression.Path _) -> Ok ()
            | _ -> Error "expected implementation open target to lift as a module expression"
          )
      | _ -> Error "expected first item to be an open statement");
  Test.case "cst interface open statements preserve open! structurally"
    (fun _ctx ->
      let result = parse_mli "open! Std.List\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | Syn.Cst.SignatureItem.OpenStatement stmt :: _ ->
          Test.assert_true (Syn.Cst.OpenStatement.has_bang stmt);
          (
            match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.Path path -> Test.assert_equal
              ~expected:(Some "List")
              ~actual:(Syn.Cst.Ident.name path)
            | _ -> ()
          );
          Test.assert_equal ~expected:(Some "List")
            ~actual:((
              match Syn.Cst.OpenStatement.module_path stmt with
              | Some module_path -> Syn.Cst.Ident.name module_path
              | None -> None
            ));
          (
            match Syn.Cst.OpenStatement.target stmt with
            | Syn.Cst.OpenStatement.Path _ -> Ok ()
            | _ -> Error "expected interface open target to remain a module path"
          )
      | _ -> Error "expected first item to be an interface open statement");
  Test.case "cst attaches top-level docstrings after open statements to the next value"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        "open Std\n\
             \n\
             (** Module overview. *)\n\
             \n\
             val create : unit -> t\n");
  Test.case "cst keeps module overviews standalone before opens and repeated docs on the first type after open"
    (fun _ctx ->
      let result = parse_mli
        "(** JSON-RPC 2.0 Protocol Implementation *)\n\
             \n\
             open Std\n\
             open Std.Data\n\
             \n\
             (** Request/response ID type. *)\n\
             (** Request identifiers. *)\n\
             type id = string\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [
        Syn.Cst.SignatureItem.Docstring overview;
        Syn.Cst.SignatureItem.OpenStatement _;
        Syn.Cst.SignatureItem.OpenStatement _;
        Syn.Cst.SignatureItem.TypeDeclaration id_decl
      ] ->
          Test.assert_equal
            ~expected:"(** JSON-RPC 2.0 Protocol Implementation *)"
            ~actual:(Syn.Cst.Docstring.text overview);
          Test.assert_equal
            ~expected:[ "(** Request/response ID type. *)"; "(** Request identifiers. *)"; ]
            ~actual:((node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node id_decl)
            |> List.map Syn.Cst.Trivia.text));
          Ok ()
      | _ -> Error "expected standalone module overview, two opens, and a type declaration with repeated leading docs");
  Test.case "cst preserves standalone implementation docstrings after open statements"
    (fun _ctx ->
      let result = parse_ml
        "open Std\n\
             \n\
             (** Module overview. *)\n\
             \n\
             let create = fun () -> 1\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.OpenStatement _ :: Syn.Cst.StructureItem.Docstring docstring :: Syn.Cst.StructureItem.LetBinding _ :: _ ->
          Test.assert_equal
            ~expected:"(** Module overview. *)"
            ~actual:(Syn.Cst.Docstring.text docstring);
          Ok ()
      | _ -> Error "expected open statement, standalone docstring, and let binding");
  Test.case "cst preserves standalone top-level comments after open statements"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        "open Std\n\
             \n\
             (* Module comment. *)\n\
             \n\
             val create : unit -> t\n");
  Test.case "cst preserves standalone implementation comments after open statements"
    (fun ctx ->
      assert_ml_cst_snapshot ~ctx
        "open Std\n\
             \n\
             (* Module comment. *)\n\
             \n\
             let create = fun () -> 1\n");
  Test.case "build_cst derives top-level standalone trivia from parser token order"
    (fun _ctx ->
      let parsed = Syn.parse ~filename:sample_mli
        "open Std\n\
             \n\
             (** Module overview. *)\n\
             (* plain comment *)\n\
             \n\
             val create : unit -> t\n"
      in
      match Syn.build_cst parsed with
      | Ok cst -> (
          match signature_items cst with
          | Syn.Cst.SignatureItem.OpenStatement _ :: Syn.Cst.SignatureItem.Docstring docstring :: Syn.Cst.SignatureItem.Comment comment :: Syn.Cst.SignatureItem.ValueDeclaration _ :: _ ->
              Test.assert_equal
                ~expected:"(** Module overview. *)"
                ~actual:(Syn.Cst.Docstring.text docstring);
              Test.assert_equal
                ~expected:"(* plain comment *)"
                ~actual:(Syn.Cst.Comment.text comment);
              Ok ()
          | _ -> Error "expected open statement, standalone docstring, standalone comment, and value declaration"
        )
      | Error _ -> Error "expected CST build to succeed from parser token stream");
  Test.case
    "cst open statements expose raw owned trivia for inline comments"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "open (* keep me *) Std\n");
  Test.case
    "cst module declarations expose raw owned trivia for inline comments"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "module (* keep me *) M = N\n");
  Test.case
    "cst module type declarations expose raw owned trivia for inline comments"
    (fun ctx -> assert_mli_cst_snapshot ~ctx "module type (* keep me *) S = sig end\n");
  Test.case "cst keeps terminal trailing value declaration docstrings standalone"
    (fun _ctx ->
      let result = parse_mli
        "val create : unit -> t\n\
             (** Create a new builder *)\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [Syn.Cst.SignatureItem.ValueDeclaration decl;Syn.Cst.SignatureItem.Docstring docstring] ->
          let leading = node_leading_trivia (Syn.Cst.ValueDeclaration.syntax_node decl) in
          Test.assert_equal ~expected:true ~actual:(List.is_empty leading);
          Test.assert_equal
            ~expected:"(** Create a new builder *)"
            ~actual:(Syn.Cst.Docstring.text docstring);
          Ok ()
      | _ -> Error "expected value declaration followed by standalone terminal docstring");
  Test.case "cst keeps trailing alias docstrings with the next type declaration"
    (fun _ctx ->
      let result = parse_mli
        "(** Protocol version string *)\n\
             type protocol_version = string\n\
             (** JSON type alias *)\n\
             type json = Data.Json.t\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [
        Syn.Cst.SignatureItem.TypeDeclaration protocol_version;
        Syn.Cst.SignatureItem.TypeDeclaration json
      ] ->
          Test.assert_equal
            ~expected:1
            ~actual:(List.length
              (node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node protocol_version)));
          Test.assert_equal
            ~expected:1
            ~actual:(List.length (node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node json)));
          (
            match node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node protocol_version) with
            | [ Syn.Cst.Trivia.Docstring doc ] -> Test.assert_equal
              ~expected:"(** Protocol version string *)"
              ~actual:(Syn.Cst.Docstring.text doc)
            | _ -> raise (Failure "expected protocol_version leading docstring")
          );
          (
            match node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node json) with
            | [ Syn.Cst.Trivia.Docstring doc ] -> Test.assert_equal
              ~expected:"(** JSON type alias *)"
              ~actual:(Syn.Cst.Docstring.text doc)
            | _ -> raise (Failure "expected json leading docstring")
          );
          Ok ()
      | _ -> Error "expected two type declarations");
  Test.case "cst splits trailing variant docs between the current and next type"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        "type request_id =\n\
             \  | String of string\n\
             \  | Number of int\n\
             \  (** JSON-RPC request ID *)\n\
             (** JSON-RPC error code *)\n\
             type error_code = int\n");
  Test.case "cst sends trailing docs after already-documented variant types to the next type"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        "(** Method parameters *)\n\
             type params =\n\
             \  | Positional of Json.t list (** Positional parameters as JSON array *)\n\
             \  | Named of (string * Json.t) list (** Named parameters as JSON object *)\n\
             \  | NoParams (** No parameters *)\n\
             (** Pre-request type used by ApplicationProtocol *)\n\
             type prerequest = {\n\
             \  method_ : string;\n\
             }\n");
  Test.case "cst keeps docstrings between top-level type declarations on the next type"
    (fun _ctx ->
      let result = parse_mli
        "type request_id =\n\
             \  | String of string\n\
             \  | Number of int\n\
             \  (** Numeric request IDs *)\n\
             (** JSON-RPC error code *)\n\
             type error_code = int\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [
        Syn.Cst.SignatureItem.TypeDeclaration request_id;
        Syn.Cst.SignatureItem.TypeDeclaration error_code
      ] -> (
          match Syn.Cst.TypeDeclaration.type_definition request_id with
          | Syn.Cst.TypeDefinition.Variant { constructors=[_;number]; _ } -> (
              match node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node error_code) with
              | [
                Syn.Cst.Trivia.Docstring numeric_request_ids;
                Syn.Cst.Trivia.Docstring error_code_doc
              ] ->
                  Test.assert_equal
                    ~expected:"(** Numeric request IDs *)"
                    ~actual:(Syn.Cst.Docstring.text numeric_request_ids);
                  Test.assert_equal
                    ~expected:"(** JSON-RPC error code *)"
                    ~actual:(Syn.Cst.Docstring.text error_code_doc);
                  Ok ()
              | _ -> Error "expected both inter-declaration docstrings on the next type"
            )
          | _ -> Error "expected request_id variant constructors"
        )
      | _ -> Error "expected request_id and error_code declarations");
  Test.case "cst keeps terminal type docstrings standalone at end of file"
    (fun _ctx ->
      let result = parse_mli
        "type a\n\
             (** doc string *)\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [Syn.Cst.SignatureItem.TypeDeclaration type_decl;Syn.Cst.SignatureItem.Docstring docstring] ->
          Test.assert_equal
            ~expected:0
            ~actual:(List.length
              (node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node type_decl)));
          Test.assert_equal ~expected:"(** doc string *)" ~actual:(Syn.Cst.Docstring.text docstring);
          Ok ()
      | _ -> Error "expected type declaration followed by standalone docstring");
  Test.case "cst keeps exception docstrings on the next item without a member stream"
    (fun _ctx ->
      let result = parse_mli
        "exception Cancelled\n\
             (** Response payload *)\n\
             type response = int\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [
        Syn.Cst.SignatureItem.ExceptionDeclaration exception_decl;
        Syn.Cst.SignatureItem.TypeDeclaration response_decl
      ] -> (
          match node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node response_decl) with
          | [ Syn.Cst.Trivia.Docstring doc ] ->
              Test.assert_equal
                ~expected:"(** Response payload *)"
                ~actual:(Syn.Cst.Docstring.text doc);
              Ok ()
          | _ -> Error "expected next type leading docstring"
        )
      | _ -> Error "expected exception declaration followed by type declaration");
  Test.case "cst keeps constructor docstrings leading on the next constructor"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        "type error =\n\
             \  | ParseError of { raw_input : string; parse_error : string }\n\
             \      (** Client failed to parse JSON or JSON-RPC structure *)\n\
             \  | UnknownServerError of { code : int; message : string; data : Json.t option }\n");
  Test.case "cst keeps terminal constructor docstrings standalone at end of file"
    (fun _ctx ->
      let result = parse_mli
        "type error =\n\
             \  | UnknownServerError of { code : int; message : string; data : Json.t option }\n\
             \      (** Server returned a JSON-RPC error object that couldn't be parsed into a\n\
             \          typed response variant *)\n\
             (** Client-side errors with rich context information. Includes\n\
             \    UnknownServerError for when the server sends a JSON-RPC error that we don't\n\
             \    have a typed response variant for. *)\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [
        Syn.Cst.SignatureItem.TypeDeclaration error_decl;
        Syn.Cst.SignatureItem.Docstring constructor_doc;
        Syn.Cst.SignatureItem.Docstring type_doc
      ] -> (
          match Syn.Cst.TypeDeclaration.type_definition error_decl with
          | Syn.Cst.TypeDefinition.Variant { constructors=[ unknown_server_error ]; _ } ->
              Test.assert_equal
                ~expected:"(** Server returned a JSON-RPC error object that couldn't be parsed into a\n\
                             \          typed response variant *)"
                ~actual:(Syn.Cst.Docstring.text constructor_doc);
              Test.assert_equal
                ~expected:"(** Client-side errors with rich context information. Includes\n\
                     \    UnknownServerError for when the server sends a JSON-RPC error that we don't\n\
                     \    have a typed response variant for. *)"
                ~actual:(Syn.Cst.Docstring.text type_doc);
              Ok ()
          | _ -> Error "expected error variant constructors"
        )
      | _ -> Error "expected type declaration followed by standalone docstrings");
  Test.case "cst keeps record field docstrings leading on the next field"
    (fun _ctx ->
      let result = parse_mli
        "type event = {\n\
             \  data : string;\n\
             \  (** Event payload *)\n\
             \  event_type : string option;\n\
             }\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [ Syn.Cst.SignatureItem.TypeDeclaration event_decl ] -> (
          match Syn.Cst.TypeDeclaration.type_definition event_decl with
          | Syn.Cst.TypeDefinition.Record { fields=[data_field;event_type_field]; _ } -> (
              match node_leading_trivia (Syn.Cst.RecordField.syntax_node event_type_field) with
              | [ Syn.Cst.Trivia.Docstring doc ] ->
                  Test.assert_equal
                    ~expected:"(** Event payload *)"
                    ~actual:(Syn.Cst.Docstring.text doc);
                  Ok ()
              | _ -> Error "expected event_type leading docstring"
            )
          | _ -> Error "expected record type definition"
        )
      | _ -> Error "expected single record type declaration");
  Test.case "cst keeps terminal record field docs off the next type's leading docs"
    (fun _ctx ->
      let result = parse_mli
        "type event = {\n\
             \  data : string;\n\
             \  id : string option;\n\
             \  (** Optional event ID field *)\n\
             }\n\
             (** Response payload *)\n\
             type response = int\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [
        Syn.Cst.SignatureItem.TypeDeclaration event_decl;
        Syn.Cst.SignatureItem.TypeDeclaration response_decl
      ] -> (
          match Syn.Cst.TypeDeclaration.type_definition event_decl with
          | Syn.Cst.TypeDefinition.Record { fields=[_;id_field]; _ } -> (
              match node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node response_decl) with
              | [ Syn.Cst.Trivia.Docstring doc ] ->
                  Test.assert_equal
                    ~expected:"(** Response payload *)"
                    ~actual:(Syn.Cst.Docstring.text doc);
                  Ok ()
              | _ -> Error "expected only the type-level docstring on the next type"
            )
          | _ -> Error "expected record type definition"
        )
      | _ -> Error "expected event and response type declarations");
  Test.case "cst builder record field items keep terminal docstrings before closing braces"
    (fun _ctx ->
      let result = parse_mli
        "type event = {\n\
             \  data : string;\n\
             \  id : string option;\n\
             \  (** Optional event ID field *)\n\
             }\n\
             (** Response payload *)\n\
             type response = int\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [Syn.Cst.SignatureItem.TypeDeclaration event_decl;Syn.Cst.SignatureItem.TypeDeclaration _] -> (
          match Syn.Cst.TypeDeclaration.type_definition event_decl with
          | Syn.Cst.TypeDefinition.Record { fields; _ } when List.length fields = 2 -> (
              match Syn.CstBuilder.record_field_items_of_fields fields with
              | [
                Syn.CstBuilder.RecordField _;
                Syn.CstBuilder.RecordField _;
                Syn.CstBuilder.Docstring doc
              ] ->
                  Test.assert_equal
                    ~expected:"(** Optional event ID field *)"
                    ~actual:(Syn.Cst.Docstring.text doc);
                  Ok ()
              | _ -> Error "expected trailing record-field docstring to stay inside the record body"
            )
          | _ -> Error "expected record type definition"
        )
      | _ -> Error "expected event and response declarations");
  Test.case "cst builder record field items keep standalone terminal comments before closing braces"
    (fun _ctx ->
      let result = parse_mli
        "type event = {\n\
             \  data : string;\n\
             \  id : string option;\n\
             \n\
             \  (* Optional event ID field *)\n\
             }\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [ Syn.Cst.SignatureItem.TypeDeclaration event_decl ] -> (
          match Syn.Cst.TypeDeclaration.type_definition event_decl with
          | Syn.Cst.TypeDefinition.Record { fields; _ } when List.length fields = 2 -> (
              match Syn.CstBuilder.record_field_items_of_fields fields with
              | [
                Syn.CstBuilder.RecordField _;
                Syn.CstBuilder.RecordField _;
                Syn.CstBuilder.Comment comment
              ] ->
                  Test.assert_equal
                    ~expected:"(* Optional event ID field *)"
                    ~actual:(Syn.Cst.Comment.text comment);
                  Ok ()
              | _ -> Error "expected trailing record-field comment to stay inside the record body"
            )
          | _ -> Error "expected record type definition"
        )
      | _ -> Error "expected single record declaration");
  Test.case "cst docstrings expose explicit section vs ordinary kinds"
    (fun _ctx ->
      let result = parse_mli
        "(** ## Types *)\n\
             type a = int\n\
             (** Ordinary docs *)\n\
             type b = int\n\
             (* plain comment *)\n\
             type c = int\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match signature_items cst with
      | [
        Syn.Cst.SignatureItem.Docstring section_doc;
        Syn.Cst.SignatureItem.TypeDeclaration _;
        Syn.Cst.SignatureItem.TypeDeclaration type_b;
        Syn.Cst.SignatureItem.Comment comment;
        Syn.Cst.SignatureItem.TypeDeclaration _
      ] ->
          Test.assert_equal
            ~expected:Syn.Cst.Docstring.Section
            ~actual:(Syn.Cst.Docstring.kind section_doc);
          (
            match node_leading_trivia (Syn.Cst.TypeDeclaration.syntax_node type_b) with
            | [ Syn.Cst.Trivia.Docstring ordinary_doc ] ->
                Test.assert_equal
                  ~expected:Syn.Cst.Docstring.Ordinary
                  ~actual:(Syn.Cst.Docstring.kind ordinary_doc);
                Test.assert_equal
                  ~expected:"(* plain comment *)"
                  ~actual:(Syn.Cst.Comment.text comment);
                Ok ()
            | _ -> Error "expected ordinary leading docstring on type b"
          )
      | _ -> Error "expected section docstring, comment, and type declarations in order");
  Test.case "cst keeps section docstrings after grouped type declarations standalone"
    (fun ctx ->
      assert_mli_cst_snapshot ~ctx
        "(** ## Types *)\n\
             \n\
             type ('kind, 'text) token = { kind : 'kind; text : 'text; width : int }\n\
             (** Green token - leaf node containing source text. *)\n\
             \n\
             type ('kind, 'text) node = {\n\
             \  kind : 'kind;\n\
             \  children : ('kind, 'text) element array;\n\
             }\n\
             (** Green node - interior node with children. *)\n\
             \n\
             (** Element can be either a token or a node. *)\n\
             and ('kind, 'text) element =\n\
             \  | Token of ('kind, 'text) token\n\
             \  | Node of ('kind, 'text) node\n\
             \n\
             (** ## Construction *)\n\
             \n\
             val make_token : kind:'kind -> text:'text -> width:int -> ('kind, 'text) token\n");
  Test.case "cst implementation keeps section docstrings after grouped type declarations standalone"
    (fun ctx ->
      assert_ml_cst_snapshot ~ctx
        "(** ## Types *)\n\
             \n\
             type ('kind, 'text) token = { kind : 'kind; text : 'text; width : int }\n\
             (** Green token - leaf node containing source text. *)\n\
             \n\
             type ('kind, 'text) node = {\n\
             \  kind : 'kind;\n\
             \  children : ('kind, 'text) element array;\n\
             }\n\
             (** Green node - interior node with children. *)\n\
             \n\
             (** Element can be either a token or a node. *)\n\
             and ('kind, 'text) element =\n\
             \  | Token of ('kind, 'text) token\n\
             \  | Node of ('kind, 'text) node\n\
             \n\
             (** ## Construction *)\n\
             \n\
             let make_token ~kind ~text ~width = { kind; text; width }\n");
  Test.case "cst keeps banner comments as standalone top-level comments"
    (fun _ctx ->
      let result = parse_ml
        "let to_list t = collect t []\n\
             \n\
             (*************************************************************************************************)\n\
             (* Transformation *)\n\
             (*************************************************************************************************)\n\
             \n\
             let iter_next = next\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding _ :: Syn.Cst.StructureItem.Comment first :: Syn.Cst.StructureItem.Comment second :: Syn.Cst.StructureItem.Comment third :: Syn.Cst.StructureItem.LetBinding _ :: _ ->
          Test.assert_equal
            ~expected:"(*************************************************************************************************)"
            ~actual:(Syn.Cst.Comment.text first);
          Test.assert_equal ~expected:"(* Transformation *)" ~actual:(Syn.Cst.Comment.text second);
          Test.assert_equal
            ~expected:"(*************************************************************************************************)"
            ~actual:(Syn.Cst.Comment.text third);
          Ok ()
      | _ -> Error "expected banner comments to remain first-class comment items");
  Test.case "cst implementation open statements lift non-path module expressions"
    (fun _ctx ->
      let result = parse_ml
        "open struct\n\
             let value = 1\n\
             end\n"
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.OpenStatement stmt :: _ -> (
          match Syn.Cst.OpenStatement.target stmt with
          | Syn.Cst.OpenStatement.ModuleExpression (Syn.Cst.ModuleExpression.Structure {
            item_syntax_nodes;
            _
          }) ->
              Test.assert_true (List.length item_syntax_nodes > 0);
              Test.assert_equal ~expected:None ~actual:(Syn.Cst.OpenStatement.module_path stmt);
              Ok ()
          | _ -> Error "expected implementation open target to preserve structure syntax"
        )
      | _ -> Error "expected first item to be an implementation open statement");
  Test.case "cst source files preserve mixed structure item ordering"
    (fun _ctx ->
      let source = "let first = 1\nmodule Middle = struct end\nlet second = 2\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding first :: Syn.Cst.StructureItem.ModuleDeclaration _ :: Syn.Cst.StructureItem.LetBinding second :: _ ->
          Test.assert_equal ~expected:"first" ~actual:(Syn.Cst.LetBinding.name first);
          Test.assert_equal ~expected:"second" ~actual:(Syn.Cst.LetBinding.name second);
          Ok ()
      | _ -> Error "expected let/module/let item ordering");
  Test.case "cst let bindings expose typed parameters"
    (fun _ctx ->
      let source = "let render userId ~displayName ?pageSize current_user = current_user\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | binding :: _ when String.equal (Syn.Cst.LetBinding.name binding) "render" ->
          let names = Syn.Cst.LetBinding.parameters binding |> List.map Syn.Cst.Parameter.name in
          Test.assert_equal
            ~expected:[ Some "userId"; Some "displayName"; Some "pageSize"; Some "current_user" ]
            ~actual:names;
          Ok ()
      | _ -> Error "expected render binding parameters");
  Test.case "cst let bindings preserve optional parameter defaults structurally"
    (fun _ctx ->
      let source = "let apply ?(f : int -> int = fun x -> x * 2) ?timeout:chosen_timeout () = f 1\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | binding :: _ -> (
          match Syn.Cst.LetBinding.parameters binding with
          | Syn.Cst.Parameter.Optional {
            label_token;
            binding_name_token=Some binding_name_token;
            binding_name_matches_label=true;
            default_value=Some (Syn.Cst.Expression.Fun _);
            binding_pattern=Some (Syn.Cst.Pattern.Typed _);
            _;

          } :: Syn.Cst.Parameter.Optional {
            label_token=renamed_label;
            binding_name_token=Some renamed_binding;
            binding_name_matches_label=false;
            default_value=None;
            _;

          } :: _ ->
              Test.assert_equal ~expected:"f" ~actual:(Syn.Cst.Token.text label_token);
              Test.assert_equal ~expected:"f" ~actual:(Syn.Cst.Token.text binding_name_token);
              Test.assert_equal ~expected:"timeout" ~actual:(Syn.Cst.Token.text renamed_label);
              Test.assert_equal
                ~expected:"chosen_timeout"
                ~actual:(Syn.Cst.Token.text renamed_binding);
              Ok ()
          | _ -> Error "expected structural optional parameters with preserved defaults"
        )
      | [] -> Error "expected let binding");
  Test.case "cst rejects spaced renamed labeled parameters"
    (fun _ctx ->
      let source = "let foo ~v: hello = v\n" in
      let result = parse_ml source in
      Test.assert_true (List.length result.diagnostics > 0);
      Test.assert_equal ~expected:None ~actual:result.cst;
      Ok ());
  Test.case "cst let bindings preserve locally abstract type parameters"
    (fun _ctx ->
      let source = "let id (type a b) value = value\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | binding :: _ -> (
          match Syn.Cst.LetBinding.parameters binding with
          | Syn.Cst.Parameter.LocallyAbstract { binders; _ } :: _ ->
              let binder_text = binders |> List.map Syn.Cst.TypeBinder.text in
              let quoted = binders |> List.map Syn.Cst.TypeBinder.is_quoted in
              Test.assert_equal ~expected:[ "a"; "b" ] ~actual:binder_text;
              Test.assert_equal ~expected:[ false; false ] ~actual:quoted;
              Ok ()
          | _ -> Error "expected leading locally abstract type parameter"
        )
      | [] -> Error "expected let binding");
  Test.case "cst let bindings preserve recursive markers"
    (fun _ctx ->
      let source = "let rec loop x = loop x\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | binding :: _ ->
          Test.assert_true (Syn.Cst.LetBinding.is_recursive binding);
          Ok ()
      | [] -> Error "expected recursive let binding");
  Test.case "cst let binding annotations wrap bound values as typed expressions"
    (fun _ctx ->
      let source = "let render : user_t -> user_t = fun value -> value\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.TypeAscription {
          expression=Syn.Cst.Expression.Fun _;
          kind=Syn.Cst.Type { type_=Syn.Cst.CoreType.Arrow _; _ };
          _;

        };
        _;

      } :: _ -> Ok ()
      | _ -> Error "expected typed let-binding value");
  Test.case "cst let binding annotations preserve explicit polymorphism"
    (fun _ctx ->
      let source = "let id : 'a. 'a -> 'a = fun x -> x\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Polymorphic {
          expression=Syn.Cst.Expression.Fun {
            body=Syn.Cst.Expression (Syn.Cst.Expression.Path { path; _ });
            _;

          };
          type_=Syn.Cst.CoreType.Poly { binders; _ };
          _;

        };
        _;

      } :: _ ->
          let binder_text = binders |> List.map Syn.Cst.TypeBinder.text in
          let quoted = binders |> List.map Syn.Cst.TypeBinder.is_quoted in
          Test.assert_equal ~expected:[ "'a" ] ~actual:binder_text;
          Test.assert_equal ~expected:[ true ] ~actual:quoted;
          Test.assert_equal ~expected:(Some "x") ~actual:(Syn.Cst.Ident.name path);
          Ok ()
      | _ -> Error "expected polymorphic let-binding value");
  Test.case
    "cst let binding annotations preserve locally abstract core types with inline comments"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "let id : type (* c *) a. a -> a = fun x -> x\n");
  Test.case "cst let bindings expose infix string concatenation values"
    (fun _ctx ->
      let source = "let banner = \"a\" ^ \"b\" ^ \"c\"\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
          match Syn.Cst.LetBinding.value binding with
          | Syn.Cst.Expression.Infix expr ->
              Test.assert_equal ~expected:"^" ~actual:(Syn.Cst.InfixExpression.operator expr);
              Ok ()
          | _ -> Error "expected infix expression value"
        )
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst let bindings expose custom infix operators structurally"
    (fun _ctx ->
      let source = "let composed = f %> g\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
          match Syn.Cst.LetBinding.value binding with
          | Syn.Cst.Expression.Infix expr ->
              Test.assert_equal ~expected:"%>" ~actual:(Syn.Cst.InfixExpression.operator expr);
              Ok ()
          | _ -> Error "expected infix expression value"
        )
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst let bindings preserve infix expressions across inline comments"
    (fun _ctx ->
      let source = "let banner = \"a\" (* c *) ^ \"b\"\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
          match Syn.Cst.LetBinding.value binding with
          | Syn.Cst.Expression.Infix expr ->
              Test.assert_equal ~expected:"^" ~actual:(Syn.Cst.InfixExpression.operator expr);
              Ok ()
          | _ -> Error "expected commented infix expression value"
        )
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst let bindings expose if expressions and unit else branches"
    (fun _ctx ->
      let source = "let render ok = if ok then log () else ()\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
          match Syn.Cst.LetBinding.value binding with
          | Syn.Cst.Expression.If expr -> (
              match expr.else_branch with
              | Some (Syn.Cst.Expression.Literal (Syn.Cst.Literal.Unit _)) -> Ok ()
              | _ -> Error "expected unit else branch"
            )
          | _ -> Error "expected if expression value"
        )
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst if expressions preserve boolean literal comparisons"
    (fun _ctx ->
      let source = "let render ok = if ok = true then log () else ()\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.If {
          condition=Syn.Cst.Expression.Infix {
            right=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Bool {
              literal_token={ syntax_token };
              _
            });
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_true (String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "true");
          Ok ()
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst typed expressions preserve the wrapped expression and type node"
    (fun _ctx ->
      let source = "let render = (value : user_t)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.TypeAscription {
          expression=Syn.Cst.Expression.Path { path; _ };
          kind=Syn.Cst.Type { type_=Syn.Cst.CoreType.Constr _; _ };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "value") ~actual:(Syn.Cst.Ident.name path);
          Ok ()
      | _ -> Error "expected typed expression value");
  Test.case "cst coerce expressions preserve optional source types"
    (fun _ctx ->
      let source = "let render = (value : user_t :> display_t)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.TypeAscription {
          expression=Syn.Cst.Expression.Path { path; _ };
          kind=Syn.Cst.ConstraintCoerce {
            from_type=Syn.Cst.CoreType.Constr _;
            to_type=Syn.Cst.CoreType.Constr _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "value") ~actual:(Syn.Cst.Ident.name path);
          Ok ()
      | _ -> Error "expected coerce expression value");
  Test.case "cst source files keep top-level eval expressions as items"
    (fun _ctx ->
      let source = "print_endline \"hello\"\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.Expression (Syn.Cst.Expression.Apply {
        callee=Syn.Cst.Expression.Path { path=Syn.Cst.Ident.Ident { name_token; _ }; _;  };
        argument=Syn.Cst.Positional (Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _));
        _;

      }) :: _ ->
          Test.assert_equal ~expected:"print_endline" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected top-level eval expression item");
  Test.case "cst source files keep top-level let-in expressions as items"
    (fun _ctx ->
      let source = "let a, b = pair in a\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.Expression (Syn.Cst.Expression.Let {
        binding_pattern=Syn.Cst.Pattern.Tuple {
          elements=[
            { pattern=Syn.Cst.Pattern.Identifier { name_token=left_name; _ }; _;  };
            { pattern=Syn.Cst.Pattern.Identifier { name_token=right_name; _ }; _;  };

          ];
          open_tail=None;
          _;

        };
        _;

      }) :: _ ->
          Test.assert_equal ~expected:"a" ~actual:(Syn.Cst.Token.text left_name);
          Test.assert_equal ~expected:"b" ~actual:(Syn.Cst.Token.text right_name);
          Ok ()
      | _ -> Error "expected top-level let expression item");
  Test.case "cst tuple patterns preserve labeled payloads and open tails"
    (fun _ctx ->
      let source = "let f value = match value with | ~state:Some x, ~rest, .. -> x | _ -> 0\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Tuple {
              elements=[
                {
                  label_token=Some state_label;
                  pattern=Syn.Cst.Pattern.Constructor {
                    constructor_path=Syn.Cst.Ident.Ident { name_token=some_name; _ };
                    arguments=[ Syn.Cst.Pattern.Identifier { name_token=x_name; _ };  ];
                    _;

                  };

                };
                {
                  label_token=Some rest_label;
                  pattern=Syn.Cst.Pattern.Identifier { name_token=rest_name; _ };

                };

              ];
              open_tail=Some { dotdot_token; _ };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"state" ~actual:(Syn.Cst.Token.text state_label);
          Test.assert_equal ~expected:"Some" ~actual:(Syn.Cst.Token.text some_name);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text x_name);
          Test.assert_equal ~expected:"rest" ~actual:(Syn.Cst.Token.text rest_label);
          Test.assert_equal ~expected:"rest" ~actual:(Syn.Cst.Token.text rest_name);
          Test.assert_equal ~expected:".." ~actual:(Syn.Cst.Token.text dotdot_token);
          Ok ()
      | _ -> Error "expected labeled tuple pattern with open tail");
  Test.case "cst tuple patterns preserve typed labeled punning"
    (fun _ctx ->
      let source = "let f = function | ~(x : int), y -> x | _ -> 0\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Function {
          cases={
            pattern=Syn.Cst.Pattern.Tuple {
              elements=[
                {
                  label_token=Some label_token;
                  pattern=Syn.Cst.Pattern.Typed {
                    pattern=Syn.Cst.Pattern.Identifier { name_token=x_name; _ };
                    _;

                  };

                };
                { label_token=None; pattern=Syn.Cst.Pattern.Identifier { name_token=y_name; _ };  };

              ];
              open_tail=None;
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text label_token);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text x_name);
          Test.assert_equal ~expected:"y" ~actual:(Syn.Cst.Token.text y_name);
          Ok ()
      | _ -> Error "expected typed labeled tuple pattern");
  Test.case "cst match cases preserve unreachable expressions"
    (fun _ctx ->
      let source = "let absurd maybe = match maybe with | Some value -> value | None -> .\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          scrutinee=Syn.Cst.Expression.Path { path=scrutinee_path; _ };
          cases=[_;{ body=Syn.Cst.Expression.Unreachable { dot_token; _ }; _;  };];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "maybe") ~actual:(Syn.Cst.Ident.name scrutinee_path);
          Test.assert_equal ~expected:"." ~actual:(Syn.Cst.Token.text dot_token);
          Ok ()
      | _ -> Error "expected match expression with unreachable branch");
  Test.case "cst let-operator expressions preserve the leading operator clause"
    (fun _ctx ->
      let source = "let ( let* ) = Result.bind\nlet* value = Ok 1 in Ok value\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | _ :: Syn.Cst.StructureItem.Expression (Syn.Cst.Expression.LetOperator { binding={
          keyword_token;
          operator_token;
          binding_pattern=Syn.Cst.Pattern.Identifier { name_token=binding_name; _ };
          bound_value=Syn.Cst.Expression.Constructor _;
          and_binding=None;

        }; body=Syn.Cst.Expression.Constructor _; _;  }) :: _ ->
          Test.assert_equal ~expected:"let" ~actual:(Syn.Cst.Token.text keyword_token);
          Test.assert_equal ~expected:"*" ~actual:(Syn.Cst.Token.text operator_token);
          Test.assert_equal ~expected:"value" ~actual:(Syn.Cst.Token.text binding_name);
          Ok ()
      | _ -> Error "expected top-level let-operator expression item");
  Test.case "cst let-operator expressions preserve parallel and-bindings"
    (fun _ctx ->
      let source = "let ( let* ) = Result.bind\n\
           let ( and* ) = Result.both\n\
           let* a = Ok 1 and* b = Ok 2 in Ok (a, b)\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | _ :: _ :: Syn.Cst.StructureItem.Expression (Syn.Cst.Expression.LetOperator { binding={
          keyword_token=let_keyword;
          operator_token=let_operator;
          binding_pattern=Syn.Cst.Pattern.Identifier { name_token=left_name; _ };
          bound_value=Syn.Cst.Expression.Constructor _;
          and_binding=Some {
            keyword_token=and_keyword;
            operator_token=and_operator;
            binding_pattern=Syn.Cst.Pattern.Identifier { name_token=right_name; _ };
            bound_value=Syn.Cst.Expression.Constructor _;
            and_binding=None;

          };

        }; body=Syn.Cst.Expression.Constructor _; _;  }) :: _ ->
          Test.assert_equal ~expected:"let" ~actual:(Syn.Cst.Token.text let_keyword);
          Test.assert_equal ~expected:"*" ~actual:(Syn.Cst.Token.text let_operator);
          Test.assert_equal ~expected:"a" ~actual:(Syn.Cst.Token.text left_name);
          Test.assert_equal ~expected:"and" ~actual:(Syn.Cst.Token.text and_keyword);
          Test.assert_equal ~expected:"*" ~actual:(Syn.Cst.Token.text and_operator);
          Test.assert_equal ~expected:"b" ~actual:(Syn.Cst.Token.text right_name);
          Ok ()
      | _ -> Error "expected parallel let-operator expression item");
  Test.case "cst let-operator expressions expose equals and in tokens"
    (fun _ctx ->
      let source = "let ( let* ) = Result.bind\n\
           let ( and* ) = Result.both\n\
           let* a = Ok 1 and* b = Ok 2 in Ok (a, b)\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | _ :: _ :: Syn.Cst.StructureItem.Expression (Syn.Cst.Expression.LetOperator {
        binding={
          equals_token=first_equals;
          and_binding=Some { equals_token=second_equals; _ };
          _;

        };
        in_token;
        _;

      }) :: _ ->
          Test.assert_equal ~expected:"=" ~actual:(Syn.Cst.Token.text first_equals);
          Test.assert_equal ~expected:"=" ~actual:(Syn.Cst.Token.text second_equals);
          Test.assert_equal ~expected:"in" ~actual:(Syn.Cst.Token.text in_token);
          Ok ()
      | _ -> Error "expected let-operator tokens to expose equals and in");
  Test.case "cst let bindings keep comment trivia before grouped and-bindings"
    (fun ctx ->
      assert_ml_cst_snapshot ~ctx
        {|
let rec first x = second x
(* Parse signature item (top-level in .mli files) *)
and second x = x
|});
  Test.case "cst type declarations keep declaration attributes attached"
    (fun _ctx ->
      let result = parse_ml
        {|type perform = { perform : int } [@@unboxed]
|}
      in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | [ Syn.Cst.StructureItem.TypeDeclaration decl ] -> (
          match Syn.Cst.TypeDeclaration.attributes decl with
          | [ attribute ] ->
              Test.assert_equal ~expected:"@@" ~actual:(attribute_sigil_text attribute);
              Test.assert_equal
                ~expected:(Some "unboxed")
                ~actual:(Syn.Cst.Ident.name attribute.name);
              Ok ()
          | _ -> Error "expected declaration-level type attribute to stay attached"
        )
      | _ -> Error "expected a single type declaration item without a trailing floating attribute");
  Test.case "cst let expressions expose unit-pattern sequencing structurally"
    (fun _ctx ->
      let source = "let render () = let () = log () in flush ()\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let assert_nested item_syntax_nodes =
        let nested_items = Syn.CstBuilder.structure_items_from_syntax_nodes item_syntax_nodes in
        match nested_items with
        | Ok (Syn.Cst.StructureItem.LetBinding binding :: _) ->
            Test.assert_equal ~expected:3 ~actual:(List.length binding.parameters);
            Ok ()
        | Ok _ ->
            Error "expected nested let binding"
        | Error _ ->
            Error "expected nested structure relift to succeed"
      in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LetModule {
          module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes; _ };
          _;

        };
        _;

      } :: _ -> assert_nested item_syntax_nodes
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Let {
          binding_pattern=Syn.Cst.Pattern.Literal { literal=Syn.Cst.PatternLiteral.Unit _; _ };
          body=Syn.Cst.Expression.Apply _;
          _;

        };
        _;

      } :: _ -> Ok ()
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst let bindings expose fun expressions structurally"
    (fun _ctx ->
      let source = "let render = fun value -> value\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Fun {
          parameters=[ param ];
          body=Syn.Cst.Expression (Syn.Cst.Expression.Path { path; _ });
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "value") ~actual:(Syn.Cst.Parameter.name param);
          Test.assert_equal ~expected:(Some "value") ~actual:(Syn.Cst.Ident.name path);
          Ok ()
      | _ -> Error "expected first item to be a let binding with a fun expression");
  Test.case "cst fun expressions preserve return type annotations"
    (fun _ctx ->
      let source = "let render = fun ~syntax_node syntax_tokens : Cst.extension -> syntax_node\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding { value=Syn.Cst.Expression.Fun {
          parameters=[labeled;positional];
          colon_token=Some _;
          return_type=Some (Syn.Cst.CoreType.Constr { constructor_path; _ });
          body=Syn.Cst.Expression (Syn.Cst.Expression.Path { path; _ });
          _;

        }; _;  } :: _ ->
          Test.assert_equal ~expected:(Some "syntax_node") ~actual:(Syn.Cst.Parameter.name labeled);
          Test.assert_equal
            ~expected:(Some "syntax_tokens")
            ~actual:(Syn.Cst.Parameter.name positional);
          Test.assert_equal
            ~expected:(Some "extension")
            ~actual:(Syn.Cst.Ident.name constructor_path);
          Test.assert_equal ~expected:(Some "syntax_node") ~actual:(Syn.Cst.Ident.name path);
          Ok ()
      | _ -> Error "expected first item to be a let binding with a typed fun expression");
  Test.case "cst let bindings expose function expressions structurally"
    (fun _ctx ->
      let source = "let render = function | 0 -> \"zero\" | _ -> \"other\"\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding { value=Syn.Cst.Expression.Function { cases; _ }; _;  } :: _ ->
          Test.assert_equal ~expected:2 ~actual:(List.length cases);
          Ok ()
      | _ -> Error "expected first item to be a let binding with a function expression");
  Test.case "cst function expressions preserve the leading bar token"
    (fun _ctx ->
      let source = "let render = function | 0 -> \"zero\" | _ -> \"other\"\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Function {
          cases={ bar_token=Some leading_bar_token; _ } :: { bar_token=Some second_bar_token; _ } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"|" ~actual:(Syn.Cst.Token.text leading_bar_token);
          Test.assert_equal ~expected:"|" ~actual:(Syn.Cst.Token.text second_bar_token);
          Ok ()
      | _ -> Error "expected function cases to preserve their leading bars");
  Test.case "cst fun expressions preserve nested function case bodies"
    (fun _ctx ->
      let source = "let render default = fun value -> function | Some current -> current | None -> default\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Fun {
          parameters=[ param ];
          body=Syn.Cst.Cases {
            cases={
              pattern=Syn.Cst.Pattern.Constructor {
                constructor_path=some_path;
                arguments=[ Syn.Cst.Pattern.Identifier { name_token=current_name; _ };  ];
                _;

              };
              body=Syn.Cst.Expression.Path { path=current_path; _ };
              _;

            } :: {
              pattern=Syn.Cst.Pattern.Constructor { constructor_path=none_path; arguments=[]; _;  };
              body=Syn.Cst.Expression.Path { path=default_path; _ };
              _;

            } :: _;
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "value") ~actual:(Syn.Cst.Parameter.name param);
          Test.assert_equal ~expected:(Some "Some") ~actual:(Syn.Cst.Ident.name some_path);
          Test.assert_equal ~expected:"current" ~actual:(Syn.Cst.Token.text current_name);
          Test.assert_equal ~expected:(Some "current") ~actual:(Syn.Cst.Ident.name current_path);
          Test.assert_equal ~expected:(Some "None") ~actual:(Syn.Cst.Ident.name none_path);
          Test.assert_equal ~expected:(Some "default") ~actual:(Syn.Cst.Ident.name default_path);
          Ok ()
      | _ -> Error "expected fun expression with nested function body");
  Test.case "cst match expressions expose boolean cases structurally"
    (fun _ctx ->
      let source = "let render flag = match flag with true -> 1 | false -> 0\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases=[
            {
              pattern=Syn.Cst.Pattern.Literal {
                literal=Syn.Cst.PatternLiteral.Bool { literal_token=first; _ };
                _;

              };
              body=_;
              _;

            };
            {
              pattern=Syn.Cst.Pattern.Literal {
                literal=Syn.Cst.PatternLiteral.Bool { literal_token=second; _ };
                _;

              };
              body=_;
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"true" ~actual:(Syn.Cst.Token.text first);
          Test.assert_equal ~expected:"false" ~actual:(Syn.Cst.Token.text second);
          Ok ()
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst try expressions expose handlers structurally"
    (fun _ctx ->
      let source = "let render value = try render_inner value with exn -> raise exn\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Try {
          cases=[
            {
              pattern=Syn.Cst.Pattern.Identifier { name_token; _ };
              body=Syn.Cst.Expression.Apply _;
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"exn" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected first item to be a let binding with a try expression");
  Test.case "cst try expressions expose effect handlers structurally"
    (fun _ctx ->
      let source = "let render thunk = try thunk () with | effect (Yield x), k -> continue k x\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Try {
          cases=[
            {
              pattern=Syn.Cst.Pattern.Effect {
                effect_pattern=Syn.Cst.Pattern.Parenthesized {
                  inner=Syn.Cst.Pattern.Constructor {
                    constructor_path=Syn.Cst.Ident.Ident { name_token=effect_name; _;  };
                    arguments=[ Syn.Cst.Pattern.Identifier { name_token=effect_value; _;  };  ];
                    _;

                  };
                  _;

                };
                continuation=Syn.Cst.Pattern.Identifier { name_token=continuation; _ };
                _;

              };
              body=Syn.Cst.Expression.Apply _;
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Yield" ~actual:(Syn.Cst.Token.text effect_name);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text effect_value);
          Test.assert_equal ~expected:"k" ~actual:(Syn.Cst.Token.text continuation);
          Ok ()
      | _ -> Error "expected first item to be a let binding with an effect handler");
  Test.case "cst let bindings preserve infix expressions structurally"
    (fun _ctx ->
      let source = "let changed = (left <> right)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | binding :: _ -> (
          let value =
            match Syn.Cst.LetBinding.value binding with
            | Syn.Cst.Expression.Parenthesized { inner; _ } -> inner
            | expr -> expr
          in
          match value with
          | Syn.Cst.Expression.Infix expr ->
              Test.assert_equal ~expected:"<>" ~actual:(Syn.Cst.InfixExpression.operator expr);
              Ok ()
          | _ -> Error "expected bound value to contain an infix expression"
        )
      | [] -> Error "expected let binding");
  Test.case "cst let bindings expose apply and field access expressions structurally"
    (fun _ctx ->
      let source = "let reversed = List.rev (List.rev xs)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
          match Syn.Cst.LetBinding.value binding with
          | Syn.Cst.Expression.Apply outer -> (
              match outer with
              | {
                callee=Syn.Cst.Expression.FieldAccess {
                  receiver=Syn.Cst.Expression.Path { path; _ };
                  field_name;
                  _;

                };
                argument=Syn.Cst.Positional (Syn.Cst.Expression.Parenthesized {
                  inner=Syn.Cst.Expression.Apply _;
                  _;

                });
                _;

              } ->
                  Test.assert_equal
                    ~expected:(Some "rev")
                    ~actual:(Some (Syn.Cst.Token.text field_name));
                  Test.assert_equal ~expected:(Some "List") ~actual:(Syn.Cst.Ident.name path);
                  Ok ()
              | _ -> Error "expected field access callee"
            )
          | _ -> Error "expected apply expression value"
        )
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst begin-end expressions preserve grouping style"
    (fun _ctx ->
      let source = "let wrapped = begin log \"start\"; log \"done\" end\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Parenthesized {
          grouping=Syn.Cst.BeginEnd;
          inner=Syn.Cst.Expression.Sequence _;
          _;

        };
        _;

      } :: _ -> Ok ()
      | _ -> Error "expected begin-end parenthesized sequence");
  Test.case "cst preserves top-level trailing-semicolon phrases as sequences"
    (fun _ctx ->
      let source = "-1;;\n\n~+2;;\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.Expression (Syn.Cst.Expression.Sequence { expressions=[ _ ]; _ }) :: Syn.Cst.StructureItem.Expression (Syn.Cst.Expression.Sequence {
        expressions=[ _ ];
        _
      }) :: _ -> Ok ()
      | _ -> Error "expected top-level trailing-semicolon sequences");
  Test.case "cst keeps newline-before-semicolon sequences inside let-in bodies"
    (fun _ctx ->
      let source = "let with_lock t f = f ()\n\
           \n\
           let render t =\n\
           \  let value = 0 in\n\
           \  with_lock t\n\
           \    (fun () ->\n\
           \      ignore 1;\n\
           \      ignore 2)\n\
           \  ;\n\
           \  (match value with\n\
           \  | 0 -> ()\n\
           \  | _ -> ());\n\
           \  ignore value\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | _ :: Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Let {
          body=Syn.Cst.Expression.Sequence { expressions=[_;_;_]; _ };
          _;

        };
        _;

      } :: _ -> Ok ()
      | _ -> Error "expected let-in body to remain a three-part sequence");
  Test.case "cst begin-end sequences survive inline comments before semicolons"
    (fun _ctx ->
      let source = "let wrapped = begin log \"start\" (* c *); log \"done\" end\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Parenthesized {
          grouping=Syn.Cst.BeginEnd;
          inner=Syn.Cst.Expression.Sequence { expressions=[_;_]; _ };
          _;

        };
        _;

      } :: _ -> Ok ()
      | _ -> Error "expected commented begin-end sequence");
  Test.case "cst sequences expose every separator token"
    (fun _ctx ->
      let source = "let wrapped = begin log \"start\"; log \"middle\"; log \"done\" end\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Parenthesized {
          grouping=Syn.Cst.BeginEnd;
          inner=Syn.Cst.Expression.Sequence { separator_tokens; expressions=[_;_;_]; _ };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:2 ~actual:(List.length separator_tokens);
          Test.assert_equal
            ~expected:[ ";"; ";" ]
            ~actual:(List.map Syn.Cst.Token.text separator_tokens);
          Ok ()
      | _ -> Error "expected begin-end sequence with separator token list");
  Test.case "cst constructor expressions preserve bare and applied forms"
    (fun _ctx ->
      let source = "let some = Some 42\n\
           let none = None\n\
           let err = Result.Error message\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | some_binding :: none_binding :: err_binding :: _ -> (
          match (
            Syn.Cst.LetBinding.value some_binding,
            Syn.Cst.LetBinding.value none_binding,
            Syn.Cst.LetBinding.value err_binding
          ) with
          | (Syn.Cst.Expression.Constructor { constructor_path=some_path; payload=Some payload; _ }, Syn.Cst.Expression.Constructor {
            constructor_path=none_path;
            payload=None;
            _
          }, Syn.Cst.Expression.Constructor {
            constructor_path=err_path;
            payload=Some err_payload;
            _
          }) ->
              Test.assert_equal ~expected:(Some "Some") ~actual:(Syn.Cst.Ident.name some_path);
              Test.assert_equal ~expected:(Some "None") ~actual:(Syn.Cst.Ident.name none_path);
              Test.assert_equal ~expected:(Some "Error") ~actual:(Syn.Cst.Ident.name err_path);
              (
                match payload with
                | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _) -> (
                    match err_payload with
                    | Syn.Cst.Expression.Path { path; _ } ->
                        Test.assert_equal
                          ~expected:(Some "message")
                          ~actual:(Syn.Cst.Ident.name path);
                        Ok ()
                    | _ -> Error "expected qualified constructor payload to remain path"
                  )
                | _ -> Error "expected constructor payload to remain literal"
              )
          | _ -> Error "expected constructor-shaped expression values"
        )
      | _ -> Error "expected three let bindings");
  Test.case "cst tuple expressions survive inline comments before commas"
    (fun _ctx ->
      let source = "let pair = (left (* c *), right)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Parenthesized {
          inner=Syn.Cst.Expression.Tuple {
            elements=[
              Syn.Cst.Expression.Path { path=left_path; _ };
              Syn.Cst.Expression.Path { path=right_path; _ };

            ];
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "left") ~actual:(Syn.Cst.Ident.name left_path);
          Test.assert_equal ~expected:(Some "right") ~actual:(Syn.Cst.Ident.name right_path);
          Ok ()
      | _ -> Error "expected commented tuple expression");
  Test.case "cst apply expressions preserve labeled arguments structurally"
    (fun _ctx ->
      let source = "let x = f ~y:1\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Apply {
          callee=Syn.Cst.Expression.Path _;
          argument=Syn.Cst.Labeled {
            sigil_token;
            label_token;
            value=Some (Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _));
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"~" ~actual:(Syn.Cst.Token.text sigil_token);
          Test.assert_equal ~expected:"y" ~actual:(Syn.Cst.Token.text label_token);
          Ok ()
      | _ -> Error "expected labeled apply argument");
  Test.case "cst tuples keep labeled applies separate from following comma expressions"
    (fun _ctx ->
      let source = "let x = (f ~y:value, fallback)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Parenthesized {
          inner=Syn.Cst.Expression.Tuple {
            elements=[
              Syn.Cst.Expression.Apply {
                callee=Syn.Cst.Expression.Path { path=callee_path; _ };
                argument=Syn.Cst.Labeled {
                  label_token;
                  value=Some (Syn.Cst.Expression.Path { path=value_path; _ });
                  _;

                };
                _;

              };
              Syn.Cst.Expression.Path { path=fallback_path; _ };

            ];
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "f") ~actual:(Syn.Cst.Ident.name callee_path);
          Test.assert_equal ~expected:"y" ~actual:(Syn.Cst.Token.text label_token);
          Test.assert_equal ~expected:(Some "value") ~actual:(Syn.Cst.Ident.name value_path);
          Test.assert_equal ~expected:(Some "fallback") ~actual:(Syn.Cst.Ident.name fallback_path);
          Ok ()
      | _ -> Error "expected tuple expression whose first element is a labeled apply");
  Test.case "cst apply expressions preserve optional shorthand arguments"
    (fun _ctx ->
      let source = "let x = f ?y\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Apply {
          argument=Syn.Cst.Optional { sigil_token; label_token; value=None; _;  };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"?" ~actual:(Syn.Cst.Token.text sigil_token);
          Test.assert_equal ~expected:"y" ~actual:(Syn.Cst.Token.text label_token);
          Ok ()
      | _ -> Error "expected optional shorthand apply argument");
  Test.case "cst apply expressions survive inline comments before positional args"
    (fun _ctx ->
      let source = "let x = f (* c *) y\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Apply {
          callee=Syn.Cst.Expression.Path { path=callee_path; _ };
          argument=Syn.Cst.Positional (Syn.Cst.Expression.Path { path=arg_path; _ });
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "f") ~actual:(Syn.Cst.Ident.name callee_path);
          Test.assert_equal ~expected:(Some "y") ~actual:(Syn.Cst.Ident.name arg_path);
          Ok ()
      | _ -> Error "expected commented positional apply expression");
  Test.case "cst local opens preserve module paths from token-only syntax"
    (fun _ctx ->
      let source = "let x =\n  let open List in\n  map f xs\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LocalOpen (Syn.Cst.LetOpen {
          module_path=Syn.Cst.Ident.Ident { name_token=module_name; _ };
          body=Syn.Cst.Expression.Apply _;
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"List" ~actual:(Syn.Cst.Token.text module_name);
          Ok ()
      | _ -> Error "expected local open expression");
  Test.case "cst local opens via let survive inline comments after let"
    (fun _ctx ->
      let source = "let x = let (* c *) open List in map f xs\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LocalOpen (Syn.Cst.LetOpen {
          module_path=Syn.Cst.Ident.Ident { name_token=module_name; _ };
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"List" ~actual:(Syn.Cst.Token.text module_name);
          Ok ()
      | _ -> Error "expected commented let-open local open expression");
  Test.case "cst local opens preserve module paths through shortcut shells"
    (fun _ctx ->
      let source = {|
let x =
  let open%foo[@foo] M in
  ()
|}
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LocalOpen (Syn.Cst.LetOpen {
          module_path=Syn.Cst.Ident.Ident { name_token=module_name; _ };
          body=Syn.Cst.Expression.Literal (Syn.Cst.Constant.Unit _);
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"M" ~actual:(Syn.Cst.Token.text module_name);
          Ok ()
      | _ -> Error "expected shortcut let-open local open expression");
  Test.case "cst prefix local opens preserve module paths and body expressions"
    (fun _ctx ->
      let source = "let x = M.{ field = 42 }\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LocalOpen (Syn.Cst.Delimited {
          module_path=Syn.Cst.Ident.Ident { name_token=module_name; _ };
          body=Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal _);
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"M" ~actual:(Syn.Cst.Token.text module_name);
          Ok ()
      | _ -> Error "expected prefix local open expression");
  Test.case "cst prefix local opens preserve paren bodies across inline comments"
    (fun _ctx ->
      let source = "let x = M (* c *).(build value)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LocalOpen (Syn.Cst.Delimited {
          module_path=Syn.Cst.Ident.Ident { name_token=module_name; _ };
          body=Syn.Cst.Expression.Apply _;
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"M" ~actual:(Syn.Cst.Token.text module_name);
          Ok ()
      | _ -> Error "expected commented paren local open expression");
  Test.case "cst prefix local opens preserve list bodies across inline comments"
    (fun _ctx ->
      let source = "let x = List.(* c *)[1; 2; 3]\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LocalOpen (Syn.Cst.Delimited {
          module_path=Syn.Cst.Ident.Ident { name_token=module_name; _ };
          body=Syn.Cst.Expression.List _;
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"List" ~actual:(Syn.Cst.Token.text module_name);
          Ok ()
      | _ -> Error "expected commented bracket local open expression");
  Test.case "cst local open patterns preserve module paths and wrapped patterns"
    (fun _ctx ->
      let source = "let unwrap = function\n| Outer.Inner.(Some x) -> x\n| Outer.Inner.(None) -> 0\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Function {
          cases={
            pattern=Syn.Cst.Pattern.LocalOpen {
              module_path=Syn.Cst.Ident.Qualified {
                prefix=Syn.Cst.Ident.Ident { name_token=outer_module; _ };
                name_token=inner_module;
                _;

              };
              pattern=Syn.Cst.Pattern.Constructor {
                constructor_path=Syn.Cst.Ident.Ident { name_token=constructor_name; _ };
                arguments=[ Syn.Cst.Pattern.Identifier { name_token=binding_name; _ };  ];
                _;

              };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Outer" ~actual:(Syn.Cst.Token.text outer_module);
          Test.assert_equal ~expected:"Inner" ~actual:(Syn.Cst.Token.text inner_module);
          Test.assert_equal ~expected:"Some" ~actual:(Syn.Cst.Token.text constructor_name);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text binding_name);
          Ok ()
      | _ -> Error "expected local open pattern");
  Test.case "cst local open patterns survive inline comments before the local-open dot"
    (fun _ctx ->
      let source = "let unwrap = function\n| Outer.Inner (* c *).(Some x) -> x\n| _ -> 0\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Function {
          cases={
            pattern=Syn.Cst.Pattern.LocalOpen {
              module_path=Syn.Cst.Ident.Qualified {
                prefix=Syn.Cst.Ident.Ident { name_token=outer_module; _ };
                name_token=inner_module;
                _;

              };
              pattern=Syn.Cst.Pattern.Constructor {
                constructor_path=Syn.Cst.Ident.Ident { name_token=constructor_name; _ };
                arguments=[ Syn.Cst.Pattern.Identifier { name_token=binding_name; _ };  ];
                _;

              };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Outer" ~actual:(Syn.Cst.Token.text outer_module);
          Test.assert_equal ~expected:"Inner" ~actual:(Syn.Cst.Token.text inner_module);
          Test.assert_equal ~expected:"Some" ~actual:(Syn.Cst.Token.text constructor_name);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text binding_name);
          Ok ()
      | _ -> Error "expected commented local open pattern");
  Test.case "cst local open record patterns preserve module paths and record fields"
    (fun _ctx ->
      let source = "let serialize frame =\n\
          \  let Frame.{ fin; opcode; payload } = frame in\n\
          \  payload\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | {
        value=Syn.Cst.Expression.Let {
          binding_pattern=Syn.Cst.Pattern.LocalOpen {
            module_path=Syn.Cst.Ident.Ident { name_token=module_name; _ };
            pattern=Syn.Cst.Pattern.Record { fields; _ };
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Frame" ~actual:(Syn.Cst.Token.text module_name);
          Test.assert_equal ~expected:3 ~actual:(List.length fields);
          Ok ()
      | _ -> Error "expected local open record pattern");
  Test.case "cst first-class module patterns preserve anonymous unpack binders"
    (fun _ctx ->
      let source = "let ignore_typed packed = let (module _ : S) = packed in ()\n\
           let ignore_plain packed = let (module _) = packed in ()\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | {
        value=Syn.Cst.Expression.Let {
          binding_pattern=Syn.Cst.Pattern.FirstClassModule {
            binding=Syn.Cst.Anonymous { wildcard_token=typed_wildcard };
            package_type=Some { module_type_path=typed_module_type; constraints=[]; _ };
            _;

          };
          _;

        };
        _;

      } :: {
        value=Syn.Cst.Expression.Let {
          binding_pattern=Syn.Cst.Pattern.FirstClassModule {
            binding=Syn.Cst.Anonymous { wildcard_token=plain_wildcard };
            package_type=None;
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"_" ~actual:(Syn.Cst.Token.text typed_wildcard);
          Test.assert_equal ~expected:"_" ~actual:(Syn.Cst.Token.text plain_wildcard);
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name typed_module_type);
          Ok ()
      | _ -> Error "expected anonymous first-class module patterns");
  Test.case "cst first-class module expressions preserve module and type nodes"
    (fun _ctx ->
      let source = "let x = (module M : S)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.ModulePack {
          module_expression=Syn.Cst.ModuleExpression.Path module_path;
          package_type=Some { module_type_path=module_type_path; constraints=[]; _ };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "M") ~actual:(Syn.Cst.Ident.name module_path);
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name module_type_path);
          Ok ()
      | _ -> Error "expected first-class module expression");
  Test.case "cst qualified module paths preserve recursive structure"
    (fun _ctx ->
      let source = "let x = (module Std.Net.TcpClient : Std.Net.Transport)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.ModulePack {
          module_expression=Syn.Cst.ModuleExpression.Path (Syn.Cst.Ident.Qualified {
            prefix=Syn.Cst.Ident.Qualified {
              prefix=Syn.Cst.Ident.Ident { name_token=root; _ };
              name_token=mid;
              _;

            };
            name_token=leaf;
            _;

          });
          package_type=Some {
            module_type_path=Syn.Cst.Ident.Qualified {
              prefix=Syn.Cst.Ident.Qualified {
                prefix=Syn.Cst.Ident.Ident { name_token=type_root; _ };
                name_token=type_mid;
                _;

              };
              name_token=type_leaf;
              _;

            };
            constraints=[];
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Std" ~actual:(Syn.Cst.Token.text root);
          Test.assert_equal ~expected:"Net" ~actual:(Syn.Cst.Token.text mid);
          Test.assert_equal ~expected:"TcpClient" ~actual:(Syn.Cst.Token.text leaf);
          Test.assert_equal ~expected:"Std" ~actual:(Syn.Cst.Token.text type_root);
          Test.assert_equal ~expected:"Net" ~actual:(Syn.Cst.Token.text type_mid);
          Test.assert_equal ~expected:"Transport" ~actual:(Syn.Cst.Token.text type_leaf);
          Ok ()
      | _ -> Error "expected qualified first-class module path");
  Test.case "cst qualified module paths survive inline comments before dots"
    (fun _ctx ->
      let source = "let x = (module Std (* c *).Net.TcpClient : Std.Net (* d *).Transport)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.ModulePack {
          module_expression=Syn.Cst.ModuleExpression.Path (Syn.Cst.Ident.Qualified {
            prefix=Syn.Cst.Ident.Qualified {
              prefix=Syn.Cst.Ident.Ident { name_token=root; _ };
              name_token=mid;
              _;

            };
            name_token=leaf;
            _;

          });
          package_type=Some {
            module_type_path=Syn.Cst.Ident.Qualified {
              prefix=Syn.Cst.Ident.Qualified {
                prefix=Syn.Cst.Ident.Ident { name_token=type_root; _ };
                name_token=type_mid;
                _;

              };
              name_token=type_leaf;
              _;

            };
            constraints=[];
            _;

          };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Std" ~actual:(Syn.Cst.Token.text root);
          Test.assert_equal ~expected:"Net" ~actual:(Syn.Cst.Token.text mid);
          Test.assert_equal ~expected:"TcpClient" ~actual:(Syn.Cst.Token.text leaf);
          Test.assert_equal ~expected:"Std" ~actual:(Syn.Cst.Token.text type_root);
          Test.assert_equal ~expected:"Net" ~actual:(Syn.Cst.Token.text type_mid);
          Test.assert_equal ~expected:"Transport" ~actual:(Syn.Cst.Token.text type_leaf);
          Ok ()
      | _ -> Error "expected commented qualified first-class module path");
  Test.case "cst first-class module expressions preserve structured packed payloads"
    (fun _ctx ->
      let source = "let x = (module struct let y = 1 end : S)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.ModulePack {
          module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes=[ item_node ]; _ };
          package_type=Some { module_type_path=module_type_path; constraints=[]; _ };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal
            ~expected:SyntaxKind.LET_BINDING
            ~actual:(Ceibo.Red.SyntaxNode.kind item_node);
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name module_type_path);
          Ok ()
      | _ -> Error "expected structured packed first-class module expression");
  Test.case "cst module type declarations preserve functor module type bodies"
    (fun _ctx ->
      let result = parse_ml "module type F = functor (X : S) -> T\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.Functor {
          parameters=[ { name_token; module_type=Syn.Cst.ModuleType.Path param_type; _ } ];
          result=Syn.Cst.ModuleType.Path result_type;
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"X" ~actual:(Syn.Cst.Token.text name_token);
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name param_type);
          Test.assert_equal ~expected:(Some "T") ~actual:(Syn.Cst.Ident.name result_type);
          Ok ()
      | _ -> Error "expected module type declaration with functor body");
  Test.case "cst module type functor lookahead survives inline comments in parameters"
    (fun _ctx ->
      let result = parse_ml "module type F = functor (X (* c *) : S) -> T\n" in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.Functor {
          parameters=[ { name_token; module_type=Syn.Cst.ModuleType.Path param_type; _ } ];
          result=Syn.Cst.ModuleType.Path result_type;
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:"X" ~actual:(Syn.Cst.Token.text name_token);
          Test.assert_equal ~expected:(Some "S") ~actual:(Syn.Cst.Ident.name param_type);
          Test.assert_equal ~expected:(Some "T") ~actual:(Syn.Cst.Ident.name result_type);
          Ok ()
      | _ -> Error "expected commented module type functor body");
  Test.case "cst let-module expressions preserve module name and body"
    (fun _ctx ->
      let source = "let run driver = let module D = (val driver) in D.execute ()\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LetModule {
          module_name_token;
          module_expression=Syn.Cst.ModuleExpression.ModuleUnpack {
            expression=Syn.Cst.Expression.Path { path=module_path; _ };
            package_type=None;
            _;

          };
          body=Syn.Cst.Expression.Apply _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"D" ~actual:(Syn.Cst.Token.text module_name_token);
          Test.assert_equal ~expected:(Some "driver") ~actual:(Syn.Cst.Ident.name module_path);
          Ok ()
      | _ -> Error "expected let-module expression");
  Test.case "cst let-module expressions preserve commented first-class module unpacking"
    (fun _ctx ->
      let source = "let run driver = let module D = ( (* c *) val driver ) in D.execute ()\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LetModule {
          module_name_token;
          module_expression=Syn.Cst.ModuleExpression.ModuleUnpack {
            expression=Syn.Cst.Expression.Path { path=module_path; _ };
            package_type=None;
            _;

          };
          body=Syn.Cst.Expression.Apply _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"D" ~actual:(Syn.Cst.Token.text module_name_token);
          Test.assert_equal ~expected:(Some "driver") ~actual:(Syn.Cst.Ident.name module_path);
          Ok ()
      | _ -> Error "expected commented let-module expression");
  Test.case "cst field access preserves nested qualified field access structurally"
    (fun _ctx ->
      let source = "let render record = record.Module.field\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.FieldAccess {
          receiver=Syn.Cst.Expression.FieldAccess {
            receiver=Syn.Cst.Expression.Path { path; _ };
            field_name=module_name;
            _;

          };
          field_name;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "record") ~actual:(Syn.Cst.Ident.name path);
          Test.assert_equal ~expected:"Module" ~actual:(Syn.Cst.Token.text module_name);
          Test.assert_equal ~expected:"field" ~actual:(Syn.Cst.Token.text field_name);
          Ok ()
      | _ -> Error "expected nested field access structure");
  Test.case "cst preserves parenthesized expressions structurally"
    (fun _ctx ->
      let source = "let wrapped = (((((value)))))\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding binding :: _ ->
          let rec depth = function
            | Syn.Cst.Expression.Parenthesized expr -> 1 + depth expr.inner
            | _ -> 0
          in
          Test.assert_equal ~expected:5 ~actual:(depth (Syn.Cst.LetBinding.value binding));
          Ok ()
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst preserves parenthesized expressions with inner comments"
    (fun _ctx ->
      let source = "let wrapped = ( (* c *) value )\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for commented parenthesized expression"
      |> Result.expect ~msg:"expected CST for commented parenthesized expression" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding binding :: _ -> (
          match Syn.Cst.LetBinding.value binding with
          | Syn.Cst.Expression.Parenthesized { inner=Syn.Cst.Expression.Path _; _ } -> Ok ()
          | _ -> Error "expected commented parenthesized expression to stay grouped"
        )
      | _ -> Error "expected first item to be a let binding");
  Test.case "cst function cases preserve constructor tuple patterns structurally"
    (fun _ctx ->
      let source = "let render = function | Some (head, _) -> head | None -> 0\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Function {
          cases={
            pattern=Syn.Cst.Pattern.Constructor {
              constructor_path=some_path;
              arguments=[
                Syn.Cst.Pattern.Parenthesized {
                  inner=Syn.Cst.Pattern.Tuple {
                    elements=[
                      { pattern=Syn.Cst.Pattern.Identifier { name_token=head_name; _ }; _;  };
                      { pattern=Syn.Cst.Pattern.Wildcard _; _;  };

                    ];
                    open_tail=None;
                    _;

                  };
                  _;

                };

              ];
              _;

            };
            _;

          } :: {
            pattern=Syn.Cst.Pattern.Constructor { constructor_path=none_path; arguments=[]; _ };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "Some") ~actual:(Syn.Cst.Ident.name some_path);
          Test.assert_equal ~expected:"head" ~actual:(Syn.Cst.Token.text head_name);
          Test.assert_equal ~expected:(Some "None") ~actual:(Syn.Cst.Ident.name none_path);
          Ok ()
      | _ -> Error "expected faithful constructor pattern structure");
  Test.case "cst constructor patterns preserve existential binders"
    (fun _ctx ->
      let source = "let unwrap = function | Pair (type a b) ((left, right) : a * b) -> left\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Function {
          cases={
            pattern=Syn.Cst.Pattern.Constructor {
              constructor_path;
              existentials=Some {
                binders=[
                  Syn.Cst.TypeBinder.Bare { name_token=a_name };
                  Syn.Cst.TypeBinder.Bare { name_token=b_name };

                ];
                _;

              };
              arguments=[ Syn.Cst.Pattern.Typed _ ];
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "Pair") ~actual:(Syn.Cst.Ident.name constructor_path);
          Test.assert_equal ~expected:"a" ~actual:(Syn.Cst.Token.text a_name);
          Test.assert_equal ~expected:"b" ~actual:(Syn.Cst.Token.text b_name);
          Ok ()
      | _ -> Error "expected constructor existential binders");
  Test.case "cst match cases preserve alias and typed patterns structurally"
    (fun _ctx ->
      let source = "let render value = match value with | (user : user_t) as current_user -> current_user\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Alias {
              pattern=Syn.Cst.Pattern.Typed {
                pattern=Syn.Cst.Pattern.Identifier { name_token=user_name; _ };
                type_=Syn.Cst.CoreType.Constr _;
                _;

              };
              name_token=alias_name;
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"user" ~actual:(Syn.Cst.Token.text user_name);
          Test.assert_equal ~expected:"current_user" ~actual:(Syn.Cst.Token.text alias_name);
          Ok ()
      | _ -> Error "expected faithful alias typed pattern structure");
  Test.case "cst or patterns survive inline comments before pipe"
    (fun _ctx ->
      let source = "let render value = match value with | None (* c *) | Some _ -> 0 | _ -> 1\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Or {
              alternatives=[
                Syn.Cst.Pattern.Constructor { constructor_path=none_path; arguments=[]; _ };
                Syn.Cst.Pattern.Constructor {
                  constructor_path=some_path;
                  arguments=[ Syn.Cst.Pattern.Wildcard _ ];
                  _;

                };

              ];
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "None") ~actual:(Syn.Cst.Ident.name none_path);
          Test.assert_equal ~expected:(Some "Some") ~actual:(Syn.Cst.Ident.name some_path);
          Ok ()
      | _ -> Error "expected commented or pattern structure");
  Test.case "cst alias patterns survive inline comments before as"
    (fun _ctx ->
      let source = "let render value = match value with | user (* c *) as current_user -> current_user\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Alias {
              pattern=Syn.Cst.Pattern.Identifier { name_token=user_name; _ };
              name_token=alias_name;
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"user" ~actual:(Syn.Cst.Token.text user_name);
          Test.assert_equal ~expected:"current_user" ~actual:(Syn.Cst.Token.text alias_name);
          Ok ()
      | _ -> Error "expected commented alias pattern structure");
  Test.case "cst cons patterns survive inline comments before coloncolon"
    (fun _ctx ->
      let source = "let first xs = match xs with | head (* c *) :: tail -> head | [] -> 0\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Cons {
              head=Syn.Cst.Pattern.Identifier { name_token=head_name; _ };
              tail=Syn.Cst.Pattern.Identifier { name_token=tail_name; _ };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"head" ~actual:(Syn.Cst.Token.text head_name);
          Test.assert_equal ~expected:"tail" ~actual:(Syn.Cst.Token.text tail_name);
          Ok ()
      | _ -> Error "expected commented cons pattern structure");
  Test.case "cst tuple patterns survive inline comments before commas"
    (fun _ctx ->
      let source = "let project pair = match pair with | (left (* c *), right) -> left\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Parenthesized {
              inner=Syn.Cst.Pattern.Tuple {
                elements=[
                  { pattern=Syn.Cst.Pattern.Identifier { name_token=left_name; _ }; _;  };
                  { pattern=Syn.Cst.Pattern.Identifier { name_token=right_name; _ }; _;  };

                ];
                _;

              };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"left" ~actual:(Syn.Cst.Token.text left_name);
          Test.assert_equal ~expected:"right" ~actual:(Syn.Cst.Token.text right_name);
          Ok ()
      | _ -> Error "expected commented tuple pattern structure");
  Test.case "cst lazy patterns preserve the wrapped pattern"
    (fun _ctx ->
      let source = "let f x = match x with | (lazy y) -> y\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases=[
            {
              pattern=Syn.Cst.Pattern.Parenthesized {
                inner=Syn.Cst.Pattern.Lazy {
                  pattern=Syn.Cst.Pattern.Identifier { name_token; _ };
                  _;

                };
                _;

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"y" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected lazy pattern structure");
  Test.case "cst exception patterns preserve the wrapped constructor"
    (fun _ctx ->
      let source = "let f x = match x with exception Not_found -> None | y -> Some y\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Exception {
              pattern=Syn.Cst.Pattern.Constructor { constructor_path; _ };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal
            ~expected:(Some "Not_found")
            ~actual:(Syn.Cst.Ident.name constructor_path);
          Ok ()
      | _ -> Error "expected exception pattern structure");
  Test.case "cst range patterns preserve the written bounds"
    (fun _ctx ->
      let source = "let f x = match x with | 'a' .. 'z' -> \"lowercase\" | _ -> \"other\"\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Range {
              lower=Syn.Cst.PatternLiteral.Char {
                literal_token=lower_token;
                contents=lower_contents;
                _
              };
              upper=Syn.Cst.PatternLiteral.Char {
                literal_token=upper_token;
                contents=upper_contents;
                _
              };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"'a'" ~actual:(Syn.Cst.Token.text lower_token);
          Test.assert_equal ~expected:"a" ~actual:lower_contents;
          Test.assert_equal ~expected:"'z'" ~actual:(Syn.Cst.Token.text upper_token);
          Test.assert_equal ~expected:"z" ~actual:upper_contents;
          Ok ()
      | _ -> Error "expected range pattern structure");
  Test.case "cst range patterns survive inline comments before dotdot"
    (fun _ctx ->
      let source = "let f x = match x with | 'a' (* c *) .. 'z' -> \"lowercase\" | _ -> \"other\"\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases={
            pattern=Syn.Cst.Pattern.Range {
              lower=Syn.Cst.PatternLiteral.Char {
                literal_token=lower_token;
                contents=lower_contents;
                _
              };
              upper=Syn.Cst.PatternLiteral.Char {
                literal_token=upper_token;
                contents=upper_contents;
                _
              };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"'a'" ~actual:(Syn.Cst.Token.text lower_token);
          Test.assert_equal ~expected:"a" ~actual:lower_contents;
          Test.assert_equal ~expected:"'z'" ~actual:(Syn.Cst.Token.text upper_token);
          Test.assert_equal ~expected:"z" ~actual:upper_contents;
          Ok ()
      | _ -> Error "expected commented range pattern structure");
  Test.case "cst literals preserve structured constant details"
    (fun _ctx ->
      let source = "let x = 0xffL\nlet y = 1.2g\nlet z = {|hello|}\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding { value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int {
          base=Syn.Cst.Hexadecimal;
          prefix;
          digits;
          suffix;
          _
        }); _;  } :: Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Float {
          integral_digits;
          fractional_digits;
          exponent=None;
          suffix=Some float_suffix;
          _;

        });
        _;

      } :: Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.String {
          delimiter=Syn.Cst.Quoted { marker=string_marker };
          contents;
          terminated;
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "0x") ~actual:prefix;
          Test.assert_equal ~expected:"ff" ~actual:digits;
          Test.assert_equal ~expected:(Some "L") ~actual:suffix;
          Test.assert_equal ~expected:"1" ~actual:integral_digits;
          Test.assert_equal ~expected:"2" ~actual:fractional_digits;
          Test.assert_equal ~expected:"g" ~actual:float_suffix;
          Test.assert_equal ~expected:"" ~actual:string_marker;
          Test.assert_equal ~expected:"hello" ~actual:contents;
          Test.assert_true terminated;
          Ok ()
      | _ -> Error "expected structured literal constants");
  Test.case "cst pattern literals preserve explicit sign tokens"
    (fun _ctx ->
      let source = "let classify = function | -1 -> true | +2 -> false | _ -> false\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Function {
          cases={
            pattern=Syn.Cst.Pattern.Literal {
              literal=Syn.Cst.PatternLiteral.Int {
                sign_token=Some negative_sign_token;
                literal_token=negative_literal_token;
                _;

              };
              _;

            };
            _;

          } :: {
            pattern=Syn.Cst.Pattern.Literal {
              literal=Syn.Cst.PatternLiteral.Int {
                sign_token=Some positive_sign_token;
                literal_token=positive_literal_token;
                _;

              };
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"-" ~actual:(Syn.Cst.Token.text negative_sign_token);
          Test.assert_equal ~expected:"1" ~actual:(Syn.Cst.Token.text negative_literal_token);
          Test.assert_equal ~expected:"+" ~actual:(Syn.Cst.Token.text positive_sign_token);
          Test.assert_equal ~expected:"2" ~actual:(Syn.Cst.Token.text positive_literal_token);
          Ok ()
      | _ -> Error "expected signed literal patterns in function cases");
  Test.case "lexer tagged quoted string literals tokenize as strings"
    (fun _ctx ->
      let kinds = Syn.tokenize "let explanation = {explain|hello|explain}\n"
      |> List.map (fun token -> Syn.Token.show_kind token.Syn.Token.kind) in
      Test.assert_equal
        ~expected:[ "keyword"; "identifier"; "="; "string"; "end of file"; ]
        ~actual:kinds;
      Ok ());
  Test.case "cst tagged quoted string literals preserve marker and contents"
    (fun _ctx ->
      let parsed = Syn.parse ~filename:sample_ml "let explanation = {explain|hello|explain}\n" in
      Test.assert_equal ~expected:0 ~actual:(List.length parsed.Syn.Parser.diagnostics);
      match Syn.build_cst parsed with
      | Error (Syn.Cst_builder_error err) ->
          Error ("expected CST builder to succeed, got "
          ^ err.Syn.CstBuilder.message
          ^ " @ "
          ^ Syn.SyntaxKind.to_string err.Syn.CstBuilder.syntax_kind
          ^ " in "
          ^ String.concat " > " err.Syn.CstBuilder.context)
      | Error (Syn.Parse_diagnostics diagnostics) ->
          let diagnostics = diagnostics |> List.map Syn.Diagnostic.to_string |> String.concat "\n" in
          Error ("expected tagged string parse to be diagnostics-free, got:\n" ^ diagnostics)
      | Ok cst -> (
          match structure_items cst with
          | Syn.Cst.StructureItem.LetBinding {
            value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.String {
              delimiter=Syn.Cst.Quoted { marker };
              contents;
              terminated;
              _;

            });
            _;

          } :: _ ->
              Test.assert_equal ~expected:"explain" ~actual:marker;
              Test.assert_equal ~expected:"hello" ~actual:contents;
              Test.assert_true terminated;
              Ok ()
          | _ -> Error "expected tagged quoted string literal"
        ));
  Test.case "cst record expressions preserve literal fields structurally"
    (fun _ctx ->
      let source = "let point = { x = 1; y = 2 }\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields=[{
              field_path=first;
              field_name=first_name;
              value=Syn.Cst.Expression.Literal _;
              source=Syn.Cst.Explicit;
              _;

            };{
              field_path=second;
              field_name=second_name;
              value=Syn.Cst.Expression.Literal _;
              source=Syn.Cst.Explicit;
              _;

            };]; _;  });
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "x") ~actual:(Syn.Cst.Ident.name first);
          Test.assert_equal ~expected:(Some "y") ~actual:(Syn.Cst.Ident.name second);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text first_name);
          Test.assert_equal ~expected:"y" ~actual:(Syn.Cst.Token.text second_name);
          Ok ()
      | _ -> Error "expected literal record expression");
  Test.case "cst record expressions preserve punning as explicit path values"
    (fun _ctx ->
      let source = "let make ~x ~y = { x; y }\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields=[{
              field_path=first;
              field_name=first_name;
              value=Syn.Cst.Expression.Path { path=first_value; _ };
              source=Syn.Cst.Punned;
              _;

            };{
              field_path=second;
              field_name=second_name;
              value=Syn.Cst.Expression.Path { path=second_value; _ };
              source=Syn.Cst.Punned;
              _;

            };]; _;  });
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "x") ~actual:(Syn.Cst.Ident.name first);
          Test.assert_equal ~expected:(Some "y") ~actual:(Syn.Cst.Ident.name second);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text first_name);
          Test.assert_equal ~expected:"y" ~actual:(Syn.Cst.Token.text second_name);
          Test.assert_equal ~expected:(Some "x") ~actual:(Syn.Cst.Ident.name first_value);
          Test.assert_equal ~expected:(Some "y") ~actual:(Syn.Cst.Ident.name second_value);
          Ok ()
      | _ -> Error "expected punned record expression");
  Test.case "cst record update expressions preserve base and updated fields"
    (fun _ctx ->
      let source = "let point = { point with x = 3 }\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update {
          base=Syn.Cst.Expression.Path { path=base_path; _ };
          fields=[ {
              field_path;
              field_name;
              value=Syn.Cst.Expression.Literal _;
              source=Syn.Cst.Explicit;
              _;

            };  ];
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "point") ~actual:(Syn.Cst.Ident.name base_path);
          Test.assert_equal ~expected:(Some "x") ~actual:(Syn.Cst.Ident.name field_path);
          Test.assert_equal ~expected:"x" ~actual:(Syn.Cst.Token.text field_name);
          Ok ()
      | _ -> Error "expected update record expression");
  Test.case "cst object override expressions preserve instance variables"
    (fun _ctx ->
      let source = "let next = {< current = state; count = total >}\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.ObjectOverride {
          fields=[
            {
              field_name=current_name;
              value=Some (Syn.Cst.Expression.Path { path=current_value; _ });
              _;

            };
            {
              field_name=count_name;
              value=Some (Syn.Cst.Expression.Path { path=count_value; _ });
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"current" ~actual:(Syn.Cst.Token.text current_name);
          Test.assert_equal ~expected:(Some "state") ~actual:(Syn.Cst.Ident.name current_value);
          Test.assert_equal ~expected:"count" ~actual:(Syn.Cst.Token.text count_name);
          Test.assert_equal ~expected:(Some "total") ~actual:(Syn.Cst.Ident.name count_value);
          Ok ()
      | _ -> Error "expected object override expression");
  Test.case "cst index and assign expressions preserve the written target"
    (fun _ctx ->
      let source = "let x = arr.(0) <- 5\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Assign {
          target=Syn.Cst.Expression.Index {
            collection=Syn.Cst.Expression.Path { path; _ };
            index=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int { literal_token; _ });
            _;

          };
          operator_token;
          value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _);
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "arr") ~actual:(Syn.Cst.Ident.name path);
          Test.assert_equal ~expected:"0" ~actual:(Syn.Cst.Token.text literal_token);
          Test.assert_equal ~expected:"<-" ~actual:(Syn.Cst.Token.text operator_token);
          Ok ()
      | _ -> Error "expected assign(index(...)) expression");
  Test.case "cst index expressions survive inline comments before dot-paren"
    (fun _ctx ->
      let source = "let x = arr (* c *).(0)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Index {
          collection=Syn.Cst.Expression.Path { path=collection_path; _ };
          index=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _);
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "arr") ~actual:(Syn.Cst.Ident.name collection_path);
          Ok ()
      | _ -> Error "expected commented index expression");
  Test.case "cst string index expressions survive inline comments before brackets"
    (fun _ctx ->
      let source = "let x = text.(* c *)[0]\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Index {
          collection=Syn.Cst.Expression.Path { path=collection_path; _ };
          index=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _);
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "text") ~actual:(Syn.Cst.Ident.name collection_path);
          Ok ()
      | _ -> Error "expected commented string index expression");
  Test.case "cst custom index expressions survive inline comments before operators"
    (fun _ctx ->
      let source = "let x = table.(* c *)?[\"key\"]\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Index {
          collection=Syn.Cst.Expression.Path { path=collection_path; _ };
          index=Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _);
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "table") ~actual:(Syn.Cst.Ident.name collection_path);
          Ok ()
      | _ -> Error "expected commented custom index expression");
  Test.case "cst assign expressions survive inline comments before arrows"
    (fun _ctx ->
      let source = "let x = arr.(0) (* c *) <- 5\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Assign {
          operator_token;
          target=Syn.Cst.Expression.Index _;
          value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _);
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"<-" ~actual:(Syn.Cst.Token.text operator_token);
          Ok ()
      | _ -> Error "expected commented assign expression");
  Test.case "cst field assignments preserve field-access targets"
    (fun _ctx ->
      let source = "let () = obj.field <- 10\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.FieldAssign {
          target={ receiver=Syn.Cst.Expression.Path { path; _ }; field_name; _;  };
          operator_token;
          value=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int _);
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "obj") ~actual:(Syn.Cst.Ident.name path);
          Test.assert_equal ~expected:"field" ~actual:(Syn.Cst.Token.text field_name);
          Test.assert_equal ~expected:"<-" ~actual:(Syn.Cst.Token.text operator_token);
          Ok ()
      | _ -> Error "expected field assignment expression");
  Test.case "cst object methods preserve instance-variable assignments"
    (fun _ctx ->
      let source = "let counter =\n  object\n    val mutable count = 0\n    method set next = count <- next\n  end\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Object {
          members=[
            Syn.Cst.ObjectMember.Value _;
            Syn.Cst.ObjectMember.Method {
              body=Syn.Cst.Expression.InstanceVariableAssign {
                name_token;
                operator_token;
                value=Syn.Cst.Expression.Path { path=value_path; _ };
                _;

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"count" ~actual:(Syn.Cst.Token.text name_token);
          Test.assert_equal ~expected:"<-" ~actual:(Syn.Cst.Token.text operator_token);
          Test.assert_equal ~expected:(Some "next") ~actual:(Syn.Cst.Ident.name value_path);
          Ok ()
      | _ -> Error "expected object method instance-variable assignment");
  Test.case
    "cst object expressions preserve extension members"
    (fun ctx -> assert_ml_cst_snapshot ~ctx "let value =\n  object\n    [%%foo]\n    method run = 1\n  end\n");
  Test.case "cst builder keeps object trailing trivia as explicit body entries"
    (fun _ctx ->
      let source = "let value =\n  object\n    method run = 1\n    (** body doc *)\n    (* body comment *)\n  end\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Object { syntax_node; members; _;  };
        _;

      } :: _ ->
          let body_items = Syn.CstBuilder.object_member_items_of_members ~source_node:syntax_node members in
          (
            match body_items with
            | [
              Syn.CstBuilder.ObjectMember (Syn.Cst.ObjectMember.Method _);
              Syn.CstBuilder.Docstring docstring;
              Syn.CstBuilder.Comment comment;

            ] ->
                Test.assert_equal
                  ~expected:"(** body doc *)"
                  ~actual:(Syn.Cst.Docstring.text docstring);
                Test.assert_equal
                  ~expected:"(* body comment *)"
                  ~actual:(Syn.Cst.Comment.text comment);
                Ok ()
            | _ -> Error "expected object stream to include method and body docstring/comment"
          )
      | _ -> Error "expected object expression in leading let binding");
  Test.case "cst record patterns preserve field punning and nested patterns"
    (fun _ctx ->
      let source = "let x = match r with { user = { id }; name } -> id\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases=[
            {
              pattern=Syn.Cst.Pattern.Record {
                fields=[
                  {
                    field_path=user_field;
                    pattern=Some (Syn.Cst.Pattern.Record {
                      fields=[ { field_path=id_field; pattern=None; _;  };  ];
                      closedness=Syn.Cst.Closed;
                      _;

                    });
                    _;

                  };
                  { field_path=name_field; pattern=None; _ };

                ];
                closedness=Syn.Cst.Closed;
                _;

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "user") ~actual:(Syn.Cst.Ident.name user_field);
          Test.assert_equal ~expected:(Some "id") ~actual:(Syn.Cst.Ident.name id_field);
          Test.assert_equal ~expected:(Some "name") ~actual:(Syn.Cst.Ident.name name_field);
          Ok ()
      | _ -> Error "expected record pattern structure");
  Test.case "cst record patterns preserve inner comments around fields"
    (fun _ctx ->
      let source = "let { (* c *) foo } = record\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for commented record pattern"
      |> Result.expect ~msg:"expected CST for commented record pattern" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        binding_pattern=Syn.Cst.Pattern.Record { fields=[ { field_path; pattern=None; _ } ]; _ };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "foo") ~actual:(Syn.Cst.Ident.name field_path);
          Ok ()
      | _ -> Error "expected commented record pattern structure");
  Test.case "cst record patterns preserve open wildcard tails"
    (fun _ctx ->
      let source = "let x = match r with { user; _ } -> user\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases=[
            {
              pattern=Syn.Cst.Pattern.Record {
                fields=[ { field_path; pattern=None; _ } ];
                closedness=Syn.Cst.Open { wildcard_token };
                _;

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "user") ~actual:(Syn.Cst.Ident.name field_path);
          Test.assert_equal ~expected:"_" ~actual:(Syn.Cst.Token.text wildcard_token);
          Ok ()
      | _ -> Error "expected open record pattern");
  Test.case "cst array patterns preserve literal element patterns"
    (fun _ctx ->
      let source = "let x = match xs with [| 1; value |] -> value\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases=[
            {
              pattern=Syn.Cst.Pattern.Array {
                elements=[
                  Syn.Cst.Pattern.Literal { literal=Syn.Cst.PatternLiteral.Int _; _ };
                  Syn.Cst.Pattern.Identifier { name_token; _ };

                ];
                _;

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"value" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected array pattern structure");
  Test.case "cst string indexing reuses the shared index expression shape"
    (fun _ctx ->
      let source = "let x = s.[0]\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding { value=Syn.Cst.Expression.Index {
          collection=Syn.Cst.Expression.Path { path; _ };
          opening_tokens;
          index=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int { literal_token; _ });
          closing_token;
          _;

        }; _;  } :: _ ->
          Test.assert_equal ~expected:(Some "s") ~actual:(Syn.Cst.Ident.name path);
          Test.assert_equal
            ~expected:[ "."; "[" ]
            ~actual:((opening_tokens |> List.map Syn.Cst.Token.text));
          Test.assert_equal ~expected:"0" ~actual:(Syn.Cst.Token.text literal_token);
          Test.assert_equal ~expected:"]" ~actual:(Syn.Cst.Token.text closing_token);
          Ok ()
      | _ -> Error "expected string index expression");
  Test.case "cst extended index expressions preserve delimiter tokens"
    (fun _ctx ->
      let source = "let z = x.%(0)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding { value=Syn.Cst.Expression.Index {
          collection=Syn.Cst.Expression.Path { path; _ };
          opening_tokens;
          index=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int { literal_token; _ });
          closing_token;
          _;

        }; _;  } :: _ ->
          Test.assert_equal ~expected:(Some "x") ~actual:(Syn.Cst.Ident.name path);
          Test.assert_equal
            ~expected:[ "."; "%"; "(" ]
            ~actual:((opening_tokens |> List.map Syn.Cst.Token.text));
          Test.assert_equal ~expected:"0" ~actual:(Syn.Cst.Token.text literal_token);
          Test.assert_equal ~expected:")" ~actual:(Syn.Cst.Token.text closing_token);
          Ok ()
      | _ -> Error "expected extended index expression");
  Test.case "cst polyvariant expressions preserve tags and payloads"
    (fun _ctx ->
      let source = "let x = `Point { y = 1; z = 2 }\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.PolyVariant {
          tag_token;
          payload=Some (Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }));
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Point" ~actual:(Syn.Cst.Token.text tag_token);
          Test.assert_equal ~expected:2 ~actual:(List.length fields);
          Ok ()
      | _ -> Error "expected polyvariant expression");
  Test.case "cst polyvariant patterns mirror bare and payload parsetree forms"
    (fun _ctx ->
      let source = "let x = match y with `Done -> 0 | `Point (a, b) -> a\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Match {
          cases=[
            {
              pattern=Syn.Cst.Pattern.PolyVariant { tag_token=bare_tag_token; payload=None; _ };
              _;

            };
            {
              pattern=Syn.Cst.Pattern.PolyVariant {
                tag_token;
                payload=Some (Syn.Cst.Pattern.Parenthesized {
                  inner=Syn.Cst.Pattern.Tuple {
                    elements=[{ pattern=Syn.Cst.Pattern.Identifier { name_token; _ }; _;  };_;];
                    open_tail=None;
                    _;

                  };
                  _;

                });
                _;

              };
              _;

            };

          ];
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Done" ~actual:(Syn.Cst.Token.text bare_tag_token);
          Test.assert_equal ~expected:"Point" ~actual:(Syn.Cst.Token.text tag_token);
          Test.assert_equal ~expected:"a" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected polyvariant pattern");
  Test.case "cst nested record updates preserve expression bases"
    (fun _ctx ->
      let source = "let x = { { point with a = 1 } with b = 2 }\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update {
          base=Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update {
            base=Syn.Cst.Expression.Path { path; _ };
            _;

          });
          fields=[ { field_path; _ } ];
          _;

        });
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "point") ~actual:(Syn.Cst.Ident.name path);
          Test.assert_equal ~expected:(Some "b") ~actual:(Syn.Cst.Ident.name field_path);
          Ok ()
      | _ -> Error "expected nested record update");
  Test.case "cst assert expressions preserve the asserted value"
    (fun _ctx ->
      let source = "let x = assert true\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Assert {
          asserted=Syn.Cst.Expression.Literal (Syn.Cst.Literal.Bool { literal_token; _ });
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"true" ~actual:(Syn.Cst.Token.text literal_token);
          Ok ()
      | _ -> Error "expected assert expression");
  Test.case "cst lazy expressions preserve the wrapped body"
    (fun _ctx ->
      let source = "let x = lazy (1 + 2)\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Lazy {
          body=Syn.Cst.Expression.Parenthesized { inner=Syn.Cst.Expression.Infix _; _;  };
          _;

        };
        _;

      } :: _ -> Ok ()
      | _ -> Error "expected lazy expression");
  Test.case "cst while expressions preserve the condition and body"
    (fun _ctx ->
      let source = "let x = while !y < 10 do y := !y + 1 done\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.While {
          condition=Syn.Cst.Expression.Infix _;
          body=Syn.Cst.Expression.Assign { operator_token; _ };
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:":=" ~actual:(Syn.Cst.Token.text operator_token);
          Ok ()
      | _ -> Error "expected while expression");
  Test.case "cst for expressions preserve iterator and direction"
    (fun _ctx ->
      let source = "let x = for i = 0 downto 1 do f i done\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.For {
          iterator_token;
          direction=Syn.Cst.Downto { direction_token };
          body=Syn.Cst.Expression.Apply _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"i" ~actual:(Syn.Cst.Token.text iterator_token);
          Test.assert_equal ~expected:"downto" ~actual:(Syn.Cst.Token.text direction_token);
          Ok ()
      | _ -> Error "expected for expression");
  Test.case "cst for expressions distinguish ascending direction"
    (fun _ctx ->
      let source = "let x = for i = 0 to 1 do f i done\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.For {
          direction=Syn.Cst.To { direction_token };
          body=Syn.Cst.Expression.Apply _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"to" ~actual:(Syn.Cst.Token.text direction_token);
          Ok ()
      | _ -> Error "expected ascending for expression");
  Test.case "cst builder keeps let-binding parameters when relifting local module structure items"
    (fun _ctx ->
      let source = "let x =\n\
          \  let module M = struct\n\
          \    let read data ?timeout:_ buf = Ok 0\n\
          \    let read_vectored data iov = Ok 0\n\
          \  end in\n\
          \  ()\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let assert_nested item_syntax_nodes =
        let nested_items = Syn.CstBuilder.structure_items_from_syntax_nodes item_syntax_nodes in
        match nested_items with
        | Ok (Syn.Cst.StructureItem.LetBinding binding :: _) ->
            Test.assert_equal ~expected:3 ~actual:(List.length binding.parameters);
            (
              match nested_items with
              | Ok (_ :: Syn.Cst.StructureItem.LetBinding second_binding :: _) -> Test.assert_equal
                ~expected:2
                ~actual:(List.length second_binding.parameters)
              | _ -> ()
            );
            Ok ()
        | Ok _ ->
            Error "expected nested let binding"
        | Error _ ->
            Error "expected nested structure relift to succeed"
      in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LetModule {
          module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes; _ };
          _;

        };
        _;

      } :: _ -> assert_nested item_syntax_nodes
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Let {
          body=Syn.Cst.Expression.LetModule {
            module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes; _ };
            _;

          };
          _;

        };
        _;

      } :: _ -> assert_nested item_syntax_nodes
      | _ -> Error "expected local module in let body");
  Test.case "cst builder keeps let-binding parameters for compact local-module bindings"
    (fun _ctx ->
      let source = "let x =\n\
          \  let module M = struct\n\
          \    let read  data ?timeout:_ buf= Ok 0\n\
          \    let read_vectored  data iov= Ok 0\n\
          \  end in\n\
          \  ()\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let assert_nested item_syntax_nodes =
        let nested_items = Syn.CstBuilder.structure_items_from_syntax_nodes item_syntax_nodes in
        match nested_items with
        | Ok (Syn.Cst.StructureItem.LetBinding binding :: _) ->
            Test.assert_equal ~expected:3 ~actual:(List.length binding.parameters);
            (
              match nested_items with
              | Ok (_ :: Syn.Cst.StructureItem.LetBinding second_binding :: _) -> Test.assert_equal
                ~expected:2
                ~actual:(List.length second_binding.parameters)
              | _ -> ()
            );
            Ok ()
        | Ok _ ->
            Error "expected nested let binding"
        | Error _ ->
            Error "expected nested structure relift to succeed"
      in
      match structure_items cst with
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.LetModule {
          module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes; _ };
          _;

        };
        _;

      } :: _ -> assert_nested item_syntax_nodes
      | Syn.Cst.StructureItem.LetBinding {
        value=Syn.Cst.Expression.Let {
          body=Syn.Cst.Expression.LetModule {
            module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes; _ };
            _;

          };
          _;

        };
        _;

      } :: _ -> assert_nested item_syntax_nodes
      | _ -> Error "expected local module in let body");
  Test.case "cst builder keeps nested floating attributes in relifted module bodies"
    (fun _ctx ->
      let source = "module M = struct\n\
          \  type t = int [@@foo]\n\
          \  [@@@foo]\n\
          end [@foo]\n\n\
          module type S = sig\n\
          \  type t = int [@@foo]\n\
          \  [@@@foo]\n\
          end [@foo]\n"
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      let assert_nested_structure item_syntax_nodes =
        Test.assert_equal ~expected:2 ~actual:(List.length item_syntax_nodes);
        match Syn.CstBuilder.structure_items_from_syntax_nodes item_syntax_nodes with
        | Ok [Syn.Cst.StructureItem.TypeDeclaration _;Syn.Cst.StructureItem.Attribute attribute;] ->
            Test.assert_equal ~expected:None ~actual:attribute.payload;
            Ok ()
        | Ok _ ->
            Error "expected relifted nested structure floating attribute"
        | Error _ ->
            Error "expected nested structure relift to succeed"
      in
      let assert_nested_signature module_type =
        match Syn.CstBuilder.signature_items_of_module_type module_type with
        | Ok [Syn.Cst.SignatureItem.TypeDeclaration _;Syn.Cst.SignatureItem.Attribute attribute;] ->
            Test.assert_equal ~expected:None ~actual:attribute.payload;
            Ok ()
        | Ok _ ->
            Error "expected relifted nested signature floating attribute"
        | Error _ ->
            Error "expected nested signature relift to succeed"
      in
      match structure_items cst with
      | Syn.Cst.StructureItem.ModuleDeclaration {
        module_expression=Syn.Cst.ModuleExpression.Attribute {
          module_expression=Syn.Cst.ModuleExpression.Structure { item_syntax_nodes; _ };
          _;

        };
        _;

      } :: Syn.Cst.StructureItem.ModuleTypeDeclaration {
        module_type=Some (Syn.Cst.ModuleType.Attribute {
          module_type=(Syn.Cst.ModuleType.Signature _ as module_type);
          _;

        });
        _;

      } :: _ -> (
          match assert_nested_structure item_syntax_nodes with
          | Ok () -> assert_nested_signature module_type
          | Error _ as error -> error
        )
      | _ -> Error "expected nested module declaration and module type declaration");
  Test.case "cst builder keeps class declaration shell modifiers explicit"
    (fun _ctx ->
      let source = "class%foo [@foo] x = x\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassDeclaration {
        declaration_extension=Some extension;
        declaration_attributes=[ attribute ];
        _;

      } :: _ ->
          Test.assert_equal ~expected:(Some "foo") ~actual:(Syn.Cst.Ident.name extension.name);
          Test.assert_equal ~expected:(Some "foo") ~actual:(Syn.Cst.Ident.name attribute.name);
          Ok ()
      | _ -> Error "expected class declaration shell extension and attribute");
  Test.case "cst builder keeps class-type declaration shell modifiers explicit"
    (fun _ctx ->
      let source = "class type%foo [@foo] x = x\n" in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match structure_items cst with
      | Syn.Cst.StructureItem.ClassTypeDeclaration {
        declaration_extension=Some extension;
        declaration_attributes=[ attribute ];
        _
      } :: _ ->
          Test.assert_equal ~expected:(Some "foo") ~actual:(Syn.Cst.Ident.name extension.name);
          Test.assert_equal ~expected:(Some "foo") ~actual:(Syn.Cst.Ident.name attribute.name);
          Ok ()
      | _ -> Error "expected class-type declaration shell extension and attribute");
  Test.case "cst keeps top-level let comments standalone instead of duplicating them"
    (fun ctx ->
      assert_ml_cst_snapshot ~ctx
        {|
(* First non-trivia token *)
let first_non_trivia_token node =
  match first_non_trivia_child node with
  | Some t -> Some t
  | _ -> None
|});
  Test.case "cst parenthesized expressions preserve trivia after the opening delimiter"
    (fun _ctx ->
      let source = {|
let classify remaining available =
  if remaining <= 0 then (
    (* Body complete but buffer empty *)
    0
  ) else if available > 0 then (
    (* Partial data available, consume it and continue *)
    1
  ) else 2
|}
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | {
        value=Syn.Cst.Expression.If {
          then_branch=Syn.Cst.Expression.Parenthesized { opening_token; inner; _ };
          else_branch=Some (Syn.Cst.Expression.If {
            then_branch=Syn.Cst.Expression.Parenthesized {
              opening_token=nested_opening_token;
              inner=nested_inner;
              _
            };
            _;

          });
          _;

        };
        _;

      } :: _ -> (
          match (
            Syn.Cst.leading_trivia_before_node
              ~after:(Syn.Cst.Token.span opening_token).end_
              (Syn.Cst.Expression.syntax_node inner),
            Syn.Cst.leading_trivia_before_node
              ~after:(Syn.Cst.Token.span nested_opening_token).end_
              (Syn.Cst.Expression.syntax_node nested_inner)
          ) with
          | [ Syn.Cst.Trivia.Comment comment ], [ Syn.Cst.Trivia.Comment nested_comment ] ->
              Test.assert_equal
                ~expected:"(* Body complete but buffer empty *)"
                ~actual:(Syn.Cst.Comment.text comment);
              Test.assert_equal
                ~expected:"(* Partial data available, consume it and continue *)"
                ~actual:(Syn.Cst.Comment.text nested_comment);
              Ok ()
          | _ -> Error "expected comment trivia before parenthesized if branches"
        )
      | _ -> Error "expected parenthesized if branches to preserve opening-delimiter trivia");
  Test.case "cst keeps polymorphic variant payload local opens distinct from raw local opens"
    (fun _ctx ->
      let source = {|
let lifted =
  match suffix_class_body, declaration_class_type with
  | Some declaration_class_body, declaration_class_type ->
      Some
        (`Definition
          Cst.ClassDefinition.
            {
              syntax_node = node;
              class_body = declaration_class_body;
            })
  | None, None ->
      None
|}
      in
      let result = parse_ml source in
      let cst = expect_some result.cst ~msg:"expected CST for diagnostics-free parse"
      |> Result.expect ~msg:"expected CST for diagnostics-free parse" in
      match top_level_let_bindings cst with
      | {
        value=Syn.Cst.Expression.Match {
          cases={
            body=Syn.Cst.Expression.Constructor {
              payload=Some (Syn.Cst.Expression.Parenthesized {
                inner=Syn.Cst.Expression.PolyVariant {
                  tag_token;
                  payload=Some (Syn.Cst.Expression.LocalOpen (Syn.Cst.Delimited {
                    module_path=Syn.Cst.Ident.Qualified { prefix; name_token; _;  };
                    _;

                  }));
                  _;

                };
                _;

              });
              _;

            };
            _;

          } :: _;
          _;

        };
        _;

      } :: _ ->
          Test.assert_equal ~expected:"Definition" ~actual:(Syn.Cst.Token.text tag_token);
          Test.assert_equal ~expected:"Cst" ~actual:(ident_text prefix);
          Test.assert_equal ~expected:"ClassDefinition" ~actual:(Syn.Cst.Token.text name_token);
          Ok ()
      | _ -> Error "expected polymorphic variant payload to lift as local-open expression");
]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"syn-cst" ~tests ~args) ~args:Env.args ()
