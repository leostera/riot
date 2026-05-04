open Std
open Std.Data
open Std.Collections
open Syn

module Iterator = Iter.Iterator

let append_path_suffix = fun path suffix ->
  Path.to_string path ^ suffix
  |> Path.from_string
  |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

let fixture_root = Path.v "packages/syn/tests/fixtures"

let lossless_snapshot_path = fun path -> append_path_suffix path ".expected_lossless.json"

let parse_skips = [ "ocaml_docstrings.ml"; "ocaml_extensions.ml"; "ocaml_shortcut_ext_attr.ml"; ]

let should_skip_parse_fixture = fun path ->
  let basename = Path.basename path in
  List.any parse_skips ~fn:(fun name -> String.equal basename name)

let load_modified_fixture_paths = fun () ->
  let cwd =
    Env.current_dir ()
    |> Result.expect ~msg:"failed to get cwd for fixture filter"
  in
  let args = [ "diff"; "--name-only"; "--"; Path.to_string fixture_root; ] in
  match Command.make "git" ~args
  |> Command.output with
  | Error _ -> HashSet.create ()
  | Ok { status; stdout; _ } when status = 0 ->
      let modified = HashSet.create () in
      let lines =
        stdout
        |> String.split_on_char '\n'
        |> List.map ~fn:String.trim
      in
      let rec loop = fun __tmp1 ->
        match __tmp1 with
        | [] -> modified
        | "" :: rest -> loop rest
        | relpath :: rest ->
            let () =
              match Path.from_string relpath with
              | Ok relpath ->
                  let _ = HashSet.insert modified ~value:(Path.join cwd relpath) in
                  ()
              | Error _ -> ()
            in
            loop rest
      in
      loop lines
  | Ok _ -> HashSet.create ()

let is_locally_modified_fixture = fun modified_fixture_paths path ->
  HashSet.contains
    modified_fixture_paths
    ~value:path

let has_lossless_snapshot = fun modified_fixture_paths path ->
  match Path.extension path with
  | Some ".ml"
  | Some ".mli" ->
      if
        should_skip_parse_fixture path || is_locally_modified_fixture modified_fixture_paths path
      then
        Test.FixtureRunner.Skip
      else
        let snapshot_path = lossless_snapshot_path path in
        let exists =
          Fs.exists snapshot_path
          |> Result.unwrap_or ~default:false
        in
        if exists then
          Test.FixtureRunner.Keep
        else
          Test.FixtureRunner.Skip
  | _ -> Test.FixtureRunner.Skip

let source_slice = fun source ->
  match IO.IoVec.IoSlice.from_string source with
  | Ok slice -> slice
  | Error error -> panic ("failed to create source slice: " ^ IO.IoVec.error_message error)

let diagnostics_to_string = fun diagnostics ->
  let items = ref [] in
  Vector.iter diagnostics
  |> Iterator.for_each ~fn:(fun diagnostic -> items := Diagnostic.to_string diagnostic :: !items);
  List.reverse !items
  |> String.concat "\n"

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let source =
    Fs.read ctx.fixture_path
    |> Result.expect ~msg:"Failed to read fixture"
  in
  let source = source_slice source in
  let parse_result = Syn.parse ~filename:ctx.fixture_path source in
  if Vector.length parse_result.Parser.diagnostics > 0 then
    Error ("unexpected parse diagnostics:\n" ^ diagnostics_to_string parse_result.Parser.diagnostics)
  else
    let root = SyntaxTree.root parse_result.Parser.tree in
    let source_length = IO.IoVec.IoSlice.length source in
    if root.SyntaxTree.full_width = source_length then
      Ok ()
    else
      Error ("parse root width mismatch: expected "
      ^ Int.to_string source_length
      ^ " bytes, got "
      ^ Int.to_string root.SyntaxTree.full_width)

let test_tagged_quoted_string_token = fun _ctx ->
  let source = "let explanation = {explain|hello|explain}\n" in
  let slice = source_slice source in
  let parse_result = Syn.parse ~filename:(Path.v "tagged_quoted_string.ml") slice in
  if Vector.length parse_result.Parser.diagnostics > 0 then
    Error ("unexpected parse diagnostics:\n" ^ diagnostics_to_string parse_result.Parser.diagnostics)
  else
    let root = Ast.root parse_result.Parser.tree in
    let string_token = ref None in
    Ast.Node.fold_token
      root
      ~init:()
      ~fn:(fun token () ->
        if SyntaxKind.(Ast.Token.kind token = STRING) then
          string_token := Some token;
        Ast.Continue ());
  match !string_token with
  | Some token ->
      Test.assert_equal ~expected:"{explain|hello|explain}" ~actual:(Ast.Token.text token);
      Ok ()
  | None -> Error "expected tagged quoted string token"

let test_fuzz_parse_implementation = fun _ctx source ->
  let source = source_slice source in
  let parse_result = Syn.parse ~filename:(Path.v "fuzz.ml") source in
  let root = SyntaxTree.root parse_result.Parser.tree in
  let _ast_root = Ast.root parse_result.Parser.tree in
  let source_length = IO.IoVec.IoSlice.length source in
  if root.SyntaxTree.full_width = source_length then
    Ok ()
  else
    Error ("parse root width mismatch: expected "
    ^ Int.to_string source_length
    ^ " bytes, got "
    ^ Int.to_string root.SyntaxTree.full_width)

let ocaml_source_dictionary = [
  "let";
  "let rec";
  "and";
  "in";
  "fun";
  "function";
  "match";
  "with";
  "when";
  "if";
  "then";
  "else";
  "type";
  "module";
  "struct";
  "sig";
  "end";
  "open";
  "include";
  "val";
  "external";
  "exception";
  "try";
  "raise";
  "while";
  "for";
  "to";
  "downto";
  "do";
  "done";
  "begin";
  "object";
  "method";
  "class";
  "inherit";
  "private";
  "mutable";
  "constraint";
  "->";
  "=>";
  "|";
  "::";
  ":";
  ":>";
  ":=";
  "=";
  "==";
  "<>";
  "+";
  "-";
  "*";
  "/";
  "&&";
  "||";
  "(";
  ")";
  "[";
  "]";
  "[|";
  "|]";
  "{";
  "}";
  "[<";
  "[>";
  "`";
  "'";
  "\"";
  ";;";
  ".";
  ",";
  ";";
  "_";
  "?";
  "~";
  "@";
  "@@";
  "@@@";
  "[%";
  "[%%";
  "[@";
  "[@@";
  "[@@@";
  "(*";
  "*)";
  "(**";
  "let x = 0\n";
  "type t = A | B\n";
  "module M = struct let y = fun x -> x end\n";
  "match x with | Some y -> y | None -> 0\n";
]

let ocaml_source_mutator =
  Test.Fuzz.Mutator.(text
  |> with_dictionary ocaml_source_dictionary
  |> with_max_len 8_192)

let module_decl_head_eof_recovery_seed =
  {|let xunc}or applications *)

module[%% N = F (X) (Y) (Z)

(* Nested application *)

module O = F (
(* Functor ereturn type *)

module P (X : S): T = struct
  ty
end

(* Include with functor application *)

include M (X)

(* Include with simple module *)

include M|}

let run_fixture = fun ctx -> test_fixture ~ctx

let main ~args =
  let modified_fixture_paths = load_modified_fixture_paths () in
  let fixture_tests =
    Test.FixtureRunner.cases
      ()
      ~dir:fixture_root
      ~filter:(has_lossless_snapshot modified_fixture_paths)
      ~snapshot_path:(fun path -> Some (lossless_snapshot_path path))
      ~run:run_fixture
  in
  let tagged_quoted_string_tests =
    Test.[ case "tagged_quoted_string_token" test_tagged_quoted_string_token ]
  in
  let fuzz_tests =
    Test.[
      fuzz
        "parser recovers arbitrary implementation input"
        ~seeds:[
          "";
          "let x = 0\n";
          "type t = A | B\n";
          "module M = struct let y = fun x -> x end\n";
          "match x with | Some y -> y | None -> 0\n";
          module_decl_head_eof_recovery_seed;
        ]
        ~corpus:(Test.Fuzz.Corpus.dir fixture_root ~extensions:[ ".ml"; ".mli"; ])
        ~mutator:ocaml_source_mutator
        test_fuzz_parse_implementation;
    ]
  in
  let tests = (tagged_quoted_string_tests @ fuzz_tests) @ fixture_tests in
  Test.Cli.main ~name:"syn-fixtures" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
