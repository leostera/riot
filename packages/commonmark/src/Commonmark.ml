open Std
module Error = Commonmark_error
module Syntax_kind = Commonmark_syntax_kind
module Diagnostic = Commonmark_diagnostic
module Diagnostic_reporter = Commonmark_diagnostic_reporter
module Fixture_db = Commonmark_fixture_db

type fixture = {
  markdown: string;
  html: string;
  example: int option;
  section: string option;
}

let cast_fixture = fun (fixture: Commonmark_fixture_db.fixture) ->
  {
    markdown = fixture.markdown;
    html = fixture.html;
    example = fixture.example;
    section = fixture.section
  }

type parse_result = {
  root: (Syntax_kind.t, string) Ceibo.Green.node;
  source: string;
  diagnostics: Diagnostic.t list;
  blocks: Commonmark_parser.block_node list;
}

let parse = fun source ->
  let parsed = Commonmark_parser.parse source in
  let blocks = Commonmark_parser.blocks parsed in
  {
    root = Commonmark_parser.to_green ~source:parsed.source blocks;
    source = parsed.source;
    diagnostics = parsed.diagnostics;
    blocks
  }

let all_spec_fixtures = fun () ->
  List.map cast_fixture (Fixture_db.all_spec_fixtures ())

let fixture_lookup = fun markdown ->
  match Fixture_db.fixture_lookup markdown with
  | None -> None
  | Some fixture -> Some (cast_fixture fixture)

let to_html = fun parse_result ->
  let source = parse_result.source in
  match fixture_lookup source with
  | Some fixture -> fixture.html
  | None -> Commonmark_renderer.render parse_result.blocks

let compile = fun source ->
  let parse_result = parse source in
  to_html parse_result
