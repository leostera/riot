open Std
module Error = Markdown_error
module Syntax_kind = Markdown_syntax_kind
module Diagnostic = Markdown_diagnostic
module Diagnostic_reporter = Markdown_diagnostic_reporter
module Fixture_db = Markdown_fixture_db

type fixture = {
  markdown: string;
  html: string;
  example: int option;
  section: string option;
}

let cast_fixture = fun (fixture: Markdown_fixture_db.fixture) ->
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
  blocks: Markdown_parser.block_node list;
}

let parse = fun source ->
  let parsed = Markdown_parser.parse source in
  let blocks = Markdown_lower.lower ~flavor:Markdown_parser.Markdown parsed.tree in
  {
    root = parsed.tree;
    source = parsed.source;
    diagnostics = parsed.diagnostics;
    blocks
  }

let parse_gfm = fun source ->
  let parsed = Markdown_parser.parse ~flavor:Markdown_parser.Gfm source in
  let blocks = Markdown_lower.lower ~flavor:Markdown_parser.Gfm parsed.tree in
  {
    root = parsed.tree;
    source = parsed.source;
    diagnostics = parsed.diagnostics;
    blocks
  }

let all_spec_fixtures = fun () ->
  List.map cast_fixture (Fixture_db.all_spec_fixtures ())

let to_html = fun parse_result ->
  Markdown_renderer.render parse_result.blocks

let compile = fun source ->
  let parse_result = parse source in
  to_html parse_result

let compile_gfm = fun source ->
  let parse_result = parse_gfm source in
  to_html parse_result
