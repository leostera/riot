# krasny AGENTS

`krasny` is Riot's owned OCaml formatter.

## Rules

1. Keep `krasny` as the single rendering pipeline for formatted OCaml output; do not grow a separate fix-only printer beside it.
2. Format only from a successful CST lift; do not pretty-print broken files or add a token-replay fallback for them.
3. Start with deterministic valid OCaml output before chasing aesthetic heuristics.
4. During the structural-formatting push, prefer “supported from CST” over “beautiful.” If a layout policy is ugly but fully structural, ship it and come back later.
5. Keep the public surface writer-oriented and `Std.IO`-friendly.
6. Treat comments and trivia as part of the formatter design, not as a post-processing hack.
7. Keep workspace formatting runners streaming-friendly: file discovery and per-file check results should be able to flow incrementally instead of requiring a full precollected file list.
8. Keep the active fixture manifest intentionally curated; prefer one category corpus per supported syntax band and add individual edge-case fixtures only after real code exposes a regression. Use `tests/FIXTURES.md` and `tests/fixture_audit.py` before adding overlapping cases.
9. When a copied real-file regression exposes a missing formatter behavior, add the smallest representative example back into the relevant `0X00` category corpus so the feature is isolated before or alongside the fix.
10. `--verify` is a normalized semantic-hash safety preflight, not another formatting-state check. Report files that would reformat safely separately from files that are unsafe to format.
11. Keep lowering context explicit and per-invocation. Do not reintroduce global mutable source/render state in `Lower`; each format run must be multicore-safe on its own.
12. Preserve standalone top-level docstrings and section headers. Treat odoc section docs and markdown-style `# ...` doc blocks as section boundaries, not declaration-owned docs to be dropped or reassigned.
13. Render variant constructor docs from `Syn.Cst.VariantConstructor.owned_trivia.leading`; do not pull docstrings backward from later gaps or EOF once `syn` has assigned leading-only constructor ownership.
13. Render record field docs from `Syn.Cst.RecordField.owned_trivia.leading`, and preserve terminal `}`-owned comment/doc trivia inside the record body instead of stealing them for the last field.
14. Use `Syn.Cst.Docstring.kind` for normal doc-vs-section decisions when lowering CST-owned docstrings; do not resniff raw docstring text once `syn` has made that distinction explicit.
15. Render top-level source files from the ordered `SourceFile.items` stream plus each item's `owned_trivia`; do not reparse raw source gaps there to rediscover standalone comments/docstrings or declaration docs.
16. Render nested `sig ... end` and `struct ... end` bodies from `Syn.CstBuilder.signature_items_of_module_type` and `Syn.CstBuilder.structure_items_of_module_expression` directly; treat wrong-shape helper calls as explicit `syn` errors, not as `Ok None`, and do not keep nested-only trailing-comment or source-gap recovery once those helper streams are token-order-complete.
17. Keep `render_structure_top_level_items` and `render_signature_top_level_items` layout-only. Top-level phrase separators should come from `Syn.Cst.SourceFile.phrase_separator_tokens`, not raw source-gap parsing, source-preserved expression runs, or ad hoc direct-token scans; ordered item streams plus `owned_trivia` already decide ownership.
18. Trust the incoming ordered structure/signature item streams. Do not re-sort top-level items by reconstructed syntax-node spans before joining them; if join boundaries still need extra facts, add those facts to the CST instead of re-deriving order in `krasny`.
19. Keep signature top-level joins span-free when there is no signature phrase-separator contract to honor. Do not carry dead per-item span reconstruction through `render_signature_top_level_items` once the ordered CST stream already determines join order and trailing suffixes are always `None`.
20. Read top-level structure phrase separators from per-item `trailing_phrase_separator_tokens` on the source-file CST root. Do not recompute `;;` attachment from item start/end spans in `lower.ml` once `syn` has already assigned those token groups to each ordered item boundary.
21. Render grouped `type ... and ...` members from each member's `Syn.Cst.TypeDeclaration.owned_trivia`; do not reparse between-member source gaps once `syn` has attached `and`-token leading trivia to the following member.
22. Delete dead formatter archaeology as soon as a CST boundary fact replaces it. Do not keep unused span-based helpers in `lower.ml` once top-level/signature joins no longer consult them.
23. Keep adjacent standalone ordinary docstrings visually separate in top-level joins; do not compact them into a single tight run just because they are both trivia items.
24. Render record bodies and inline record constructor arguments from `Syn.CstBuilder.record_field_items_of_fields`; do not inspect raw record syntax children or closing-token trivia to rediscover terminal `}`-owned comments/docstrings.
25. Do not add new source-preserving reconstruction paths in `Lower`. Unsupported shapes should fail formatting until `syn` exposes enough structure for a purely structural lowering.
26. The old top-level parameterized-`let` phrase-boundary preservation path is gone. Do not reintroduce source-preserved expression runs; if another phrase-boundary case needs help, model it structurally.
27. Treat `Syn.Ceibo.Red.SyntaxNode.tokens` and `direct_tokens` as real-token-only streams in `krasny`. Do not keep dead filters for impossible `WHITESPACE` / `COMMENT` / `DOCSTRING` token kinds after the token-trivia migration.
28. Render parameters from `Syn.Cst.Parameter` structure. Do not reintroduce `Source.source_of_parameter` or other raw parameter-text reconstruction once optional defaults and typed binding patterns are preserved structurally.
29. Render signature `val` names from CST token structure. Do not reparse declaration source to recover operator spelling or parentheses once `Syn.Cst.value_declaration.name_token` is available.
30. Render inherited polymorphic-variant rows from `Syn.Cst.RowField.Inherit.type_` directly. Do not scan raw token text to reconstruct row paths that the CST already models as a core type.
31. Distinguish `let f = fun ...` from `let f x = ...` from `Syn.Cst.let_binding` shape, not from scanning tokens around `=` in the original source.
32. `Krasny.format` output policy is explicit: non-empty formatted output ends with a final newline, independent of whether the input source had one.
33. Render trivia around `if ... then ... else` from the existing CST tokens plus the following branch node through `Syn.Cst.leading_trivia_after` and `leading_trivia_before_node`; do not reparse raw source spans between `then`, `else`, and branch bodies.
34. Render trivia after `=` and `in` in ordinary `let ... in` expressions from the existing CST tokens plus the RHS/body node. Do not reparse raw source spans for those boundaries once the CST already exposes `equals_token`, `in_token`, and the branch node.
35. Render sequence-expression trivia from `Syn.Cst.sequence_expression.separator_tokens` plus neighboring expression nodes through `Syn.Cst.leading_trivia_after_token_before_node`; do not require duplicated per-step trivia fields in the CST.
36. Render binding-operator clause and body trivia from `Syn.Cst.binding_operator_binding.equals_token` and `Syn.Cst.let_operator_expression.in_token`; do not reconstruct `let*` / `and*` / `let+` trivia from raw spans once `syn` exposes the tokens explicitly.
37. Render `let` / `and` keyword-leading trivia from the original CST tokens. Prefer `Syn.Cst.Token.leading_trivia` plus `trivia_of_syntax_trivia` over any parent-node cache when comments/docstrings live before `and`.
38. Singleton list-pattern spacing is explicit formatter policy, not source preservation. Do not sniff original `"[ value ]"` spacing from raw node text to decide pattern edge spaces.
39. Render `if` conditions through ordinary expression lowering. Do not scan token text for `&&` / `||` or token-leading comment trivia to decide boolean-condition layout.
40. Local binding layout should follow rendered RHS structure, not raw source newlines inside the RHS syntax node. Do not keep multiline `let ... =` layouts just because the original subtree text contained embedded newlines.
40. Render first-class module core types and type definitions from `Syn.Cst.package_type`. Do not reconstruct `(module ...)` text from raw module-type syntax-node text or widen package-only positions back to general module-type rendering.
41. Render floating and expression-attached attribute payloads directly from `Syn.Cst.attribute.payload`. Do not relift payload bodies as OCaml while the payload contract is opaque-only.
42. Render extension payloads from the CST payload contract exactly as provided. `Syn.Cst.Payload.Opaque` should print by replaying the CST-owned token slice without OCaml relift or source sniffing.
44. Join `owned_trivia` with explicit formatter separators. Do not recover comment/docstring spacing from raw source gaps or thread source text through nested item renderers just to preserve whitespace between trivia items.
45. Render attribute and extension payloads from the CST payload contract only. When a payload is `Payload.Opaque`, print that token slice exactly; do not parse, relift, or normalize it in `krasny`.
46. When relifted nested item streams expose a floating attribute immediately after a `type` declaration, keep that join tight on the next line. Do not open a blank paragraph there or re-split the nested body to recover the attribute twice.
47. Render polymorphic-variant expression and pattern heads from the explicit `tag_token` fields in the CST. Do not replay raw direct-token text for the leading backtick tag once `syn` exposes it structurally.
48. Render `CoreType.Poly`'s optional leading `type` prefix from `type_keyword_token`. Do not scan raw poly-type tokens to rediscover whether a locally abstract type was written with `type`.
49. Render `CoreType.Var` from `sigil_token` plus `name_token`. Do not drop quoted `'a` sigils by printing only the bare name token, and do not reintroduce raw token replay to recover them.
50. When `render_local_binding` synthesizes an outer `: type ...` annotation from parameter types, drop duplicate inner type annotations from the unsugared `fun` parameter list too. Normalize `~(fn : a -> b)` to `~fn` once the outer arrow already carries `fn:(a -> b)`.
51. Render index expressions from `Syn.Cst.index_expression.opening_tokens` plus `closing_token`. Do not reconstruct `.[ ]`, `.( )`, or extended index-operator punctuation from raw direct-token text once `syn` exposes the delimiter tokens explicitly.
52. Render signed int/float literals from `sign_token` when the CST constant carries one. Do not rescan literal-node direct tokens for leading `+` / `-`, especially for signed literal patterns.
53. Render operator expressions, operator patterns, and infix/prefix expression operator docs from CST-carried operator tokens directly. Do not concatenate raw token text back into an operator string when the CST already preserves the exact token sequence.
54. Render structure/signature `open!` statements from `bang_token` when present. Do not hardcode `!` in the formatter when the CST already carries the token.
55. Render `CoreType.Alias` binders from `sigil_token` plus `name_token`. Do not synthesize a leading apostrophe for `... as 'a` by inspecting token text once `syn` exposes the alias-binder spelling explicitly.
56. Render named and optional parameter sugar from `binding_name_matches_label` on `Syn.Cst.Parameter`. Do not compare label token text with identifier binding patterns to choose `~label` vs `~label:pattern` once `syn` has made that equivalence explicit.
57. Let record type and type-definition fields break after `:` from rendered type structure and document width only. Do not force multiline field layout from arbitrary field-name length thresholds once the structural renderer already has the field type.
58. Render fixed-operator layout, operator-like name decisions, and same-operator infix-chain grouping from `Syn.Cst.Token` helpers such as `fixed_operator`, `is_operator_like_name`, and `same_text`. Do not compare raw token text for `|>`, `&&`, `||`, `-`, `~-`, keyword operators, or infix-chain grouping once `syn` exposes that classification explicitly.
59. Keep the local-binding infix-chain inline cutoff explicit formatter policy. If `let ... = a + b + ...` keeps using a term-count threshold, name that cutoff and cover its boundary in formatter tests instead of hiding a raw magic number inside `expression_is_simple_after_equals`.
60. Keep local-binding header/body placement policy explicit too. Decisions such as whether typed parameters stay in the binding header or whether a recursive local body forces multiline layout should live in named helpers, not anonymous booleans embedded inside `render_local_binding`.
61. Keep positional function-parameter parens idempotent. If a CST parameter pattern already renders with the necessary outer parentheses, do not wrap it again in `render_positional_parameter_pattern`; constructor-pattern parameters such as `fun (Conn conn) -> ...` must stay stable across repeated formatting.
62. Render typed first-class-module patterns from `Syn.Cst.Pattern.FirstClassModule.package_type` through the structural package-type renderer. Do not keep `(module M : S)` on an explicit unsupported path when the CST already carries the binder and package-type shape.
63. Parenthesize attributed non-atomic expressions before rendering postfix `[@attr]`. Constructor payloads, ordinary applications, and infix expressions such as `Some 0 [@inline always]`, `f x [@inline always]`, and `a + b [@inline always]` must keep their full attributed expression and remain idempotent across repeated formatting.
64. Render polymorphic-variant inherit patterns by printing `#` separately from `Syn.Cst.Pattern.PolyVariantInherit.type_path`. That path should already exclude the sigil, so `#color` and `#M.color` must not collapse to `##` or duplicate the sigil during formatting.
65. Render plain `object ... end` expressions structurally from `Syn.Cst.Expression.Object`. Support empty objects, self patterns, `method`/`val`/`inherit`/`initializer`/extension members, and postfix member attributes without replaying source text.
66. Render plain expression, module-expression, module-type, floating-item, core-type, first-class-module-type, pattern extensions, and attributes from their shared CST shells without formatter-side reconstruction. Payloads may stay opaque; the shells still must lower structurally.
67. When body/branch boundary trivia still comes from token-attached leading trivia, read it through `Syn.Cst.leading_trivia_after`, `leading_trivia_before_node`, and `leading_trivia_after_token_before_node`. Do not walk `Ceibo.Red.SyntaxNode.tokens` in `lower.ml` just to rediscover the first token and its attached trivia.
68. When a formatter boundary still needs a node’s real-token span, read it through `Syn.Cst.token_body_span`. Do not keep local `SyntaxNode.tokens` scans in `lower.ml` just to recover start/end offsets of the first and last nontrivia tokens.
69. Do not reach back into `RecordField.syntax_node` parents from `lower.ml` just to choose inline-vs-multiline record-constructor layout. That decision should come from field structure, owned trivia, and explicit formatter policy only.
70. Keep unsupported-shape diagnostics behind `Syn.Cst` helpers too. `lower.ml` should not read `Ceibo.Red.SyntaxNode.kind` directly just to annotate an error path with the current syntax kind, and it should keep that kind typed until final error rendering.
71. Prefer original CST-carried tokens and child nodes over duplicated boundary-trivia fields. Use `leading_trivia_after`, `leading_trivia_before_node`, and `leading_trivia_after_token_before_node` on those structural tokens/nodes before asking `syn` for any new dedicated boundary field.
72. Render class bodies and class-type signatures from `Syn.CstBuilder.class_field_items_of_fields` and `class_type_field_items_of_fields`; do not reconstruct `object ... end` member ownership for class declarations inside `krasny`.
73. Render `class%foo` / `class type%foo` shortcut declaration shells from explicit CST declaration modifier fields. Do not rescan declaration syntax to rediscover shell extensions or attributes once `syn` exposes them.
74. When `syn` tightens a declaration node into explicit valid-shape variants, follow that split directly in `krasny`. Do not reconstruct old optional-field products in the formatter just to preserve previous call sites.
75. Follow class/object member validity splits directly too. Render concrete member definitions from mandatory bodies/values, virtual member definitions from mandatory types, and initializers from mandatory bodies; do not preserve formatter branches for old `option`-shaped member states once `syn` removes them.
76. Render expression `: t`, `:> t`, and `: t :> u` forms from `Syn.Cst.Expression.TypeAscription` and its explicit inner variants. Do not reconstruct the old split between typed expressions and coercions in `krasny`.
77. Render nested `sig ... end` and `struct ... end` bodies from the normalized nested item streams exactly as lifted by `syn`; do not let nested relift paths skip inter-item comments/docstrings that file-level rendering would preserve.
78. Render comments/docstrings between `->` and a match-case body from `case.arrow_token` plus `case.body` through `Syn.Cst.leading_trivia_before_node`. Do not recover that boundary from raw token walks or let it disappear when the body starts with `let`, `match`, or another nested expression.
79. Render local opens from the original token-backed CST variants. Use `Syn.Cst.LetOpen` tokens for `let open M in ...` and `Syn.Cst.Delimited` tokens for `M.(...)` / `M.[...]` / `M.{...}`; do not synthesize `.` or delimiter punctuation, and do not expect any type-side local-open variants.
80. Render declaration separators from the original CST tokens when trivia can attach there. Use `ValueDeclaration.colon_token`, `ExternalDeclaration.colon_token`, `ExternalDeclaration.equals_token`, and `ModuleTypeDeclaration.equals_token` instead of synthesized `:` / `=` docs so separator-owned comments survive formatting.
81. Render module declaration heads from the original CST shell tokens too. Use `ModuleSignature.keyword_token` / `rec_token` and `ModuleStructure.keyword_token` / `rec_token` so comments attached to `module`, `and`, or `rec` survive formatting instead of being lost behind synthesized keywords.
82. Render object/class member modifiers from CST-carried tokens too. Preserve comments on `!`, `private`, `mutable`, and `virtual` by lowering `modifier_tokens` and virtual-definition tokens directly instead of rebuilding those shells from booleans.
83. Keep new fixture migrations on the shared `Std.Test.FixtureRunner` + `Std.Test.Snapshot` path. When a `krasny` fixture family already has adjacent approved outputs, point `snapshot_path` at those `.expected` files instead of inventing a second storage convention.

## Validate

`timeout 30 riot build krasny`
`timeout 30 riot test krasny:format_tests`
`timeout 180 riot test krasny:fixture_tests`

Use the legacy Python helper only for workspace-wide syntax-hash verification:
`timeout 900 python3 packages/krasny/tests/test_runner.py --verify-workspace`

Audit fixture taxonomy and duplicate pressure when curating the corpus:
`python3 packages/krasny/tests/fixture_audit.py`
