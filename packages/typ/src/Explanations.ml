open Std

type t = {
  diagnostic_id: string;
  name: string;
  summary: string;
  details: string list;
}

let entry = fun ~diagnostic_id ~name ~summary ~details -> { diagnostic_id; name; summary; details }

let all_entries = [
  entry
    ~diagnostic_id:"TYP1001"
    ~name:"unsupported-syntax"
    ~summary:"The parser produced syntax that the current prototype lowers only through recovery nodes."
    ~details:[
      "This is the catch-all diagnostic for syntax outside the currently implemented functional subset.";
      "The checker keeps going by lowering the syntax into placeholder items, recovery patterns, or hole expressions.";
      "As the prototype grows, specific unsupported cases can split out into narrower diagnostics without changing the general recovery model.";
    ];
  entry
    ~diagnostic_id:"TYP1004"
    ~name:"ignored-pattern-type-constraint"
    ~summary:"A type constraint attached to a pattern was parsed, but the prototype does not enforce it yet."
    ~details:[
      "Lowering keeps the pattern shape but drops the annotation from semantic checking.";
      "This usually means the surrounding code is still checked, but the annotation itself does not constrain inference yet.";
    ];
  entry
    ~diagnostic_id:"TYP1005"
    ~name:"parameter-lowered-as-positional"
    ~summary:"A parameter form with richer OCaml semantics was lowered as an ordinary positional binder."
    ~details:[
      "This currently covers labeled, optional, and locally abstract parameter forms that are outside the prototype subset.";
      "The function body may still be analyzed, but parameter semantics are only approximated.";
    ];
  entry
    ~diagnostic_id:"TYP1007"
    ~name:"application-argument-lowered-as-positional"
    ~summary:"A labeled or optional application argument was lowered as an ordinary positional argument."
    ~details:[
      "This keeps more real OCaml call sites analyzable while the prototype still lacks first-class label semantics.";
      "The call can still be inferred, but labels do not yet participate in argument matching or reordering.";
    ];
  entry
    ~diagnostic_id:"TYP1008"
    ~name:"ignored-type-ascription"
    ~summary:"A type ascription was parsed, but the prototype does not check it against the inferred type yet."
    ~details:[
      "The underlying expression is still lowered and inferred.";
      "This diagnostic means the annotation is currently documentation only from the typechecker's point of view.";
    ];
  entry
    ~diagnostic_id:"TYP1009"
    ~name:"ignored-polymorphic-annotation"
    ~summary:"An explicit polymorphic annotation was parsed, but the prototype does not implement it yet."
    ~details:[
      "This mostly affects advanced let-polymorphism cases.";
      "The surrounding code may still typecheck, but explicit rank-like constraints are not enforced.";
    ];
  entry
    ~diagnostic_id:"TYP1010"
    ~name:"unsupported-interface-file"
    ~summary:".mli interface files are recognized, but the prototype does not lower them into a semantic interface model yet."
    ~details:[
      "This is a prototype boundary, not a parser failure.";
      "The long-term architecture supports interface-aware exports, but that lane is not implemented in this first cut.";
    ];
  entry
    ~diagnostic_id:"TYP1011"
    ~name:"cst-builder-error"
    ~summary:"Parsing succeeded far enough to produce a parse result, but CST construction failed before semantic lowering."
    ~details:[
      "The diagnostic carries the raw structured Syn.CstBuilder.error payload.";
      "This usually points to a gap between parse recovery and the lossless CST builder, rather than an inference failure.";
    ];
  entry
    ~diagnostic_id:"TYP2001"
    ~name:"unbound-name"
    ~summary:"A value name was referenced without any visible binding in the current typing environment."
    ~details:[
      "The prototype recovers by introducing a hole type so later expressions can still be analyzed.";
      "This makes editor use-cases and snapshot tests more resilient on incomplete code.";
    ];
  entry
    ~diagnostic_id:"TYP2002"
    ~name:"type-mismatch"
    ~summary:"Two types were required to unify, but the prototype found incompatible structure instead."
    ~details:[
      "Structured mismatch payloads distinguish ordinary expected/actual mismatches, tuple arity mismatches, and occurs-check failures.";
      "The checker reports the mismatch and keeps going when it can so later diagnostics and partial types are still available.";
    ];
  entry
    ~diagnostic_id:"TYP2005"
    ~name:"application-label-mismatch"
    ~summary:"A function application could not match the remaining source arguments against the expected labeled parameter."
    ~details:[
      "This diagnostic is specific to call-site label matching, including omitted optional parameters and labeled-argument reordering.";
      "The structured payload records the expected parameter label together with the labels that were still present at the call site.";
    ];
  entry
    ~diagnostic_id:"TYP2006"
    ~name:"record-resolution-error"
    ~summary:"A record operation could not resolve one nominal record owner for the labels used at the source site."
    ~details:[
      "This diagnostic covers record construction, updates, field access, and record patterns.";
      "The structured payload records both the operation kind and whether the failure came from unknown labels, ambiguity, missing fields, or incompatible owners.";
    ];
  entry
    ~diagnostic_id:"TYP2007"
    ~name:"or-pattern-bindings-mismatch"
    ~summary:"An or-pattern used alternatives that do not bind the same set of value names."
    ~details:[
      "Each alternative is checked against the same scrutinee type, but the body can only see names that are bound in every branch.";
      "The structured payload records the normalized names from the first alternative and the mismatching alternative.";
    ];
  entry
    ~diagnostic_id:"TYP2004"
    ~name:"recursive-group-requires-simple-variable-binders"
    ~summary:"The prototype only supports let-rec groups whose binders are simple variables."
    ~details:[
      "Recursive destructuring and richer binder forms need extra semantic handling that is not in place yet.";
      "The group is still surfaced with a structured diagnostic rather than silently rejected.";
    ];
  entry
    ~diagnostic_id:"TYP2010"
    ~name:"unsupported-semantic-expression"
    ~summary:"Lowering produced a semantic expression shape that the inferencer does not handle yet."
    ~details:[
      "This is an internal prototype boundary between semantic lowering and inference.";
      "The diagnostic is valuable because it tells us which semantic forms still need inference rules.";
    ];
]

let all = fun () -> all_entries

let normalize_id = fun diagnostic_id -> String.uppercase_ascii (String.trim diagnostic_id)

let explain = fun diagnostic_id ->
  let normalized = normalize_id diagnostic_id in
  List.find_opt
    (fun explanation ->
      String.equal (normalize_id explanation.diagnostic_id) normalized)
    all_entries

let to_json = fun explanation ->
  Data.Json.Object [
    ("diagnostic_id", Data.Json.String explanation.diagnostic_id);
    ("name", Data.Json.String explanation.name);
    ("summary", Data.Json.String explanation.summary);
    (
      "details",
      Data.Json.Array (List.map (fun detail -> Data.Json.String detail) explanation.details)
    );
  ]

let format = fun explanation ->
  let detail_lines = explanation.details |> List.map (fun detail -> "- " ^ detail) |> String.concat "\n" in
  if String.equal detail_lines "" then
    explanation.diagnostic_id ^ " " ^ explanation.name ^ "\n\n" ^ explanation.summary
  else
    explanation.diagnostic_id ^ " " ^ explanation.name ^ "\n\n" ^ explanation.summary ^ "\n\n" ^ detail_lines
