# Bidirectional Typing Contract

This document records the algorithmic contract for `Typ`'s supported expression
fragment. It is deliberately smaller than OCaml's full typing language. Every
syntax form admitted by the checker must either follow one of these rules or
produce a source-located unsupported diagnostic; recovery metavariables are
never evidence that a file passed typechecking.

## Judgments

`Γ ⊢ e ⇒ τ` means expression `e` synthesizes type `τ` in environment `Γ`.
`Γ ⊢ e ⇐ τ` means expression `e` checks against an expected type `τ`.
`Γ` maps value identifiers to rank-1 schemes; each scheme instantiation gets
fresh metavariables. Type constructors, constructors, labels, and module paths
are separate namespaces with explicit lookup and identity rules.

The algorithm is syntax-directed:

| Form | Direction | Rule shape |
| --- | --- | --- |
| Literal | synthesize | Its primitive type is fixed. |
| Identifier / constructor reference | synthesize | Resolve in its namespace, then instantiate its scheme. Missing names are errors. |
| Application | synthesize | Synthesize the callee. For each known arrow, check the corresponding argument against its parameter; only synthesize an argument when the callee type is still an unsolved metavariable. |
| Function abstraction | check when an arrow is expected; otherwise synthesize | Check/bind each parameter at the expected arrow domain and check the body at the expected result. Synthesis introduces fresh domains and checks the body after those binders enter scope. |
| Tuple, list, immutable record, constructor payload | check when the expected shape/type is known; otherwise synthesize | Check components against the corresponding expected types. A mismatched shape reports at the introducing expression. |
| `if`, `match`, `try` | check branches against a common expected result when supplied; otherwise synthesize one branch and check/unify the rest | Conditions and guards check against `bool`; pattern bindings are scoped to one arm. |
| Sequence / local `let` | check the final/body expression when an expected result is supplied | Earlier sequence operands check against `unit`; local bindings use the let rule below. |
| Annotation | bridge | Check the expression against the annotation; then require the annotation to agree with any outer expected type. |
| Assignment, loops, assertions, arrays, field updates | synthesize or check using their declared primitive/mutable type rules | Mutable/expansive bindings are not unrestrictedly generalized. |

These directions are guidance, not a license to invent a fresh type for a form
whose typing rule is missing. In particular, a fallback that synthesizes an
introduction form and unifies the result afterward is permitted only as a
recovery path that also emits a diagnostic, not as successful checking.

## Rank-1 HM and mutation

A let-bound scheme quantifies only metavariables whose creation level is
strictly deeper than the environment level at the binding. Unification must
perform an occurs check and lower every reachable metavariable to the older
variable's level before linking it. Instantiation replaces each quantified
variable once per use and preserves sharing within that one instance.

Generalization is permitted only for nonexpansive values. The current
conservative value classification accepts literals, identifiers, functions,
immutable constructor/tuple/list values, and immutable record literals whose
components are values. It rejects applications, arrays, updates, assignments,
loops, and unknown forms. This is a sound conservative restriction for
mutation; if corpus compatibility requires OCaml's relaxed value restriction,
variance must be modeled and tested explicitly rather than silently permitting
unrestricted generalization.

The current mutable primitive model includes `ref`, dereference, and reference
assignment. Their schemes are rank-1 and instantiated per use, while the value
restriction keeps one allocated cell monomorphic. As new mutable primitives are
added, their types and expansiveness must be added to this contract and covered
by a polymorphic-reference regression.

## Record literals

A record pun such as `{ field }` keeps `field` as the label and lowers its
omitted value to the value identifier with the label's final path segment. Thus
`{ M.field }` looks up label `M.field` while reading value `field`, matching the
OCaml compiler behavior validated for the repository's 5.5 toolchain. When an
expected nominal record type is available, it may disambiguate an unqualified
label that is not otherwise in scope; the selected declaration's type
parameters are instantiated and checked against that expected type. An
unresolved label remains an error. Record updates also check their base
expression against the expected record type before checking updated fields; an
empty literal is explicitly rejected instead of receiving a recovery type.

## Modules and signatures

A top-level `open M` resolves an already available module and adds its direct
exports to subsequent unqualified lookups in that structure or signature.
Among opened modules, the most recent open providing a name wins. Lookup is
source-ordered: an open shadows earlier bindings it provides, while later local
declarations shadow opened names. Opened bindings are lookup scope, not
declarations, so they are not re-exported by the containing module. Nominal
type references resolved through an open retain the defining module's canonical
type identity. An unavailable open target is an unresolved-module diagnostic.

Nested `.mli` module signatures can contain values, types, constructors,
records, nested signatures, and module aliases. A simple `module type of M`
alias forwards the resolved interface summary; `include M` and
`include module type of M` re-export the target's direct members in the
containing signature. Includes with module-type constraints and unsupported
module-type expressions still receive explicit diagnostics rather than silently
ignoring constraints.

`.mli` declarations create schemes with their declared type variables bound in
the declaration scope. Abstract type identities remain nominal. Manifest
equalities are represented when exported. A module member lookup must resolve
the full canonical module/type path; same-spelled names from different modules
are not interchangeable. Implementations are checked against their visible
interface, and clients consume that interface rather than inferred holes.

## Error and recovery contract

- Unknown identifiers, constructors, fields, types, or modules are errors, not
  unconstrained success variables.
- Unsupported AST/type forms produce an `UnsupportedSyntax` or
  `UnsupportedType` diagnostic at the original span.
- Type mismatches preserve the span of the expression, argument, pattern, or
  annotation that introduced the failed constraint.
- A recovery metavariable may be used to continue checking after an error, but
  the error remains in the result and causes the checker command to fail.
- The checker must not erase/convert diagnostics merely to obtain a clean
  corpus run. Coverage changes require explicit review of the affected rule.

## Soundness boundary

The intended claim is soundness for the explicitly supported rank-1 fragment,
relative to the primitive signatures, module interfaces, and value-restriction
classification documented here. Bidirectional syntax alone is not a soundness
proof. The declarative rules, unifier/level invariants, trusted initial
signatures, and rejection of unsupported syntax are all part of that claim.
Higher-rank polymorphism, GADT refinement, polymorphic variants, objects,
first-class modules, and functor applications remain unsupported unless the
tracked source corpus requires them and their rules are added explicitly.

See the companion research note in the private ai-research repository:
`personal/riot/typ-bidirectional-soundness/`.
