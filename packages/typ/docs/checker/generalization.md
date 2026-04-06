# Typ Generalization

This document specifies generalization, nonexpansiveness, and value
restriction for `typ`.

This sits underneath the feature docs and on top of
[checker.md](./checker.md) and [solver.md](./solver.md).

The point here is simple: a lot of the checker only looks like ordinary
Hindley-Milner until generalization enters the picture. The moment `let`,
mutable data, modules, GADTs, or effects show up, the question is no longer
"what type does this expression have?" It is also "which parts of that type are
allowed to escape polymorphically?"

If `typ` gets this wrong, then either:

- it rejects valid programs for no good reason, or
- it accepts unsound polymorphism

Neither is acceptable.

## 1. Scope

This document covers:

- generalization boundaries
- instantiation points
- nonexpansive expressions
- expansive expressions
- value restriction
- recursive-binding approximation
- the relationship between generalization and module exports

This document does not cover:

- the internal mutable representation used by the solver
- the exact algorithm for row rigidification
- the exact algorithm for GADT reification

Those are solver details. The contract here is the observable behavior.

## 2. Core Rule

`typ` should generalize at explicit binding boundaries.

In practice that means:

- `let` bindings
- structure items that export values
- signature items that introduce value schemes
- module summaries that persist exported value schemes

Everything else is monomorphic unless another doc says otherwise.

The important rule is:

generalization does not happen "whenever an expression looks polymorphic."
It happens at the language boundaries that introduce reusable bindings.

## 3. Instantiation

Using a value does not reuse its scheme directly. It instantiates that scheme.

So if the environment contains:

```text
x : Forall(a1, ..., an). tau
```

then using `x` produces a fresh copy of `tau` where each quantified variable is
replaced by a fresh inference variable.

This is the ordinary HM rule and it should remain the rule throughout `typ`,
including through modules and persisted summaries.

## 4. Generalization Boundary

At a binding boundary, `typ` should:

1. infer the expression type in a local inference region
2. finish the local unifications for that region
3. decide which variables are allowed to generalize
4. freeze the result into a scheme for the environment or export summary

The point of the local region is important.

Generalization is always about which variables were created locally and remain
unconstrained by the surrounding environment once the binding finishes.

That is the behavioral contract behind upstream OCaml's level and pool
machinery.

### Example

```ocaml
let id = fun x -> x
let answer = id 42
```

`id` is nonexpansive, so it should generalize to:

```text
Forall(a). a -> a
```

`answer` is an application result, so it does not become a fresh polymorphic
scheme of its own just because it came from a polymorphic value.

## 5. Nonexpansiveness

`typ` should distinguish nonexpansive expressions from expansive ones.

The rule is not "pure" versus "impure" in the abstract. The rule is closer to:

"can this binding safely be treated as a reusable value without inventing
unsound polymorphism?"

For the core functional subset, these are nonexpansive:

- variables
- constants
- functions
- constructor applications whose arguments are nonexpansive
- immutable record construction whose fields are nonexpansive
- field projection from a nonexpansive expression
- tuples of nonexpansive expressions
- `lazy e` when `e` is nonexpansive
- `match e with ...` when the scrutinee and all reachable branches are
  nonexpansive
- packaged modules whose underlying module expression is nonexpansive

These are expansive:

- ordinary function application
- array allocation with elements
- mutable field updates
- loops
- sequencing whose left side matters observationally
- `try`
- module application
- any later feature slice that explicitly allocates or performs effects

This list should expand as features land, but the meaning should stay stable:
nonexpansive means safe to use as the unrestricted generalization case.

### Pseudocode

```ocaml
let generalize_binding gamma expr ty =
  let local_vars = ftv ty - ftv gamma in
  let quantifiers =
    if is_nonexpansive expr
    then local_vars
    else safe_generalizable_subset local_vars ty
  in
  Forall (quantifiers, ty)
```

## 6. Value Restriction

The value restriction is the rule that decides how much polymorphism an
expansive binding is allowed to keep.

For `typ`, the rule should be:

- if a binding is nonexpansive, eligible local variables may generalize
  normally
- if a binding is expansive, `typ` must not generalize variables whose escape
  would be unsound

The exact implementation may use levels, variance lowering, or some other
solver technique. The observable contract is:

- expansive bindings do not gain unsound reusable polymorphism
- nonexpansive bindings still get ordinary ML-style polymorphism

For the OCaml-aligned contract, this includes the relaxed value restriction:
expansive bindings may still generalize variables that remain provably safe
under variance constraints.

For lowered nominal data, that means ordinary aliases, variants, and immutable
record fields should contribute declaration-aware variance information.
Abstract or recursively self-dependent declarations may conservatively fall
back to invariant parameters until a stronger variance proof exists.

That means this is acceptable as a target shape:

```text
let x = []        (* generalizes *)
let f = fun y -> y  (* generalizes *)
let r = ref []    (* does not generalize unsafely *)
```

The exact supported examples depend on which surface features `typ` supports at
that moment. The contract does not.

### Example

```ocaml
let xs = []
let cell = ref []
```

The first binding should generalize in the ordinary ML way.

The second should not gain unsound polymorphism just because the empty list on
the right-hand side looks generic before the reference is considered.

## 7. Recursive Bindings

Recursive bindings are not typed the same way as ordinary `let`.

The contract for recursive bindings is:

1. the recursive binder starts with a fresh monomorphic approximation
2. the binding body is checked under that approximation
3. the inferred result is unified with the approximation
4. only then does generalization happen, under the same value-restriction
   discipline as ordinary `let`

For the functional subset, `typ` may initially restrict recursive bindings to
function-shaped definitions. If it does, that restriction should be explicit in
the feature slice that relies on it.

The important rule is:

recursive typing is approximation-first, not "pretend the final scheme is
already known."

### Pseudocode

```ocaml
let infer_letrec gamma f expr body =
  let alpha = fresh () in
  let s1, ty1 = infer (Env.add_mono gamma f alpha) expr in
  let u = unify (Subst.apply_ty s1 alpha) ty1 in
  let gamma' = Subst.apply_env (Subst.compose u s1) gamma in
  let sigma =
    generalize_binding gamma' expr (Subst.apply_ty u ty1)
  in
  infer (Env.add_scheme gamma' f sigma) body
```

## 8. Modules And Exports

Generalization does not stop at local `let`.

When a module exports a value, that exported scheme is the result of the same
generalization rules applied at the structure boundary.

That means:

- the in-memory environment
- the typed semantic model
- and the persisted `ModuleTypings`

must all agree on the generalized scheme for an exported value.

This is one reason `ModuleTypings` is a semantic artifact rather than a pretty
printer dump.

## 9. Signatures

Signature checking can constrain generalization rather than create it.

If an implementation is checked against a declared interface, then:

- the implementation may infer more internal detail
- but the exported scheme is the one admitted by the interface check

So the final generalized scheme visible outside the module is the interface
result, not whatever internal shape happened to appear first.

## 10. Query Consequences

The generalization rules affect more than compilation.

They also control:

- `type_at`
- hover text
- persisted summaries
- cross-module `definition_at`
- later lint and macro passes

If a scheme is wrong at generalization time, every later consumer gets the
wrong answer.

That is why this document exists as its own slice instead of being buried in
`checker.md`.

## 11. References

The main upstream extraction points here are:

- `typecore.ml`
  `is_nonexpansive`, `type_let`, recursive-binding approximation, and
  contravariance lowering before generalization
- `ctype.ml`
  local-level regions, pools, and the actual generalization boundary mechanics

`typ` does not need to copy the exact implementation shape, but it should keep
the same semantic boundary:

- local region
- solve locally
- generalize what is allowed
- instantiate on use
