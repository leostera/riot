# Typ Modules

This document specifies the module-language slice for `typ`:

- structures
- signatures
- module identifiers
- module type identifiers
- named and unit functors
- functor application
- module type constraints
- module inclusion
- strengthening

This builds on top of [checker.md](./checker.md) and
[nominal_data.md](./nominal_data.md).

The point here is simple: modules are not just a nicer namespace on top of the
core language. They are a separate calculus with their own environments, their
own typing judgments, and their own equalities.

If `typ` gets this wrong, then package-level typing, reusable module summaries,
and cross-module queries all become shaky immediately.

## 1. Scope

This slice includes:

- structures
- signatures
- module bindings
- module type bindings
- module identifiers and aliases
- named functors
- unit functors
- functor application
- module type constraints
- module inclusion checks
- strengthening of exported module types
- dependency elimination for anonymous functor arguments

This slice does not include:

- first-class modules
- recursive modules
- local modules inside core expressions
- applicative versus generative semantics as a user-visible switch
- `with type` and other `with`-constraints
- destructive substitution
- private module components
- classes or objects in signatures

Those are all separate extensions. They should be specified separately.

First-class modules come immediately after this slice. They are the bridge
between the core calculus and the module calculus, so they should not be
quietly folded into either side.

## 2. What This Slice Adds

`checker.md` gave us a tiny HM-style core.

`nominal_data.md` added nominal type declarations, constructors, and labels.

This slice adds modules.

That means the checker now needs to model:

1. structures which elaborate into signatures
2. signatures and module types as semantic objects in their own right
3. module environments containing values, types, modules, and module types
4. functors, where the result type may depend on the argument
5. strengthening, so exported module paths carry useful equalities

The important bit is number 5.

Without strengthening, a module export is just a bag of components. With
strengthening, the export remembers that abstract things inside it are really
the things reachable through that module path.

That is what makes downstream module typing and path-based queries actually
work.

## 3. Semantic Objects

For this slice, the module-language semantic objects should have roughly this
shape:

```text
module_type ::= Signature(sig_item list)
              | Functor(functor_param, module_type)
              | Alias(module_path)

functor_param ::= UnitParam
                | NamedParam(module_name option, module_type)

sig_item ::= SigValue(value_name, sigma)
           | SigType(type_name, type_head)
           | SigModule(module_name, module_presence, module_type)
           | SigModtype(modtype_name, module_type option)

module_presence ::= Present
                  | Absent
```

This deliberately leaves out runtime representation details and typed-tree
shapes. The point of this document is the typing contract.

Two details matter here:

- a module type may be a real signature, a functor, or an alias
- module signatures are heterogeneous; they can export values, types, modules,
  and module types together

For this slice, we assume module expressions lower to a semantic layer with at
least these forms:

```text
m ::= MVar(path)
    | Struct(struct_item list)
    | Functor(functor_param, m)
    | Apply(m, arg)
    | ApplyUnit(m)
    | Constrain(m, module_type)
```

where `arg` is either a typed module argument or the unit argument.

## 4. Environments

The module calculus does not replace the environments from
[checker.md](./checker.md) and [nominal_data.md](./nominal_data.md). It extends
them.

For this slice, the checker should carry an environment with at least:

```text
Gamma  : value-name -> sigma
Delta  : type-name -> type_head
Kappa  : constructor-name -> constructor_decl list
Lambda : label-name -> label_decl list
Mu     : module-name -> module_binding
Xi     : modtype-name -> module_type option
```

where:

```text
module_binding ::= ModuleBinding(module_type, module_presence)
```

`Mu` and `Xi` are the new parts.

The important operational rule is that structures are typed left to right.

Each structure item extends the environment for later items. That means:

- later values can use earlier values
- later type declarations can use earlier modules
- later module bindings can use earlier values, types, and modules

This is the exact same general shape as upstream `type_structure`.

## 5. Judgments

This slice adds four main judgments:

```text
E |-m  m   => mty
E |-mt smt => mty
E |-sg s   => Sigma
E |-incl mty1 <: mty2
```

Read these as:

- `E |-m m => mty`
  module expression `m` elaborates to module type `mty`
- `E |-mt smt => mty`
  module type syntax elaborates to semantic module type `mty`
- `E |-sg s => Sigma`
  a structure or signature elaborates to the signature `Sigma`
- `E |-incl mty1 <: mty2`
  module type `mty1` is included in module type `mty2`

There is no point pretending the module calculus is just an annotation on the
core calculus. It is not.

## 6. Structures

A structure is typed sequentially.

Conceptually:

```text
type_structure(E, []):
  return ([], E)

type_structure(E, item :: rest):
  (sig_items_1, E1) = type_structure_item(E, item)
  (sig_items_2, E2) = type_structure(E1, rest)
  return (sig_items_1 ++ sig_items_2, E2)
```

The result of typing a structure is:

- the typed structure items
- the exported signature
- the final environment after all items

This is the module-language equivalent of typing a `let`-chain left to right.

### Structure Items

This slice assumes these structure-item families:

- value items
- type items
- module items
- module type items
- open items
- include items

Value and type items reuse the rules from [checker.md](./checker.md) and
[nominal_data.md](./nominal_data.md). The only new requirement here is that
they must also produce signature items for the surrounding structure.

So:

- a value binding contributes `SigValue`
- a type declaration contributes `SigType`
- a module binding contributes `SigModule`
- a module type binding contributes `SigModtype`

`open M` is different.

It extends lookup for later structure items, but it does not itself need to
persist as an exported signature item.

`include M` is different in the other direction.

It elaborates `M` to a module type, extracts its exported signature items, and
splices those items into both:

- the environment for later structure items
- the exported signature of the enclosing structure

So a later value can refer to names introduced by the include, and downstream
users can also see those included exports.

`module X = M` is a module item, but when `M` is a stable path the checker
should preserve the alias information instead of flattening it away
immediately.

That preserved alias is what lets downstream summaries and queries recover
exports such as `X.y` with the right path identity.

## 7. Signatures And Module Types

Typing a signature or module type expression elaborates syntax into a semantic
`module_type`.

For this slice, the important forms are:

- signature literals
- module type identifiers
- module aliases
- functor module types

### Signatures

A signature literal elaborates by typing each signature item in order and
building a semantic signature.

This is parallel to structure typing, but without term bodies.

### Module Type Identifiers

If a module type name is in `Xi`, then looking it up yields its semantic module
type.

If it is abstract, it may elaborate to an abstract module-type binding whose
definition is only known through later constraints or inclusion.

### Aliases

If a module type is just a module path alias, then the semantic result is:

```text
Alias(path)
```

This matters because alias information is not just cosmetic. It changes how
path equalities are preserved downstream.

### Functor Module Types

For:

```text
functor (X : S) -> T
```

the checker must:

1. elaborate `S` into a semantic module type
2. extend the environment with `X : S`
3. elaborate `T` in that extended environment
4. produce `Functor(NamedParam(Some X, S), T)`

For unit functors:

```text
functor () -> T
```

the checker produces:

```text
Functor(UnitParam, T)
```

## 8. Module Expressions

### Module Identifiers

For a module identifier `M`, the checker looks up `M` in `Mu`.

Semantically this may remain an alias if the path is aliasable, or it may be
resolved to a strengthened module type.

The important thing is not which internal node shape we choose. The important
thing is the exported type:

- if the path is treated as an alias, the result preserves that alias
- otherwise the result must behave like the strengthened module type at that
  path

### Structures

For:

```text
struct ... end
```

the checker types the structure sequentially and returns:

```text
Signature(exported_items)
```

This is the main introduction form for concrete module values.

### Functors

For:

```text
functor (X : S) -> M
```

the checker must:

1. elaborate `S`
2. extend the environment with `X : S`
3. type `M` in that extended environment
4. return `Functor(NamedParam(Some X, S), MtyBody)`

This is just the module-language function rule.

The important difference from core functions is that the result is a module
type, not a core arrow type, and the body may export path-dependent components.

### Module Type Constraints

For:

```text
(M : S)
```

the checker must:

1. type `M` to get `mty_actual`
2. elaborate `S` to get `mty_expected`
3. check inclusion `mty_actual <: mty_expected`
4. return `mty_expected`

So a module type constraint behaves like a checked coercion, not like an
unchecked rewrite.

## 9. Functor Application

Functor application is the part of the module calculus that forces us to care
about paths, strengthening, and dependency elimination all at once.

That is why it deserves its own section.

For:

```text
F(M)
```

or:

```text
F()
```

the checker must:

1. type the function module `F`
2. require its type to be a functor type
3. type the argument module if one exists
4. check that the argument module type is included in the functor parameter
   type
5. compute the result module type

Step 5 splits in two important cases.

### 9.1 Application To A Stable Path

If the argument has a stable module path, then the result module type is
obtained by substituting that path for the functor parameter in the result
module type.

So if:

```text
F : functor (X : S) -> T(X)
M : S
```

and `M` has path `P`, then:

```text
F(M) : T(P)
```

This is the easy case, and it is the one that keeps applicative behavior
useful.

### 9.2 Application To An Anonymous Module

If the argument does not have a stable path, then the result cannot keep a raw
dependency on the functor parameter.

So the checker must compute a dependency-erased result type:

```text
nondep_supertype(T, X)
```

meaning: remove the dependency of `T` on the parameter `X` while keeping the
result a valid supertype of the original result.

If this cannot be done, the application is ill-typed.

That is the exact contract we want from this slice.

This is not an optimization. This is part of the typing rule for anonymous
functor application.

## 10. Module Inclusion

Module inclusion is the rule behind:

- checking a module against a signature
- checking a functor argument against its parameter type
- checking implementation against interface

For this slice, module inclusion is structural and componentwise.

At a high level:

- signatures are included when every required item on the right is provided by
  a compatible item on the left
- values are checked by type inclusion
- types are checked by declaration compatibility
- modules are checked recursively through module-type inclusion
- module types are checked by equivalence or inclusion, depending on form
- functors are checked by compatible parameter types and compatible result types

This document does not need to restate every sub-rule for values and nominal
data. Those come from [checker.md](./checker.md) and
[nominal_data.md](./nominal_data.md).

What matters here is that module inclusion is a real judgment in the module
calculus. It is not “just compare two pretty-printed signatures and hope”.

## 11. Strengthening

Strengthening is required for exported module types.

The rule is:

when a module expression is bound to a path `P`, the checker should strengthen
its exported module type with respect to `P`.

Intuitively, strengthening rewrites abstract exported components so they become
manifestly tied to the path they came from.

Examples:

- if `P.X` exports an abstract type `t`, the strengthened export should retain
  the equality that this is really `P.X.t`
- if `P.X` exports a submodule `Y`, the strengthened export should retain that
  this is really `P.X.Y`

The point is not to make the signature bigger. The point is to preserve path
equalities that downstream typing needs.

Without strengthening, two downstream modules can both “see” `X.t` and still
fail to understand that they mean the same path-carried type.

## 12. Queries And Summaries

This slice has a direct consequence for `typ`'s reusable artifacts.

A `ModuleTypings` value for a module must contain enough information to reconstruct
its exported module signature, including:

- exported values and their schemes
- exported type declarations
- exported constructors and labels when applicable
- exported submodules and their module types
- exported module types
- exact definition origins and spans for exported symbols
- dependency provenance and fingerprints

This is not optional.

If `ModuleTypings` cannot answer downstream inclusion, name lookup, and
definition queries without reopening arbitrary compiler artifacts, then we have
missed the point of building `typ` in-process.

## 13. Mapping To `typ`

This slice implies a few architectural constraints for `typ`.

1. The semantic tree must have a real module layer.

It cannot treat modules as just names attached to CST nodes.

2. Lowering must preserve module semantics, not surface spelling.

So:

- `module X = struct ... end`
- `module X = (struct ... end)`

lower to the same semantic shape.

3. Module summaries must be strong enough to drive later sessions.

That means a stored module summary has to carry strengthened exported facts, not
just a bag of human-readable strings.

4. `definition_at` for exported module members must work from summary data.

If a symbol is exported from another module, the summary should carry the exact
origin span where it was defined.

## 14. Relationship To Upstream OCaml

This slice is extracted from upstream OCaml's module-typing implementation, but
it is not a transliteration of that implementation.

The main extraction points are:

- `typing/typemod.ml`
  for `type_module`, `type_structure`, `type_interface`, functor typing, and
  application
- `typing/mtype.ml`
  for strengthening and dependency elimination through `nondep_supertype`
- `typing/includemod.mli`
  for the inclusion and application contracts
- `typing/types.mli`
  for the semantic shapes of module types, signatures, functor parameters, and
  module declarations

The upstream implementation is full of operational detail we do not need to
copy into the spec.

What we do want to preserve is the real contract:

- modules are a separate calculus
- structures elaborate to signatures
- functor application checks inclusion and computes a result type
- strengthening preserves path equalities
- anonymous functor application requires dependency elimination

That is the module-calculus contract `typ` actually needs to implement.
