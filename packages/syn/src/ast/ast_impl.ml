open Std
open Std.Collections

type node = {
  tree: Syntax_tree.t;
  id: int;
}

type token = {
  tree: Syntax_tree.t;
  id: int;
}

type cast_error = {
  expected: Syntax_kind.t list;
  actual: Syntax_kind.t;
  node: node;
}

type 'value cast_result =
  | Node of 'value
  | Unknown of node
  | Error of cast_error

type 'value control =
  | Continue of 'value
  | Return of 'value

let count_fold = fun fold value ->
  fold
    value
    ~init:0
    ~fn:(fun _ count -> Continue (Int.add count 1))

let root = fun tree -> ({ tree; id = tree.Syntax_tree.root }: node)

let wrap_node = fun tree id -> ({ tree; id }: node)

let wrap_token = fun tree id -> ({ tree; id }: token)

let syntax_node = fun (node: node) -> Syntax_tree.node node.tree node.id

let syntax_token = fun (token: token) -> Syntax_tree.token token.tree token.id

let kind_is = Syntax_kind.is

let node_kind_is = fun (node: node) kind -> kind_is (syntax_node node).Syntax_tree.kind kind

let token_kind_is = fun (token: token) kind -> kind_is (syntax_token token).Syntax_tree.kind kind

let same_node = fun (left: node) (right: node) -> Int.equal left.id right.id

let node_matches = fun (node: node) matches -> matches (syntax_node node).Syntax_tree.kind

let token_matches = fun (token: token) matches -> matches (syntax_token token).Syntax_tree.kind

let cast_failure = fun node ~expected ->
  Error { expected; actual = (syntax_node node).Syntax_tree.kind; node }

let cast_matching = fun node ~expected ~matches ->
  if node_matches node matches then
    Node node
  else
    cast_failure node ~expected

let cast_kind = fun node expected ->
  if node_kind_is node expected then
    Node node
  else
    cast_failure node ~expected:[ expected ]

let cast_result_to_option = fun __tmp1 ->
  match __tmp1 with
  | Node value -> Some value
  | Unknown _
  | Error _ -> None

let child_count = fun (node: node) -> (syntax_node node).Syntax_tree.child_count

let child_token_at = fun (node: node) index ->
  match Syntax_tree.child_at node.tree (syntax_node node) index with
  | Some (Syntax_tree.Token id) -> Some (wrap_token node.tree id)
  | Some (Syntax_tree.Node _)
  | Some (Syntax_tree.Missing _)
  | None -> None

module Ident = struct
  type t =
    | Bare of token
    | Qualified of token * t

  type view = t

  let is_ident_kind = fun __tmp1 ->
    match __tmp1 with
    | Syntax_kind.PATH_EXPR
    | Syntax_kind.PATH_PATTERN
    | Syntax_kind.PATH_TYPE
    | Syntax_kind.PATH_MODULE_EXPR
    | Syntax_kind.PATH_MODULE_TYPE -> true
    | _ -> false

  let ident_expected_kinds =
    Syntax_kind.[ PATH_EXPR; PATH_PATTERN; PATH_TYPE; PATH_MODULE_EXPR; PATH_MODULE_TYPE; ]

  let segment_token_kind = fun __tmp1 ->
    match __tmp1 with
    | Syntax_kind.IDENT
    | Syntax_kind.UNDERSCORE
    | Syntax_kind.OPERATOR_KW
    | Syntax_kind.EQ
    | Syntax_kind.LT
    | Syntax_kind.GT
    | Syntax_kind.LTE
    | Syntax_kind.GTE
    | Syntax_kind.NE
    | Syntax_kind.PLUS
    | Syntax_kind.MINUS
    | Syntax_kind.STAR
    | Syntax_kind.SLASH
    | Syntax_kind.PERCENT
    | Syntax_kind.CARET
    | Syntax_kind.BANG
    | Syntax_kind.AMPAMP
    | Syntax_kind.BARBAR
    | Syntax_kind.DOTDOT
    | Syntax_kind.LEFT_ARROW
    | Syntax_kind.FAT_ARROW
    | Syntax_kind.COLONCOLON
    | Syntax_kind.COLONEQ
    | Syntax_kind.QUESTION
    | Syntax_kind.AT
    | Syntax_kind.HASH
    | Syntax_kind.TILDE
    | Syntax_kind.DOLLAR
    | Syntax_kind.PIPE
    | Syntax_kind.AMPERSAND
    | Syntax_kind.STARSTAR
    | Syntax_kind.EQEQ
    | Syntax_kind.BANGEQ
    | Syntax_kind.ATAT
    | Syntax_kind.PIPEGT
    | Syntax_kind.PERCENTGT
    | Syntax_kind.LTPERCENT
    | Syntax_kind.PLUSDOT
    | Syntax_kind.MINUSDOT
    | Syntax_kind.STARDOT
    | Syntax_kind.SLASHDOT -> true
    | _ -> false

  let from_segments = fun segments ->
    let length = Vector.length segments in
    if Int.equal length 0 then
      None
    else
      let rec loop index =
        let token = Vector.get_unchecked segments ~at:index in
        if Int.equal index (Int.sub length 1) then
          Bare token
        else
          Qualified (token, loop (Int.add index 1))
      in
      Some (loop 0)

  let from_child_range_option = fun (node: node) ~start_index ~stop_index ->
    let segments = Vector.with_capacity ~size:Int.(stop_index - start_index) in
    let rec loop index =
      if Int.(index < stop_index) then (
        (
          match child_token_at node index with
          | Some token when segment_token_kind (syntax_token token).Syntax_tree.kind ->
              Vector.push segments ~value:token
          | _ -> ()
        );
        loop (Int.add index 1)
      )
    in
    loop start_index;
    from_segments segments

  let from_child_range = fun node ~start_index ~stop_index ->
    from_child_range_option node ~start_index ~stop_index
    |> Option.expect ~msg:"expected identifier range to contain at least one segment"

  let from_node_option = fun (node: node) ->
    from_child_range_option
      node
      ~start_index:0
      ~stop_index:(child_count node)

  let from_node = fun node ->
    from_node_option node
    |> Option.expect ~msg:"expected identifier node to contain at least one segment"

  let first_child_matching = fun (node: node) ~matches ->
    let child_count = child_count node in
    let rec loop index =
      if index >= child_count then
        None
      else
        match child_token_at node index with
        | Some token when matches (syntax_token token).Syntax_tree.kind -> Some (Bare token)
        | _ -> loop (index + 1)
    in
    loop 0

  let last_child_matching = fun (node: node) ~matches ->
    let found = ref None in
    let child_count = child_count node in
    let rec loop index =
      if index >= child_count then
        ()
      else (
        (
          match child_token_at node index with
          | Some token when matches (syntax_token token).Syntax_tree.kind ->
              found := Some (Bare token)
          | _ -> ()
        );
        loop (index + 1)
      )
    in
    loop 0;
    !found

  let first = fun (node: node) ->
    first_child_matching
      node
      ~matches:(fun kind -> Syntax_kind.(kind = IDENT))

  let first_or_underscore = fun (node: node) ->
    first_child_matching
      node
      ~matches:(fun kind -> Syntax_kind.(kind = IDENT || kind = UNDERSCORE))

  let last = fun (node: node) ->
    last_child_matching
      node
      ~matches:(fun kind -> Syntax_kind.(kind = IDENT))

  let first_segment = fun (Bare token | Qualified (token, _)) -> Some token

  let rec last_segment = fun __tmp1 ->
    match __tmp1 with
    | Bare token -> Some token
    | Qualified (_, rest) -> last_segment rest

  let first_token = fun node ->
    first node
    |> Option.and_then ~fn:first_segment

  let first_or_underscore_token = fun node ->
    first_or_underscore node
    |> Option.and_then ~fn:first_segment

  let last_token = fun node ->
    last node
    |> Option.and_then ~fn:last_segment

  let kind = fun ident ->
    match first_segment ident with
    | Some token -> (syntax_token token).Syntax_tree.kind
    | None -> Syntax_kind.UNKNOWN

  let rec fold_segment = fun ident ~init ~fn ->
    match ident with
    | Bare token -> (
        match fn token init with
        | Continue next -> next
        | Return value -> value
      )
    | Qualified (token, rest) -> (
        match fn token init with
        | Continue next -> fold_segment rest ~init:next ~fn
        | Return value -> value
      )

  let fold_token = fold_segment

  let for_each_segment = fun ident ~fn ->
    fold_segment
      ident
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let for_each_token = for_each_segment

  let segment_count = fun ident -> count_fold fold_segment ident

  let span = fun ident ->
    match (first_segment ident, last_segment ident) with
    | (Some first, Some last) ->
        let first_leaf = syntax_token first in
        let last_leaf = syntax_token last in
        let first_raw =
          Vector.get_unchecked first.tree.Syntax_tree.raw_tokens ~at:first_leaf.body_raw
        in
        let last_raw =
          Vector.get_unchecked last.tree.Syntax_tree.raw_tokens ~at:last_leaf.body_raw
        in
        Span.make ~start:first_raw.Raw_token.span.start ~end_:last_raw.Raw_token.span.end_
    | _ -> Span.make ~start:0 ~end_:0

  let rec width = fun __tmp1 ->
    match __tmp1 with
    | Bare token -> Syntax_tree.token_width token.tree (syntax_token token)
    | Qualified (token, rest) ->
        Syntax_tree.token_width token.tree (syntax_token token) + 1 + width rest

  let cast = fun (node: node) ->
    match cast_matching node ~expected:ident_expected_kinds ~matches:is_ident_kind with
    | Node node -> (
        match from_node_option node with
        | Some ident -> Node ident
        | None -> Unknown node
      )
    | Unknown node -> Unknown node
    | Error error -> Error error

  let rec text = fun __tmp1 ->
    match __tmp1 with
    | Bare token -> Syntax_tree.token_text token.tree (syntax_token token)
    | Qualified (token, rest) ->
        Syntax_tree.token_text token.tree (syntax_token token) ^ "." ^ text rest

  let view = fun ident -> Some ident

  let node_is_single_text = fun node expected ->
    match from_node_option node with
    | Some ident -> Int.equal (segment_count ident) 1 && String.equal (text ident) expected
    | None -> false
end

type source_file = node

type implementation = node

type interface = node

type structure_item = node

type signature_item = node

type let_declaration = node

type let_binding = node

type type_declaration = node

type type_extension_declaration = node

type module_declaration = node

type module_expr = node

type module_type_expr = node

type module_type_declaration = node

type module_type_constraint = node

type open_declaration = node

type include_declaration = node

type value_declaration = node

type external_declaration = node

type exception_declaration = node

type extension_item = node

type attribute_item = node

type expr_item = node

type expr = node

type pattern = node

type parameter = node

type match_case = node

type type_expr = node

type record_type = node

type record_field = node

type record_expr_field = node

type variant_type = node

type variant_constructor = node

type record_expr_field_view =
  | RecordExprField of {
      ident: Ident.t;
      value: expr option;
      node: record_expr_field;
    }
  | UnknownRecordExprField of {
      node: record_expr_field;
    }

type record_pattern_field_view =
  | RecordPatternField of {
      ident: Ident.t;
      pattern: pattern option;
      node: pattern;
    }
  | UnknownRecordPatternField of {
      node: pattern;
    }

type first_class_module_pattern_ascription =
  | NoAscription
  | IdentAscription
  | UnsupportedAscription

type type_item =
  | TypeDeclarationItem of type_declaration
  | TypeExtensionItem of type_extension_declaration

let is_expr_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.LET_EXPR
  | Syntax_kind.LOCAL_OPEN_EXPR
  | Syntax_kind.LET_MODULE_EXPR
  | Syntax_kind.LET_EXCEPTION_EXPR
  | Syntax_kind.BINDING_OPERATOR_EXPR
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.UNREACHABLE_EXPR
  | Syntax_kind.IF_EXPR
  | Syntax_kind.MATCH_EXPR
  | Syntax_kind.FUN_EXPR
  | Syntax_kind.FUNCTION_EXPR
  | Syntax_kind.TRY_EXPR
  | Syntax_kind.WHILE_EXPR
  | Syntax_kind.FOR_EXPR
  | Syntax_kind.ASSERT_EXPR
  | Syntax_kind.LAZY_EXPR
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.SEQUENCE_EXPR
  | Syntax_kind.APPLY_EXPR
  | Syntax_kind.INFIX_EXPR
  | Syntax_kind.PREFIX_EXPR
  | Syntax_kind.ASSIGN_EXPR
  | Syntax_kind.FIELD_ACCESS_EXPR
  | Syntax_kind.POLY_VARIANT_EXPR
  | Syntax_kind.LABELED_ARG
  | Syntax_kind.OPTIONAL_ARG
  | Syntax_kind.ARRAY_INDEX_EXPR
  | Syntax_kind.STRING_INDEX_EXPR
  | Syntax_kind.TYPED_EXPR
  | Syntax_kind.PATH_EXPR
  | Syntax_kind.LITERAL_EXPR
  | Syntax_kind.PAREN_EXPR
  | Syntax_kind.TUPLE_EXPR
  | Syntax_kind.LIST_EXPR
  | Syntax_kind.ARRAY_EXPR
  | Syntax_kind.RECORD_EXPR
  | Syntax_kind.RECORD_UPDATE_EXPR -> true
  | _ -> false

let is_pattern_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.PATH_PATTERN
  | Syntax_kind.CONSTRUCT_PATTERN
  | Syntax_kind.LITERAL_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.EXTENSION_PATTERN
  | Syntax_kind.ATTRIBUTE_PATTERN
  | Syntax_kind.LOCAL_OPEN_PATTERN
  | Syntax_kind.LOCALLY_ABSTRACT_TYPE_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.INTERVAL_PATTERN
  | Syntax_kind.CONSTRAINT_PATTERN
  | Syntax_kind.ALIAS_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN -> true
  | _ -> false

let is_parameter_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.LABELED_PARAM
  | Syntax_kind.OPTIONAL_PARAM
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT -> true
  | _ -> false

let is_parameter_node_kind = fun kind -> is_parameter_kind kind || is_pattern_kind kind

let is_type_expr_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.TYPE_EXPR
  | Syntax_kind.PATH_TYPE
  | Syntax_kind.VAR_TYPE
  | Syntax_kind.WILDCARD_TYPE
  | Syntax_kind.ARROW_TYPE
  | Syntax_kind.POLY_TYPE
  | Syntax_kind.LABELED_TYPE
  | Syntax_kind.TUPLE_TYPE
  | Syntax_kind.APPLY_TYPE
  | Syntax_kind.PAREN_TYPE
  | Syntax_kind.OPAQUE_TYPE -> true
  | _ -> false

let is_record_type_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.RECORD_TYPE -> true
  | _ -> false

let is_record_field_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.RECORD_FIELD -> true
  | _ -> false

let is_record_expr_field_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.RECORD_EXPR_FIELD -> true
  | _ -> false

let is_variant_type_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.VARIANT_TYPE -> true
  | _ -> false

let is_variant_constructor_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.VARIANT_CONSTRUCTOR -> true
  | _ -> false

let is_module_expr_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.MODULE_EXPR
  | Syntax_kind.PATH_MODULE_EXPR
  | Syntax_kind.STRUCT_MODULE_EXPR
  | Syntax_kind.FUNCTOR_MODULE_EXPR
  | Syntax_kind.APPLY_MODULE_EXPR
  | Syntax_kind.CONSTRAINT_MODULE_EXPR
  | Syntax_kind.PAREN_MODULE_EXPR
  | Syntax_kind.OPAQUE_MODULE_EXPR -> true
  | _ -> false

let is_module_type_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.MODULE_TYPE_EXPR
  | Syntax_kind.PATH_MODULE_TYPE
  | Syntax_kind.SIGNATURE_MODULE_TYPE
  | Syntax_kind.TYPEOF_MODULE_TYPE
  | Syntax_kind.FUNCTOR_MODULE_TYPE
  | Syntax_kind.WITH_MODULE_TYPE
  | Syntax_kind.PAREN_MODULE_TYPE
  | Syntax_kind.OPAQUE_MODULE_TYPE -> true
  | _ -> false

let is_match_case_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.MATCH_CASE -> true
  | _ -> false

let is_let_binding_kind = fun __tmp1 ->
  match __tmp1 with
  | Syntax_kind.LET_BINDING -> true
  | _ -> false

let expr_expected_kinds =
  Syntax_kind.[
    LET_EXPR;
    LOCAL_OPEN_EXPR;
    LET_MODULE_EXPR;
    LET_EXCEPTION_EXPR;
    BINDING_OPERATOR_EXPR;
    FIRST_CLASS_MODULE_EXPR;
    EXTENSION_EXPR;
    UNREACHABLE_EXPR;
    IF_EXPR;
    MATCH_EXPR;
    FUN_EXPR;
    FUNCTION_EXPR;
    TRY_EXPR;
    WHILE_EXPR;
    FOR_EXPR;
    ASSERT_EXPR;
    LAZY_EXPR;
    ATTRIBUTE_EXPR;
    SEQUENCE_EXPR;
    APPLY_EXPR;
    INFIX_EXPR;
    PREFIX_EXPR;
    ASSIGN_EXPR;
    FIELD_ACCESS_EXPR;
    POLY_VARIANT_EXPR;
    LABELED_ARG;
    OPTIONAL_ARG;
    ARRAY_INDEX_EXPR;
    STRING_INDEX_EXPR;
    TYPED_EXPR;
    PATH_EXPR;
    LITERAL_EXPR;
    PAREN_EXPR;
    TUPLE_EXPR;
    LIST_EXPR;
    ARRAY_EXPR;
    RECORD_EXPR;
    RECORD_UPDATE_EXPR;
  ]

let pattern_expected_kinds =
  Syntax_kind.[
    WILDCARD_PATTERN;
    PATH_PATTERN;
    CONSTRUCT_PATTERN;
    LITERAL_PATTERN;
    PAREN_PATTERN;
    TUPLE_PATTERN;
    LIST_PATTERN;
    ARRAY_PATTERN;
    RECORD_PATTERN;
    POLY_VARIANT_PATTERN;
    EXTENSION_PATTERN;
    ATTRIBUTE_PATTERN;
    LOCAL_OPEN_PATTERN;
    LOCALLY_ABSTRACT_TYPE_PATTERN;
    FIRST_CLASS_MODULE_PATTERN;
    INTERVAL_PATTERN;
    CONSTRAINT_PATTERN;
    ALIAS_PATTERN;
    OR_PATTERN;
    CONS_PATTERN;
    LAZY_PATTERN;
    EXCEPTION_PATTERN;
  ]

let parameter_expected_kinds =
  Syntax_kind.[ LABELED_PARAM; OPTIONAL_PARAM; OPTIONAL_PARAM_DEFAULT; ] @ pattern_expected_kinds

let type_expr_expected_kinds =
  Syntax_kind.[
    TYPE_EXPR;
    PATH_TYPE;
    VAR_TYPE;
    WILDCARD_TYPE;
    ARROW_TYPE;
    POLY_TYPE;
    LABELED_TYPE;
    TUPLE_TYPE;
    APPLY_TYPE;
    PAREN_TYPE;
    OPAQUE_TYPE;
  ]

let module_expr_expected_kinds =
  Syntax_kind.[
    MODULE_EXPR;
    PATH_MODULE_EXPR;
    STRUCT_MODULE_EXPR;
    FUNCTOR_MODULE_EXPR;
    APPLY_MODULE_EXPR;
    CONSTRAINT_MODULE_EXPR;
    PAREN_MODULE_EXPR;
    OPAQUE_MODULE_EXPR;
  ]

let module_type_expected_kinds =
  Syntax_kind.[
    MODULE_TYPE_EXPR;
    PATH_MODULE_TYPE;
    SIGNATURE_MODULE_TYPE;
    TYPEOF_MODULE_TYPE;
    FUNCTOR_MODULE_TYPE;
    WITH_MODULE_TYPE;
    PAREN_MODULE_TYPE;
    OPAQUE_MODULE_TYPE;
  ]

let first_child_node_matching = fun (node: node) ~matches ->
  let found = ref None in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Node id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let child = wrap_node node.tree id in
              if node_matches child matches then
                found := Some child
        )
      | Syntax_tree.Token _
      | Syntax_tree.Missing _ -> ());
  !found

let nth_child_node_matching = fun (node: node) target ~matches ->
  let found = ref None in
  let seen = ref 0 in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Node id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let child = wrap_node node.tree id in
              if node_matches child matches then
                if Int.equal !seen target then
                  found := Some child
                else
                  seen := !seen + 1
        )
      | Syntax_tree.Token _
      | Syntax_tree.Missing _ -> ());
  !found

let last_child_node_matching = fun (node: node) ~matches ->
  let found = ref None in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Node id ->
          let child = wrap_node node.tree id in
          if node_matches child matches then
            found := Some child
      | Syntax_tree.Token _
      | Syntax_tree.Missing _ -> ());
  !found

let for_each_child_node_matching = fun (node: node) ~matches ~fn ->
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Node id ->
          let child = wrap_node node.tree id in
          if node_matches child matches then
            fn child
      | Syntax_tree.Token _
      | Syntax_tree.Missing _ -> ())

let fold_child_node_matching = fun (node: node) ~matches ~init ~fn ->
  let acc = ref init in
  let returned = ref false in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Node id when not !returned ->
          let child = wrap_node node.tree id in
          if node_matches child matches then (
            match fn child !acc with
            | Continue next -> acc := next
            | Return value ->
                acc := value;
                returned := true
          )
      | Syntax_tree.Node _
      | Syntax_tree.Token _
      | Syntax_tree.Missing _ -> ());
  !acc

let first_child_token_matching = fun (node: node) ~matches ->
  let found = ref None in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Token id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let token = wrap_token node.tree id in
              if token_matches token matches then
                found := Some token
        )
      | Syntax_tree.Node _
      | Syntax_tree.Missing _ -> ());
  !found

let rec first_descendant_token_matching = fun (node: node) ~matches ->
  let found = ref None in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Token id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let token = wrap_token node.tree id in
              if token_matches token matches then
                found := Some token
        )
      | Syntax_tree.Node id -> (
          match !found with
          | Some _ -> ()
          | None -> (
              match first_descendant_token_matching (wrap_node node.tree id) ~matches with
              | Some token -> found := Some token
              | None -> ()
            )
        )
      | Syntax_tree.Missing _ -> ());
  !found

let child_node_at = fun (node: node) index ->
  match Syntax_tree.child_at node.tree (syntax_node node) index with
  | Some (Syntax_tree.Node id) -> Some (wrap_node node.tree id)
  | Some (Syntax_tree.Token _)
  | Some (Syntax_tree.Missing _)
  | None -> None

let has_child_token_kind = fun (node: node) expected_kind ->
  let found = ref false in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Token id ->
          let token = wrap_token node.tree id in
          if token_kind_is token expected_kind then
            found := true
      | Syntax_tree.Node _
      | Syntax_tree.Missing _ -> ());
  !found

let nth_child_token_matching = fun (node: node) target ~matches ->
  let found = ref None in
  let seen = ref 0 in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Token id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let token = wrap_token node.tree id in
              if token_matches token matches then
                if Int.equal !seen target then
                  found := Some token
                else
                  seen := !seen + 1
        )
      | Syntax_tree.Node _
      | Syntax_tree.Missing _ -> ());
  !found

let child_token_kind_at = fun (node: node) index ->
  match child_token_at node index with
  | Some token -> Some (syntax_token token).Syntax_tree.kind
  | None -> None

let first_expr_child = fun (node: node) -> first_child_node_matching node ~matches:is_expr_kind

let nth_expr_child = fun (node: node) target ->
  nth_child_node_matching
    node
    target
    ~matches:is_expr_kind

let last_expr_child = fun (node: node) -> last_child_node_matching node ~matches:is_expr_kind

let first_pattern_child = fun (node: node) ->
  first_child_node_matching
    node
    ~matches:is_pattern_kind

let nth_pattern_child = fun (node: node) target ->
  nth_child_node_matching
    node
    target
    ~matches:is_pattern_kind

let first_type_expr_child = fun (node: node) ->
  first_child_node_matching
    node
    ~matches:is_type_expr_kind

let rec normalize_expr_node = fun (expr: expr) ->
  match (syntax_node expr).Syntax_tree.kind with
  | Syntax_kind.PAREN_EXPR
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match first_expr_child expr with
      | Some inner -> normalize_expr_node inner
      | None -> expr
    )
  | _ -> expr

let normalize_expr_option = fun __tmp1 ->
  match __tmp1 with
  | Some expr -> Some (normalize_expr_node expr)
  | None -> None

let rec normalize_pattern_node = fun (pattern: pattern) ->
  match (syntax_node pattern).Syntax_tree.kind with
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.ATTRIBUTE_PATTERN -> (
      match first_pattern_child pattern with
      | Some inner -> normalize_pattern_node inner
      | None -> pattern
    )
  | _ -> pattern

let normalize_pattern_option = fun __tmp1 ->
  match __tmp1 with
  | Some pattern -> Some (normalize_pattern_node pattern)
  | None -> None

let rec normalize_type_expr_node = fun (type_expr: type_expr) ->
  match (syntax_node type_expr).Syntax_tree.kind with
  | Syntax_kind.TYPE_EXPR
  | Syntax_kind.PAREN_TYPE -> (
      match first_type_expr_child type_expr with
      | Some inner -> normalize_type_expr_node inner
      | None -> type_expr
    )
  | _ -> type_expr

let normalize_type_expr_option = fun __tmp1 ->
  match __tmp1 with
  | Some type_expr -> Some (normalize_type_expr_node type_expr)
  | None -> None

let rec first_type_expr_descendant_of_pattern = fun (node: node) ->
  match first_type_expr_child node with
  | Some type_expr -> Some type_expr
  | None ->
      let found = ref None in
      Syntax_tree.for_each_child
        node.tree
        (syntax_node node)
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Syntax_tree.Node id -> (
              match !found with
              | Some _ -> ()
              | None ->
                  let child = wrap_node node.tree id in
                  if node_matches child is_pattern_kind then
                    found := first_type_expr_descendant_of_pattern child
            )
          | Syntax_tree.Token _
          | Syntax_tree.Missing _ -> ());
      !found

let first_match_case_child = fun (node: node) ->
  first_child_node_matching
    node
    ~matches:is_match_case_kind

let first_let_binding_child = fun (node: node) ->
  first_child_node_matching
    node
    ~matches:is_let_binding_kind

let span_of_raw_range = fun tree ~raw_lo ~raw_hi ->
  if Int.(raw_hi <= raw_lo) then
    Span.make ~start:0 ~end_:0
  else
    let first = Vector.get_unchecked tree.Syntax_tree.raw_tokens ~at:raw_lo in
    let last = Vector.get_unchecked tree.Syntax_tree.raw_tokens ~at:(Int.sub raw_hi 1) in
    Span.make ~start:first.Raw_token.span.start ~end_:last.Raw_token.span.end_

let rec for_each_token_in_node = fun (node: node) ~fn ->
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Token id -> fn (wrap_token node.tree id)
      | Syntax_tree.Node id -> for_each_token_in_node (wrap_node node.tree id) ~fn
      | Syntax_tree.Missing _ -> ())

let for_each_token_after_child_token = fun (node: node) ~matches ~fn ->
  let seen_boundary = ref false in
  Syntax_tree.for_each_child
    node.tree
    (syntax_node node)
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Syntax_tree.Token id ->
          let token = wrap_token node.tree id in
          if !seen_boundary then
            fn token
          else if token_matches token matches then
            seen_boundary := true
      | Syntax_tree.Node id ->
          if !seen_boundary then
            for_each_token_in_node (wrap_node node.tree id) ~fn
      | Syntax_tree.Missing _ -> ())

module Lex_token = Token

module Token = struct
  type t = token

  type delimited_trivia = {
    text: string;
    opening: string;
    content: string;
    closing: string option;
  }

  type leading_trivia =
    | Whitespace
    | Comment of delimited_trivia
    | Docstring of delimited_trivia

  let kind = fun (token: token) -> (syntax_token token).Syntax_tree.kind

  let width = fun (token: token) -> Syntax_tree.token_width token.tree (syntax_token token)

  let contains_char = fun (token: token) needle ->
    Syntax_tree.token_contains_char
      token.tree
      (syntax_token token)
      needle

  let text_is = fun (token: token) expected ->
    Syntax_tree.token_text_is
      token.tree
      (syntax_token token)
      expected

  let text_equal = fun (left: token) (right: token) ->
    let left_width = width left in
    if not (Int.equal left_width (width right)) then
      false
    else if Int.equal left_width 0 then
      true
    else
      let left_raw =
        Vector.get_unchecked
          left.tree.Syntax_tree.raw_tokens
          ~at:(syntax_token left).Syntax_tree.body_raw
      in
      let right_raw =
        Vector.get_unchecked
          right.tree.Syntax_tree.raw_tokens
          ~at:(syntax_token right).Syntax_tree.body_raw
      in
      let left_slice = Raw_token.slice ~source:left.tree.Syntax_tree.source left_raw in
      let right_slice = Raw_token.slice ~source:right.tree.Syntax_tree.source right_raw in
      let rec loop index =
        if Int.(index >= left_width) then
          true
        else if
          Char.equal
            (IO.IoVec.IoSlice.get_unchecked left_slice ~at:index)
            (IO.IoVec.IoSlice.get_unchecked right_slice ~at:index)
        then
          loop Int.(index + 1)
        else
          false
      in
      loop 0

  let slice = fun (token: token) -> Syntax_tree.token_text_slice token.tree (syntax_token token)

  let has_newline = fun (token: token) ->
    Syntax_tree.token_has_newline
      token.tree
      (syntax_token token)

  let text = fun (token: token) -> Syntax_tree.token_text token.tree (syntax_token token)

  let span = fun (token: token) ->
    let leaf = syntax_token token in
    (Vector.get_unchecked token.tree.Syntax_tree.raw_tokens ~at:leaf.Syntax_tree.body_raw).Raw_token.span

  let span_start = fun token -> (span token).Span.start

  let span_end = fun token -> (span token).Span.end_

  let leading_text = fun (token: token) ->
    let syntax_token = syntax_token token in
    Syntax_tree.raw_range_text
      token.tree
      ~raw_lo:syntax_token.Syntax_tree.raw_lo
      ~raw_hi:syntax_token.Syntax_tree.body_raw

  let leading_trivia_text = fun (token: token) raw ->
    match raw.Raw_token.legacy_kind with
    | Lex_token.Whitespace -> " "
    | _ -> Raw_token.text_slice ~source:token.tree.Syntax_tree.source raw

  let fold_leading_trivia = fun (token: token) ~init ~fn ->
    let syntax_token = syntax_token token in
    let rec loop raw_index acc =
      if Int.(raw_index >= syntax_token.Syntax_tree.body_raw) then
        acc
      else
        let raw = Vector.get_unchecked token.tree.Syntax_tree.raw_tokens ~at:raw_index in
        match fn
          ~kind:raw.Raw_token.kind
          ~text:(leading_trivia_text token raw)
          acc with
        | Continue next -> loop Int.(raw_index + 1) next
        | Return value -> value
    in
    loop syntax_token.Syntax_tree.raw_lo init

  let for_each_leading_trivia = fun token ~fn ->
    fold_leading_trivia
      token
      ~init:()
      ~fn:(fun ~kind ~text () ->
        fn ~kind ~text;
        Continue ())

  let closing_if_terminated = fun terminated ->
    if terminated then
      Some "*)"
    else
      None

  let leading_trivia_item_of_raw = fun (token: token) raw ->
    match raw.Raw_token.legacy_kind with
    | Lex_token.Whitespace -> Whitespace
    | Lex_token.Comment { value; terminated } ->
        let text = Raw_token.text_slice ~source:token.tree.Syntax_tree.source raw in
        Comment {
          text;
          opening = "(*";
          content = value;
          closing = closing_if_terminated terminated;
        }
    | Lex_token.Docstring { value; terminated } ->
        let text = Raw_token.text_slice ~source:token.tree.Syntax_tree.source raw in
        Docstring {
          text;
          opening = "(**";
          content = value;
          closing = closing_if_terminated terminated;
        }
    | _ -> panic "Ast.Token.leading_trivia_item_of_raw received non-trivia raw token"

  let fold_leading_trivia_item = fun (token: token) ~init ~fn ->
    let syntax_token = syntax_token token in
    let rec loop raw_index acc =
      if Int.(raw_index >= syntax_token.Syntax_tree.body_raw) then
        acc
      else
        let raw = Vector.get_unchecked token.tree.Syntax_tree.raw_tokens ~at:raw_index in
        match fn (leading_trivia_item_of_raw token raw) acc with
        | Continue next -> loop Int.(raw_index + 1) next
        | Return value -> value
    in
    loop syntax_token.Syntax_tree.raw_lo init

  let for_each_leading_trivia_item = fun token ~fn ->
    fold_leading_trivia_item
      token
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let has_leading_raw = fun (token: token) ~matches ->
    let syntax_token = syntax_token token in
    let rec loop raw_index =
      if Int.(raw_index >= syntax_token.Syntax_tree.body_raw) then
        false
      else
        let raw = Vector.get_unchecked token.tree.Syntax_tree.raw_tokens ~at:raw_index in
        if matches raw.Raw_token.kind then
          true
        else
          loop Int.(raw_index + 1)
    in
    loop syntax_token.Syntax_tree.raw_lo

  let has_leading_whitespace = fun token ->
    has_leading_raw
      token
      ~matches:(fun kind -> Syntax_kind.(kind = WHITESPACE))

  let has_leading_comment = fun token ->
    has_leading_raw
      token
      ~matches:(fun kind -> Syntax_kind.(kind = COMMENT || kind = DOCSTRING))

  let has_leading_docstring = fun token ->
    has_leading_raw
      token
      ~matches:(fun kind -> Syntax_kind.(kind = DOCSTRING))

  let full_text = fun (token: token) ->
    let (raw_lo, raw_hi) =
      let token = syntax_token token in
      (token.Syntax_tree.raw_lo, token.Syntax_tree.raw_hi)
    in
    Syntax_tree.raw_range_text token.tree ~raw_lo ~raw_hi

  let raw_range = fun (token: token) ->
    let token = syntax_token token in
    (token.Syntax_tree.raw_lo, token.Syntax_tree.raw_hi)
end

module Node = struct
  type t = node

  let kind = fun (node: node) -> (syntax_node node).Syntax_tree.kind

  let text = fun (node: node) -> Syntax_tree.node_text node.tree (syntax_node node)

  let span = fun (node: node) ->
    let first = ref None in
    let last = ref None in
    for_each_token_in_node
      node
      ~fn:(fun token ->
        let span = Token.span token in
        (
          match !first with
          | Some _ -> ()
          | None -> first := Some span.Span.start
        );
        last := Some span.Span.end_);
    match (!first, !last) with
    | (Some start, Some end_) -> Span.make ~start ~end_
    | _ -> Span.make ~start:0 ~end_:0

  let span_start = fun node -> (span node).Span.start

  let span_end = fun node -> (span node).Span.end_

  let raw_range = fun (node: node) ->
    let node = syntax_node node in
    (node.Syntax_tree.raw_lo, node.Syntax_tree.raw_hi)

  let full_width = fun (node: node) -> (syntax_node node).Syntax_tree.full_width

  let token_width = fun (node: node) -> Syntax_tree.node_token_width node.tree (syntax_node node)

  let width = token_width

  let child_count = fun (node: node) -> (syntax_node node).Syntax_tree.child_count

  let child_at = fun (node: node) index -> Syntax_tree.child_at node.tree (syntax_node node) index

  let fold_child = fun (node: node) ~init ~fn ->
    let acc = ref init in
    let returned = ref false in
    Syntax_tree.for_each_child
      node.tree
      (syntax_node node)
      ~fn:(fun child ->
        if not !returned then
          match fn child !acc with
          | Continue next -> acc := next
          | Return value ->
              acc := value;
              returned := true);
    !acc

  let for_each_child = fun node ~fn ->
    fold_child
      node
      ~init:()
      ~fn:(fun child () ->
        fn child;
        Continue ())

  let fold_child_node = fun (node: node) ~init ~fn ->
    fold_child
      node
      ~init
      ~fn:(fun child acc ->
        match child with
        | Syntax_tree.Node id -> fn (wrap_node node.tree id) acc
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> Continue acc)

  let for_each_child_node = fun node ~fn ->
    fold_child_node
      node
      ~init:()
      ~fn:(fun child () ->
        fn child;
        Continue ())

  let fold_child_token = fun (node: node) ~init ~fn ->
    fold_child
      node
      ~init
      ~fn:(fun child acc ->
        match child with
        | Syntax_tree.Token id -> fn (wrap_token node.tree id) acc
        | Syntax_tree.Node _
        | Syntax_tree.Missing _ -> Continue acc)

  let for_each_child_token = fun node ~fn ->
    fold_child_token
      node
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let fold_token = fun node ~init ~fn ->
    let acc = ref init in
    let returned = ref false in
    for_each_token_in_node
      node
      ~fn:(fun token ->
        if not !returned then
          match fn token !acc with
          | Continue next -> acc := next
          | Return value ->
              acc := value;
              returned := true);
    !acc

  let for_each_token = fun node ~fn ->
    fold_token
      node
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let first_child_node = fun (node: node) ~kind:expected_kind ->
    first_child_node_matching
      node
      ~matches:(fun kind -> Syntax_kind.(kind = expected_kind))

  let first_child_token = fun (node: node) ~kind:expected_kind ->
    first_child_token_matching
      node
      ~matches:(fun kind -> Syntax_kind.(kind = expected_kind))

  let first_token = fun (node: node) -> first_child_token_matching node ~matches:(fun _ -> true)

  let first_descendant_token = fun (node: node) ->
    first_descendant_token_matching
      node
      ~matches:(fun _ -> true)
end

let node_colon_has_leading_whitespace = fun node ->
  match Node.first_child_token node ~kind:Syntax_kind.COLON with
  | Some colon -> Token.has_leading_whitespace colon
  | None -> false

let rec fold_parameter_spine_node = fun node ~acc ~fn ->
  match Node.kind node with
  | kind when is_parameter_kind kind -> (
      match fn node acc with
      | Return value -> Return value
      | Continue acc -> (
          if node_colon_has_leading_whitespace node then
            Continue acc
          else
            match first_pattern_child node with
            | Some pattern when node_kind_is pattern Syntax_kind.CONSTRUCT_PATTERN -> (
                match nth_child_node_matching pattern 1 ~matches:is_parameter_node_kind with
                | Some rest -> fold_parameter_spine_node rest ~acc ~fn
                | None -> Continue acc
              )
            | Some _
            | None -> Continue acc
        )
    )
  | Syntax_kind.CONSTRUCT_PATTERN ->
      fold_child_node_matching
        node
        ~matches:is_parameter_node_kind
        ~init:(Continue acc)
        ~fn:(fun child state ->
          match state with
          | Return _ -> Return state
          | Continue acc -> (
              match fold_parameter_spine_node child ~acc ~fn with
              | Continue next -> Continue (Continue next)
              | Return value -> Return (Return value)
            ))
  | Syntax_kind.CONSTRAINT_PATTERN -> (
      match first_pattern_child node with
      | Some pattern -> fold_parameter_spine_node pattern ~acc ~fn
      | None -> fn node acc
    )
  | _ -> fn node acc

let record_pattern_open_wildcard = fun (record: pattern) ->
  let rec loop index previous_token_kind =
    if index >= Node.child_count record then
      None
    else
      match Node.child_at record index with
      | Some (Syntax_tree.Token id) ->
          let token = wrap_token record.tree id in
          loop (index + 1) (Some (Token.kind token))
      | Some (Syntax_tree.Node id) ->
          let child = wrap_node record.tree id in
          if node_kind_is child Syntax_kind.WILDCARD_PATTERN then
            match previous_token_kind with
            | Some kind when Syntax_kind.(kind = EQ) -> loop (index + 1) previous_token_kind
            | _ -> Node.first_child_token child ~kind:Syntax_kind.UNDERSCORE
          else
            loop (index + 1) previous_token_kind
      | Some (Syntax_tree.Missing _)
      | None -> loop (index + 1) previous_token_kind
  in
  loop 0 None

let collect_record_pattern_fields = fun (record: pattern) ->
  let fields = Vector.with_capacity ~size:(Node.child_count record) in
  let child_count = Node.child_count record in
  let rec loop index =
    if index >= child_count then
      ()
    else
      match child_node_at record index with
      | Some child when node_kind_is child Syntax_kind.PATH_PATTERN ->
          let (pattern, next) =
            match child_token_kind_at record (index + 1) with
            | Some kind when Syntax_kind.(kind = EQ) -> (
                match child_node_at record (index + 2) with
                | Some value when node_matches value is_pattern_kind -> (Some value, index + 3)
                | _ -> (None, index + 2)
              )
            | _ -> (None, index + 1)
          in
          Vector.push
            fields
            ~value:(
              if node_matches child Ident.is_ident_kind then
                match Ident.from_node_option child with
                | Some ident -> RecordPatternField { ident; pattern; node = child }
                | None -> UnknownRecordPatternField { node = child }
              else
                UnknownRecordPatternField { node = child }
            );
          loop next
      | Some child when node_kind_is child Syntax_kind.WILDCARD_PATTERN -> loop (index + 1)
      | Some child when node_matches child is_pattern_kind ->
          Vector.push fields ~value:(UnknownRecordPatternField { node = child });
          loop (index + 1)
      | Some _
      | None -> loop (index + 1)
  in
  loop 0;
  fields

let first_class_module_pattern_token_index = fun pattern ~from ~matches ->
  let count = Node.child_count pattern in
  let rec loop index =
    if index >= count then
      None
    else
      match child_token_at pattern index with
      | Some token when matches (Token.kind token) -> Some index
      | _ -> loop (index + 1)
  in
  loop from

let first_class_module_pattern_module_index = fun pattern ->
  first_class_module_pattern_token_index
    pattern
    ~from:0
    ~matches:(fun kind -> Syntax_kind.(kind = MODULE_KW))

let first_class_module_pattern_binder = fun pattern ->
  match first_class_module_pattern_module_index pattern with
  | Some module_index -> (
      match child_token_at pattern (module_index + 1) with
      | Some token when token_kind_is token Syntax_kind.IDENT
      || token_kind_is token Syntax_kind.UNDERSCORE ->
          Ident.from_child_range_option
            pattern
            ~start_index:(module_index + 1)
            ~stop_index:(module_index + 2)
      | _ -> None
    )
  | None -> None

let first_class_module_pattern_range_is_ident = fun pattern start stop ->
  let rec loop index saw_ident expect_ident =
    if index >= stop then
      saw_ident && not expect_ident
    else
      match child_token_at pattern index with
      | Some token when token_kind_is token Syntax_kind.IDENT && expect_ident ->
          loop (index + 1) true false
      | Some token when token_kind_is token Syntax_kind.DOT && saw_ident && not expect_ident ->
          loop (index + 1) saw_ident true
      | _ -> false
  in
  loop start false true

let first_class_module_pattern_ascription_bounds = fun pattern ->
  match first_class_module_pattern_token_index
    ~from:0
    pattern
    ~matches:(fun kind -> Syntax_kind.(kind = COLON)) with
  | None -> None
  | Some colon_index ->
      let start = colon_index + 1 in
      first_class_module_pattern_token_index
        pattern
        ~from:start
        ~matches:(fun kind -> Syntax_kind.(kind = RPAREN))
      |> Option.map ~fn:(fun stop -> (start, stop))

let first_class_module_pattern_ascription = fun pattern ->
  match (
    Node.first_child_token pattern ~kind:Syntax_kind.COLON,
    first_class_module_pattern_ascription_bounds pattern
  ) with
  | (None, _) -> NoAscription
  | (Some _, Some (start, stop)) when first_class_module_pattern_range_is_ident pattern start stop ->
      IdentAscription
  | (Some _, _) -> UnsupportedAscription

let first_class_module_pattern_ascription_ident = fun pattern ->
  match first_class_module_pattern_ascription_bounds pattern with
  | Some (start, stop) when first_class_module_pattern_range_is_ident pattern start stop ->
      Ident.from_child_range_option pattern ~start_index:start ~stop_index:stop
  | _ -> None

module TypeExpr = struct
  type t = type_expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let text = Node.text

  type tuple_separator =
    | Star
    | Comma
    | UnknownSeparator

  type arrow_label = {
    name: token option;
    optional_: bool;
  }

  type view =
    | Ident of {
        ident: Ident.t;
      }
    | Var of {
        name: Token.t;
      }
    | Wildcard
    | Arrow of {
        label: arrow_label option;
        arg: t;
        ret: t;
      }
    | Forall of {
        names: Token.t Vector.t;
        body: t;
      }
    | Alias of {
        typ: t;
        name: Token.t;
      }
    | Tuple of {
        parts: t Vector.t;
      }
    | Apply of {
        ident: Ident.t;
        args: t Vector.t;
      }
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:type_expr_expected_kinds
      ~matches:is_type_expr_kind

  let tuple_separator = fun type_expr ->
    let rec loop index =
      if index >= Node.child_count type_expr then
        UnknownSeparator
      else
        match child_token_at type_expr index with
        | Some token when token_kind_is token Syntax_kind.STAR -> Star
        | Some token when token_kind_is token Syntax_kind.COMMA -> Comma
        | _ -> loop (index + 1)
    in
    loop 0

  let rec unwrap_poly_node = fun type_expr ->
    if node_kind_is type_expr Syntax_kind.TYPE_EXPR then
      match first_child_node_matching type_expr ~matches:is_type_expr_kind with
      | Some child -> unwrap_poly_node child
      | None -> type_expr
    else
      type_expr

  let child_type_exprs = fun type_expr ->
    let items = Vector.with_capacity ~size:(Node.child_count type_expr) in
    for_each_child_node_matching
      type_expr
      ~matches:is_type_expr_kind
      ~fn:(fun child -> Vector.push items ~value:(normalize_type_expr_node child));
    items

  let rec parenthesized_inner = fun type_expr ->
    match Node.kind type_expr with
    | Syntax_kind.TYPE_EXPR -> (
        match first_child_node_matching type_expr ~matches:is_type_expr_kind with
        | Some child -> parenthesized_inner child
        | None -> None
      )
    | Syntax_kind.PAREN_TYPE -> first_child_node_matching type_expr ~matches:is_type_expr_kind
    | _ -> Some type_expr

  let labeled_arrow_argument = fun type_expr ->
    match parenthesized_inner type_expr with
    | Some inner when node_kind_is inner Syntax_kind.LABELED_TYPE ->
        Some (
          {
            name = Ident.first_token inner;
            optional_ = Option.is_some
              (first_child_token_matching inner ~matches:(fun kind -> Syntax_kind.(kind = QUESTION)));
          },
          normalize_type_expr_option (first_child_node_matching inner ~matches:is_type_expr_kind)
        )
    | _ -> None

  let comma_tuple_parts = fun type_expr ->
    match parenthesized_inner type_expr with
    | Some inner when node_kind_is inner Syntax_kind.TUPLE_TYPE && tuple_separator inner = Comma ->
        Some (child_type_exprs inner)
    | _ -> None

  let tuple_parts = fun type_expr ->
    let parts = Vector.with_capacity ~size:(Node.child_count type_expr) in
    let rec collect item =
      match parenthesized_inner item with
      | Some inner when node_kind_is inner Syntax_kind.TUPLE_TYPE
      && tuple_separator inner = tuple_separator type_expr ->
          for_each_child_node_matching inner ~matches:is_type_expr_kind ~fn:collect
      | Some inner -> Vector.push parts ~value:(normalize_type_expr_node inner)
      | None -> ()
    in
    collect type_expr;
    parts

  let type_constructor_ident = fun type_expr ->
    match parenthesized_inner type_expr with
    | Some inner when node_kind_is inner Syntax_kind.PATH_TYPE -> Some inner
    | _ -> None

  let apply_parts = fun type_expr ->
    let rec loop expr args =
      match Node.kind expr with
      | Syntax_kind.APPLY_TYPE -> (
          let argument = nth_child_node_matching expr 0 ~matches:is_type_expr_kind in
          let constructor = nth_child_node_matching expr 1 ~matches:is_type_expr_kind in
          let args =
            match argument with
            | Some argument -> (
                match comma_tuple_parts argument with
                | Some tuple_args ->
                    Vector.for_each
                      tuple_args
                      ~fn:(fun arg -> Vector.push args ~value:(normalize_type_expr_node arg));
                    args
                | None ->
                    Vector.push args ~value:(normalize_type_expr_node argument);
                    args
              )
            | None -> args
          in
          match constructor with
          | Some constructor -> loop (normalize_type_expr_node constructor) args
          | None -> (None, args)
        )
      | Syntax_kind.TYPE_EXPR -> (
          match first_child_node_matching expr ~matches:is_type_expr_kind with
          | Some child -> loop child args
          | None -> (None, args)
        )
      | Syntax_kind.PATH_TYPE -> (Some expr, args)
      | _ -> (None, args)
    in
    loop type_expr (Vector.with_capacity ~size:(Node.child_count type_expr))

  let fold_poly_type_name = fun (type_expr: type_expr) ~init ~fn ->
    let type_expr = unwrap_poly_node type_expr in
    Node.fold_child_token
      type_expr
      ~init:(true, init)
      ~fn:(fun token (before_dot, acc) ->
        if token_kind_is token Syntax_kind.DOT then
          Return (false, acc)
        else if before_dot && token_kind_is token Syntax_kind.IDENT then
          match fn token acc with
          | Continue next -> Continue (before_dot, next)
          | Return value -> Return (before_dot, value)
        else
          Continue (before_dot, acc))
    |> fun (_, acc) -> acc

  let poly_type_name_count = fun type_expr -> count_fold fold_poly_type_name type_expr

  let for_each_poly_type_name = fun type_expr ~fn ->
    fold_poly_type_name
      type_expr
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let poly_type_names = fun type_expr ->
    let names = Vector.with_capacity ~size:(Node.child_count type_expr) in
    for_each_poly_type_name type_expr ~fn:(fun name -> Vector.push names ~value:name);
    names

  let alias_parts = fun type_expr ->
    let typ =
      first_child_node_matching type_expr ~matches:is_type_expr_kind
      |> normalize_type_expr_option
    in
    let name =
      first_child_node_matching type_expr ~matches:(fun kind -> Syntax_kind.(kind = VAR_TYPE))
      |> Option.and_then ~fn:Ident.last_token
    in
    match (typ, name) with
    | (Some typ, Some name) -> Some (typ, name)
    | _ -> None

  let rec view = fun (type_expr: type_expr) ->
    match Node.kind type_expr with
    | Syntax_kind.TYPE_EXPR -> (
        match (
          first_child_node_matching type_expr ~matches:is_type_expr_kind,
          nth_child_node_matching type_expr 1 ~matches:is_type_expr_kind
        ) with
        | (Some child, None) -> (
            match Node.kind child with
            | Syntax_kind.TYPE_EXPR -> Unknown type_expr
            | _ -> view child
          )
        | _ -> Unknown type_expr
      )
    | Syntax_kind.PATH_TYPE -> (
        match Ident.from_node_option type_expr with
        | Some ident when Ident.node_is_single_text type_expr "unit" ->
            Apply { ident; args = Vector.with_capacity ~size:0 }
        | Some ident -> Ident { ident }
        | None -> Unknown type_expr
      )
    | Syntax_kind.VAR_TYPE -> (
        match Ident.last_token type_expr with
        | Some name -> Var { name }
        | None -> Unknown type_expr
      )
    | Syntax_kind.WILDCARD_TYPE -> Wildcard
    | Syntax_kind.ARROW_TYPE -> (
        let left = nth_child_node_matching type_expr 0 ~matches:is_type_expr_kind in
        let ret = nth_child_node_matching type_expr 1 ~matches:is_type_expr_kind in
        let (label, arg) =
          match left with
          | Some left -> (
              match labeled_arrow_argument left with
              | Some (label, arg) -> (Some label, arg)
              | None -> (None, Some left)
            )
          | None -> (None, None)
        in
        match (normalize_type_expr_option arg, normalize_type_expr_option ret) with
        | (Some arg, Some ret) -> Arrow { label; arg; ret }
        | _ -> Unknown type_expr
      )
    | Syntax_kind.POLY_TYPE -> (
        match normalize_type_expr_option
          (first_child_node_matching type_expr ~matches:is_type_expr_kind) with
        | Some body -> Forall { names = poly_type_names type_expr; body }
        | None -> Unknown type_expr
      )
    | Syntax_kind.LABELED_TYPE -> Unknown type_expr
    | Syntax_kind.TUPLE_TYPE -> Tuple { parts = tuple_parts type_expr }
    | Syntax_kind.APPLY_TYPE ->
        let (ident, args) = apply_parts type_expr in
        (
          match ident with
          | Some ident -> (
              match Ident.from_node_option ident with
              | Some ident -> Apply { ident; args }
              | None -> Unknown type_expr
            )
          | None -> Unknown type_expr
        )
    | Syntax_kind.PAREN_TYPE -> (
        match first_child_node_matching type_expr ~matches:is_type_expr_kind with
        | Some inner -> view inner
        | None -> Unknown type_expr
      )
    | Syntax_kind.OPAQUE_TYPE -> (
        match alias_parts type_expr with
        | Some (typ, name) -> Alias { typ; name }
        | None -> Unknown type_expr
      )
    | Syntax_kind.ERROR -> Error type_expr
    | _ -> Unknown type_expr

  let fold_child_type = fun (type_expr: type_expr) ~init ~fn ->
    fold_child_node_matching
      type_expr
      ~matches:is_type_expr_kind
      ~init
      ~fn:(fun child acc -> fn (normalize_type_expr_node child) acc)

  let child_type_count = fun type_expr -> count_fold fold_child_type type_expr

  let for_each_child_type = fun type_expr ~fn ->
    fold_child_type
      type_expr
      ~init:()
      ~fn:(fun child () ->
        fn child;
        Continue ())

  let child_token_kind_is_in = fun node index kind ->
    match child_token_kind_at node index with
    | Some actual -> Syntax_kind.(actual = kind)
    | None -> false

  let attribute_suffix_start_at = fun node close_index ->
    if not (child_token_kind_is_in node close_index Syntax_kind.RBRACKET) then
      None
    else
      let rec loop index depth =
        if Int.(index < 0) then
          None
        else if child_token_kind_is_in node index Syntax_kind.RBRACKET then
          loop Int.(index - 1) Int.(depth + 1)
        else if child_token_kind_is_in node index Syntax_kind.LBRACKET then
          if Int.equal depth 1 then
            let next = Int.add index 1 in
            if
              child_token_kind_is_in node next Syntax_kind.AT
              || child_token_kind_is_in node next Syntax_kind.ATAT
            then
              Some index
            else
              None
          else
            loop Int.(index - 1) Int.(depth - 1)
        else
          loop Int.(index - 1) depth
      in
      loop Int.(close_index - 1) 1

  let last_non_attribute_suffix_child_index = fun node ->
    let rec loop index =
      if Int.(index < 0) then (
        (-1)
      ) else
        match attribute_suffix_start_at node index with
        | Some start -> loop Int.(start - 1)
        | None -> index
    in
    loop Int.(Node.child_count node - 1)

  let rec attribute_suffix_host = fun type_expr ->
    match Node.kind type_expr with
    | Syntax_kind.TYPE_EXPR -> (
        match (
          first_child_node_matching type_expr ~matches:is_type_expr_kind,
          nth_child_node_matching type_expr 1 ~matches:is_type_expr_kind
        ) with
        | (Some child, None) -> attribute_suffix_host child
        | _ -> type_expr
      )
    | _ -> type_expr

  let first_attribute_suffix_child_index = fun (type_expr: type_expr) ->
    let type_expr = attribute_suffix_host type_expr in
    let last_body_index = last_non_attribute_suffix_child_index type_expr in
    let first_suffix_index = Int.add last_body_index 1 in
    if Int.(first_suffix_index < Node.child_count type_expr) then
      match child_token_kind_at type_expr first_suffix_index with
      | Some Syntax_kind.LBRACKET -> Some first_suffix_index
      | _ -> None
    else
      None

  let inner_without_attribute_suffix = fun (type_expr: type_expr) ->
    let type_expr = attribute_suffix_host type_expr in
    match first_attribute_suffix_child_index type_expr with
    | None -> None
    | Some first_suffix_index ->
        let rec loop index =
          if Int.(index >= first_suffix_index) then
            None
          else
            match Node.child_at type_expr index with
            | Some (Syntax_tree.Node id) ->
                let node = wrap_node type_expr.tree id in
                if node_matches node is_type_expr_kind then
                  Some node
                else
                  loop (Int.add index 1)
            | Some (Syntax_tree.Token _)
            | Some (Syntax_tree.Missing _)
            | None -> loop (Int.add index 1)
        in
        loop 0

  let fold_attribute_suffix_token = fun (type_expr: type_expr) ~init ~fn ->
    let type_expr = attribute_suffix_host type_expr in
    match first_attribute_suffix_child_index type_expr with
    | None -> init
    | Some first_suffix_index ->
        let rec loop index acc =
          if Int.(index >= Node.child_count type_expr) then
            acc
          else
            match Node.child_at type_expr index with
            | Some (Syntax_tree.Token id) -> (
                match fn (wrap_token type_expr.tree id) acc with
                | Continue next -> loop (Int.add index 1) next
                | Return value -> value
              )
            | Some (Syntax_tree.Node id) ->
                let node = wrap_node type_expr.tree id in
                let acc = Node.fold_token node ~init:acc ~fn in
                loop (Int.add index 1) acc
            | Some (Syntax_tree.Missing _)
            | None -> loop (Int.add index 1) acc
        in
        loop first_suffix_index init

  let attribute_suffix_token_count = fun type_expr ->
    count_fold
      fold_attribute_suffix_token
      type_expr

  let for_each_attribute_suffix_token = fun type_expr ~fn ->
    fold_attribute_suffix_token
      type_expr
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let poly_type_keyword_token = fun (type_expr: type_expr) ->
    let type_expr = unwrap_poly_node type_expr in
    first_child_token_matching type_expr ~matches:(fun kind -> Syntax_kind.(kind = TYPE_KW))
end

module RecordField = struct
  type t = record_field

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Field of {
        mutable_token: Token.t option;
        name: Ident.t;
        colon_token: Token.t;
        annotation: type_expr;
      }
    | Unknown of node

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:[ Syntax_kind.RECORD_FIELD ]
      ~matches:is_record_field_kind

  let mutable_token = fun field -> Node.first_child_token field ~kind:Syntax_kind.MUTABLE_KW

  let name = Ident.first

  let colon_token = fun field -> Node.first_child_token field ~kind:Syntax_kind.COLON

  let type_annotation = fun field -> first_child_node_matching field ~matches:is_type_expr_kind

  let view = fun field ->
    match (name field, colon_token field, type_annotation field) with
    | (Some name, Some colon_token, Some annotation) ->
        Field {
          mutable_token = mutable_token field;
          name;
          colon_token;
          annotation;
        }
    | _ -> Unknown field
end

module RecordType = struct
  type t = record_type

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:[ Syntax_kind.RECORD_TYPE ]
      ~matches:is_record_type_kind

  let private_token = fun record_type ->
    Node.first_child_token
      record_type
      ~kind:Syntax_kind.PRIVATE_KW

  let opening_token = fun record_type -> Node.first_child_token record_type ~kind:Syntax_kind.LBRACE

  let closing_token = fun record_type -> Node.first_child_token record_type ~kind:Syntax_kind.RBRACE

  let fold_field = fun record_type ~init ~fn ->
    fold_child_node_matching
      record_type
      ~matches:is_record_field_kind
      ~init
      ~fn

  let field_count = fun record_type -> count_fold fold_field record_type

  let for_each_field = fun record_type ~fn ->
    fold_field
      record_type
      ~init:()
      ~fn:(fun field () ->
        fn field;
        Continue ())
end

module RecordExprField = struct
  type t = record_expr_field

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width
end

module VariantConstructor = struct
  type t = variant_constructor

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type payload =
    | TypeExpr of type_expr
    | Record of record_type

  type rhs =
    | Plain
    | Payload of {
        of_token: Token.t;
        payload: payload;
      }
    | Gadt of {
        colon_token: Token.t;
        record_payload: record_type option;
        arrow_token: Token.t option;
        result: type_expr;
      }

  type view =
    | Constructor of {
        pipe_token: Token.t option;
        name: Ident.t;
        rhs: rhs;
      }
    | Unknown of node

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:[ Syntax_kind.VARIANT_CONSTRUCTOR ]
      ~matches:is_variant_constructor_kind

  let pipe_token = fun constructor -> Node.first_child_token constructor ~kind:Syntax_kind.PIPE

  let name = Ident.first

  let of_token = fun constructor -> Node.first_child_token constructor ~kind:Syntax_kind.OF_KW

  let colon_token = fun constructor -> Node.first_child_token constructor ~kind:Syntax_kind.COLON

  let arrow_token = fun constructor -> Node.first_child_token constructor ~kind:Syntax_kind.ARROW

  let payload_type = fun constructor ->
    match of_token constructor with
    | Some _ -> first_child_node_matching constructor ~matches:is_type_expr_kind
    | None -> None

  let result_type = fun constructor ->
    match colon_token constructor with
    | Some _ -> first_child_node_matching constructor ~matches:is_type_expr_kind
    | None -> None

  let record_payload = fun constructor ->
    first_child_node_matching
      constructor
      ~matches:is_record_type_kind

  let payload = fun constructor ->
    match record_payload constructor with
    | Some record -> Some (Record record)
    | None -> (
        match payload_type constructor with
        | Some type_expr -> Some (TypeExpr type_expr)
        | None -> None
      )

  let rhs = fun constructor ->
    match colon_token constructor with
    | Some colon_token -> (
        match result_type constructor with
        | Some result ->
            Some (
              Gadt {
                colon_token;
                record_payload = record_payload constructor;
                arrow_token = arrow_token constructor;
                result;
              }
            )
        | None -> None
      )
    | None -> (
        match (of_token constructor, payload constructor) with
        | (Some of_token, Some payload) -> Some (Payload { of_token; payload })
        | (None, _) -> Some Plain
        | (Some _, None) -> None
      )

  let view = fun constructor ->
    match (name constructor, rhs constructor) with
    | (Some name, Some rhs) -> Constructor { pipe_token = pipe_token constructor; name; rhs }
    | _ -> Unknown constructor
end

module VariantType = struct
  type t = variant_type

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:[ Syntax_kind.VARIANT_TYPE ]
      ~matches:is_variant_type_kind

  let private_token = fun variant_type ->
    Node.first_child_token
      variant_type
      ~kind:Syntax_kind.PRIVATE_KW

  let fold_constructor = fun variant_type ~init ~fn ->
    fold_child_node_matching
      variant_type
      ~matches:is_variant_constructor_kind
      ~init
      ~fn

  let constructor_count = fun variant_type -> count_fold fold_constructor variant_type

  let for_each_constructor = fun variant_type ~fn ->
    fold_constructor
      variant_type
      ~init:()
      ~fn:(fun constructor () ->
        fn constructor;
        Continue ())
end

module ModuleTypeExpr = struct
  type t = module_type_expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Ident of {
        ident: Ident.t;
      }
    | Signature of {
        body: node;
      }
    | With of {
        body: node;
        base: t option;
        constraints: module_type_constraint Vector.t;
      }
    | Typeof of {
        body: module_expr option;
      }
    | Functor of {
        body: node;
      }
    | Error of node
    | Unknown of node

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:module_type_expected_kinds
      ~matches:is_module_type_kind

  let rec specific_node = fun module_type ->
    match Node.kind module_type with
    | Syntax_kind.MODULE_TYPE_EXPR
    | Syntax_kind.PAREN_MODULE_TYPE -> (
        match first_child_node_matching module_type ~matches:is_module_type_kind with
        | Some child -> specific_node child
        | None -> None
      )
    | kind when is_module_type_kind kind -> Some module_type
    | _ -> None

  let constraints = fun module_type ->
    let items = Vector.with_capacity ~size:(Node.child_count module_type) in
    Node.for_each_child_node
      module_type
      ~fn:(fun child ->
        if
          node_kind_is child Syntax_kind.WITH_TYPE_CONSTRAINT
          || node_kind_is child Syntax_kind.WITH_MODULE_CONSTRAINT
        then
          Vector.push items ~value:child);
    items

  let view = fun module_type ->
    match specific_node module_type with
    | Some node when node_kind_is node Syntax_kind.PATH_MODULE_TYPE -> (
        match Ident.cast node with
        | Node ident -> Ident { ident }
        | Unknown _
        | Error _ -> Unknown node
      )
    | Some node when node_kind_is node Syntax_kind.SIGNATURE_MODULE_TYPE ->
        Signature { body = node }
    | Some node when node_kind_is node Syntax_kind.WITH_MODULE_TYPE ->
        let base =
          match first_child_node_matching node ~matches:is_module_type_kind with
          | Some base -> cast_result_to_option (cast base)
          | None -> None
        in
        With { body = node; base; constraints = constraints node }
    | Some node when node_kind_is node Syntax_kind.TYPEOF_MODULE_TYPE ->
        Typeof { body = first_child_node_matching node ~matches:is_module_expr_kind }
    | Some node when node_kind_is node Syntax_kind.FUNCTOR_MODULE_TYPE -> Functor { body = node }
    | Some node when node_kind_is node Syntax_kind.ERROR -> Error node
    | Some node -> Unknown node
    | None -> Unknown module_type

  let signature_body_node = fun module_type ->
    match specific_node module_type with
    | Some node when node_kind_is node Syntax_kind.SIGNATURE_MODULE_TYPE -> Some node
    | _ -> None

  let sig_token = fun module_type ->
    match signature_body_node module_type with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.SIG_KW
    | None -> None

  let end_token = fun module_type ->
    match signature_body_node module_type with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.END_KW
    | None -> None

  let ident = fun module_type ->
    match view module_type with
    | Ident { ident } -> Some ident
    | _ -> None

  let fold_signature_item = fun module_type ~init ~fn ->
    match signature_body_node module_type with
    | Some node ->
        fold_child_node_matching
          node
          ~matches:(fun kind -> Syntax_kind.(kind = SIGNATURE_ITEM))
          ~init
          ~fn
    | None -> init

  let for_each_signature_item = fun module_type ~fn ->
    fold_signature_item
      module_type
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let signature_item_count = fun module_type -> count_fold fold_signature_item module_type

  let fold_sig_body_token = fun module_type ~init ~fn ->
    match signature_body_node module_type with
    | None -> init
    | Some node ->
        let acc = ref init in
        let inside = ref false in
        let returned = ref false in
        Node.for_each_child
          node
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Syntax_tree.Token id ->
                if not !returned then (
                  let token = wrap_token node.tree id in
                  if token_kind_is token Syntax_kind.SIG_KW then
                    inside := true
                  else if token_kind_is token Syntax_kind.END_KW then
                    inside := false
                  else if !inside then
                    match fn token !acc with
                    | Continue next -> acc := next
                    | Return value ->
                        acc := value;
                        returned := true
                )
            | Syntax_tree.Node id ->
                if !inside && not !returned then (
                  let next =
                    Node.fold_token
                      (wrap_node node.tree id)
                      ~init:!acc
                      ~fn:(fun token acc ->
                        match fn token acc with
                        | Continue next -> Continue next
                        | Return value ->
                            returned := true;
                            Return value)
                  in
                  acc := next
                )
            | Syntax_tree.Missing _ -> ());
        !acc

  let for_each_sig_body_token = fun module_type ~fn ->
    fold_sig_body_token
      module_type
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let sig_body_token_count = fun module_type -> count_fold fold_sig_body_token module_type
end

module ModuleExpr = struct
  type t = module_expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Ident of {
        ident: Ident.t;
      }
    | Structure of {
        body: node;
      }
    | Functor of {
        body: node;
      }
    | Apply of {
        body: node;
        callee: t option;
        argument: t option;
      }
    | Constraint of {
        body: node;
        expr: t option;
        ascription: module_type_expr option;
      }
    | Opaque of node
    | Error of node
    | Unknown of node

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:module_expr_expected_kinds
      ~matches:is_module_expr_kind

  let rec specific_node = fun module_expr ->
    match Node.kind module_expr with
    | Syntax_kind.MODULE_EXPR
    | Syntax_kind.PAREN_MODULE_EXPR -> (
        match first_child_node_matching module_expr ~matches:is_module_expr_kind with
        | Some child -> specific_node child
        | None -> None
      )
    | kind when is_module_expr_kind kind -> Some module_expr
    | _ -> None

  let nth_specific_child = fun module_expr n ->
    match nth_child_node_matching module_expr n ~matches:is_module_expr_kind with
    | Some child -> cast_result_to_option (cast child)
    | None -> None

  let view = fun module_expr ->
    match specific_node module_expr with
    | Some node when node_kind_is node Syntax_kind.PATH_MODULE_EXPR -> (
        match Ident.cast node with
        | Node ident -> Ident { ident }
        | Unknown _
        | Error _ -> Unknown node
      )
    | Some node when node_kind_is node Syntax_kind.STRUCT_MODULE_EXPR -> Structure { body = node }
    | Some node when node_kind_is node Syntax_kind.FUNCTOR_MODULE_EXPR -> Functor { body = node }
    | Some node when node_kind_is node Syntax_kind.APPLY_MODULE_EXPR ->
        Apply {
          body = node;
          callee = nth_specific_child node 0;
          argument = nth_specific_child node 1;
        }
    | Some node when node_kind_is node Syntax_kind.CONSTRAINT_MODULE_EXPR ->
        Constraint {
          body = node;
          expr =
            first_child_node_matching node ~matches:is_module_expr_kind
            |> Option.and_then ~fn:(fun node -> cast_result_to_option (cast node));
          ascription = first_child_node_matching node ~matches:is_module_type_kind;
        }
    | Some node when node_kind_is node Syntax_kind.OPAQUE_MODULE_EXPR -> Opaque node
    | Some node when node_kind_is node Syntax_kind.ERROR -> Error node
    | Some node -> Unknown node
    | None -> Unknown module_expr

  let structure_body_node = fun module_expr ->
    match view module_expr with
    | Structure { body } -> Some body
    | _ -> None

  let struct_token = fun module_expr ->
    match structure_body_node module_expr with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.STRUCT_KW
    | None -> None

  let end_token = fun module_expr ->
    match structure_body_node module_expr with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.END_KW
    | None -> None

  let ident = fun module_expr ->
    match view module_expr with
    | Ident { ident } -> Some ident
    | _ -> None

  let fold_structure_item = fun module_expr ~init ~fn ->
    match structure_body_node module_expr with
    | Some node ->
        fold_child_node_matching
          node
          ~matches:(fun kind -> Syntax_kind.(kind = STRUCTURE_ITEM))
          ~init
          ~fn
    | None -> init

  let for_each_structure_item = fun module_expr ~fn ->
    fold_structure_item
      module_expr
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let structure_item_count = fun module_expr -> count_fold fold_structure_item module_expr
end

module Expr: sig
  type t = expr
  type fun_body =
    | Body_expr of t
    | Body_cases of {
        first_case: match_case;
      }
  type view =
    | Unit
    | Let of {
        first_binding: let_binding;
        body: t;
      }
    | LocalOpen of { body: t }
    | LetModule of { body: t }
    | LetException of { body: t }
    | If of {
        condition: t;
        then_branch: t;
        else_branch: t option;
      }
    | Match of {
        scrutinee: t;
        first_case: match_case;
      }
    | Fun of {
        parameters: parameter Vector.t;
        return_annotation: type_expr option;
        body: fun_body;
      }
    | Try of {
        body: t;
        first_case: match_case;
      }
    | While of { condition: t; body: t }
    | For of {
        pattern: pattern;
        start_: t;
        stop: t;
        body: t;
      }
    | Sequence of {
        left: t;
        right: t option;
      }
    | Apply of { callee: t; argument: t }
    | Infix of {
        left: t;
        operator: token;
        right: t;
      }
    | Prefix of {
        operator: token;
        operand: t;
      }
    | Assign of {
        target: t;
        operator: token;
        value: t;
      }
    | FieldAccess of {
        target: t;
        field: Ident.t;
      }
    | PolyVariant of {
        tag: token;
        payload: t option;
      }
    | Constructor of {
        constructor: Ident.t;
        payload: t option;
      }
    | Ident of {
        ident: Ident.t;
      }
    | Literal of {
        token: token;
      }
    | Tuple of {
        items: t Vector.t;
      }
    | List of {
        items: t Vector.t;
      }
    | Array of {
        items: t Vector.t;
      }
    | Record of {
        base: t option;
        fields: record_expr_field_view Vector.t;
      }
    | Annotated of {
        expr: t;
        annotation: type_expr;
      }
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val literal_token: t -> token option

  val list_has_trailing_separator: t -> bool

  val fold_child_expr: t -> init:'acc -> fn:(t -> 'acc -> 'acc control) -> 'acc

  val child_expr_count: t -> int

  val for_each_child_expr: t -> fn:(t -> unit) -> unit

  val fold_match_case: t -> init:'acc -> fn:(match_case -> 'acc -> 'acc control) -> 'acc

  val match_case_count: t -> int

  val for_each_match_case: t -> fn:(match_case -> unit) -> unit

  val fold_parameter: t -> init:'acc -> fn:(parameter -> 'acc -> 'acc control) -> 'acc

  val parameter_count: t -> int
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type fun_body =
    | Body_expr of t
    | Body_cases of {
        first_case: match_case;
      }

  type view =
    | Unit
    | Let of {
        first_binding: let_binding;
        body: t;
      }
    | LocalOpen of { body: t }
    | LetModule of { body: t }
    | LetException of { body: t }
    | If of {
        condition: t;
        then_branch: t;
        else_branch: t option;
      }
    | Match of {
        scrutinee: t;
        first_case: match_case;
      }
    | Fun of {
        parameters: parameter Vector.t;
        return_annotation: type_expr option;
        body: fun_body;
      }
    | Try of {
        body: t;
        first_case: match_case;
      }
    | While of { condition: t; body: t }
    | For of {
        pattern: pattern;
        start_: t;
        stop: t;
        body: t;
      }
    | Sequence of {
        left: t;
        right: t option;
      }
    | Apply of { callee: t; argument: t }
    | Infix of {
        left: t;
        operator: token;
        right: t;
      }
    | Prefix of {
        operator: token;
        operand: t;
      }
    | Assign of {
        target: t;
        operator: token;
        value: t;
      }
    | FieldAccess of {
        target: t;
        field: Ident.t;
      }
    | PolyVariant of {
        tag: token;
        payload: t option;
      }
    | Constructor of {
        constructor: Ident.t;
        payload: t option;
      }
    | Ident of {
        ident: Ident.t;
      }
    | Literal of {
        token: token;
      }
    | Tuple of {
        items: t Vector.t;
      }
    | List of {
        items: t Vector.t;
      }
    | Array of {
        items: t Vector.t;
      }
    | Record of {
        base: t option;
        fields: record_expr_field_view Vector.t;
      }
    | Annotated of {
        expr: t;
        annotation: type_expr;
      }
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:expr_expected_kinds
      ~matches:is_expr_kind

  let first_operator_token = fun (node: node) ->
    first_child_token_matching
      node
      ~matches:(fun kind ->
        not
          (Syntax_kind.(kind = IDENT)
          || Syntax_kind.(kind = INT)
          || Syntax_kind.(kind = FLOAT)
          || Syntax_kind.(kind = STRING)
          || Syntax_kind.(kind = CHAR)
          || Syntax_kind.(kind = TRUE_KW)
          || Syntax_kind.(kind = FALSE_KW)))

  let first_direct_token = fun (node: node) ->
    first_child_token_matching
      node
      ~matches:(fun _kind -> true)

  let literal_token = Node.first_token

  let child_exprs = fun expr ->
    let items = Vector.with_capacity ~size:(Node.child_count expr) in
    for_each_child_node_matching
      expr
      ~matches:is_expr_kind
      ~fn:(fun child -> Vector.push items ~value:(normalize_expr_node child));
    items

  let record_expr_base = fun record ->
    if node_kind_is record Syntax_kind.RECORD_UPDATE_EXPR then
      normalize_expr_option (nth_expr_child record 0)
    else
      None

  let record_expr_field_of_node = fun (field: record_expr_field) ->
    let normalize_value = normalize_expr_option in
    let field_view = fun ident value -> RecordExprField { ident; value; node = field } in
    let unknown () = UnknownRecordExprField { node = field } in
    match (nth_expr_child field 0, nth_expr_child field 1) with
    | (Some expr, value) when node_kind_is expr Syntax_kind.PATH_EXPR -> (
        match Ident.cast expr with
        | Node ident -> field_view ident (normalize_value value)
        | Unknown _
        | Error _ -> unknown ()
      )
    | (Some expr, _) when node_kind_is expr Syntax_kind.INFIX_EXPR -> (
        match (nth_expr_child expr 0, nth_expr_child expr 1) with
        | (Some left, Some right) ->
            if node_kind_is left Syntax_kind.PATH_EXPR then
              match Ident.cast left with
              | Node ident -> field_view ident (Some (normalize_expr_node right))
              | Unknown _
              | Error _ -> unknown ()
            else
              unknown ()
        | _ -> unknown ()
      )
    | (Some expr, value) -> (
        match Ident.cast expr with
        | Node ident -> field_view ident (normalize_value value)
        | Unknown _
        | Error _ -> unknown ()
      )
    | (None, _) -> unknown ()

  let record_expr_fields = fun record ->
    let fields = Vector.with_capacity ~size:(Node.child_count record) in
    Syntax_tree.for_each_child
      record.tree
      (syntax_node record)
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Syntax_tree.Node id ->
            let child = wrap_node record.tree id in
            if node_matches child is_record_expr_field_kind then
              Vector.push fields ~value:(record_expr_field_of_node child)
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> ());
    fields

  let list_has_trailing_separator = fun (expr: expr) ->
    if not (node_kind_is expr Syntax_kind.LIST_EXPR) then
      false
    else
      (
        let item_count = ref 0 in
        let separator_count = ref 0 in
        for_each_child_node_matching
          expr
          ~matches:is_expr_kind
          ~fn:(fun _ -> item_count := Int.add !item_count 1);
        Node.for_each_child_token
          expr
          ~fn:(fun token ->
            if Syntax_kind.(Token.kind token = SEMI) then
              separator_count := Int.add !separator_count 1);
        let items = !item_count in
        let separators = !separator_count in
        Int.(items > 0 && separators >= items)
      )

  let fun_parameters = fun expr ->
    let parameters = Vector.with_capacity ~size:(Node.child_count expr) in
    for_each_child_node_matching
      expr
      ~matches:is_parameter_node_kind
      ~fn:(fun parameter ->
        ignore
          (
            fold_parameter_spine_node
              parameter
              ~acc:()
              ~fn:(fun parameter () ->
                Vector.push parameters ~value:parameter;
                Continue ())
          ));
    parameters

  let ident_is_constructor = fun ident ->
    match Ident.last_token ident with
    | None -> false
    | Some ident ->
        let text = Token.text ident in
        if Int.equal (String.length text) 0 then
          false
        else
          match String.get_unchecked text ~at:0 with
          | 'A' .. 'Z' -> true
          | _ -> false

  let field_access_ident = fun expr ->
    let child_count = Node.child_count expr in
    let rec first_dot index =
      if Int.(index >= child_count) then
        None
      else
        match child_token_at expr index with
        | Some token when token_kind_is token Syntax_kind.DOT -> Some index
        | _ -> first_dot Int.(index + 1)
    in
    match first_dot 0 with
    | Some dot_index ->
        Ident.from_child_range_option expr ~start_index:Int.(dot_index + 1) ~stop_index:child_count
    | None -> None

  let rec view = fun (expr: expr) ->
    match Node.kind expr with
    | Syntax_kind.PAREN_EXPR -> (
        match first_expr_child expr with
        | Some inner -> view inner
        | None -> Unit
      )
    | Syntax_kind.LET_EXPR -> (
        match (first_let_binding_child expr, normalize_expr_option (nth_expr_child expr 0)) with
        | (Some first_binding, Some body) -> Let { first_binding; body }
        | _ -> Unknown expr
      )
    | Syntax_kind.LOCAL_OPEN_EXPR -> (
        match normalize_expr_option (nth_expr_child expr 1) with
        | Some body -> LocalOpen { body }
        | None -> Unknown expr
      )
    | Syntax_kind.LET_MODULE_EXPR -> (
        match normalize_expr_option (first_expr_child expr) with
        | Some body -> LetModule { body }
        | None -> Unknown expr
      )
    | Syntax_kind.LET_EXCEPTION_EXPR -> (
        match normalize_expr_option (first_expr_child expr) with
        | Some body -> LetException { body }
        | None -> Unknown expr
      )
    | Syntax_kind.BINDING_OPERATOR_EXPR -> (
        match (first_let_binding_child expr, normalize_expr_option (nth_expr_child expr 0)) with
        | (Some first_binding, Some body) -> Let { first_binding; body }
        | _ -> Unknown expr
      )
    | Syntax_kind.FIRST_CLASS_MODULE_EXPR
    | Syntax_kind.EXTENSION_EXPR
    | Syntax_kind.UNREACHABLE_EXPR -> Unknown expr
    | Syntax_kind.IF_EXPR -> (
        match (
          normalize_expr_option (nth_expr_child expr 0),
          normalize_expr_option (nth_expr_child expr 1)
        ) with
        | (Some condition, Some then_branch) ->
            If {
              condition;
              then_branch;
              else_branch = normalize_expr_option (nth_expr_child expr 2);
            }
        | _ -> Unknown expr
      )
    | Syntax_kind.MATCH_EXPR -> (
        match (normalize_expr_option (nth_expr_child expr 0), first_match_case_child expr) with
        | (Some scrutinee, Some first_case) -> Match { scrutinee; first_case }
        | _ -> Unknown expr
      )
    | Syntax_kind.FUN_EXPR -> (
        match normalize_expr_option (last_expr_child expr) with
        | Some body ->
            Fun {
              parameters = fun_parameters expr;
              return_annotation = normalize_type_expr_option (first_type_expr_child expr);
              body = Body_expr body;
            }
        | None -> Unknown expr
      )
    | Syntax_kind.FUNCTION_EXPR -> (
        match first_match_case_child expr with
        | Some first_case ->
            Fun {
              parameters = Vector.with_capacity ~size:0;
              return_annotation = None;
              body = Body_cases { first_case };
            }
        | None -> Unknown expr
      )
    | Syntax_kind.TRY_EXPR -> (
        match (normalize_expr_option (nth_expr_child expr 0), first_match_case_child expr) with
        | (Some body, Some first_case) -> Try { body; first_case }
        | _ -> Unknown expr
      )
    | Syntax_kind.WHILE_EXPR -> (
        match (
          normalize_expr_option (nth_expr_child expr 0),
          normalize_expr_option (nth_expr_child expr 1)
        ) with
        | (Some condition, Some body) -> While { condition; body }
        | _ -> Unknown expr
      )
    | Syntax_kind.FOR_EXPR -> (
        match (
          normalize_pattern_option (first_pattern_child expr),
          normalize_expr_option (nth_expr_child expr 0),
          normalize_expr_option (nth_expr_child expr 1),
          normalize_expr_option (nth_expr_child expr 2)
        ) with
        | (Some pattern, Some start_, Some stop, Some body) ->
            For {
              pattern;
              start_;
              stop;
              body;
            }
        | _ -> Unknown expr
      )
    | Syntax_kind.ASSERT_EXPR
    | Syntax_kind.LAZY_EXPR -> Unknown expr
    | Syntax_kind.ATTRIBUTE_EXPR -> (
        match first_expr_child expr with
        | Some inner -> view inner
        | None -> Unknown expr
      )
    | Syntax_kind.SEQUENCE_EXPR -> (
        match normalize_expr_option (nth_expr_child expr 0) with
        | Some left -> Sequence { left; right = normalize_expr_option (nth_expr_child expr 1) }
        | _ -> Unknown expr
      )
    | Syntax_kind.APPLY_EXPR -> (
        match (
          normalize_expr_option (nth_expr_child expr 0),
          normalize_expr_option (nth_expr_child expr 1)
        ) with
        | (Some callee, Some argument) -> (
            match view callee with
            | Constructor { constructor; payload = None } ->
                Constructor { constructor; payload = Some argument }
            | _ -> Apply { callee; argument }
          )
        | _ -> Unknown expr
      )
    | Syntax_kind.INFIX_EXPR -> (
        match (
          normalize_expr_option (nth_expr_child expr 0),
          first_direct_token expr,
          normalize_expr_option (nth_expr_child expr 1)
        ) with
        | (Some left, Some operator, Some right) -> Infix { left; operator; right }
        | _ -> Unknown expr
      )
    | Syntax_kind.PREFIX_EXPR -> (
        match (first_operator_token expr, normalize_expr_option (first_expr_child expr)) with
        | (Some operator, Some operand) -> Prefix { operator; operand }
        | _ -> Unknown expr
      )
    | Syntax_kind.ASSIGN_EXPR -> (
        match (
          normalize_expr_option (nth_expr_child expr 0),
          first_direct_token expr,
          normalize_expr_option (nth_expr_child expr 1)
        ) with
        | (Some target, Some operator, Some value) -> Assign { target; operator; value }
        | _ -> Unknown expr
      )
    | Syntax_kind.FIELD_ACCESS_EXPR -> (
        match (normalize_expr_option (nth_expr_child expr 0), field_access_ident expr) with
        | (Some target, Some field) -> FieldAccess { target; field }
        | _ -> Unknown expr
      )
    | Syntax_kind.POLY_VARIANT_EXPR -> (
        match first_child_token_matching expr ~matches:(fun kind -> Syntax_kind.(kind = IDENT)) with
        | Some tag -> PolyVariant { tag; payload = normalize_expr_option (first_expr_child expr) }
        | None -> Unknown expr
      )
    | Syntax_kind.PATH_EXPR -> (
        match Ident.from_node_option expr with
        | None -> Unknown expr
        | Some ident ->
            if ident_is_constructor expr then
              Constructor { constructor = ident; payload = None }
            else
              Ident { ident }
      )
    | Syntax_kind.LITERAL_EXPR -> (
        match literal_token expr with
        | Some token -> Literal { token }
        | None -> Unknown expr
      )
    | Syntax_kind.TUPLE_EXPR -> Tuple { items = child_exprs expr }
    | Syntax_kind.LIST_EXPR -> List { items = child_exprs expr }
    | Syntax_kind.ARRAY_EXPR -> Array { items = child_exprs expr }
    | Syntax_kind.RECORD_EXPR
    | Syntax_kind.RECORD_UPDATE_EXPR ->
        Record { base = record_expr_base expr; fields = record_expr_fields expr }
    | Syntax_kind.ARRAY_INDEX_EXPR -> Unknown expr
    | Syntax_kind.STRING_INDEX_EXPR -> Unknown expr
    | Syntax_kind.TYPED_EXPR -> (
        match (
          normalize_expr_option (first_expr_child expr),
          normalize_type_expr_option (first_type_expr_child expr)
        ) with
        | (Some expr, Some annotation) -> Annotated { expr; annotation }
        | _ -> Unknown expr
      )
    | Syntax_kind.LABELED_ARG
    | Syntax_kind.OPTIONAL_ARG -> Unknown expr
    | Syntax_kind.ERROR -> Error expr
    | _ -> Unknown expr

  let fold_child_expr = fun (expr: expr) ~init ~fn ->
    fold_child_node_matching
      expr
      ~matches:is_expr_kind
      ~init
      ~fn:(fun child acc -> fn (normalize_expr_node child) acc)

  let child_expr_count = fun expr -> count_fold fold_child_expr expr

  let for_each_child_expr = fun expr ~fn ->
    fold_child_expr
      expr
      ~init:()
      ~fn:(fun child () ->
        fn child;
        Continue ())

  let fold_match_case = fun (expr: expr) ~init ~fn ->
    fold_child_node_matching
      expr
      ~matches:is_match_case_kind
      ~init
      ~fn

  let match_case_count = fun expr -> count_fold fold_match_case expr

  let for_each_match_case = fun expr ~fn ->
    fold_match_case
      expr
      ~init:()
      ~fn:(fun case () ->
        fn case;
        Continue ())

  let fold_parameter = fun (expr: expr) ~init ~fn ->
    fold_child_node_matching
      expr
      ~matches:is_parameter_node_kind
      ~init
      ~fn:(fun parameter acc ->
        fold_parameter_spine_node parameter ~acc ~fn)

  let parameter_count = fun expr -> count_fold fold_parameter expr
end

module AttributeExpr: sig
  type t = expr

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val inner: t -> expr option

  val fold_shell_token: t -> init:'acc -> fn:(token -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int

  val for_each_shell_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (expr: expr) -> cast_kind expr Syntax_kind.ATTRIBUTE_EXPR

  let inner = first_expr_child

  let fold_shell_token = Node.fold_child_token

  let shell_token_count = fun expr -> count_fold fold_shell_token expr

  let for_each_shell_token = Node.for_each_child_token
end

module ExtensionExpr: sig
  type t = expr

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_shell_token: t -> init:'acc -> fn:(token -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int

  val for_each_shell_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (expr: expr) -> cast_kind expr Syntax_kind.EXTENSION_EXPR

  let fold_shell_token = Node.fold_child_token

  let shell_token_count = fun expr -> count_fold fold_shell_token expr

  let for_each_shell_token = Node.for_each_child_token
end

module RecordExpr: sig
  type t = expr
  type field = record_expr_field_view

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val base: t -> expr option

  val fold_field: t -> init:'acc -> fn:(field -> 'acc -> 'acc control) -> 'acc

  val field_count: t -> int

  val for_each_field: t -> fn:(field -> unit) -> unit
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type field = record_expr_field_view

  let cast = fun (expr: expr) ->
    if
      node_kind_is expr Syntax_kind.RECORD_EXPR || node_kind_is expr Syntax_kind.RECORD_UPDATE_EXPR
    then
      Node expr
    else
      cast_failure expr ~expected:Syntax_kind.[ RECORD_EXPR; RECORD_UPDATE_EXPR ]

  let base = fun (record: t) ->
    if node_kind_is record Syntax_kind.RECORD_UPDATE_EXPR then
      nth_expr_child record 0
    else
      None

  let field_of_node = fun (field: record_expr_field) ->
    let field_view = fun ident value -> RecordExprField { ident; value; node = field } in
    let unknown () = UnknownRecordExprField { node = field } in
    match (nth_expr_child field 0, nth_expr_child field 1) with
    | (Some expr, value) when node_kind_is expr Syntax_kind.PATH_EXPR -> (
        match Ident.cast expr with
        | Node ident -> field_view ident value
        | Unknown _
        | Error _ -> unknown ()
      )
    | (Some expr, _) when node_kind_is expr Syntax_kind.INFIX_EXPR -> (
        match (nth_expr_child expr 0, nth_expr_child expr 1) with
        | (Some left, Some right) ->
            if node_kind_is left Syntax_kind.PATH_EXPR then
              match Ident.cast left with
              | Node ident -> field_view ident (Some right)
              | Unknown _
              | Error _ -> unknown ()
            else
              unknown ()
        | _ -> unknown ()
      )
    | (Some expr, value) -> (
        match Ident.cast expr with
        | Node ident -> field_view ident value
        | Unknown _
        | Error _ -> unknown ()
      )
    | (None, _) -> unknown ()

  let fold_field = fun (record: t) ~init ~fn ->
    fold_child_node_matching
      record
      ~matches:is_record_expr_field_kind
      ~init
      ~fn:(fun child acc -> fn (field_of_node child) acc)

  let field_count = fun record -> count_fold fold_field record

  let for_each_field = fun record ~fn ->
    fold_field
      record
      ~init:()
      ~fn:(fun field () ->
        fn field;
        Continue ())
end

module LocalOpenExpr: sig
  type t = expr
  type view =
    | LetOpen of {
        let_token: token;
        open_token: token;
        bang_token: token option;
        module_ident: Ident.t;
        in_token: token;
        body: expr;
      }
    | Delimited of {
        module_ident: Ident.t;
        dot_token: token;
        opening_token: token;
        body: expr;
        closing_token: token;
      }
    | Unknown of node

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | LetOpen of {
        let_token: token;
        open_token: token;
        bang_token: token option;
        module_ident: Ident.t;
        in_token: token;
        body: expr;
      }
    | Delimited of {
        module_ident: Ident.t;
        dot_token: token;
        opening_token: token;
        body: expr;
        closing_token: token;
      }
    | Unknown of node

  let cast = fun (expr: expr) -> cast_kind expr Syntax_kind.LOCAL_OPEN_EXPR

  let ident_expr_child = fun expr index ->
    match nth_expr_child expr index with
    | Some child -> cast_result_to_option (Ident.cast child)
    | None -> None

  let opening_token = fun expr ->
    match first_child_token_matching
      expr
      ~matches:(fun kind ->
        Syntax_kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE)) with
    | Some token -> Some token
    | None -> (
        match nth_expr_child expr 1 with
        | Some body ->
            first_child_token_matching
              body
              ~matches:(fun kind ->
                Syntax_kind.(kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE))
        | None -> None
      )

  let closing_token = fun expr ->
    match first_child_token_matching
      expr
      ~matches:(fun kind ->
        Syntax_kind.(kind = RPAREN || kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE)) with
    | Some token -> Some token
    | None -> (
        match nth_expr_child expr 1 with
        | Some body ->
            first_child_token_matching
              body
              ~matches:(fun kind ->
                Syntax_kind.(kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE))
        | None -> None
      )

  let view = fun expr ->
    if has_child_token_kind expr Syntax_kind.LET_KW then
      match (
        Node.first_child_token expr ~kind:Syntax_kind.LET_KW,
        Node.first_child_token expr ~kind:Syntax_kind.OPEN_KW,
        ident_expr_child expr 0,
        Node.first_child_token expr ~kind:Syntax_kind.IN_KW,
        nth_expr_child expr 1
      ) with
      | (Some let_token, Some open_token, Some module_ident, Some in_token, Some body) ->
          LetOpen {
            let_token;
            open_token;
            bang_token = Node.first_child_token expr ~kind:Syntax_kind.BANG;
            module_ident;
            in_token;
            body;
          }
      | _ -> Unknown expr
    else
      match (
        ident_expr_child expr 0,
        Node.first_child_token expr ~kind:Syntax_kind.DOT,
        opening_token expr,
        nth_expr_child expr 1,
        closing_token expr
      ) with
      | (Some module_ident, Some dot_token, Some opening_token, Some body, Some closing_token) ->
          Delimited {
            module_ident;
            dot_token;
            opening_token;
            body;
            closing_token;
          }
      | _ -> Unknown expr
end

module LetModuleExpr: sig
  type t = expr
  type module_body =
    | Ident
    | Struct
    | Unsupported

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val let_token: t -> token option

  val module_token: t -> token option

  val name: t -> Ident.t option

  val equals_token: t -> token option

  val in_token: t -> token option

  val module_body: t -> module_body

  val module_body_node: t -> node option

  val body: t -> expr option

  val module_body_ident: t -> Ident.t option
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type module_body =
    | Ident
    | Struct
    | Unsupported

  let cast = fun (expr: expr) -> cast_kind expr Syntax_kind.LET_MODULE_EXPR

  let let_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.LET_KW

  let module_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.MODULE_KW

  let equals_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.EQ

  let in_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.IN_KW

  let token_index = fun expr ~from ~matches ->
    let count = Node.child_count expr in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at expr index with
        | Some token when matches (Token.kind token) -> Some index
        | _ -> loop (index + 1)
    in
    loop from

  let module_index = fun expr ->
    token_index
      expr
      ~from:0
      ~matches:(fun kind -> Syntax_kind.(kind = MODULE_KW))

  let equals_index = fun expr ->
    token_index
      expr
      ~from:0
      ~matches:(fun kind -> Syntax_kind.(kind = EQ))

  let in_index = fun expr ->
    token_index
      expr
      ~from:0
      ~matches:(fun kind -> Syntax_kind.(kind = IN_KW))

  let name = fun expr ->
    match module_index expr with
    | Some module_index -> (
        match child_token_at expr (module_index + 1) with
        | Some token when token_kind_is token Syntax_kind.IDENT ->
            Ident.from_child_range_option
              expr
              ~start_index:(module_index + 1)
              ~stop_index:(module_index + 2)
        | _ -> None
      )
    | None -> None

  let module_body_bounds = fun expr ->
    match (equals_index expr, in_index expr) with
    | (Some equals_index, Some in_index) when equals_index < in_index ->
        Some (equals_index + 1, in_index)
    | _ -> None

  let module_body_node = fun expr ->
    let body_group_node =
      match module_body_bounds expr with
      | Some (start, stop) ->
          let rec loop index =
            if index >= stop then
              None
            else
              match Node.child_at expr index with
              | Some (Syntax_tree.Node id) ->
                  let node = wrap_node expr.tree id in
                  if
                    node_matches
                      node
                      (fun kind -> is_module_expr_kind kind || Syntax_kind.(kind = MODULE_EXPR))
                  then
                    Some node
                  else
                    loop (index + 1)
              | Some (Syntax_tree.Token _)
              | Some (Syntax_tree.Missing _)
              | None -> loop (index + 1)
          in
          loop start
      | None -> None
    in
    match body_group_node with
    | Some node -> (
        match Node.kind node with
        | Syntax_kind.MODULE_EXPR -> first_child_node_matching node ~matches:is_module_expr_kind
        | kind when is_module_expr_kind kind -> Some node
        | _ -> None
      )
    | None -> None

  let module_body = fun expr ->
    match module_body_node expr with
    | Some node when node_kind_is node Syntax_kind.PATH_MODULE_EXPR -> Ident
    | Some node when node_kind_is node Syntax_kind.STRUCT_MODULE_EXPR -> Struct
    | _ -> Unsupported

  let body = first_expr_child

  let module_body_ident = fun expr ->
    match module_body_node expr with
    | Some node when node_kind_is node Syntax_kind.PATH_MODULE_EXPR -> Ident.from_node_option node
    | _ -> None
end

module LetExceptionExpr: sig
  type t = expr

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val let_token: t -> token option

  val exception_token: t -> token option

  val name: t -> Ident.t option

  val of_token: t -> token option

  val in_token: t -> token option

  val body: t -> expr option

  val fold_payload_token: t -> init:'acc -> fn:(token -> 'acc -> 'acc control) -> 'acc

  val payload_token_count: t -> int

  val for_each_payload_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (expr: expr) -> cast_kind expr Syntax_kind.LET_EXCEPTION_EXPR

  let let_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.LET_KW

  let exception_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.EXCEPTION_KW

  let of_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.OF_KW

  let in_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.IN_KW

  let token_index = fun expr ~from ~matches ->
    let count = Node.child_count expr in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at expr index with
        | Some token when matches (Token.kind token) -> Some index
        | _ -> loop (index + 1)
    in
    loop from

  let exception_index = fun expr ->
    token_index
      expr
      ~from:0
      ~matches:(fun kind -> Syntax_kind.(kind = EXCEPTION_KW))

  let of_index = fun expr ->
    token_index
      expr
      ~from:0
      ~matches:(fun kind -> Syntax_kind.(kind = OF_KW))

  let in_index = fun expr ->
    token_index
      expr
      ~from:0
      ~matches:(fun kind -> Syntax_kind.(kind = IN_KW))

  let name = fun expr ->
    match exception_index expr with
    | Some exception_index -> (
        match child_token_at expr (exception_index + 1) with
        | Some token when token_kind_is token Syntax_kind.IDENT ->
            Ident.from_child_range_option
              expr
              ~start_index:(exception_index + 1)
              ~stop_index:(exception_index + 2)
        | _ -> None
      )
    | None -> None

  let body = first_expr_child

  let payload_bounds = fun expr ->
    match (of_index expr, in_index expr) with
    | (Some of_index, Some in_index) when of_index < in_index -> Some (of_index + 1, in_index)
    | _ -> None

  let fold_payload_token = fun expr ~init ~fn ->
    match payload_bounds expr with
    | None -> init
    | Some (start, stop) ->
        let rec loop index acc =
          if index >= stop then
            acc
          else
            match child_token_at expr index with
            | Some token -> (
                match fn token acc with
                | Continue next -> loop (index + 1) next
                | Return value -> value
              )
            | None -> loop (index + 1) acc
        in
        loop start init

  let payload_token_count = fun expr -> count_fold fold_payload_token expr

  let for_each_payload_token = fun expr ~fn ->
    fold_payload_token
      expr
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())
end

module UnreachableExpr: sig
  type t = expr

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val dot_token: t -> token option
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (expr: expr) -> cast_kind expr Syntax_kind.UNREACHABLE_EXPR

  let dot_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.DOT
end

module FirstClassModuleExpr: sig
  type t = expr
  type ascription =
    | NoAscription
    | IdentAscription
    | UnsupportedAscription

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val opening_token: t -> token option

  val module_token: t -> token option

  val colon_token: t -> token option

  val closing_token: t -> token option

  val module_ident: t -> Ident.t option

  val ascription: t -> ascription

  val ascription_ident: t -> Ident.t option
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type module_ident =
    | ModuleIdent
    | UnsupportedModuleIdent

  type ascription =
    | NoAscription
    | IdentAscription
    | UnsupportedAscription

  let cast = fun (expr: expr) -> cast_kind expr Syntax_kind.FIRST_CLASS_MODULE_EXPR

  let opening_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.LPAREN

  let module_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.MODULE_KW

  let colon_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.COLON

  let closing_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind.RPAREN

  let token_index = fun expr ~from ~matches ->
    let count = Node.child_count expr in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at expr index with
        | Some token when matches (Token.kind token) -> Some index
        | _ -> loop (index + 1)
    in
    loop from

  let range_is_ident = fun expr start stop ->
    let rec loop index saw_ident expect_ident =
      if index >= stop then
        saw_ident && not expect_ident
      else
        match child_token_at expr index with
        | Some token when token_kind_is token Syntax_kind.IDENT && expect_ident ->
            loop (index + 1) true false
        | Some token when token_kind_is token Syntax_kind.DOT && saw_ident && not expect_ident ->
            loop (index + 1) saw_ident true
        | _ -> false
    in
    loop start false true

  let module_ident_bounds = fun expr ->
    match token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind.(kind = MODULE_KW)) with
    | None -> None
    | Some module_index ->
        let start = module_index + 1 in
        token_index
          expr
          ~from:start
          ~matches:(fun kind -> Syntax_kind.(kind = COLON || kind = RPAREN))
        |> Option.map ~fn:(fun stop -> (start, stop))

  let ascription_bounds = fun expr ->
    match token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind.(kind = COLON)) with
    | None -> None
    | Some colon_index ->
        let start = colon_index + 1 in
        token_index expr ~from:start ~matches:(fun kind -> Syntax_kind.(kind = RPAREN))
        |> Option.map ~fn:(fun stop -> (start, stop))

  let module_ident = fun expr ->
    match module_ident_bounds expr with
    | Some (start, stop) when range_is_ident expr start stop ->
        Ident.from_child_range_option expr ~start_index:start ~stop_index:stop
    | _ -> None

  let ascription = fun expr ->
    match (colon_token expr, ascription_bounds expr) with
    | (None, _) -> NoAscription
    | (Some _, Some (start, stop)) when range_is_ident expr start stop -> IdentAscription
    | (Some _, _) -> UnsupportedAscription

  let ascription_ident = fun expr ->
    match ascription_bounds expr with
    | Some (start, stop) when range_is_ident expr start stop ->
        Ident.from_child_range_option expr ~start_index:start ~stop_index:stop
    | _ -> None
end

module BindingOperatorExpr: sig
  type t = expr
  type clause = {
    keyword: Token.t option;
    operator: Token.t option;
    binding: let_binding;
  }

  val cast: expr -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val in_token: t -> Token.t option

  val body: t -> expr option

  val fold_clause: t -> init:'acc -> fn:(clause -> 'acc -> 'acc control) -> 'acc

  val clause_count: t -> int

  val for_each_clause: t -> fn:(clause -> unit) -> unit
end = struct
  type t = expr

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type clause = {
    keyword: Token.t option;
    operator: Token.t option;
    binding: let_binding;
  }

  let cast = fun (expr: expr) -> cast_kind expr Syntax_kind.BINDING_OPERATOR_EXPR

  let in_token = fun (expr: t) -> Node.first_child_token expr ~kind:Syntax_kind.IN_KW

  let body = first_expr_child

  let binding_operator_keyword = fun token ->
    token_kind_is token Syntax_kind.LET_KW || token_kind_is token Syntax_kind.AND_KW

  let binding_operator_suffix = fun token ->
    token_kind_is token Syntax_kind.STAR || token_kind_is token Syntax_kind.PLUS

  let fold_clause = fun (expr: t) ~init ~fn ->
    let child_count = Node.child_count expr in
    let rec loop index keyword operator acc =
      if Int.(index >= child_count) then
        acc
      else
        match Node.child_at expr index with
        | Some (Syntax_tree.Token id) ->
            let token = wrap_token expr.tree id in
            if binding_operator_keyword token then
              loop Int.(index + 1) (Some token) None acc
            else if binding_operator_suffix token then
              loop Int.(index + 1) keyword (Some token) acc
            else
              loop Int.(index + 1) keyword operator acc
        | Some (Syntax_tree.Node id) ->
            let child = wrap_node expr.tree id in
            if node_matches child is_let_binding_kind then
              match fn { keyword; operator; binding = child } acc with
              | Continue next -> loop Int.(index + 1) None None next
              | Return value -> value
            else
              loop Int.(index + 1) keyword operator acc
        | Some (Syntax_tree.Missing _)
        | None -> loop Int.(index + 1) keyword operator acc
    in
    loop 0 None None init

  let clause_count = fun expr -> count_fold fold_clause expr

  let for_each_clause = fun expr ~fn ->
    fold_clause
      expr
      ~init:()
      ~fn:(fun clause () ->
        fn clause;
        Continue ())
end

module Pattern: sig
  type t = pattern
  type view =
    | Unit
    | Wildcard
    | Ident of {
        ident: Ident.t;
      }
    | Constructor of {
        constructor: Ident.t;
        payload: t option;
      }
    | Literal of {
        token: token;
      }
    | Tuple of {
        parts: t Vector.t;
      }
    | List of {
        items: t Vector.t;
      }
    | Array of {
        items: t Vector.t;
      }
    | Record of {
        fields: record_pattern_field_view Vector.t;
        open_wildcard: token option;
      }
    | PolyVariant of {
        tag: token;
        payload: t option;
      }
    | FirstClassModule of {
        binder: Ident.t;
        ascription: first_class_module_pattern_ascription;
        ascription_ident: Ident.t option;
      }
    | Interval of { left: t; right: t }
    | Constraint of {
        pattern: t;
        annotation: type_expr;
      }
    | Alias of { pattern: t; alias: t }
    | Or of { left: t; right: t }
    | Cons of { head: t; tail: t }
    | Lazy of { pattern: t }
    | Exception of { pattern: t }
    | Error of Node.t
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val text: t -> string

  val view: t -> view

  val literal_token: t -> token option

  val literal_sign_token: t -> token option

  val fold_child_pattern: t -> init:'acc -> fn:(t -> 'acc -> 'acc control) -> 'acc

  val child_pattern_count: t -> int

  val for_each_child_pattern: t -> fn:(t -> unit) -> unit
end = struct
  type t = pattern

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let text = Node.text

  type view =
    | Unit
    | Wildcard
    | Ident of {
        ident: Ident.t;
      }
    | Constructor of {
        constructor: Ident.t;
        payload: t option;
      }
    | Literal of {
        token: token;
      }
    | Tuple of {
        parts: t Vector.t;
      }
    | List of {
        items: t Vector.t;
      }
    | Array of {
        items: t Vector.t;
      }
    | Record of {
        fields: record_pattern_field_view Vector.t;
        open_wildcard: token option;
      }
    | PolyVariant of {
        tag: token;
        payload: t option;
      }
    | FirstClassModule of {
        binder: Ident.t;
        ascription: first_class_module_pattern_ascription;
        ascription_ident: Ident.t option;
      }
    | Interval of { left: t; right: t }
    | Constraint of {
        pattern: t;
        annotation: type_expr;
      }
    | Alias of { pattern: t; alias: t }
    | Or of { left: t; right: t }
    | Cons of { head: t; tail: t }
    | Lazy of { pattern: t }
    | Exception of { pattern: t }
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:pattern_expected_kinds
      ~matches:is_pattern_kind

  let literal_token = fun pattern ->
    first_child_token_matching
      pattern
      ~matches:(fun kind ->
        Syntax_kind.(kind = INT
        || kind = FLOAT
        || kind = STRING
        || kind = CHAR
        || kind = TRUE_KW
        || kind = FALSE_KW))

  let literal_sign_token = fun pattern ->
    first_child_token_matching
      pattern
      ~matches:(fun kind ->
        Syntax_kind.(kind = PLUS || kind = MINUS || kind = PLUSDOT || kind = MINUSDOT))

  let child_patterns = fun pattern ->
    let items = Vector.with_capacity ~size:(Node.child_count pattern) in
    for_each_child_node_matching
      pattern
      ~matches:is_pattern_kind
      ~fn:(fun child -> Vector.push items ~value:(normalize_pattern_node child));
    items

  let ident_is_constructor = fun ident ->
    match Ident.last_token ident with
    | None -> false
    | Some ident ->
        let text = Token.text ident in
        if Int.equal (String.length text) 0 then
          false
        else
          match String.get_unchecked text ~at:0 with
          | 'A' .. 'Z' -> true
          | _ -> false

  let rec view = fun (pattern: pattern) ->
    match Node.kind pattern with
    | Syntax_kind.PAREN_PATTERN -> (
        match first_pattern_child pattern with
        | Some inner -> view inner
        | None -> Unit
      )
    | Syntax_kind.ATTRIBUTE_PATTERN -> (
        match first_pattern_child pattern with
        | Some inner -> view inner
        | None -> Unknown pattern
      )
    | Syntax_kind.WILDCARD_PATTERN -> Wildcard
    | Syntax_kind.PATH_PATTERN -> (
        match Ident.from_node_option pattern with
        | None -> Unknown pattern
        | Some ident ->
            if ident_is_constructor pattern then
              Constructor { constructor = ident; payload = None }
            else
              Ident { ident }
      )
    | Syntax_kind.CONSTRUCT_PATTERN -> (
        let callee = nth_pattern_child pattern 0 in
        let payload = normalize_pattern_option (nth_pattern_child pattern 1) in
        match callee with
        | Some callee -> (
            match view callee with
            | Constructor { constructor; payload = None } -> Constructor { constructor; payload }
            | Ident { ident } -> Constructor { constructor = ident; payload }
            | _ -> Unknown pattern
          )
        | None -> Unknown pattern
      )
    | Syntax_kind.LITERAL_PATTERN -> (
        match literal_token pattern with
        | Some token -> Literal { token }
        | None -> Unknown pattern
      )
    | Syntax_kind.TUPLE_PATTERN -> Tuple { parts = child_patterns pattern }
    | Syntax_kind.LIST_PATTERN -> List { items = child_patterns pattern }
    | Syntax_kind.ARRAY_PATTERN -> Array { items = child_patterns pattern }
    | Syntax_kind.RECORD_PATTERN ->
        Record {
          fields = collect_record_pattern_fields pattern;
          open_wildcard = record_pattern_open_wildcard pattern;
        }
    | Syntax_kind.POLY_VARIANT_PATTERN -> (
        match Ident.first_token pattern with
        | Some tag ->
            PolyVariant { tag; payload = normalize_pattern_option (first_pattern_child pattern) }
        | None -> Unknown pattern
      )
    | Syntax_kind.EXTENSION_PATTERN
    | Syntax_kind.LOCAL_OPEN_PATTERN
    | Syntax_kind.LOCALLY_ABSTRACT_TYPE_PATTERN -> Unknown pattern
    | Syntax_kind.FIRST_CLASS_MODULE_PATTERN -> (
        match first_class_module_pattern_binder pattern with
        | Some binder ->
            FirstClassModule {
              binder;
              ascription = first_class_module_pattern_ascription pattern;
              ascription_ident = first_class_module_pattern_ascription_ident pattern;
            }
        | None -> Unknown pattern
      )
    | Syntax_kind.INTERVAL_PATTERN -> (
        match (
          normalize_pattern_option (nth_pattern_child pattern 0),
          normalize_pattern_option (nth_pattern_child pattern 1)
        ) with
        | (Some left, Some right) -> Interval { left; right }
        | _ -> Unknown pattern
      )
    | Syntax_kind.CONSTRAINT_PATTERN -> (
        match (
          normalize_pattern_option (first_pattern_child pattern),
          normalize_type_expr_option (first_type_expr_child pattern)
        ) with
        | (Some pattern, Some annotation) -> Constraint { pattern; annotation }
        | _ -> Unknown pattern
      )
    | Syntax_kind.ALIAS_PATTERN -> (
        match (
          normalize_pattern_option (nth_pattern_child pattern 0),
          normalize_pattern_option (nth_pattern_child pattern 1)
        ) with
        | (Some pattern, Some alias) -> Alias { pattern; alias }
        | _ -> Unknown pattern
      )
    | Syntax_kind.OR_PATTERN -> (
        match (
          normalize_pattern_option (nth_pattern_child pattern 0),
          normalize_pattern_option (nth_pattern_child pattern 1)
        ) with
        | (Some left, Some right) -> Or { left; right }
        | _ -> Unknown pattern
      )
    | Syntax_kind.CONS_PATTERN -> (
        match (
          normalize_pattern_option (nth_pattern_child pattern 0),
          normalize_pattern_option (nth_pattern_child pattern 1)
        ) with
        | (Some head, Some tail) -> Cons { head; tail }
        | _ -> Unknown pattern
      )
    | Syntax_kind.LAZY_PATTERN -> (
        match normalize_pattern_option (first_pattern_child pattern) with
        | Some pattern -> Lazy { pattern }
        | None -> Unknown pattern
      )
    | Syntax_kind.EXCEPTION_PATTERN -> (
        match normalize_pattern_option (first_pattern_child pattern) with
        | Some pattern -> Exception { pattern }
        | None -> Unknown pattern
      )
    | Syntax_kind.LABELED_PARAM
    | Syntax_kind.OPTIONAL_PARAM
    | Syntax_kind.OPTIONAL_PARAM_DEFAULT -> Unknown pattern
    | Syntax_kind.ERROR -> Error pattern
    | _ -> Unknown pattern

  let fold_child_pattern = fun (pattern: pattern) ~init ~fn ->
    fold_child_node_matching
      pattern
      ~matches:is_pattern_kind
      ~init
      ~fn:(fun child acc -> fn (normalize_pattern_node child) acc)

  let child_pattern_count = fun pattern -> count_fold fold_child_pattern pattern

  let for_each_child_pattern = fun pattern ~fn ->
    fold_child_pattern
      pattern
      ~init:()
      ~fn:(fun child () ->
        fn child;
        Continue ())
end

module AttributePattern: sig
  type t = pattern

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val inner: t -> pattern option

  val fold_shell_token: t -> init:'acc -> fn:(token -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int

  val for_each_shell_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = pattern

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (pattern: pattern) -> cast_kind pattern Syntax_kind.ATTRIBUTE_PATTERN

  let inner = first_pattern_child

  let fold_shell_token = Node.fold_child_token

  let shell_token_count = fun pattern -> count_fold fold_shell_token pattern

  let for_each_shell_token = Node.for_each_child_token
end

module ExtensionPattern: sig
  type t = pattern

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val fold_shell_token: t -> init:'acc -> fn:(token -> 'acc -> 'acc control) -> 'acc

  val shell_token_count: t -> int

  val for_each_shell_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = pattern

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (pattern: pattern) -> cast_kind pattern Syntax_kind.EXTENSION_PATTERN

  let fold_shell_token = Node.fold_child_token

  let shell_token_count = fun pattern -> count_fold fold_shell_token pattern

  let for_each_shell_token = Node.for_each_child_token
end

module LocallyAbstractTypePattern: sig
  type t = pattern

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val opening_token: t -> token option

  val type_token: t -> token option

  val closing_token: t -> token option

  val type_ident: t -> Ident.t option

  val fold_type_ident: t -> init:'acc -> fn:(Ident.t -> 'acc -> 'acc control) -> 'acc

  val type_ident_count: t -> int
end = struct
  type t = pattern

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (pattern: pattern) -> cast_kind pattern Syntax_kind.LOCALLY_ABSTRACT_TYPE_PATTERN

  let opening_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind.LPAREN

  let type_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind.TYPE_KW

  let closing_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind.RPAREN

  let type_ident = Ident.first

  let fold_type_ident = fun pattern ~init ~fn ->
    Node.fold_child_token
      pattern
      ~init
      ~fn:(fun token acc ->
        if token_kind_is token Syntax_kind.IDENT then
          fn (Ident.Bare token) acc
        else
          Continue acc)

  let type_ident_count = fun pattern -> count_fold fold_type_ident pattern
end

module FirstClassModulePattern: sig
  type t = pattern
  type ascription = first_class_module_pattern_ascription =
    | NoAscription
    | IdentAscription
    | UnsupportedAscription

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val opening_token: t -> token option

  val module_token: t -> token option

  val binder: t -> Ident.t option

  val colon_token: t -> token option

  val closing_token: t -> token option

  val ascription: t -> ascription

  val ascription_ident: t -> Ident.t option
end = struct
  type t = pattern

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type ascription = first_class_module_pattern_ascription =
    | NoAscription
    | IdentAscription
    | UnsupportedAscription

  let cast = fun (pattern: pattern) -> cast_kind pattern Syntax_kind.FIRST_CLASS_MODULE_PATTERN

  let opening_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind.LPAREN

  let module_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind.MODULE_KW

  let colon_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind.COLON

  let closing_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind.RPAREN

  let binder = first_class_module_pattern_binder

  let ascription = first_class_module_pattern_ascription

  let ascription_ident = first_class_module_pattern_ascription_ident
end

module RecordPattern: sig
  type t = pattern
  type field = record_pattern_field_view

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val open_wildcard: t -> Token.t option

  val fold_field: t -> init:'acc -> fn:(field -> 'acc -> 'acc control) -> 'acc

  val field_count: t -> int

  val for_each_field: t -> fn:(field -> unit) -> unit
end = struct
  type t = pattern

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type field = record_pattern_field_view

  let cast = fun (pattern: pattern) -> cast_kind pattern Syntax_kind.RECORD_PATTERN

  let open_wildcard = record_pattern_open_wildcard

  let fold_field = fun (record: t) ~init ~fn ->
    let acc = ref init in
    let returned = ref false in
    collect_record_pattern_fields record
    |> Vector.for_each
      ~fn:(fun field ->
        if not !returned then
          match fn field !acc with
          | Continue next -> acc := next
          | Return value ->
              acc := value;
              returned := true);
    !acc

  let field_count = fun record -> count_fold fold_field record

  let for_each_field = fun record ~fn ->
    fold_field
      record
      ~init:()
      ~fn:(fun field () ->
        fn field;
        Continue ())
end

module LocalOpenPattern: sig
  type t = pattern
  type view =
    | Delimited of {
        module_ident: Ident.t;
        dot_token: token;
        opening_token: token;
        pattern: pattern;
        closing_token: token;
      }
    | Unknown of node

  val cast: pattern -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val dot_token: t -> token option

  val opening_token: t -> token option

  val closing_token: t -> token option

  val pattern: t -> pattern option

  val module_ident: t -> Ident.t option
end = struct
  type t = pattern

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Delimited of {
        module_ident: Ident.t;
        dot_token: token;
        opening_token: token;
        pattern: pattern;
        closing_token: token;
      }
    | Unknown of node

  let cast = fun (pattern: pattern) -> cast_kind pattern Syntax_kind.LOCAL_OPEN_PATTERN

  let dot_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind.DOT

  let delimiter_child = first_pattern_child

  let opening_token = fun pattern ->
    let matches kind =
      Syntax_kind.(kind = LPAREN || kind = LBRACE || kind = LBRACKET || kind = LBRACKET_BAR)
    in
    match first_child_token_matching pattern ~matches with
    | Some token -> Some token
    | None -> (
        match delimiter_child pattern with
        | Some child -> first_child_token_matching child ~matches
        | None -> None
      )

  let closing_token = fun pattern ->
    let matches kind =
      Syntax_kind.(kind = RPAREN || kind = RBRACE || kind = RBRACKET || kind = BAR_RBRACKET)
    in
    match first_child_token_matching pattern ~matches with
    | Some token -> Some token
    | None -> (
        match delimiter_child pattern with
        | Some child -> first_child_token_matching child ~matches
        | None -> None
      )

  let pattern = first_pattern_child

  let direct_child_token_index = fun pattern ~matches ->
    let child_count = Node.child_count pattern in
    let rec loop index =
      if index >= child_count then
        None
      else
        match child_token_at pattern index with
        | Some token when matches (Token.kind token) -> Some index
        | None -> loop (index + 1)
        | Some _ -> loop (index + 1)
    in
    loop 0

  let module_ident = fun pattern ->
    let stop_index =
      match direct_child_token_index
        pattern
        ~matches:(fun kind ->
          Syntax_kind.(kind = LPAREN || kind = LBRACE || kind = LBRACKET || kind = LBRACKET_BAR)) with
      | Some opening_index -> opening_index
      | None -> Node.child_count pattern
    in
    let rec last_dot_before index found =
      if index >= stop_index then
        found
      else
        match child_token_at pattern index with
        | Some token when token_kind_is token Syntax_kind.DOT ->
            last_dot_before (index + 1) (Some index)
        | Some _
        | None -> last_dot_before (index + 1) found
    in
    match last_dot_before 0 None with
    | Some dot_index when dot_index > 0 ->
        Ident.from_child_range_option pattern ~start_index:0 ~stop_index:dot_index
    | Some _
    | None -> (
        match direct_child_token_index
          pattern
          ~matches:(fun kind ->
            Syntax_kind.(kind = LPAREN || kind = LBRACE || kind = LBRACKET || kind = LBRACKET_BAR)) with
        | Some opening_index when opening_index > 0 ->
            Ident.from_child_range_option pattern ~start_index:0 ~stop_index:opening_index
        | Some _
        | None -> None
      )

  let view = fun pattern ->
    match (
      dot_token pattern,
      opening_token pattern,
      first_pattern_child pattern,
      closing_token pattern,
      module_ident pattern
    ) with
    | (Some dot_token, Some opening_token, Some body, Some closing_token, Some module_ident) ->
        Delimited {
          module_ident;
          dot_token;
          opening_token;
          pattern = body;
          closing_token;
        }
    | _ -> Unknown pattern
end

module Parameter: sig
  type t = parameter
  type label =
    | NoLabel
    | Labeled of {
        name: token option;
      }
    | Optional of {
        name: token option;
        default: expr option;
      }
  type view =
    | Param of {
        label: label;
        pattern: pattern option;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val label: t -> label

  val label_token: t -> token option

  val pattern: t -> pattern option

  val default: t -> expr option

  val has_explicit_pattern_parens: t -> bool
end = struct
  type t = parameter

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type label =
    | NoLabel
    | Labeled of {
        name: token option;
      }
    | Optional of {
        name: token option;
        default: expr option;
      }

  type view =
    | Param of {
        label: label;
        pattern: pattern option;
      }
    | Unknown of Node.t

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:parameter_expected_kinds
      ~matches:is_parameter_node_kind

  let parameter_label_token = fun parameter ->
    match Ident.first_token parameter with
    | Some token -> Some token
    | None ->
        first_descendant_token_matching parameter ~matches:(fun kind -> Syntax_kind.(kind = IDENT))

  let parameter_payload_pattern = fun parameter ->
    match first_pattern_child parameter with
    | Some pattern when node_kind_is pattern Syntax_kind.CONSTRUCT_PATTERN
    && not (node_colon_has_leading_whitespace parameter) ->
        normalize_pattern_option (nth_pattern_child pattern 0)
    | pattern -> normalize_pattern_option pattern

  let view = fun (parameter: parameter) ->
    match Node.kind parameter with
    | kind when is_pattern_kind kind ->
        Param { label = NoLabel; pattern = Some (normalize_pattern_node parameter) }
    | Syntax_kind.LABELED_PARAM ->
        Param {
          label = Labeled { name = parameter_label_token parameter };
          pattern = parameter_payload_pattern parameter;
        }
    | Syntax_kind.OPTIONAL_PARAM ->
        Param {
          label = Optional { name = parameter_label_token parameter; default = None };
          pattern = parameter_payload_pattern parameter;
        }
    | Syntax_kind.OPTIONAL_PARAM_DEFAULT ->
        Param {
          label = Optional {
            name = parameter_label_token parameter;
            default = normalize_expr_option (first_expr_child parameter);
          };
          pattern = parameter_payload_pattern parameter;
        }
    | _ -> Unknown parameter

  let label = fun parameter ->
    match view parameter with
    | Param { label; _ } -> label
    | Unknown _ -> NoLabel

  let label_token = fun parameter ->
    match label parameter with
    | NoLabel -> None
    | Labeled { name }
    | Optional { name; _ } -> name

  let pattern = fun parameter ->
    match view parameter with
    | Param { pattern; _ } -> pattern
    | Unknown _ -> None

  let default = fun parameter ->
    match label parameter with
    | Optional { default; _ } -> default
    | NoLabel
    | Labeled _ -> None

  let has_explicit_pattern_parens = fun parameter ->
    Option.is_some
      (Node.first_child_token parameter ~kind:Syntax_kind.LPAREN)
end

module MatchCase: sig
  type t = match_case
  type view =
    | Case of {
        pattern: pattern;
        guard: expr option;
        body: expr;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val pattern: t -> pattern option

  val guard: t -> expr option

  val body: t -> expr option
end = struct
  type t = match_case

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Case of {
        pattern: pattern;
        guard: expr option;
        body: expr;
      }
    | Unknown of Node.t

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:[ Syntax_kind.MATCH_CASE ]
      ~matches:is_match_case_kind

  let pattern = first_pattern_child

  let guard_and_body = fun (match_case: match_case) ->
    let (guard, body) =
      if has_child_token_kind match_case Syntax_kind.WHEN_KW then
        (nth_expr_child match_case 0, nth_expr_child match_case 1)
      else
        (None, nth_expr_child match_case 0)
    in
    (guard, body)

  let guard = fun match_case ->
    let (guard, _) = guard_and_body match_case in
    guard

  let body = fun match_case ->
    let (_, body) = guard_and_body match_case in
    body

  let view = fun (match_case: match_case) ->
    match (pattern match_case, guard_and_body match_case) with
    | (Some pattern, (guard, Some body)) -> Case { pattern; guard; body }
    | _ -> Unknown match_case
end

module LetBinding: sig
  type t = let_binding
  type view =
    | Binding of {
        pattern: pattern;
        body: expr;
      }
    | Unknown of Node.t

  val cast: Node.t -> t cast_result

  val as_node: t -> Node.t

  val kind: t -> Syntax_kind.t

  val span: t -> Span.t

  val width: t -> int

  val view: t -> view

  val pattern: t -> pattern option

  val body: t -> expr option

  val fold_parameter: t -> init:'acc -> fn:(parameter -> 'acc -> 'acc control) -> 'acc

  val parameter_count: t -> int

  val for_each_parameter: t -> fn:(parameter -> unit) -> unit

  val return_type_annotation: t -> type_expr option

  val type_annotation: t -> type_expr option
end = struct
  type t = let_binding

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Binding of {
        pattern: pattern;
        body: expr;
      }
    | Unknown of Node.t

  let cast = fun (node: node) ->
    cast_matching
      node
      ~expected:[ Syntax_kind.LET_BINDING ]
      ~matches:is_let_binding_kind

  let pattern = first_pattern_child

  let body = first_expr_child

  let view = fun (binding: let_binding) ->
    match (pattern binding, body binding) with
    | (Some pattern, Some body) -> Binding { pattern; body }
    | _ -> Unknown binding

  let rec fold_parameter_node = fun node ~acc ~fn ->
    match Node.kind node with
    | kind when is_parameter_kind kind -> (
        match fn node acc with
        | Return value -> Return value
        | Continue acc -> (
            if node_colon_has_leading_whitespace node then
              Continue acc
            else
              match first_pattern_child node with
              | Some pattern when node_kind_is pattern Syntax_kind.CONSTRUCT_PATTERN -> (
                  match nth_child_node_matching pattern 1 ~matches:is_parameter_node_kind with
                  | Some rest -> fold_parameter_node rest ~acc ~fn
                  | None -> Continue acc
                )
              | Some _
              | None -> Continue acc
          )
      )
    | Syntax_kind.CONSTRUCT_PATTERN ->
        fold_child_node_matching
          node
          ~matches:is_parameter_node_kind
          ~init:(Continue acc)
          ~fn:(fun child state ->
            match state with
            | Return _ -> Return state
            | Continue acc -> (
                match fold_parameter_node child ~acc ~fn with
                | Continue next -> Continue (Continue next)
                | Return value -> Return (Return value)
              ))
    | Syntax_kind.CONSTRAINT_PATTERN -> (
        match first_pattern_child node with
        | Some pattern -> fold_parameter_node pattern ~acc ~fn
        | None -> fn node acc
      )
    | _ -> fn node acc

  let fold_parameter = fun (binding: let_binding) ~init ~fn ->
    let seen_first = ref false in
    fold_child_node_matching
      binding
      ~matches:is_parameter_node_kind
      ~init
      ~fn:(fun parameter acc ->
        if !seen_first then
          fold_parameter_node parameter ~acc ~fn
        else (
          seen_first := true;
          Continue acc
        ))

  let parameter_count = fun binding -> count_fold fold_parameter binding

  let for_each_parameter = fun binding ~fn ->
    fold_parameter
      binding
      ~init:()
      ~fn:(fun parameter () ->
        fn parameter;
        Continue ())

  let direct_binding_return_annotation = fun (binding: let_binding) ->
    let found = ref None in
    let seen_binding_pattern = ref false in
    for_each_child_node_matching
      binding
      ~matches:is_pattern_kind
      ~fn:(fun pattern ->
        match !found with
        | Some _ -> ()
        | None ->
            if !seen_binding_pattern then
              match Node.kind pattern with
              | Syntax_kind.CONSTRAINT_PATTERN -> (
                  match Pattern.view pattern with
                  | Constraint { annotation; _ } -> found := Some annotation
                  | _ -> ()
                )
              | _ -> ()
            else
              seen_binding_pattern := true);
    !found

  let return_type_annotation = fun binding ->
    match direct_binding_return_annotation binding with
    | Some annotation -> Some annotation
    | None -> first_type_expr_child binding

  let type_annotation = fun (binding: let_binding) ->
    match return_type_annotation binding with
    | Some annotation -> Some annotation
    | None -> (
        match first_type_expr_child binding with
        | Some type_expr -> Some type_expr
        | None -> (
            match pattern binding with
            | Some pattern -> first_type_expr_descendant_of_pattern pattern
            | None -> None
          )
      )
end

module LetDeclaration = struct
  type t = let_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) -> cast_kind node Syntax_kind.LET_DECL

  let rec_token = fun (decl: let_declaration) ->
    Node.first_child_token
      decl
      ~kind:Syntax_kind.REC_KW

  let first_binding = first_let_binding_child

  let fold_binding = fun (decl: let_declaration) ~init ~fn ->
    fold_child_node_matching
      decl
      ~matches:is_let_binding_kind
      ~init
      ~fn

  let binding_count = fun decl -> count_fold fold_binding decl

  let for_each_binding = fun decl ~fn ->
    fold_binding
      decl
      ~init:()
      ~fn:(fun binding () ->
        fn binding;
        Continue ())
end

module TypeDeclaration = struct
  type t = type_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type member = {
    declaration: type_declaration;
    node: node;
    start_index: int;
    stop_index: int;
  }

  type parameter =
    | Named of {
        name: Token.t;
        quote: Token.t option;
        variance: Token.t option;
        injective: Token.t option;
      }
    | Wildcard of {
        wildcard: Token.t;
        variance: Token.t option;
        injective: Token.t option;
      }

  let cast = fun (node: node) -> cast_kind node Syntax_kind.TYPE_DECL

  let first_member_node = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = TYPE_DECL_MEMBER))

  let member_or_decl = fun decl ->
    match first_member_node decl with
    | Some member -> member
    | None -> decl

  let fold_token = fun decl ~init ~fn -> Node.fold_token decl ~init ~fn

  let for_each_token = fun decl ~fn ->
    fold_token
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let token_count = fun decl -> count_fold fold_token decl

  let keyword_token = fun decl ->
    Node.first_child_token
      (member_or_decl decl)
      ~kind:Syntax_kind.TYPE_KW

  let nonrec_token = fun decl ->
    Node.first_child_token
      (member_or_decl decl)
      ~kind:Syntax_kind.NONREC_KW

  let child_token_at_node = fun node index ->
    match Node.child_at node index with
    | Some (Syntax_tree.Token id) -> Some (wrap_token node.tree id)
    | Some (Syntax_tree.Node _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let child_token_at = fun (decl: type_declaration) index -> child_token_at_node decl index

  let child_token_kind_at_node = fun node index ->
    match child_token_at_node node index with
    | Some token -> Some (Token.kind token)
    | None -> None

  let child_token_kind_at = fun decl index ->
    match child_token_at decl index with
    | Some token -> Some (Token.kind token)
    | None -> None

  let rec collect_type_parameter_modifiers_in = fun node index variance injective ->
    match child_token_at_node node index with
    | Some token when token_kind_is token Syntax_kind.PLUS || token_kind_is token Syntax_kind.MINUS ->
        collect_type_parameter_modifiers_in node (index + 1) (Some token) injective
    | Some token when token_kind_is token Syntax_kind.BANG ->
        collect_type_parameter_modifiers_in node (index + 1) variance (Some token)
    | _ -> (index, variance, injective)

  let skip_type_parameter_in = fun node index ->
    let (index, _, _) = collect_type_parameter_modifiers_in node index None None in
    match child_token_kind_at_node node index with
    | Some Syntax_kind.QUOTE -> (
        match child_token_kind_at_node node (index + 1) with
        | Some Syntax_kind.IDENT -> index + 2
        | _ -> index + 1
      )
    | Some Syntax_kind.UNDERSCORE -> index + 1
    | _ -> index

  let emit_type_parameter_in = fun node index ~fn ->
    let (index, variance, injective) = collect_type_parameter_modifiers_in node index None None in
    match child_token_at_node node index with
    | Some quote when token_kind_is quote Syntax_kind.QUOTE -> (
        match child_token_at_node node (index + 1) with
        | Some name when token_kind_is name Syntax_kind.IDENT ->
            fn
              (
                Named {
                  name;
                  quote = Some quote;
                  variance;
                  injective;
                }
              );
            index + 2
        | _ -> index + 1
      )
    | Some wildcard when token_kind_is wildcard Syntax_kind.UNDERSCORE ->
        fn (Wildcard { wildcard; variance; injective });
        index + 1
    | _ -> index

  let rec skip_parenthesized_type_parameters_in = fun node index ->
    match child_token_kind_at_node node index with
    | Some Syntax_kind.RPAREN -> index + 1
    | Some Syntax_kind.EOF
    | None -> index
    | _ -> skip_parenthesized_type_parameters_in node (index + 1)

  let rec find_node_in = fun node index ~matches ->
    if index >= Node.child_count node then
      None
    else
      match Node.child_at node index with
      | Some (Syntax_tree.Node id) ->
          let child = wrap_node node.tree id in
          if node_matches child matches then
            Some child
          else
            find_node_in node (index + 1) ~matches
      | Some (Syntax_tree.Token _)
      | Some (Syntax_tree.Missing _)
      | None -> find_node_in node (index + 1) ~matches

  let name = fun (decl: t) ->
    let node = member_or_decl decl in
    let rec loop index =
      match child_token_at_node node index with
      | Some token when token_kind_is token Syntax_kind.TYPE_KW
      || token_kind_is token Syntax_kind.NONREC_KW -> loop (index + 1)
      | Some token when token_kind_is token Syntax_kind.LPAREN ->
          loop (skip_parenthesized_type_parameters_in node (index + 1))
      | Some token when token_kind_is token Syntax_kind.PLUS
      || token_kind_is token Syntax_kind.MINUS
      || token_kind_is token Syntax_kind.BANG
      || token_kind_is token Syntax_kind.QUOTE
      || token_kind_is token Syntax_kind.UNDERSCORE ->
          let next = skip_type_parameter_in node index in
          if next > index then
            loop next
          else
            None
      | Some token when token_kind_is token Syntax_kind.IDENT ->
          Ident.from_child_range_option node ~start_index:index ~stop_index:(index + 1)
      | _ -> None
    in
    loop 0

  let fold_parameter = fun decl ~init ~fn ->
    let node = member_or_decl decl in
    let acc = ref init in
    let returned = ref false in
    let emit_parameter = fun parameter ->
      if not !returned then
        match fn parameter !acc with
        | Continue next -> acc := next
        | Return value ->
            acc := value;
            returned := true
    in
    let rec parse_parenthesized index =
      match child_token_kind_at_node node index with
      | Some Syntax_kind.RPAREN -> index + 1
      | Some Syntax_kind.COMMA -> parse_parenthesized (index + 1)
      | Some Syntax_kind.EOF
      | None -> index
      | _ ->
          let next = emit_type_parameter_in node index ~fn:emit_parameter in
          parse_parenthesized
            (
              if next > index then
                next
              else
                index + 1
            )
    in
    let rec parse_head index =
      if !returned then
        ()
      else
        match child_token_at_node node index with
        | Some token when token_kind_is token Syntax_kind.TYPE_KW
        || token_kind_is token Syntax_kind.NONREC_KW -> parse_head (index + 1)
        | Some token when token_kind_is token Syntax_kind.LPAREN ->
            parse_head (parse_parenthesized (index + 1))
        | Some token when token_kind_is token Syntax_kind.PLUS
        || token_kind_is token Syntax_kind.MINUS
        || token_kind_is token Syntax_kind.BANG
        || token_kind_is token Syntax_kind.QUOTE
        || token_kind_is token Syntax_kind.UNDERSCORE ->
            let next = emit_type_parameter_in node index ~fn:emit_parameter in
            if next > index then
              parse_head next
        | _ -> ()
    in
    parse_head 0;
    !acc

  let for_each_parameter = fun decl ~fn ->
    fold_parameter
      decl
      ~init:()
      ~fn:(fun parameter () ->
        fn parameter;
        Continue ())

  let parameter_count = fun decl -> count_fold fold_parameter decl

  let manifest = fun decl -> find_node_in (member_or_decl decl) 0 ~matches:is_type_expr_kind

  module Member = struct
    type t = member

    let declaration = fun member -> member.declaration

    let start_index = fun member -> member.start_index

    let stop_index = fun member -> member.stop_index

    let covers_declaration = fun member ->
      Int.equal member.start_index 0
      && Int.equal member.stop_index (Node.child_count member.declaration)

    let child_count = fun member -> Node.child_count member.node

    let child_at = fun member index ->
      if index < 0 || index >= child_count member then
        None
      else
        Node.child_at member.node index

    let child_token_at = fun member index ->
      match child_at member index with
      | Some (Syntax_tree.Token id) -> Some (wrap_token member.node.tree id)
      | Some (Syntax_tree.Node _)
      | Some (Syntax_tree.Missing _)
      | None -> None

    let child_node_at = fun member index ->
      match child_at member index with
      | Some (Syntax_tree.Node id) -> Some (wrap_node member.node.tree id)
      | Some (Syntax_tree.Token _)
      | Some (Syntax_tree.Missing _)
      | None -> None

    let child_token_kind_at = fun member index ->
      match child_token_at member index with
      | Some token -> Some (Token.kind token)
      | None -> None

    let child_token_kind_is = fun member index kind ->
      match child_token_at member index with
      | Some token -> token_kind_is token kind
      | None -> false

    let fold_child = fun member ~init ~fn ->
      let rec loop index =
        if index >= Node.child_count member.node then
          init
        else
          match Node.child_at member.node index with
          | Some child -> (
              match fn child init with
              | Continue next -> loop_with_acc (index + 1) next
              | Return value -> value
            )
          | None -> loop_with_acc (index + 1) init
      and loop_with_acc index acc =
        if index >= Node.child_count member.node then
          acc
        else
          match Node.child_at member.node index with
          | Some child -> (
              match fn child acc with
              | Continue next -> loop_with_acc (index + 1) next
              | Return value -> value
            )
          | None -> loop_with_acc (index + 1) acc
      in
      loop 0

    let for_each_child = fun member ~fn ->
      fold_child
        member
        ~init:()
        ~fn:(fun child () ->
          fn child;
          Continue ())

    let fold_child_token = fun member ~init ~fn ->
      fold_child
        member
        ~init
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Syntax_tree.Token id -> fn (wrap_token member.node.tree id)
          | Syntax_tree.Node _
          | Syntax_tree.Missing _ -> fun acc -> Continue acc)

    let for_each_child_token = fun member ~fn ->
      fold_child_token
        member
        ~init:()
        ~fn:(fun token () ->
          fn token;
          Continue ())

    let fold_child_node = fun member ~init ~fn ->
      fold_child
        member
        ~init
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Syntax_tree.Node id -> fn (wrap_node member.node.tree id)
          | Syntax_tree.Token _
          | Syntax_tree.Missing _ -> fun acc -> Continue acc)

    let for_each_child_node = fun member ~fn ->
      fold_child_node
        member
        ~init:()
        ~fn:(fun node () ->
          fn node;
          Continue ())

    let find_node = fun member ~matches -> find_node_in member.node 0 ~matches

    let record_type = fun member -> find_node member ~matches:is_record_type_kind

    let variant_type = fun member -> find_node member ~matches:is_variant_type_kind

    let shell_token = fun member -> child_token_at member 0

    let nonrec_token = fun member ->
      let count = child_count member in
      let rec loop index =
        if index >= count then
          None
        else
          match child_token_at member index with
          | Some token when token_kind_is token Syntax_kind.NONREC_KW -> Some token
          | Some token when token_kind_is token Syntax_kind.IDENT -> None
          | _ -> loop (index + 1)
      in
      loop 0

    let name = fun member ->
      let rec loop index =
        match child_token_at member index with
        | Some token when token_kind_is token Syntax_kind.TYPE_KW
        || token_kind_is token Syntax_kind.AND_KW
        || token_kind_is token Syntax_kind.NONREC_KW -> loop (index + 1)
        | Some token when token_kind_is token Syntax_kind.LPAREN ->
            loop (skip_parenthesized_type_parameters_in member.node (index + 1))
        | Some token when token_kind_is token Syntax_kind.PLUS
        || token_kind_is token Syntax_kind.MINUS
        || token_kind_is token Syntax_kind.BANG
        || token_kind_is token Syntax_kind.QUOTE
        || token_kind_is token Syntax_kind.UNDERSCORE ->
            let next = skip_type_parameter_in member.node index in
            if next > index then
              loop next
            else
              None
        | Some token when token_kind_is token Syntax_kind.IDENT ->
            Ident.from_child_range_option member.node ~start_index:index ~stop_index:(index + 1)
        | _ -> None
      in
      loop 0

    let fold_parameter = fun member ~init ~fn ->
      let acc = ref init in
      let returned = ref false in
      let emit_parameter = fun parameter ->
        if not !returned then
          match fn parameter !acc with
          | Continue next -> acc := next
          | Return value ->
              acc := value;
              returned := true
      in
      let rec parse_parenthesized index =
        match child_token_kind_at member index with
        | Some Syntax_kind.RPAREN -> index + 1
        | Some Syntax_kind.COMMA -> parse_parenthesized (index + 1)
        | Some Syntax_kind.EOF
        | None -> index
        | _ ->
            let next = emit_type_parameter_in member.node index ~fn:emit_parameter in
            parse_parenthesized
              (
                if next > index then
                  next
                else
                  index + 1
              )
      in
      let rec parse_head index =
        if !returned then
          ()
        else
          match child_token_at member index with
          | Some token when token_kind_is token Syntax_kind.TYPE_KW
          || token_kind_is token Syntax_kind.AND_KW
          || token_kind_is token Syntax_kind.NONREC_KW -> parse_head (index + 1)
          | Some token when token_kind_is token Syntax_kind.LPAREN ->
              parse_head (parse_parenthesized (index + 1))
          | Some token when token_kind_is token Syntax_kind.PLUS
          || token_kind_is token Syntax_kind.MINUS
          || token_kind_is token Syntax_kind.BANG
          || token_kind_is token Syntax_kind.QUOTE
          || token_kind_is token Syntax_kind.UNDERSCORE ->
              let next = emit_type_parameter_in member.node index ~fn:emit_parameter in
              if next > index then
                parse_head next
          | _ -> ()
      in
      parse_head 0;
      !acc

    let for_each_parameter = fun member ~fn ->
      fold_parameter
        member
        ~init:()
        ~fn:(fun parameter () ->
          fn parameter;
          Continue ())

    let parameter_count = fun member -> count_fold fold_parameter member

    let manifest = fun member -> find_node_in member.node 0 ~matches:is_type_expr_kind
  end

  let for_each_member = fun decl ~fn ->
    let saw_member = ref false in
    let index = ref 0 in
    Node.for_each_child
      decl
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Syntax_tree.Node id ->
            let node = wrap_node decl.tree id in
            if node_kind_is node Syntax_kind.TYPE_DECL_MEMBER then (
              saw_member := true;
              fn
                {
                  declaration = decl;
                  node;
                  start_index = !index;
                  stop_index = !index + 1;
                }
            );
            index := !index + 1
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> index := !index + 1);
    if not !saw_member then
      fn
        {
          declaration = decl;
          node = decl;
          start_index = 0;
          stop_index = Node.child_count decl;
        }

  let fold_member = fun decl ~init ~fn ->
    let acc = ref init in
    let returned = ref false in
    for_each_member
      decl
      ~fn:(fun member ->
        if not !returned then
          match fn member !acc with
          | Continue next -> acc := next
          | Return value ->
              acc := value;
              returned := true);
    !acc

  let member_count = fun decl -> count_fold fold_member decl

  let fold_members = fun decl init fn ->
    fold_member
      decl
      ~init
      ~fn:(fun member acc -> Continue (fn acc member))
end

module TypeExtensionDeclaration = struct
  type t = type_extension_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type parameter = TypeDeclaration.parameter =
    | Named of {
        name: Token.t;
        quote: Token.t option;
        variance: Token.t option;
        injective: Token.t option;
      }
    | Wildcard of {
        wildcard: Token.t;
        variance: Token.t option;
        injective: Token.t option;
      }

  let cast = fun (node: node) -> cast_kind node Syntax_kind.TYPE_EXTENSION_DECL

  let head_node = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = TYPE_EXTENSION_DECL_HEAD))

  let body_node = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = TYPE_EXTENSION_DECL_BODY))

  let keyword_token = fun decl ->
    match head_node decl with
    | Some head -> Node.first_child_token head ~kind:Syntax_kind.TYPE_KW
    | None -> None

  let plus_token = fun decl ->
    match body_node decl with
    | Some body -> Node.first_child_token body ~kind:Syntax_kind.PLUS
    | None -> None

  let equals_token = fun decl ->
    match body_node decl with
    | Some body -> Node.first_child_token body ~kind:Syntax_kind.EQ
    | None -> None

  let rec collect_type_parameter_modifiers = fun decl index variance injective ->
    match child_token_at decl index with
    | Some token when token_kind_is token Syntax_kind.PLUS || token_kind_is token Syntax_kind.MINUS ->
        collect_type_parameter_modifiers decl (index + 1) (Some token) injective
    | Some token when token_kind_is token Syntax_kind.BANG ->
        collect_type_parameter_modifiers decl (index + 1) variance (Some token)
    | _ -> (index, variance, injective)

  let skip_type_parameter = fun decl index ->
    let (index, _, _) = collect_type_parameter_modifiers decl index None None in
    match child_token_kind_at decl index with
    | Some Syntax_kind.QUOTE -> (
        match child_token_kind_at decl (index + 1) with
        | Some Syntax_kind.IDENT -> index + 2
        | _ -> index + 1
      )
    | Some Syntax_kind.UNDERSCORE -> index + 1
    | _ -> index

  let emit_type_parameter = fun decl index ~fn ->
    let (index, variance, injective) = collect_type_parameter_modifiers decl index None None in
    match child_token_at decl index with
    | Some quote when token_kind_is quote Syntax_kind.QUOTE -> (
        match child_token_at decl (index + 1) with
        | Some name when token_kind_is name Syntax_kind.IDENT ->
            fn
              (
                Named {
                  name;
                  quote = Some quote;
                  variance;
                  injective;
                }
              );
            index + 2
        | _ -> index + 1
      )
    | Some wildcard when token_kind_is wildcard Syntax_kind.UNDERSCORE ->
        fn (Wildcard { wildcard; variance; injective });
        index + 1
    | _ -> index

  let rec skip_parenthesized_type_parameters = fun decl index ->
    match child_token_kind_at decl index with
    | Some Syntax_kind.RPAREN -> index + 1
    | Some Syntax_kind.EOF
    | None -> index
    | _ -> skip_parenthesized_type_parameters decl (index + 1)

  let fold_parameter = fun decl ~init ~fn ->
    match head_node decl with
    | None -> init
    | Some head ->
        let acc = ref init in
        let returned = ref false in
        let emit_parameter = fun parameter ->
          if not !returned then
            match fn parameter !acc with
            | Continue next -> acc := next
            | Return value ->
                acc := value;
                returned := true
        in
        let rec parse_parenthesized index =
          match child_token_kind_at head index with
          | Some Syntax_kind.RPAREN -> index + 1
          | Some Syntax_kind.COMMA -> parse_parenthesized (index + 1)
          | Some Syntax_kind.EOF
          | None -> index
          | _ ->
              let next = emit_type_parameter head index ~fn:emit_parameter in
              parse_parenthesized
                (
                  if next > index then
                    next
                  else
                    index + 1
                )
        in
        let rec parse_head index =
          if !returned then
            ()
          else
            match child_token_at head index with
            | Some token when token_kind_is token Syntax_kind.TYPE_KW -> parse_head (index + 1)
            | Some token when token_kind_is token Syntax_kind.LPAREN ->
                parse_head (parse_parenthesized (index + 1))
            | Some token when token_kind_is token Syntax_kind.PLUS
            || token_kind_is token Syntax_kind.MINUS
            || token_kind_is token Syntax_kind.BANG
            || token_kind_is token Syntax_kind.QUOTE
            || token_kind_is token Syntax_kind.UNDERSCORE ->
                let next = emit_type_parameter head index ~fn:emit_parameter in
                if next > index then
                  parse_head next
            | _ -> ()
        in
        parse_head 0;
        !acc

  let for_each_parameter = fun decl ~fn ->
    fold_parameter
      decl
      ~init:()
      ~fn:(fun parameter () ->
        fn parameter;
        Continue ())

  let parameter_count = fun decl -> count_fold fold_parameter decl

  let fold_name_ident = fun decl ~init ~fn ->
    match head_node decl with
    | None -> init
    | Some head ->
        let acc = ref init in
        let returned = ref false in
        let emit_ident = fun token ->
          if not !returned then
            match fn token !acc with
            | Continue next -> acc := next
            | Return value ->
                acc := value;
                returned := true
        in
        let rec parse_name index =
          if !returned then
            ()
          else
            match child_token_at head index with
            | Some token when token_kind_is token Syntax_kind.IDENT ->
                emit_ident token;
                parse_name (index + 1)
            | Some token when token_kind_is token Syntax_kind.DOT -> parse_name (index + 1)
            | Some _ -> parse_name (index + 1)
            | None -> ()
        in
        let rec parse_head index =
          if !returned then
            ()
          else
            match child_token_at head index with
            | Some token when token_kind_is token Syntax_kind.TYPE_KW -> parse_head (index + 1)
            | Some token when token_kind_is token Syntax_kind.LPAREN ->
                parse_head (skip_parenthesized_type_parameters head (index + 1))
            | Some token when token_kind_is token Syntax_kind.PLUS
            || token_kind_is token Syntax_kind.MINUS
            || token_kind_is token Syntax_kind.BANG
            || token_kind_is token Syntax_kind.QUOTE
            || token_kind_is token Syntax_kind.UNDERSCORE ->
                let next = skip_type_parameter head index in
                if next > index then
                  parse_head next
            | _ -> parse_name index
        in
        parse_head 0;
        !acc

  let for_each_name_ident = fun decl ~fn ->
    fold_name_ident
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let name_ident_count = fun decl -> count_fold fold_name_ident decl

  let name = fun decl ->
    match head_node decl with
    | None -> None
    | Some head ->
        let rec skip_head index =
          match child_token_at head index with
          | Some token when token_kind_is token Syntax_kind.TYPE_KW -> skip_head (index + 1)
          | Some token when token_kind_is token Syntax_kind.LPAREN ->
              skip_head (skip_parenthesized_type_parameters head (index + 1))
          | Some token when token_kind_is token Syntax_kind.PLUS
          || token_kind_is token Syntax_kind.MINUS
          || token_kind_is token Syntax_kind.BANG
          || token_kind_is token Syntax_kind.QUOTE
          || token_kind_is token Syntax_kind.UNDERSCORE ->
              let next = skip_type_parameter head index in
              if next > index then
                skip_head next
              else
                index
          | _ -> index
        in
        let start = skip_head 0 in
        let rec stop index =
          match child_token_at head index with
          | Some token when token_kind_is token Syntax_kind.IDENT
          || token_kind_is token Syntax_kind.DOT -> stop (index + 1)
          | _ -> index
        in
        let stop = stop start in
        if stop > start then
          Ident.from_child_range_option head ~start_index:start ~stop_index:stop
        else
          None

  let variant_type = fun decl ->
    match body_node decl with
    | Some body -> first_child_node_matching body ~matches:is_variant_type_kind
    | None -> None
end

let child_token_kind_is = fun node index kind ->
  match child_token_at node index with
  | Some token -> token_kind_is token kind
  | None -> false

let attribute_suffix_start_at = fun node close_index ->
  if not (child_token_kind_is node close_index Syntax_kind.RBRACKET) then
    None
  else
    let rec loop index depth =
      if Int.(index < 0) then
        None
      else if child_token_kind_is node index Syntax_kind.RBRACKET then
        loop Int.(index - 1) Int.(depth + 1)
      else if child_token_kind_is node index Syntax_kind.LBRACKET then
        if Int.equal depth 1 then
          let next = Int.(index + 1) in
          if
            child_token_kind_is node next Syntax_kind.AT
            || child_token_kind_is node next Syntax_kind.ATAT
          then
            Some index
          else
            None
        else
          loop Int.(index - 1) Int.(depth - 1)
      else
        loop Int.(index - 1) depth
    in
    loop Int.(close_index - 1) 1

let last_non_attribute_suffix_token_index = fun node ->
  let rec loop index =
    if Int.(index < 0) then (
      (-1)
    ) else
      match attribute_suffix_start_at node index with
      | Some start -> loop Int.(start - 1)
      | None -> index
  in
  loop Int.(Node.child_count node - 1)

module ModuleDeclaration = struct
  type t = module_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type member = {
    declaration: module_declaration;
    node: node;
    start_index: int;
    stop_index: int;
  }

  type body =
    | Expr of {
        body: module_expr;
      }
    | Type of {
        body: module_type_expr;
      }
    | Unsupported of {
        body: node option;
      }

  let cast = fun (node: node) -> cast_kind node Syntax_kind.MODULE_DECL

  let first_member_node = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = MODULE_DECL_MEMBER))

  let member_or_decl = fun decl ->
    match first_member_node decl with
    | Some member -> member
    | None -> decl

  let name = fun decl -> Ident.first_or_underscore (member_or_decl decl)

  let rec_token = fun decl -> Node.first_child_token (member_or_decl decl) ~kind:Syntax_kind.REC_KW

  let is_recursive = fun decl ->
    match rec_token decl with
    | Some _ -> true
    | None -> false

  let separator_token = fun decl ->
    first_child_token_matching
      (member_or_decl decl)
      ~matches:(fun kind -> Syntax_kind.(kind = EQ || kind = COLON))

  module Member = struct
    type t = member

    let declaration = fun member -> member.declaration

    let start_index = fun member -> member.start_index

    let stop_index = fun member -> member.stop_index

    let child_count = fun member -> Node.child_count member.node

    let child_at = fun member index -> Node.child_at member.node index

    let child_token_at = fun member index ->
      match child_at member index with
      | Some (Syntax_tree.Token id) -> Some (wrap_token member.node.tree id)
      | Some (Syntax_tree.Node _)
      | Some (Syntax_tree.Missing _)
      | None -> None

    let child_node_at = fun member index ->
      match child_at member index with
      | Some (Syntax_tree.Node id) -> Some (wrap_node member.node.tree id)
      | Some (Syntax_tree.Token _)
      | Some (Syntax_tree.Missing _)
      | None -> None

    let child_token_kind_is = fun member index kind ->
      match child_token_at member index with
      | Some token -> token_kind_is token kind
      | None -> false

    let fold_child = fun member ~init ~fn ->
      let rec loop index =
        if index >= Node.child_count member.node then
          init
        else
          match Node.child_at member.node index with
          | Some child -> (
              match fn child init with
              | Continue next -> loop_with_acc (index + 1) next
              | Return value -> value
            )
          | None -> loop_with_acc (index + 1) init
      and loop_with_acc index acc =
        if index >= Node.child_count member.node then
          acc
        else
          match Node.child_at member.node index with
          | Some child -> (
              match fn child acc with
              | Continue next -> loop_with_acc (index + 1) next
              | Return value -> value
            )
          | None -> loop_with_acc (index + 1) acc
      in
      loop 0

    let for_each_child = fun member ~fn ->
      fold_child
        member
        ~init:()
        ~fn:(fun child () ->
          fn child;
          Continue ())

    let fold_child_token = fun member ~init ~fn ->
      fold_child
        member
        ~init
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Syntax_tree.Token id -> fn (wrap_token member.node.tree id)
          | Syntax_tree.Node _
          | Syntax_tree.Missing _ -> fun acc -> Continue acc)

    let for_each_child_token = fun member ~fn ->
      fold_child_token
        member
        ~init:()
        ~fn:(fun token () ->
          fn token;
          Continue ())

    let fold_child_node = fun member ~init ~fn ->
      fold_child
        member
        ~init
        ~fn:(fun __tmp1 ->
          match __tmp1 with
          | Syntax_tree.Node id -> fn (wrap_node member.node.tree id)
          | Syntax_tree.Token _
          | Syntax_tree.Missing _ -> fun acc -> Continue acc)

    let for_each_child_node = fun member ~fn ->
      fold_child_node
        member
        ~init:()
        ~fn:(fun node () ->
          fn node;
          Continue ())

    let name = fun member -> Ident.first_or_underscore member.node

    let find_token = fun member kind ->
      let count = child_count member in
      let rec loop index =
        if index >= count then
          None
        else if child_token_kind_is member index kind then
          Some index
        else
          loop (index + 1)
      in
      loop 0

    let find_node = fun member ~matches ->
      let count = child_count member in
      let rec loop index =
        if index >= count then
          None
        else
          match child_node_at member index with
          | Some node when node_matches node matches -> Some node
          | _ -> loop (index + 1)
      in
      loop 0

    let first_specific_module_expr = fun node ->
      match Node.kind node with
      | Syntax_kind.MODULE_EXPR -> first_child_node_matching node ~matches:is_module_expr_kind
      | kind when is_module_expr_kind kind -> Some node
      | _ -> None

    let first_specific_module_type = fun node ->
      match Node.kind node with
      | Syntax_kind.MODULE_TYPE_EXPR -> first_child_node_matching node ~matches:is_module_type_kind
      | kind when is_module_type_kind kind -> Some node
      | _ -> None

    let module_expr = fun member ->
      match find_node
        member
        ~matches:(fun kind -> is_module_expr_kind kind || Syntax_kind.(kind = MODULE_EXPR)) with
      | Some node -> first_specific_module_expr node
      | None -> None

    let module_type = fun member ->
      match find_node
        member
        ~matches:(fun kind -> is_module_type_kind kind || Syntax_kind.(kind = MODULE_TYPE_EXPR)) with
      | Some node -> first_specific_module_type node
      | None -> None
  end

  let for_each_member = fun decl ~fn ->
    let saw_member = ref false in
    let index = ref 0 in
    Node.for_each_child
      decl
      ~fn:(fun __tmp1 ->
        match __tmp1 with
        | Syntax_tree.Node id ->
            let node = wrap_node decl.tree id in
            if node_kind_is node Syntax_kind.MODULE_DECL_MEMBER then (
              saw_member := true;
              fn
                {
                  declaration = decl;
                  node;
                  start_index = !index;
                  stop_index = !index + 1;
                }
            );
            index := !index + 1
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> index := !index + 1);
    if not !saw_member then
      fn
        {
          declaration = decl;
          node = decl;
          start_index = 0;
          stop_index = Node.child_count decl;
        }

  let fold_member = fun decl ~init ~fn ->
    let acc = ref init in
    let returned = ref false in
    for_each_member
      decl
      ~fn:(fun member ->
        if not !returned then
          match fn member !acc with
          | Continue next -> acc := next
          | Return value ->
              acc := value;
              returned := true);
    !acc

  let member_count = fun decl -> count_fold fold_member decl

  let fold_members = fun decl init fn ->
    fold_member
      decl
      ~init
      ~fn:(fun member acc -> Continue (fn acc member))

  let child_node_at = fun (decl: module_declaration) index ->
    match Node.child_at decl index with
    | Some (Syntax_tree.Node id) -> Some (wrap_node decl.tree id)
    | Some (Syntax_tree.Token _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let separator_index = fun node ->
    let count = Node.child_count node in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at node index with
        | Some token when token_kind_is token Syntax_kind.EQ
        || token_kind_is token Syntax_kind.COLON -> Some index
        | _ -> loop (index + 1)
    in
    loop 0

  let body_node = fun decl ->
    let node = member_or_decl decl in
    match separator_index node with
    | None -> None
    | Some separator_index ->
        let count = Node.child_count node in
        let rec loop index =
          if index >= count then
            None
          else
            match child_node_at node index with
            | Some node when node_matches
              node
              (fun kind -> is_module_expr_kind kind || is_module_type_kind kind) -> Some node
            | _ -> loop (index + 1)
        in
        loop (separator_index + 1)

  let first_specific_module_body = fun node ->
    match Node.kind node with
    | Syntax_kind.MODULE_EXPR -> first_child_node_matching node ~matches:is_module_expr_kind
    | Syntax_kind.MODULE_TYPE_EXPR -> first_child_node_matching node ~matches:is_module_type_kind
    | kind when is_module_expr_kind kind || is_module_type_kind kind -> Some node
    | _ -> None

  let body_specific_node = fun decl ->
    match body_node decl with
    | Some node -> first_specific_module_body node
    | None -> None

  let body = fun decl ->
    match body_node decl with
    | Some node -> (
        match ModuleExpr.cast node with
        | Node body -> Expr { body }
        | Unknown _
        | Error _ -> (
            match ModuleTypeExpr.cast node with
            | Node body -> Type { body }
            | Unknown _
            | Error _ -> Unsupported { body = Some node }
          )
      )
    | None -> Unsupported { body = None }

  let structure_body_node = fun decl ->
    match body decl with
    | Expr { body } -> (
        match ModuleExpr.view body with
        | ModuleExpr.Structure { body } -> Some body
        | _ -> None
      )
    | _ -> None

  let signature_body_node = fun decl ->
    match body decl with
    | Type { body } -> (
        match ModuleTypeExpr.view body with
        | ModuleTypeExpr.Signature { body } -> Some body
        | _ -> None
      )
    | _ -> None

  let struct_token = fun decl ->
    match structure_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.STRUCT_KW
    | None -> None

  let sig_token = fun decl ->
    match signature_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.SIG_KW
    | None -> None

  let end_token = fun decl ->
    match structure_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.END_KW
    | None -> (
        match signature_body_node decl with
        | Some node -> Node.first_child_token node ~kind:Syntax_kind.END_KW
        | None -> None
      )

  let body_ident = fun decl ->
    match body decl with
    | Expr { body } -> ModuleExpr.ident body
    | Type { body } -> ModuleTypeExpr.ident body
    | _ -> None

  let typeof_body_node = fun decl ->
    match body decl with
    | Type { body } -> (
        match ModuleTypeExpr.view body with
        | ModuleTypeExpr.Typeof _ -> Some body
        | _ -> None
      )
    | _ -> None

  let has_typeof_body = fun decl ->
    match typeof_body_node decl with
    | Some _ -> true
    | None -> false

  let typeof_body_ident = fun decl ->
    match body decl with
    | Type { body } -> (
        match ModuleTypeExpr.view body with
        | ModuleTypeExpr.Typeof { body = Some body } -> ModuleExpr.ident body
        | _ -> None
      )
    | _ -> None

  let fold_structure_item = fun decl ~init ~fn ->
    match structure_body_node decl with
    | Some node ->
        fold_child_node_matching
          node
          ~matches:(fun kind -> Syntax_kind.(kind = STRUCTURE_ITEM))
          ~init
          ~fn
    | None -> init

  let for_each_structure_item = fun decl ~fn ->
    fold_structure_item
      decl
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let structure_item_count = fun decl -> count_fold fold_structure_item decl

  let fold_signature_item = fun decl ~init ~fn ->
    match signature_body_node decl with
    | Some node ->
        fold_child_node_matching
          node
          ~matches:(fun kind -> Syntax_kind.(kind = SIGNATURE_ITEM))
          ~init
          ~fn
    | None -> init

  let for_each_signature_item = fun decl ~fn ->
    fold_signature_item
      decl
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let signature_item_count = fun decl -> count_fold fold_signature_item decl

  let fold_sig_body_token = fun decl ~init ~fn ->
    match signature_body_node decl with
    | None -> init
    | Some node ->
        let acc = ref init in
        let returned = ref false in
        let inside = ref false in
        Node.for_each_child
          node
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Syntax_tree.Token id ->
                let token = wrap_token node.tree id in
                if !returned then
                  ()
                else if token_kind_is token Syntax_kind.SIG_KW then
                  inside := true
                else if token_kind_is token Syntax_kind.END_KW then
                  inside := false
                else if !inside then (
                  match fn token !acc with
                  | Continue next -> acc := next
                  | Return value ->
                      acc := value;
                      returned := true
                )
            | Syntax_tree.Node id ->
                if !inside && not !returned then
                  acc := Node.fold_token
                    (wrap_node node.tree id)
                    ~init:!acc
                    ~fn:(fun token acc ->
                      match fn token acc with
                      | Continue next -> Continue next
                      | Return value ->
                          returned := true;
                          Return value)
            | Syntax_tree.Missing _ -> ());
        !acc

  let for_each_sig_body_token = fun decl ~fn ->
    fold_sig_body_token
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let sig_body_token_count = fun decl -> count_fold fold_sig_body_token decl
end

module ModuleTypeDeclaration = struct
  type t = module_type_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type body =
    | Abstract
    | Manifest of {
        body: module_type_expr;
      }
    | Unsupported of {
        body: node option;
      }

  let cast = fun (node: node) -> cast_kind node Syntax_kind.MODULE_TYPE_DECL

  let head_node = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = MODULE_TYPE_DECL_HEAD))

  let body_group = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = MODULE_TYPE_DECL_BODY))

  let name = fun decl ->
    match head_node decl with
    | Some head -> Ident.last head
    | None -> None

  let equals_token = fun decl ->
    match body_group decl with
    | Some body -> Node.first_child_token body ~kind:Syntax_kind.EQ
    | None -> None

  let fold_head_token = fun decl ~init ~fn ->
    match head_node decl with
    | Some head -> Node.fold_child_token head ~init ~fn
    | None -> init

  let for_each_head_token = fun decl ~fn ->
    fold_head_token
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let head_token_count = fun decl -> count_fold fold_head_token decl

  let body_node = fun decl ->
    match body_group decl with
    | Some body ->
        first_child_node_matching body ~matches:(fun kind -> Syntax_kind.(kind = MODULE_TYPE_EXPR))
    | None -> None

  let first_specific_module_type = fun node ->
    match Node.kind node with
    | Syntax_kind.MODULE_TYPE_EXPR -> first_child_node_matching node ~matches:is_module_type_kind
    | kind when is_module_type_kind kind -> Some node
    | _ -> None

  let body_specific_node = fun decl ->
    match body_node decl with
    | Some node -> first_specific_module_type node
    | None -> None

  let body = fun decl ->
    match body_group decl with
    | None -> Abstract
    | Some body_group -> (
        match body_node decl with
        | Some body -> Manifest { body }
        | None -> Unsupported { body = Some body_group }
      )

  let signature_body_node = fun decl ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind.SIGNATURE_MODULE_TYPE -> Some node
    | _ -> None

  let sig_token = fun decl ->
    match signature_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.SIG_KW
    | None -> None

  let end_token = fun decl ->
    match signature_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind.END_KW
    | None -> None

  let body_ident = fun decl ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind.PATH_MODULE_TYPE -> Ident.from_node_option node
    | _ -> None

  let fold_signature_item = fun decl ~init ~fn ->
    match signature_body_node decl with
    | Some node ->
        fold_child_node_matching
          node
          ~matches:(fun kind -> Syntax_kind.(kind = SIGNATURE_ITEM))
          ~init
          ~fn
    | None -> init

  let for_each_signature_item = fun decl ~fn ->
    fold_signature_item
      decl
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let signature_item_count = fun decl -> count_fold fold_signature_item decl

  let fold_sig_body_token = fun decl ~init ~fn ->
    match signature_body_node decl with
    | None -> init
    | Some node ->
        let acc = ref init in
        let returned = ref false in
        let inside = ref false in
        Node.for_each_child
          node
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Syntax_tree.Token id ->
                let token = wrap_token node.tree id in
                if !returned then
                  ()
                else if token_kind_is token Syntax_kind.SIG_KW then
                  inside := true
                else if token_kind_is token Syntax_kind.END_KW then
                  inside := false
                else if !inside then (
                  match fn token !acc with
                  | Continue next -> acc := next
                  | Return value ->
                      acc := value;
                      returned := true
                )
            | Syntax_tree.Node id ->
                if !inside && not !returned then
                  acc := Node.fold_token
                    (wrap_node node.tree id)
                    ~init:!acc
                    ~fn:(fun token acc ->
                      match fn token acc with
                      | Continue next -> Continue next
                      | Return value ->
                          returned := true;
                          Return value)
            | Syntax_tree.Missing _ -> ());
        !acc

  let for_each_sig_body_token = fun decl ~fn ->
    fold_sig_body_token
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let sig_body_token_count = fun decl -> count_fold fold_sig_body_token decl

  let constrained_body_node = fun decl ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind.WITH_MODULE_TYPE -> Some node
    | _ -> None

  let base_module_type = fun decl ->
    match constrained_body_node decl with
    | Some node -> first_child_node_matching node ~matches:is_module_type_kind
    | None -> None

  let fold_constraint = fun decl ~init ~fn ->
    match constrained_body_node decl with
    | None -> init
    | Some node ->
        Node.fold_child_node
          node
          ~init
          ~fn:(fun child acc ->
            if
              node_kind_is child Syntax_kind.WITH_TYPE_CONSTRAINT
              || node_kind_is child Syntax_kind.WITH_MODULE_CONSTRAINT
            then
              fn child acc
            else
              Continue acc)

  let constraint_count = fun decl -> count_fold fold_constraint decl

  let for_each_constraint = fun decl ~fn ->
    fold_constraint
      decl
      ~init:()
      ~fn:(fun constraint_ () ->
        fn constraint_;
        Continue ())
end

module ModuleTypeConstraint = struct
  type t = module_type_constraint

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Type of {
        ident: Ident.t;
        operator: token;
        body: type_expr;
      }
    | Module of {
        ident: Ident.t;
        operator: token;
        body: node;
      }
    | Unknown of node

  let cast = fun (node: node) ->
    if
      node_kind_is node Syntax_kind.WITH_TYPE_CONSTRAINT
      || node_kind_is node Syntax_kind.WITH_MODULE_CONSTRAINT
    then
      Node node
    else
      cast_failure node ~expected:Syntax_kind.[ WITH_TYPE_CONSTRAINT; WITH_MODULE_CONSTRAINT ]

  let type_ident = fun constraint_ ->
    match first_child_node_matching constraint_ ~matches:Ident.is_ident_kind with
    | Some node -> cast_result_to_option (Ident.cast node)
    | None -> None

  let type_body = fun constraint_ ->
    match nth_child_node_matching constraint_ 1 ~matches:is_type_expr_kind with
    | Some node -> cast_result_to_option (TypeExpr.cast node)
    | None -> None

  let module_ident = fun constraint_ ->
    match first_child_node_matching constraint_ ~matches:Ident.is_ident_kind with
    | Some node -> cast_result_to_option (Ident.cast node)
    | None -> None

  let module_body = fun constraint_ ->
    nth_child_node_matching
      constraint_
      1
      ~matches:is_module_expr_kind

  let type_operator = fun constraint_ ->
    first_child_token_matching
      constraint_
      ~matches:(fun kind -> Syntax_kind.(kind = EQ || kind = COLONEQ || kind = PLUS))

  let module_operator = fun constraint_ ->
    first_child_token_matching
      constraint_
      ~matches:(fun kind -> Syntax_kind.(kind = EQ || kind = COLONEQ))

  let view = fun (constraint_: t) ->
    if node_kind_is constraint_ Syntax_kind.WITH_TYPE_CONSTRAINT then
      match (type_ident constraint_, type_operator constraint_, type_body constraint_) with
      | (Some ident, Some operator, Some body) -> Type { ident; operator; body }
      | _ -> Unknown constraint_
    else if node_kind_is constraint_ Syntax_kind.WITH_MODULE_CONSTRAINT then
      match (module_ident constraint_, module_operator constraint_, module_body constraint_) with
      | (Some ident, Some operator, Some body) -> Module { ident; operator; body }
      | _ -> Unknown constraint_
    else
      Unknown constraint_
end

module OpenDeclaration = struct
  type t = open_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) -> cast_kind node Syntax_kind.OPEN_DECL

  let ident = fun decl ->
    match first_child_node_matching decl ~matches:is_module_expr_kind with
    | Some node -> ModuleExpr.ident node
    | _ -> (
        match first_child_node_matching decl ~matches:is_module_type_kind with
        | Some node -> ModuleTypeExpr.ident node
        | _ ->
            let start_index =
              if child_token_kind_is decl 1 Syntax_kind.BANG then
                2
              else
                1
            in
            Ident.from_child_range_option decl ~start_index ~stop_index:(Node.child_count decl)
      )
end

module IncludeDeclaration = struct
  type t = include_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) -> cast_kind node Syntax_kind.INCLUDE_DECL

  let first_specific_body = fun decl ->
    let rec find_body index =
      if index >= Node.child_count decl then
        None
      else
        match Node.child_at decl index with
        | Some (Syntax_tree.Node id) ->
            let node = wrap_node decl.tree id in
            if
              node_matches node (fun kind -> is_module_expr_kind kind || is_module_type_kind kind)
            then (
              match Node.kind node with
              | Syntax_kind.MODULE_EXPR ->
                  first_child_node_matching node ~matches:is_module_expr_kind
              | Syntax_kind.MODULE_TYPE_EXPR ->
                  first_child_node_matching node ~matches:is_module_type_kind
              | kind when is_module_expr_kind kind || is_module_type_kind kind -> Some node
              | _ -> None
            ) else
              find_body (index + 1)
        | Some (Syntax_tree.Token _)
        | Some (Syntax_tree.Missing _)
        | None -> find_body (index + 1)
    in
    find_body 0

  let body_node = first_specific_body

  let body_ident = fun decl ->
    match first_specific_body decl with
    | Some node when node_kind_is node Syntax_kind.PATH_MODULE_EXPR
    || node_kind_is node Syntax_kind.PATH_MODULE_TYPE -> Ident.from_node_option node
    | _ -> None
end

let fold_declaration_name_token = fun decl ~keyword ~init ~fn ->
  let (_, _, acc) =
    Node.fold_child_token
      decl
      ~init:(false, false, init)
      ~fn:(fun token (seen_keyword, done_, acc) ->
        if done_ then
          Return (seen_keyword, done_, acc)
        else if token_kind_is token Syntax_kind.COLON then
          Return (seen_keyword, true, acc)
        else if seen_keyword then
          match fn token acc with
          | Continue next -> Continue (seen_keyword, done_, next)
          | Return value -> Return (seen_keyword, true, value)
        else if token_kind_is token keyword then
          Continue (true, done_, acc)
        else
          Continue (seen_keyword, done_, acc))
  in
  acc

let for_each_declaration_name_token = fun decl ~keyword ~fn ->
  fold_declaration_name_token
    decl
    ~keyword
    ~init:()
    ~fn:(fun token () ->
      fn token;
      Continue ())

let declaration_name_ident = fun decl ~keyword ->
  let child_count = Node.child_count decl in
  let rec find_keyword index =
    if index >= child_count then
      None
    else
      match child_token_at decl index with
      | Some token when token_kind_is token keyword -> Some index
      | _ -> find_keyword (index + 1)
  in
  match find_keyword 0 with
  | None -> None
  | Some keyword_index ->
      let start_index = keyword_index + 1 in
      let rec find_colon index =
        if index >= child_count then
          None
        else
          match child_token_at decl index with
          | Some token when token_kind_is token Syntax_kind.COLON -> Some index
          | _ -> find_colon (index + 1)
      in
      match find_colon start_index with
      | Some stop_index when stop_index > start_index ->
          Ident.from_child_range_option decl ~start_index ~stop_index
      | _ -> None

module ValueDeclaration = struct
  type t = value_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Value of {
        name: Ident.t;
        colon_token: Token.t;
        annotation: type_expr;
      }
    | Unknown of node

  let cast = fun (node: node) -> cast_kind node Syntax_kind.VAL_DECL

  let name = fun decl -> declaration_name_ident decl ~keyword:Syntax_kind.VAL_KW

  let colon_token = fun decl -> Node.first_child_token decl ~kind:Syntax_kind.COLON

  let type_annotation = first_type_expr_child

  let fold_name_token = fun decl ~init ~fn ->
    fold_declaration_name_token
      decl
      ~keyword:Syntax_kind.VAL_KW
      ~fn
      ~init

  let for_each_name_token = fun decl ~fn ->
    fold_name_token
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let name_token_count = fun decl -> count_fold fold_name_token decl

  let name_tokens = fun decl ->
    let tokens = Vector.with_capacity ~size:(Node.child_count decl) in
    for_each_name_token decl ~fn:(fun token -> Vector.push tokens ~value:token);
    tokens

  let view = fun decl ->
    match (name decl, colon_token decl, type_annotation decl) with
    | (Some name, Some colon_token, Some annotation) -> Value { name; colon_token; annotation }
    | _ -> Unknown decl

  let fold_annotation_token = fun decl ~init ~fn ->
    let acc = ref init in
    let returned = ref false in
    for_each_token_after_child_token
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = COLON))
      ~fn:(fun token ->
        if not !returned then
          match fn token !acc with
          | Continue next -> acc := next
          | Return value ->
              acc := value;
              returned := true);
    !acc

  let for_each_annotation_token = fun decl ~fn ->
    fold_annotation_token
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let annotation_token_count = fun decl -> count_fold fold_annotation_token decl
end

module ExternalDeclaration = struct
  type t = external_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | External of {
        name: Ident.t;
        colon_token: Token.t;
        annotation: type_expr;
        equals_token: Token.t;
        primitives: Token.t Vector.t;
        attributes: Token.t Vector.t;
      }
    | Unknown of node

  let cast = fun (node: node) -> cast_kind node Syntax_kind.EXTERNAL_DECL

  let name = fun decl -> declaration_name_ident decl ~keyword:Syntax_kind.EXTERNAL_KW

  let colon_token = fun decl -> Node.first_child_token decl ~kind:Syntax_kind.COLON

  let equals_token = fun decl -> Node.first_child_token decl ~kind:Syntax_kind.EQ

  let type_annotation = first_type_expr_child

  let fold_name_token = fun decl ~init ~fn ->
    fold_declaration_name_token
      decl
      ~keyword:Syntax_kind.EXTERNAL_KW
      ~fn
      ~init

  let for_each_name_token = fun decl ~fn ->
    fold_name_token
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let name_token_count = fun decl -> count_fold fold_name_token decl

  let name_tokens = fun decl ->
    let tokens = Vector.with_capacity ~size:(Node.child_count decl) in
    for_each_name_token decl ~fn:(fun token -> Vector.push tokens ~value:token);
    tokens

  let fold_primitive_string = fun decl ~init ~fn ->
    Node.fold_child_token
      decl
      ~init
      ~fn:(fun token acc ->
        if token_kind_is token Syntax_kind.STRING then
          fn token acc
        else
          Continue acc)

  let for_each_primitive_string = fun decl ~fn ->
    fold_primitive_string
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let primitive_string_count = fun decl -> count_fold fold_primitive_string decl

  let primitive_strings = fun decl ->
    let tokens = Vector.with_capacity ~size:(Node.child_count decl) in
    for_each_primitive_string decl ~fn:(fun token -> Vector.push tokens ~value:token);
    tokens

  let fold_attribute_token = fun decl ~init ~fn ->
    let (_, _, acc) =
      Node.fold_child_token
        decl
        ~init:(false, false, init)
        ~fn:(fun token (seen_primitive, after_primitives, acc) ->
          if after_primitives then
            match fn token acc with
            | Continue next -> Continue (seen_primitive, after_primitives, next)
            | Return value -> Return (seen_primitive, after_primitives, value)
          else if token_kind_is token Syntax_kind.STRING then
            Continue (true, after_primitives, acc)
          else if seen_primitive then
            match fn token acc with
            | Continue next -> Continue (seen_primitive, true, next)
            | Return value -> Return (seen_primitive, true, value)
          else
            Continue (seen_primitive, after_primitives, acc))
    in
    acc

  let attribute_token_count = fun decl -> count_fold fold_attribute_token decl

  let for_each_attribute_token = fun decl ~fn ->
    fold_attribute_token
      decl
      ~init:()
      ~fn:(fun token () ->
        fn token;
        Continue ())

  let attribute_tokens = fun decl ->
    let tokens = Vector.with_capacity ~size:(Node.child_count decl) in
    for_each_attribute_token decl ~fn:(fun token -> Vector.push tokens ~value:token);
    tokens

  let view = fun decl ->
    match (name decl, colon_token decl, type_annotation decl, equals_token decl) with
    | (Some name, Some colon_token, Some annotation, Some equals_token) ->
        let primitives = primitive_strings decl in
        if Int.equal (Vector.length primitives) 0 then
          Unknown decl
        else
          External {
            name;
            colon_token;
            annotation;
            equals_token;
            primitives;
            attributes = attribute_tokens decl;
          }
    | _ -> Unknown decl
end

module ExceptionDeclaration = struct
  type t = exception_declaration

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type payload =
    | TypeExpr of type_expr
    | Record of record_type

  type view =
    | Bare
    | Alias of {
        equals_token: token;
        ident: Ident.t;
      }
    | Payload of {
        of_token: token;
        payload: payload;
      }
    | Unknown of node

  let cast = fun (node: node) -> cast_kind node Syntax_kind.EXCEPTION_DECL

  let head_node = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = EXCEPTION_DECL_HEAD))

  let keyword_token = fun decl ->
    match head_node decl with
    | Some head -> Node.first_child_token head ~kind:Syntax_kind.EXCEPTION_KW
    | None -> None

  let name = fun decl ->
    match head_node decl with
    | Some head -> Ident.last head
    | None -> None

  let view = fun decl ->
    match first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind.(kind = EXCEPTION_ALIAS || kind = EXCEPTION_PAYLOAD)) with
    | Some rhs when node_kind_is rhs Syntax_kind.EXCEPTION_ALIAS ->
        let ident =
          match first_child_node_matching rhs ~matches:Ident.is_ident_kind with
          | Some ident -> cast_result_to_option (Ident.cast ident)
          | None -> None
        in
        (
          match (Node.first_child_token rhs ~kind:Syntax_kind.EQ, ident) with
          | (Some equals_token, Some ident) -> Alias { equals_token; ident }
          | _ -> Unknown rhs
        )
    | Some rhs when node_kind_is rhs Syntax_kind.EXCEPTION_PAYLOAD ->
        let payload =
          match first_child_node_matching rhs ~matches:is_record_type_kind with
          | Some record -> Some (Record record)
          | None -> (
              match first_child_node_matching rhs ~matches:is_type_expr_kind with
              | Some type_expr -> Some (TypeExpr type_expr)
              | None -> None
            )
        in
        (
          match (Node.first_child_token rhs ~kind:Syntax_kind.OF_KW, payload) with
          | (Some of_token, Some payload) -> Payload { of_token; payload }
          | _ -> Unknown rhs
        )
    | _ -> Bare
end

module ExtensionItem = struct
  type t = extension_item

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) -> cast_kind node Syntax_kind.EXTENSION_ITEM

  let fold_shell_token = Node.fold_child_token

  let shell_token_count = fun item -> count_fold fold_shell_token item

  let for_each_shell_token = Node.for_each_child_token
end

module AttributeItem = struct
  type t = attribute_item

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) -> cast_kind node Syntax_kind.ATTRIBUTE_ITEM

  let fold_shell_token = Node.fold_child_token

  let shell_token_count = fun item -> count_fold fold_shell_token item

  let for_each_shell_token = Node.for_each_child_token
end

module ExprItem = struct
  type t = expr_item

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) -> cast_kind node Syntax_kind.EXPR_ITEM

  let expr = first_expr_child
end

module StructureItem = struct
  type t = structure_item

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Let of let_declaration
    | Type of type_item
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Extension of extension_item
    | Attribute of attribute_item
    | Expr of expr_item
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) -> cast_kind node Syntax_kind.STRUCTURE_ITEM

  let declaration = fun (item: structure_item) ->
    first_child_node_matching
      item
      ~matches:(fun kind -> not Syntax_kind.(kind = ERROR))

  let fold_attribute_suffix = fun item ~init ~fn ->
    match declaration item with
    | None -> init
    | Some declaration ->
        let after_declaration = ref false in
        Node.fold_child_node
          item
          ~init
          ~fn:(fun child acc ->
            if !after_declaration then
              if node_kind_is child Syntax_kind.ATTRIBUTE_ITEM then
                match AttributeItem.cast child with
                | Node attribute -> fn attribute acc
                | Unknown _
                | Error _ -> Continue acc
              else
                Continue acc
            else (
              if same_node child declaration then
                after_declaration := true;
              Continue acc
            ))

  let attribute_suffix_count = fun item -> count_fold fold_attribute_suffix item

  let view = fun (item: structure_item) ->
    match declaration item with
    | Some node -> (
        match Node.kind node with
        | Syntax_kind.LET_DECL -> Let node
        | Syntax_kind.TYPE_DECL -> Type (TypeDeclarationItem node)
        | Syntax_kind.TYPE_EXTENSION_DECL -> Type (TypeExtensionItem node)
        | Syntax_kind.MODULE_DECL -> Module node
        | Syntax_kind.MODULE_TYPE_DECL -> ModuleType node
        | Syntax_kind.OPEN_DECL -> Open node
        | Syntax_kind.INCLUDE_DECL -> Include node
        | Syntax_kind.EXTERNAL_DECL -> External node
        | Syntax_kind.EXCEPTION_DECL -> Exception node
        | Syntax_kind.EXTENSION_ITEM -> Extension node
        | Syntax_kind.ATTRIBUTE_ITEM -> Attribute node
        | Syntax_kind.EXPR_ITEM -> Expr node
        | Syntax_kind.ERROR -> Error node
        | kind when is_expr_kind kind -> Expr node
        | _ -> Unknown node
      )
    | None -> Unknown item
end

module SignatureItem = struct
  type t = signature_item

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  type view =
    | Value of value_declaration
    | Type of type_item
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Extension of extension_item
    | Attribute of attribute_item
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) -> cast_kind node Syntax_kind.SIGNATURE_ITEM

  let declaration = fun (item: signature_item) ->
    first_child_node_matching
      item
      ~matches:(fun _ -> true)

  let fold_attribute_suffix = fun item ~init ~fn ->
    match declaration item with
    | None -> init
    | Some declaration ->
        let after_declaration = ref false in
        Node.fold_child_node
          item
          ~init
          ~fn:(fun child acc ->
            if !after_declaration then
              if node_kind_is child Syntax_kind.ATTRIBUTE_ITEM then
                match AttributeItem.cast child with
                | Node attribute -> fn attribute acc
                | Unknown _
                | Error _ -> Continue acc
              else
                Continue acc
            else (
              if same_node child declaration then
                after_declaration := true;
              Continue acc
            ))

  let attribute_suffix_count = fun item -> count_fold fold_attribute_suffix item

  let view = fun (item: signature_item) ->
    match declaration item with
    | Some node -> (
        match Node.kind node with
        | Syntax_kind.VAL_DECL -> Value node
        | Syntax_kind.TYPE_DECL -> Type (TypeDeclarationItem node)
        | Syntax_kind.TYPE_EXTENSION_DECL -> Type (TypeExtensionItem node)
        | Syntax_kind.MODULE_DECL -> Module node
        | Syntax_kind.MODULE_TYPE_DECL -> ModuleType node
        | Syntax_kind.OPEN_DECL -> Open node
        | Syntax_kind.INCLUDE_DECL -> Include node
        | Syntax_kind.EXTERNAL_DECL -> External node
        | Syntax_kind.EXCEPTION_DECL -> Exception node
        | Syntax_kind.EXTENSION_ITEM -> Extension node
        | Syntax_kind.ATTRIBUTE_ITEM -> Attribute node
        | Syntax_kind.ERROR -> Error node
        | _ -> Unknown node
      )
    | None -> Unknown item
end

module Implementation = struct
  type t = implementation

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) -> cast_kind node Syntax_kind.IMPLEMENTATION

  let fold_item = fun (impl: implementation) ~init ~fn ->
    fold_child_node_matching
      impl
      ~matches:(fun kind -> Syntax_kind.(kind = STRUCTURE_ITEM))
      ~init
      ~fn

  let for_each_item = fun impl ~fn ->
    fold_item
      impl
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let item_count = fun impl -> count_fold fold_item impl
end

module Interface = struct
  type t = interface

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let cast = fun (node: node) -> cast_kind node Syntax_kind.INTERFACE

  let fold_item = fun (interface: interface) ~init ~fn ->
    fold_child_node_matching
      interface
      ~matches:(fun kind -> Syntax_kind.(kind = SIGNATURE_ITEM))
      ~init
      ~fn

  let for_each_item = fun interface ~fn ->
    fold_item
      interface
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let item_count = fun interface -> count_fold fold_item interface
end

module SourceFile = struct
  type t = source_file

  let as_node = fun (value: t) -> (value: node)

  let kind = Node.kind

  let span = Node.span

  let width = Node.width

  let full_width = Node.full_width

  type view =
    | Implementation of implementation
    | Interface of interface

  let make = root

  let implementation = fun (source_file: source_file) ->
    Node.first_child_node
      source_file
      ~kind:Syntax_kind.IMPLEMENTATION

  let interface = fun (source_file: source_file) ->
    Node.first_child_node
      source_file
      ~kind:Syntax_kind.INTERFACE

  let view = fun (source_file: source_file) ->
    match implementation source_file with
    | Some impl -> Implementation impl
    | None -> (
        match interface source_file with
        | Some interface -> Interface interface
        | None -> panic "Ast.SourceFile.view expected implementation or interface"
      )

  let fold_item = fun (source_file: source_file) ~init ~fn ->
    match view source_file with
    | Implementation impl -> Implementation.fold_item impl ~init ~fn
    | Interface interface -> Interface.fold_item interface ~init ~fn

  let for_each_item = fun source_file ~fn ->
    fold_item
      source_file
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let item_count = fun source_file -> count_fold fold_item source_file

  let fold_structure_item = fun (source_file: source_file) ~init ~fn ->
    match view source_file with
    | Implementation impl -> Implementation.fold_item impl ~init ~fn
    | Interface _ -> init

  let for_each_structure_item = fun source_file ~fn ->
    fold_structure_item
      source_file
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let structure_item_count = fun source_file -> count_fold fold_structure_item source_file

  let fold_signature_item = fun (source_file: source_file) ~init ~fn ->
    match view source_file with
    | Implementation _ -> init
    | Interface interface -> Interface.fold_item interface ~init ~fn

  let for_each_signature_item = fun source_file ~fn ->
    fold_signature_item
      source_file
      ~init:()
      ~fn:(fun item () ->
        fn item;
        Continue ())

  let signature_item_count = fun source_file -> count_fold fold_signature_item source_file
end
