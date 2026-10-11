open Std
open Ast
open TypeScheme
open Constraint
open TypeExpr

module InferenceEnv = State.InferenceEnv
module SurfacePath = Model.Surface_path

let unsupported_syntax state (origin: Ast.origin) summary =
  State.add_diagnostic
    state
    (Diagnostics.Diagnostic.UnsupportedSyntax {
      span = origin.span;
      kind = origin.kind;
      summary;
    })

let unsupported_pattern state (pattern: pattern) =
  unsupported_syntax state pattern.origin "pattern form is not supported by typ yet"

let unresolved_name state (origin: Ast.origin) ~namespace name =
  State.add_diagnostic
    state
    (Diagnostics.Diagnostic.unresolved_name
      ~span:origin.span
      ~namespace
      ~name:(SurfacePath.to_string name))

let parameter_label_to_type_label = fun __tmp1 ->
  match __tmp1 with
  | Unlabeled -> Type.Label.NoLabel
  | Labeled label -> Type.Label.Labelled (SurfacePath.to_string label)
  | Optional label -> Type.Label.Optional (SurfacePath.to_string label)

let bind_value ~mode state ~name type_ =
  let scheme =
    match mode with
    | Local -> TypeScheme.monomorphic type_
    | Generalized -> Quantifier.generalize state type_
  in
  State.add_value state ~name ~scheme

let literal_type (literal: literal) =
  match literal with
  | Int -> Builtin.int
  | Float -> Builtin.float
  | Char -> Builtin.char
  | String -> Builtin.string
  | Bool -> Builtin.bool

let record_is_immutable state (fields: record_expression_field list) =
  match fields with
  | [] -> false
  | first :: _ -> (
      match State.get_record_field state ~name:first.name with
      | None -> false
      | Some info ->
          match info.State.InferenceEnv.owner.definition.kind with
          | Record declared_fields ->
              List.all declared_fields ~fn:(fun field -> not field.mutable_)
          | _ -> false
    )

let rec expression_is_nonexpansive state (expr: expression) =
  match expr.kind with
  | Literal _
  | Ident _
  | Function _ -> true
  | Constructor { payload = None; _ } -> true
  | Constructor { payload = Some payload; _ } -> expression_is_nonexpansive state payload
  | Tuple parts
  | List parts -> List.all parts ~fn:(expression_is_nonexpansive state)
  | Record { update = None; fields } ->
      record_is_immutable state fields
      && List.all fields ~fn:(fun field -> expression_is_nonexpansive state field.value)
  | _ -> false

let rec bind_pattern ~mode (state: State.t) (pattern: pattern) type_ =
  pattern.type_ <- Some type_;
  match pattern.kind with
  | Wildcard -> ()
  | Constructor ctr -> bind_constructor state pattern ctr type_
  | Bind name -> bind_value ~mode state ~name type_
  | Literal literal ->
      let expected = literal_type literal in
      unify state ~expected ~actual:type_ ~on_error:(pattern_constraint_diagnostic pattern)
  | List patterns ->
      let element = State.fresh_var state in
      unify
        state
        ~expected:(Builtin.list element)
        ~actual:type_
        ~on_error:(pattern_constraint_diagnostic pattern);
      List.for_each patterns ~fn:(fun pattern -> bind_pattern ~mode state pattern element)
  | Cons { head; tail } ->
      let element = State.fresh_var state in
      let list = Builtin.list element in
      unify state ~expected:list ~actual:type_ ~on_error:(pattern_constraint_diagnostic pattern);
      bind_pattern ~mode state head element;
      bind_pattern ~mode state tail list
  | Constraint { pattern; annotation } ->
      let expected = core_type_to_type_scoped state annotation in
      unify state ~expected ~actual:type_ ~on_error:(annotation_diagnostic annotation);
      bind_pattern ~mode state pattern expected
  | Attribute pattern -> bind_pattern ~mode state pattern type_
  | Alias { pattern; alias } ->
      bind_pattern ~mode state pattern type_;
      bind_pattern ~mode state alias type_
  | Or { left; right } ->
      bind_pattern ~mode state left type_;
      State.push_scope state;
      bind_pattern ~mode state right type_;
      State.pop_scope state
  | Tuple parts -> bind_tuple ~mode state pattern parts type_
  | Record record -> bind_record ~mode state pattern record type_
  | _ -> unsupported_pattern state pattern

and bind_record_field ~mode state pattern record_type (field: record_pattern_field) =
  match State.get_record_field state ~name:field.name with
  | None -> unresolved_name state field.origin ~namespace:"field" field.name
  | Some info ->
      let (owner_type, field_type) = instantiate_record_field state info in
      unify
        state
        ~expected:record_type
        ~actual:owner_type
        ~on_error:(pattern_constraint_diagnostic pattern);
      match field.pattern with
      | Some field_pattern -> bind_pattern ~mode state field_pattern field_type
      | None -> bind_value ~mode state ~name:field.name field_type

and bind_record ~mode state pattern fields type_ =
  match fields with
  | [] -> ()
  | first_field :: _ ->
      match State.get_record_field state ~name:first_field.name with
      | None ->
          List.for_each fields ~fn:(fun field ->
            unresolved_name state field.origin ~namespace:"field" field.name)
      | Some info ->
          let (record_type, _) = instantiate_record_field state info in
          unify
            state
            ~expected:record_type
            ~actual:type_
            ~on_error:(pattern_constraint_diagnostic pattern);
          List.for_each fields ~fn:(bind_record_field ~mode state pattern record_type)

and bind_tuple ~mode state pattern parts type_ =
  let expected_parts = List.map parts ~fn:(fun _ -> State.fresh_var state) in
  let expected = Type.Tuple expected_parts in
  unify state ~expected ~actual:type_ ~on_error:(pattern_constraint_diagnostic pattern);
  List.zip parts expected_parts
  |> List.for_each ~fn:(fun (part, type_) -> bind_pattern ~mode state part type_)

and inline_record_find_field inline_record name =
  List.find
    inline_record.InferenceEnv.fields
    ~fn:(fun field -> SurfacePath.equal field.InferenceEnv.declaration.name name)

and bind_inline_record_pattern
  state
  (payload_pattern: pattern)
  (fields: record_pattern_field list)
  inline_record =
  payload_pattern.type_ <- Some inline_record.State.InferenceEnv.payload_type;
  List.for_each
    fields
    ~fn:(fun field ->
      match inline_record_find_field inline_record field.name with
      | None -> unresolved_name state field.origin ~namespace:"field" field.name
      | Some expected_field -> (
          match field.pattern with
          | Some field_pattern -> bind_pattern ~mode:Local state field_pattern expected_field.type_
          | None -> bind_value ~mode:Local state ~name:field.name expected_field.type_
        ))

and bind_constructor state pattern ctr type_ =
  match ctr with
  | { ident; payload = None } when Builtin.is_unit ident ->
      unify
        state
        ~expected:Builtin.unit
        ~actual:type_
        ~on_error:(pattern_constraint_diagnostic pattern)
  | { ident; payload = None } ->
      let constructor_type =
        match ConstructorDescription.instantiate_from_state state ident with
        | None ->
            unresolved_name state pattern.origin ~namespace:"constructor" ident;
            State.fresh_var state
        | Some constructor -> constructor.scheme.body
      in
      unify
        state
        ~expected:constructor_type
        ~actual:type_
        ~on_error:(pattern_constraint_diagnostic pattern)
  | { ident; payload = Some ({ kind = Record fields; _ } as payload_pattern) } -> (
      match ConstructorDescription.instantiate_from_state state ident with
      | None ->
          unresolved_name state pattern.origin ~namespace:"constructor" ident;
          let payload_type = State.fresh_var state in
          unify
            state
            ~expected:(Ast.Type.arrow payload_type type_)
            ~actual:(State.fresh_var state)
            ~on_error:(pattern_constraint_diagnostic pattern);
          bind_pattern ~mode:Local state payload_pattern payload_type
      | Some constructor -> (
          let payload_type = State.fresh_var state in
          unify
            state
            ~expected:constructor.scheme.body
            ~actual:(Ast.Type.arrow payload_type type_)
            ~on_error:(pattern_constraint_diagnostic pattern);
          match constructor.arguments with
          | InferenceEnv.InlineRecord inline_record ->
              bind_inline_record_pattern state payload_pattern fields inline_record
          | InferenceEnv.Tuple _ -> bind_pattern ~mode:Local state payload_pattern payload_type
        )
    )
  | { ident; payload = Some payload_pattern } ->
      let constructor_type =
        match ConstructorDescription.instantiate_from_state state ident with
        | None ->
            unresolved_name state pattern.origin ~namespace:"constructor" ident;
            State.fresh_var state
        | Some constructor -> constructor.scheme.body
      in
      let payload_type = State.fresh_var state in
      unify
        state
        ~expected:constructor_type
        ~actual:(Ast.Type.arrow payload_type type_)
        ~on_error:(pattern_constraint_diagnostic pattern);
      bind_pattern ~mode:Local state payload_pattern payload_type

let infer_literal _state (lit: literal) = literal_type lit

let is_builtin_value ident name =
  match SurfacePath.to_segments ident with
  | [ segment ] -> String.equal segment name
  | _ -> false

let infer_ident (state: State.t) (expr: expression) ident =
  let fresh = fun () -> State.fresh_var state in
  let arrow = Ast.Type.arrow in
  match State.get_value state ~name:ident with
  | Some scheme -> Quantifier.instantiate state scheme
  | None when SurfacePath.equal ident Builtin.raise_ident -> arrow Builtin.exn (fresh ())
  | None when SurfacePath.equal ident Builtin.ref_ident ->
      let value = fresh () in
      arrow value (Builtin.ref_ value)
  | None when SurfacePath.equal ident Builtin.deref_ident ->
      let value = fresh () in
      arrow (Builtin.ref_ value) value
  | None when SurfacePath.equal ident Builtin.assign_ident ->
      let value = fresh () in
      arrow (Builtin.ref_ value) (arrow value Builtin.unit)
  | None when is_builtin_value ident "print_endline" || is_builtin_value ident "print_string" ->
      arrow Builtin.string Builtin.unit
  | None when is_builtin_value ident "print_int" -> arrow Builtin.int Builtin.unit
  | None when is_builtin_value ident "ignore" -> arrow (fresh ()) Builtin.unit
  | None when is_builtin_value ident "failwith" || is_builtin_value ident "invalid_arg" ->
      arrow Builtin.string (fresh ())
  | None when is_builtin_value ident "not" -> arrow Builtin.bool Builtin.bool
  | None when is_builtin_value ident "assert" -> arrow Builtin.bool Builtin.unit
  | None when is_builtin_value ident "string_of_int" -> arrow Builtin.int Builtin.string
  | None when is_builtin_value ident "int_of_string" -> arrow Builtin.string Builtin.int
  | None when is_builtin_value ident "float_of_string" -> arrow Builtin.string Builtin.float
  | None when is_builtin_value ident "int_of_float" -> arrow Builtin.float Builtin.int
  | None when is_builtin_value ident "float_of_int" -> arrow Builtin.int Builtin.float
  | None when is_builtin_value ident "char_of_int" -> arrow Builtin.int Builtin.char
  | None when is_builtin_value ident "int_of_char" -> arrow Builtin.char Builtin.int
  | None when is_builtin_value ident "fst" || is_builtin_value ident "snd" ->
      let first = fresh () in
      let second = fresh () in
      let result = if is_builtin_value ident "fst" then first else second in
      arrow (Type.Tuple [ first; second ]) result
  | None ->
      unresolved_name state expr.origin ~namespace:"value" ident;
      fresh ()

let rec check_expression (state: State.t) (expr: expression) expected =
  let expected =
    match expr.type_hint with
    | None -> expected
    | Some hint ->
        let annotation = core_type_to_type_scoped state hint.type_ in
        unify
          state
          ~expected:annotation
          ~actual:expected
          ~on_error:(expression_hint_diagnostic expr hint);
        annotation
  in
  let _ = check_expression_node state expr expected in
  expr.type_ <- Some expected;
  expected

and infer_expression (state: State.t) (expr: expression) =
  match expr.type_hint with
  | None -> infer_expression_node state expr
  | Some hint ->
      let expected = core_type_to_type_scoped state hint.type_ in
      let _ = check_expression_node state expr expected in
      expr.type_ <- Some expected;
      expected

and infer_expression_node (state: State.t) (expr: expression) =
  let inferred =
    match expr.kind with
    | If ifelse -> infer_if_else state ifelse
    | Function fn -> infer_function state expr fn
    | Apply apply -> infer_apply state apply
    | Infix infix -> infer_infix state expr infix
    | Literal lit -> infer_literal state lit
    | Ident ident -> infer_ident state expr ident
    | Constructor constructor -> infer_constructor state expr constructor
    | Tuple parts -> infer_tuple state parts
    | List items -> infer_list state items
    | Array items -> infer_array state items
    | Sequence sequence -> infer_sequence state sequence
    | Assign assignment -> infer_assignment state assignment
    | Try try_ -> infer_try state try_
    | While loop -> infer_while state loop
    | For loop -> infer_for state loop
    | Assert condition -> infer_assert state condition
    | Let letexpr -> infer_let_expr state letexpr
    | Match match_ -> infer_match state match_
    | Record record -> infer_record state record
    | FieldAccess access -> infer_field_access state expr access
    | _ ->
        unsupported_syntax state expr.origin "expression form is not supported by typ yet";
        State.fresh_var state
  in
  expr.type_ <- Some inferred;
  inferred

and check_expression_node state (expr: expression) expected =
  match expr.kind with
  | Function fn -> check_function state expr fn expected
  | Tuple parts -> check_tuple state expr parts expected
  | List items -> check_list state expr items expected
  | If ifelse -> check_if_else state ifelse expected
  | Match match_ -> check_match state match_ expected
  | Try try_ -> check_try state try_ expected
  | Let let_ -> check_let_expr state let_ expected
  | Sequence sequence ->
      let _ = check_expression state sequence.left Builtin.unit in
      check_expression state sequence.right expected
  | _ ->
      let actual = infer_expression_node state expr in
      unify
        state
        ~expected
        ~actual
        ~on_error:(expression_constraint_diagnostic expr);
      expected

and check_tuple state expr parts expected =
  let expected_parts =
    match Unifier.resolve expected with
    | Type.Tuple expected_parts when List.length expected_parts = List.length parts ->
        Some expected_parts
    | Type.Var _ ->
        let expected_parts = List.map parts ~fn:(fun _ -> State.fresh_var state) in
        unify
          state
          ~expected
          ~actual:(Type.Tuple expected_parts)
          ~on_error:(expression_constraint_diagnostic expr);
        Some expected_parts
    | _ -> None
  in
  match expected_parts with
  | Some expected_parts ->
      List.zip parts expected_parts
      |> List.for_each ~fn:(fun (part, expected) ->
        let _ = check_expression state part expected in
        ());
      expected
  | None ->
      let actual = infer_tuple state parts in
      unify state ~expected ~actual ~on_error:(expression_constraint_diagnostic expr);
      expected

and check_list state expr items expected =
  let element_type =
    match Unifier.resolve expected with
    | Type.Apply { ident; arguments = [ element ] }
      when SurfacePath.equal ident Builtin.list_ident -> Some element
    | Type.Var _ ->
        let element = State.fresh_var state in
        unify
          state
          ~expected
          ~actual:(Builtin.list element)
          ~on_error:(expression_constraint_diagnostic expr);
        Some element
    | _ -> None
  in
  match element_type with
  | Some element_type ->
      List.for_each items ~fn:(fun item ->
        let _ = check_expression state item element_type in
        ());
      expected
  | None ->
      let actual = infer_list state items in
      unify state ~expected ~actual ~on_error:(expression_constraint_diagnostic expr);
      expected

and check_if_else state ifelse expected =
  let _ = check_expression state ifelse.condition Builtin.bool in
  match ifelse.else_branch with
  | Some else_branch ->
      let _ = check_expression state ifelse.then_branch expected in
      let _ = check_expression state else_branch expected in
      expected
  | None ->
      unify
        state
        ~expected:Builtin.unit
        ~actual:expected
        ~on_error:(expression_constraint_diagnostic ifelse.then_branch);
      let _ = check_expression state ifelse.then_branch Builtin.unit in
      Builtin.unit

and check_match state match_ expected =
  let scrutinee_type = infer_expression state match_.scrutinee in
  check_cases state match_.cases scrutinee_type expected;
  expected

and check_try state try_ expected =
  let _ = check_expression state try_.body expected in
  List.for_each try_.cases ~fn:(fun (case: match_case) ->
    State.push_scope state;
    bind_pattern ~mode:Local state case.pattern Builtin.exn;
    (match case.guard with
    | None -> ()
    | Some guard ->
        let _ = check_expression state guard Builtin.bool in
        ());
    let _ = check_expression state case.body expected in
    State.pop_scope state);
  expected

and check_let_expr state (let_: let_expression) expected =
  State.push_scope state;
  List.for_each let_.bindings ~fn:(fun binding ->
    let _ = infer_let_binding state ~mode:Generalized binding in
    ());
  let body = check_expression state let_.body expected in
  State.pop_scope state;
  body

and check_function state (expr: expression) (fn: fun_decl) expected =
  match fn.body with
  | Cases cases when fn.parameters = [] ->
      let expected_arrow =
        match Unifier.resolve expected with
        | Type.Arrow arrow -> Some arrow
        | Type.Var _ ->
            let parameter = State.fresh_var state in
            let result = State.fresh_var state in
            let arrow = Type.arrow parameter result in
            unify state ~expected ~actual:arrow ~on_error:(expression_constraint_diagnostic expr);
            Some { Type.label = Type.Label.NoLabel; parameter; result }
        | _ -> None
      in
      (match expected_arrow with
      | Some arrow ->
          check_cases state cases arrow.parameter arrow.result;
          expected
      | None ->
          let actual = infer_function state expr fn in
          unify state ~expected ~actual ~on_error:(expression_constraint_diagnostic expr);
          expected)
  | _ ->
      let rec collect expected parameters reversed =
        match parameters with
        | [] -> Some (List.reverse reversed, expected)
        | (parameter: parameter) :: rest ->
            let label = parameter_label_to_type_label parameter.label in
            (match Unifier.resolve expected with
            | Type.Arrow arrow when Type.Label.equal arrow.label label ->
                collect arrow.result rest ((parameter, arrow.parameter) :: reversed)
            | Type.Var _ ->
                let parameter_type = State.fresh_var state in
                let result_type = State.fresh_var state in
                let arrow = Type.arrow ~label parameter_type result_type in
                unify state ~expected ~actual:arrow ~on_error:(expression_constraint_diagnostic expr);
                collect result_type rest ((parameter, parameter_type) :: reversed)
            | _ -> None)
      in
      (match collect expected fn.parameters [] with
      | None ->
          let actual = infer_function state expr fn in
          unify state ~expected ~actual ~on_error:(expression_constraint_diagnostic expr);
          expected
      | Some (parameters, result_type) ->
          State.push_scope state;
          List.for_each parameters ~fn:(fun ((parameter: parameter), parameter_type) ->
            (match parameter.annotation with
            | None -> ()
            | Some annotation ->
                let annotation_type = core_type_to_type_scoped state annotation in
                unify
                  state
                  ~expected:parameter_type
                  ~actual:annotation_type
                  ~on_error:(annotation_diagnostic annotation));
            bind_pattern ~mode:Local state parameter.pattern parameter_type;
            (match parameter.default with
            | None -> ()
            | Some default ->
                let _ = check_expression state default parameter_type in
                ()));
          (match fn.body with
          | Body body ->
              let _ = check_expression state body result_type in
              ()
          | Cases cases -> check_cases state cases result_type result_type);
          State.pop_scope state;
          expected)

and check_cases state cases scrutinee_type result_type =
  List.for_each cases ~fn:(fun (case: match_case) ->
    State.push_scope state;
    bind_pattern ~mode:Local state case.pattern scrutinee_type;
    (match case.guard with
    | None -> ()
    | Some guard ->
        let _ = check_expression state guard Builtin.bool in
        ());
    let _ = check_expression state case.body result_type in
    State.pop_scope state)

and infer_unknown_record state (fields: record_expression_field list) =
  List.for_each
    fields
    ~fn:(fun field ->
      unresolved_name state field.origin ~namespace:"field" field.name;
      let _ = infer_expression state field.value in
      ());
  State.fresh_var state

and infer_visible_record_field_type state record_type (field: record_expression_field) =
  match State.get_record_field state ~name:field.name with
  | Some info ->
      let (field_owner_type, field_type) = instantiate_record_field state info in
      unify
        state
        ~expected:record_type
        ~actual:field_owner_type
        ~on_error:(expression_constraint_diagnostic field.value);
      Some field_type
  | None -> None

and infer_record_fields state (fields: record_expression_field list) ~field_type =
  List.for_each
    fields
    ~fn:(fun field ->
      match field_type field with
      | None ->
          unresolved_name state field.origin ~namespace:"field" field.name;
          let _ = infer_expression state field.value in
          ()
      | Some expected ->
          let _ = check_expression state field.value expected in
          ())

and infer_record_body state (fields: record_expression_field list) =
  match fields with
  | [] -> State.fresh_var state
  | first_field :: _ -> (
      match State.get_record_field state ~name:first_field.name with
      | None -> infer_unknown_record state fields
      | Some info ->
          let (record_type, _) = instantiate_record_field state info in
          infer_record_fields
            state
            fields
            ~field_type:(infer_visible_record_field_type state record_type);
          record_type
    )

and infer_record state record =
  match record with
  (* TODO(@leostera): model empty records instead of falling back to a hole. *)
  | { update = None; fields = [] } -> State.fresh_var state

  (* TODO(@leostera): decide whether empty updates should be accepted here. *)
  | { update = Some update; fields = [] } -> infer_expression state update

  (* if you have `{hello=1}` we'll type the record body *)
  | { update = None; fields } -> infer_record_body state fields

  (* if you have `{hello with x=1}` we'll type the record body and unify against the updated value *)
  | { update = Some update; fields } ->
      let record_type = infer_record_body state fields in
      let update_type = infer_expression state update in
      unify
        state
        ~expected:update_type
        ~actual:record_type
        ~on_error:(expression_constraint_diagnostic update);
      record_type

and infer_field_access state expr access =
  let receiver_type = infer_expression state access.receiver in
  match State.get_record_field state ~name:access.field with
  | None ->
      unresolved_name state expr.origin ~namespace:"field" access.field;
      State.fresh_var state
  | Some info ->
      let (owner_type, field_type) = instantiate_record_field state info in
      unify
        state
        ~expected:owner_type
        ~actual:receiver_type
        ~on_error:(expression_constraint_diagnostic access.receiver);
      field_type

and infer_match state match_ =
  let scrutinee_type = infer_expression state match_.scrutinee in
  infer_cases state match_.cases scrutinee_type

and infer_cases state cases scrutinee_type =
  let result_type = State.fresh_var state in
  List.for_each cases ~fn:(fun (case: match_case) ->
    State.push_scope state;
    bind_pattern ~mode:Local state case.pattern scrutinee_type;
    (match case.guard with
    | Some guard ->
        let _ = check_expression state guard Builtin.bool in
        ()
    | None -> ());
    let body_type = infer_expression state case.body in
    unify
      state
      ~expected:result_type
      ~actual:body_type
      ~on_error:(expression_constraint_diagnostic case.body);
    State.pop_scope state);
  result_type

and infer_let_binding state ~mode (bind: let_binding) =
  (* Infer at a deeper HM level, then generalize only variables that do not
     escape through the surrounding environment. *)
  State.push_scope state;
  let binding_type =
    match bind.type_hint with
    | None -> infer_expression state bind.expr
    | Some hint ->
        let expected = core_type_to_type_scoped state hint in
        let _ = check_expression state bind.expr expected in
        expected
  in
  State.pop_scope state;
  let binding_mode =
    match mode with
    | Generalized when not (expression_is_nonexpansive state bind.expr) -> Local
    | _ -> mode
  in
  bind_pattern ~mode:binding_mode state bind.pattern binding_type;
  binding_type

and infer_let_expr state (let_: let_expression) =
  State.push_scope state;
  List.for_each
    let_.bindings
    ~fn:(fun binding ->
      let _ = infer_let_binding state ~mode:Generalized binding in
      ());
  let body_type = infer_expression state let_.body in
  State.pop_scope state;
  body_type

(**
   When inferring lists, we will start with a fresh variable and unify it
   against every list element type.
*)
and infer_list state items =
  let element = State.fresh_var state in
  List.for_each
    items
    ~fn:(fun item ->
      let actual = infer_expression state item in
      unify state ~expected:element ~actual ~on_error:(expression_constraint_diagnostic item));
  Builtin.list element

and infer_apply state (apply: application) =
  Apply.infer
    state
    ~infer_expression:(infer_expression state)
    ~check_expression:(check_expression state)
    apply

and infer_inline_record_field_type inline_record (field: record_expression_field) =
  match inline_record_find_field inline_record field.name with
  | None -> None
  | Some expected_field -> Some expected_field.type_

and infer_inline_record_payload
  state
  payload_expr
  (fields: record_expression_field list)
  (inline_record: InferenceEnv.inline_record) =
  payload_expr.type_ <- Some inline_record.payload_type;
  infer_record_fields state fields ~field_type:(infer_inline_record_field_type inline_record)

and infer_inline_record_constructor
  state
  (expr: expression)
  (payload_expr: expression)
  (fields: record_expression_field list)
  (constructor: InferenceEnv.constructor_description)
  inline_record =
  let result = State.fresh_var state in
  unify
    state
    ~expected:constructor.scheme.body
    ~actual:(Ast.Type.arrow inline_record.InferenceEnv.payload_type result)
    ~on_error:(expression_constraint_diagnostic expr);
  infer_inline_record_payload state payload_expr fields inline_record;
  result

and infer_constructor state (expr: expression) (constructor: constructor_expression) =
  match constructor with
  (* when there's no payload and the ident is a built in, just return the builtin type *)
  | { ident; payload = None } when Builtin.is_unit ident -> Builtin.unit
  | { ident; payload = None } -> (
      match ConstructorDescription.instantiate_from_state state ident with
      | Some constructor -> constructor.scheme.body
      | None ->
          unresolved_name state expr.origin ~namespace:"constructor" ident;
          State.fresh_var state
    )
  | { ident; payload = Some ({ kind = Record { update = None; fields }; _ } as payload_expr) } -> (
      match ConstructorDescription.instantiate_from_state state ident with
      | Some ({ arguments = InferenceEnv.InlineRecord inline_record; _ } as constructor) ->
          infer_inline_record_constructor state expr payload_expr fields constructor inline_record
      | _ -> infer_constructor_payload state expr ident payload_expr
    )
  | { ident; payload = Some payload_expr } ->
      infer_constructor_payload state expr ident payload_expr

and infer_constructor_payload state expr ident payload_expr =
  let payload_type = infer_expression state payload_expr in
  let result = State.fresh_var state in
  let constructor_type =
    match ConstructorDescription.instantiate_from_state state ident with
    | None ->
        unresolved_name state expr.origin ~namespace:"constructor" ident;
        State.fresh_var state
    | Some constructor -> constructor.scheme.body
  in
  unify
    state
    ~expected:constructor_type
    ~actual:(Ast.Type.arrow payload_type result)
    ~on_error:(expression_constraint_diagnostic expr);
  result

and infer_infix state expr (infix: infix_operation) =
  let left = infer_expression state infix.left in
  let right = infer_expression state infix.right in
  let operator = SurfacePath.to_string infix.operator in
  let unify_operand expected actual source =
    unify
      state
      ~expected
      ~actual
      ~on_error:(expression_constraint_diagnostic source)
  in
  match operator with
  | "+"
  | "-"
  | "*"
  | "/"
  | "mod"
  | "land"
  | "lor"
  | "lxor"
  | "lsl"
  | "lsr"
  | "asr" ->
      unify_operand Builtin.int left infix.left;
      unify_operand Builtin.int right infix.right;
      Builtin.int
  | "+."
  | "-."
  | "*."
  | "/." ->
      unify_operand Builtin.float left infix.left;
      unify_operand Builtin.float right infix.right;
      Builtin.float
  | "^" ->
      unify_operand Builtin.string left infix.left;
      unify_operand Builtin.string right infix.right;
      Builtin.string
  | "&&"
  | "||" ->
      unify_operand Builtin.bool left infix.left;
      unify_operand Builtin.bool right infix.right;
      Builtin.bool
  | "="
  | "<>"
  | "<"
  | "<="
  | ">"
  | ">="
  | "=="
  | "!=" ->
      unify_operand left right infix.right;
      Builtin.bool
  | "::" ->
      let element = State.fresh_var state in
      unify_operand element left infix.left;
      unify_operand (Builtin.list element) right infix.right;
      Builtin.list element
  | "@" ->
      let element = State.fresh_var state in
      let list = Builtin.list element in
      unify_operand list left infix.left;
      unify_operand list right infix.right;
      list
  | _ ->
      let parameter = State.fresh_var state in
      let second_parameter = State.fresh_var state in
      let result = State.fresh_var state in
      let expected = Type.arrow parameter (Type.arrow second_parameter result) in
      let operator_type = infer_ident state expr infix.operator in
      unify_operand expected operator_type expr;
      unify_operand parameter left infix.left;
      unify_operand second_parameter right infix.right;
      result

and infer_array state items =
  let element = State.fresh_var state in
  List.for_each items ~fn:(fun item ->
    let actual = infer_expression state item in
    unify
      state
      ~expected:element
      ~actual
      ~on_error:(expression_constraint_diagnostic item));
  Builtin.array element

and infer_sequence state sequence =
  let _ = infer_expression state sequence.left in
  infer_expression state sequence.right

and infer_assignment state assignment =
  let value_type = infer_expression state assignment.value in
  match assignment.target.kind with
  | FieldAccess access -> (
      let receiver_type = infer_expression state access.receiver in
      match State.get_record_field state ~name:access.field with
      | None ->
          unsupported_syntax
            state
            assignment.target.origin
            "assignment target is not a known record field";
          Builtin.unit
      | Some info ->
          let (owner_type, field_type) = instantiate_record_field state info in
          unify
            state
            ~expected:owner_type
            ~actual:receiver_type
            ~on_error:(expression_constraint_diagnostic access.receiver);
          unify
            state
            ~expected:field_type
            ~actual:value_type
            ~on_error:(expression_constraint_diagnostic assignment.value);
          Builtin.unit
    )
  | _ ->
      let target_type = infer_expression state assignment.target in
      let content_type = State.fresh_var state in
      unify
        state
        ~expected:(Builtin.ref_ content_type)
        ~actual:target_type
        ~on_error:(expression_constraint_diagnostic assignment.target);
      unify
        state
        ~expected:content_type
        ~actual:value_type
        ~on_error:(expression_constraint_diagnostic assignment.value);
      Builtin.unit

and infer_try state try_ =
  let body_type = infer_expression state try_.body in
  List.for_each
    try_.cases
    ~fn:(fun (case: match_case) ->
      State.push_scope state;
      bind_pattern ~mode:Local state case.pattern Builtin.exn;
      (
        match case.guard with
        | None -> ()
        | Some guard ->
            let guard_type = infer_expression state guard in
            unify
              state
              ~expected:Builtin.bool
              ~actual:guard_type
              ~on_error:(expression_constraint_diagnostic guard)
      );
      let result_type = infer_expression state case.body in
      unify
        state
        ~expected:body_type
        ~actual:result_type
        ~on_error:(expression_constraint_diagnostic case.body);
      State.pop_scope state);
  body_type

and infer_while state loop =
  let condition_type = infer_expression state loop.condition in
  unify
    state
    ~expected:Builtin.bool
    ~actual:condition_type
    ~on_error:(expression_constraint_diagnostic loop.condition);
  let body_type = infer_expression state loop.body in
  unify
    state
    ~expected:Builtin.unit
    ~actual:body_type
    ~on_error:(expression_constraint_diagnostic loop.body);
  Builtin.unit

and infer_for state loop =
  let start_type = infer_expression state loop.start_ in
  let stop_type = infer_expression state loop.stop in
  unify
    state
    ~expected:Builtin.int
    ~actual:start_type
    ~on_error:(expression_constraint_diagnostic loop.start_);
  unify
    state
    ~expected:Builtin.int
    ~actual:stop_type
    ~on_error:(expression_constraint_diagnostic loop.stop);
  State.push_scope state;
  bind_pattern ~mode:Local state loop.pattern Builtin.int;
  let body_type = infer_expression state loop.body in
  State.pop_scope state;
  unify
    state
    ~expected:Builtin.unit
    ~actual:body_type
    ~on_error:(expression_constraint_diagnostic loop.body);
  Builtin.unit

and infer_assert state condition =
  let condition_type = infer_expression state condition in
  unify
    state
    ~expected:Builtin.bool
    ~actual:condition_type
    ~on_error:(expression_constraint_diagnostic condition);
  Builtin.unit

and infer_if_else state ifelse =
  let condition = infer_expression state ifelse.condition in
  unify
    state
    ~expected:Builtin.bool
    ~actual:condition
    ~on_error:(expression_constraint_diagnostic ifelse.condition);
  let then_ = infer_expression state ifelse.then_branch in
  (
    match ifelse.else_branch with
    | Some else_branch ->
        let else_ = infer_expression state else_branch in
        unify
          state
          ~expected:then_
          ~actual:else_
          ~on_error:(expression_constraint_diagnostic else_branch);
        then_
    | None ->
        unify
          state
          ~expected:Builtin.unit
          ~actual:then_
          ~on_error:(expression_constraint_diagnostic ifelse.then_branch);
        Builtin.unit
  )

and infer_tuple (state: State.t) parts =
  let types = List.map ~fn:(infer_expression state) parts in
  Type.Tuple types

and parameter_is_positional ({ label; _ }: parameter) =
  match label with
  | Unlabeled -> true
  | Labeled _
  | Optional _ -> false

and warn_unerasable_optional_arguments (state: State.t) (params: parameter list) =
  let rec loop (params: parameter list) =
    match params with
    | [] -> ()
    | ({ label = param_label; origin; _ }: parameter) :: rest ->
        (
          match param_label with
          | Optional label when not (List.any rest ~fn:parameter_is_positional) ->
              State.add_diagnostic
                state
                (Diagnostics.Diagnostic.unerasable_optional_argument
                  ~span:origin.span
                  ~label:(SurfacePath.to_string label))
          | Unlabeled
          | Labeled _
          | Optional _ -> ()
        );
        loop rest
  in
  loop params

and infer_function_param state (param: parameter) =
  let param_type = State.fresh_var state in
  bind_pattern ~mode:Local state param.pattern param_type;
  (
    match param.annotation with
    | Some hint ->
        let expected = core_type_to_type_scoped state hint in
        unify state ~expected ~actual:param_type ~on_error:(annotation_diagnostic hint)
    | None -> ()
  );
  (
    match param.default with
    | Some default ->
        let _ = check_expression state default param_type in
        ()
    | None -> ()
  );
  (parameter_label_to_type_label param.label, param_type)

and infer_function state expr fn_decl =
  warn_unerasable_optional_arguments state fn_decl.parameters;
  State.push_scope state;
  let params = List.map ~fn:(infer_function_param state) fn_decl.parameters in
  let body =
    match fn_decl.body with
    | Body expr -> infer_expression state expr
    | Cases cases when fn_decl.parameters = [] ->
        let argument = State.fresh_var state in
        let result = infer_cases state cases argument in
        Ast.Type.arrow argument result
    | Cases (case :: _) ->
        unsupported_syntax state case.origin "function-case bodies with explicit parameters are not supported";
        State.fresh_var state
    | Cases [] ->
        unsupported_syntax state expr.origin "empty function-case body is not supported";
        State.fresh_var state
  in
  State.pop_scope state;
  List.fold_right
    params
    ~init:body
    ~fn:(fun (label, parameter) result ->
      Ast.Type.arrow ~label parameter result)

let register_constructor state (decl: type_declaration) (ctr: type_constructor) =
  let description = ConstructorDescription.from_type_constructor state decl ctr in
  State.add_constructor state ~name:ctr.name ~description

let register_record_field state (decl: type_declaration) (field: record_field_declaration) =
  State.add_record_field state ~name:field.name ~owner:decl ~field

let register_type_contents state (decl: type_declaration) =
  match decl.definition.kind with
  | Variant constructors -> List.for_each constructors ~fn:(register_constructor state decl)
  | Record fields -> List.for_each fields ~fn:(register_record_field state decl)
  | _ -> ()

let register_type_declarations state declarations =
  let declarations =
    List.map declarations ~fn:(fun (source_decl: type_declaration) ->
      let declaration = {
        source_decl with
        name = State.qualify_current_module state source_decl.name;
      }
      in
      (source_decl, declaration))
  in
  List.for_each declarations ~fn:(fun ((source_decl: type_declaration), declaration) ->
    State.add_type state ~name:source_decl.name ~declaration);
  List.for_each declarations ~fn:(fun (_, (declaration: type_declaration)) ->
    register_type_contents state declaration)

let register_type_decl state (decl: type_declaration) =
  register_type_declarations state [ decl ]

let add_unique_type_variable variables name =
  if List.any variables ~fn:(String.equal name) then
    variables
  else
    List.append variables [ name ]

let append_unique_type_variables variables additions =
  List.fold_left
    additions
    ~init:variables
    ~fn:add_unique_type_variable

let rec core_type_variables (annotation: core_type) =
  let variables_of_types types =
    List.fold_left
      types
      ~init:[]
      ~fn:(fun variables type_ ->
        append_unique_type_variables variables (core_type_variables type_))
  in
  match annotation.kind with
  | Var (Some name) -> [ name ]
  | Apply { constructor; arguments } ->
      variables_of_types (constructor :: arguments)
  | Arrow { parameter; result; _ } -> variables_of_types [ parameter; result ]
  | Tuple parts -> variables_of_types parts
  | ForAll { parameters; body } ->
      append_unique_type_variables parameters (core_type_variables body)
  | PolyVariant fields ->
      fields
      |> List.filter_map ~fn:(fun (field: poly_variant_type_field) -> field.payload)
      |> variables_of_types
  | Package { constraints; _ } ->
      constraints
      |> List.map ~fn:(fun constraint_ -> constraint_.manifest)
      |> variables_of_types
  | Parenthesized inner -> core_type_variables inner
  | Wildcard
  | Var None
  | TypeIdent _ -> []

let signature_type_scheme state annotation =
  let parameters =
    core_type_variables annotation
    |> List.map ~fn:(fun name -> Some name)
  in
  let (scope, _) = TypeExpr.fresh_type_parameters state parameters in
  State.with_type_params
    state
    scope
    (fun state ->
      annotation
      |> core_type_to_type state
      |> Quantifier.generalize_all)

let register_exception state (declaration: exception_declaration) =
  let parameters =
    match declaration.payload with
    | None -> []
    | Some payload ->
        core_type_variables payload
        |> List.map ~fn:(fun name -> Some name)
  in
  let exception_type: type_declaration = {
    origin = declaration.origin;
    name = Builtin.exn_ident;
    parameters;
    definition = { origin = declaration.origin; kind = Abstract };
  }
  in
  let result: core_type = {
    origin = declaration.origin;
    type_ = None;
    kind = TypeIdent Builtin.exn_ident;
  }
  in
  let arguments: constructor_arguments =
    match declaration.payload with
    | None -> Tuple []
    | Some payload -> Tuple [ payload ]
  in
  let constructor: type_constructor = {
    origin = declaration.origin;
    name = declaration.name;
    arguments;
    result = Some result;
  }
  in
  register_constructor state exception_type constructor

let register_type_extension state (item: structure_item) (extension: type_extension_declaration) =
  match State.get_type state ~name:extension.name with
  | Some declaration ->
      List.for_each extension.constructors ~fn:(register_constructor state declaration)
  | None ->
      State.add_diagnostic
        state
        (Diagnostics.Diagnostic.UnsupportedSyntax {
          span = item.origin.span;
          kind = item.origin.kind;
          summary = "cannot extend an unavailable type " ^ SurfacePath.to_string extension.name;
        })

let unsupported_structure_item state (item: structure_item) summary =
  State.add_diagnostic
    state
    (Diagnostics.Diagnostic.UnsupportedSyntax {
      span = item.origin.span;
      kind = item.origin.kind;
      summary;
    })

let unsupported_signature_item state (item: signature_item) summary =
  State.add_diagnostic
    state
    (Diagnostics.Diagnostic.UnsupportedSyntax {
      span = item.origin.span;
      kind = item.origin.kind;
      summary;
    })

let open_module state origin module_path =
  if not (State.open_module state ~path:module_path) then
    State.add_diagnostic
      state
      (Diagnostics.Diagnostic.unresolved_name
        ~span:origin.Ast.span
        ~namespace:"module"
        ~name:(SurfacePath.to_string module_path))

let type_let_binding (state: State.t) (lb: let_binding) =
  let _ = infer_let_binding state ~mode:Generalized lb in
  ()

let type_let_decl (state: State.t) (ld: let_declaration) =
  List.for_each ld.bindings ~fn:(type_let_binding state)

let rec type_impl_item (state: State.t) (item: structure_item) =
  match item.kind with
  | Type declarations -> register_type_declarations state declarations
  | TypeExtension extension -> register_type_extension state item extension
  | Let declaration -> type_let_decl state declaration
  | External declaration ->
      let scheme = signature_type_scheme state declaration.type_annotation in
      State.add_value state ~name:declaration.name ~scheme
  | Exception declaration -> register_exception state declaration
  | Expression expression ->
      let _ = infer_expression state expression in
      ()
  | Module declarations -> List.for_each declarations ~fn:(type_module_declaration state)
  | Open module_path -> open_module state item.origin module_path
  | ModuleType _ -> unsupported_structure_item state item "module type declarations are not supported yet"
  | Include _ -> unsupported_structure_item state item "include declarations are not supported yet"

and type_impl (state: State.t) (items: structure_item list) =
  List.for_each items ~fn:(type_impl_item state)

and type_module_declaration state (declaration: module_declaration) =
  match (declaration.parameters, declaration.application, declaration.alias) with
  | (_ :: _, _, _)
  | (_, Some _, _) ->
      unsupported_syntax state declaration.origin "functor and module-application declarations are not supported yet"
  | ([], None, Some target) ->
      if not (State.add_module_alias state ~name:declaration.name ~target) then
        unsupported_syntax
          state
          declaration.origin
          ("module alias target is unavailable: " ^ SurfacePath.to_string target)
  | ([], None, None) ->
      State.push_module state ~name:declaration.name;
      type_impl state declaration.items;
      State.pop_module state

let type_intf_item state (item: signature_item) =
  match item.kind with
  | Value declaration ->
      let scheme = signature_type_scheme state declaration.type_annotation in
      State.add_value state ~name:declaration.name ~scheme
  | External declaration ->
      let scheme = signature_type_scheme state declaration.type_annotation in
      State.add_value state ~name:declaration.name ~scheme
  | Type declarations -> register_type_declarations state declarations
  | TypeExtension extension -> (
      match State.get_type state ~name:extension.name with
      | Some declaration ->
          List.for_each extension.constructors ~fn:(register_constructor state declaration)
      | None ->
          unsupported_signature_item
            state
            item
            ("cannot extend an unavailable type " ^ SurfacePath.to_string extension.name)
    )
  | Exception declaration -> register_exception state declaration
  | Open module_path -> open_module state item.origin module_path

let type_intf state (items: signature_item list) =
  List.for_each items ~fn:(type_intf_item state)

let type_implementation state (implementation: Ast.implementation) =
  type_impl state implementation.items

let type_interface state (interface: Ast.interface) = type_intf state interface.items

let type_ast (state: State.t) (ast: Ast.t) =
  match ast with
  | Implementation implementation -> type_implementation state implementation
  | Interface interface -> type_interface state interface
