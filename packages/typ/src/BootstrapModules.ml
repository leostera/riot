open Std

let monomorphic = fun ty -> TypeScheme.Forall ([], ty)

let var = fun id -> TypeRepr.Var { id; link = None }

let arrow = fun ?(label = TypeRepr.Nolabel) lhs rhs -> TypeRepr.Arrow { label; lhs; rhs }

let named = fun name -> TypeRepr.Named { name; arguments = [] }

let module_typings = fun ~source_id ~module_name exports ->
  let file_summary = FileSummary.trusted ~source_id exports in
  let export_result = file_summary.export_result in
  let type_decls = file_summary.type_decls in
  let source_hash = ModuleTypings.synthetic_source_hash ~module_name ~export_result ~type_decls in
  ModuleTypings.of_file_summary ~module_name ~source_hash file_summary

let int_binop = monomorphic (arrow TypeRepr.Int (arrow TypeRepr.Int TypeRepr.Int))

let float_binop = monomorphic (arrow TypeRepr.Float (arrow TypeRepr.Float TypeRepr.Float))

let polymorphic_list_map =
  let source = var 0 in
  let target = var 1 in
  TypeScheme.Forall (
    [ 0; 1 ],
    arrow (arrow source target) (arrow (TypeRepr.List source) (TypeRepr.List target))
  )

let polymorphic_list_of_seq =
  let element = var 0 in
  TypeScheme.Forall ([ 0 ], arrow (TypeRepr.Seq element) (TypeRepr.List element))

let polymorphic_list_rev =
  let element = var 0 in
  TypeScheme.Forall ([ 0 ], arrow (TypeRepr.List element) (TypeRepr.List element))

let runtime_args = TypeScheme.Forall ([], TypeRepr.Hole (-2))

let runtime_run =
  let args = var 0 in
  let exit_reason = var 1 in
  TypeScheme.Forall (
    [ 1; 0 ],
    arrow
      ~label:(TypeRepr.Labelled "main")
      (arrow ~label:(TypeRepr.Labelled "args") args (TypeRepr.Result (TypeRepr.Unit, exit_reason)))
      (arrow ~label:(TypeRepr.Labelled "args") args (arrow TypeRepr.Unit TypeRepr.Unit))
  )

let summaries = [
  (
    "Int",
    [
      ("to_string", monomorphic (arrow TypeRepr.Int TypeRepr.String));
      ("of_string", monomorphic (arrow TypeRepr.String TypeRepr.Int));
      ("min", int_binop);
      ("max", int_binop);
    ]
  );
  (
    "Float",
    [
      ("to_string", monomorphic (arrow TypeRepr.Float TypeRepr.String));
      ("of_int", monomorphic (arrow TypeRepr.Int TypeRepr.Float));
      ("to_int", monomorphic (arrow TypeRepr.Float TypeRepr.Int));
      ("pow", monomorphic (arrow TypeRepr.Float (arrow TypeRepr.Float TypeRepr.Float)));
      ("cbrt", monomorphic (arrow TypeRepr.Float TypeRepr.Float));
      ("min", float_binop);
      ("max", float_binop);
    ]
  );
  (
    "String",
    [
      ("length", monomorphic (arrow TypeRepr.String TypeRepr.Int));
      (
        "sub",
        monomorphic (arrow TypeRepr.String (arrow TypeRepr.Int (arrow TypeRepr.Int TypeRepr.String)))
      );
      ("make", monomorphic (arrow TypeRepr.Int (arrow TypeRepr.Char TypeRepr.String)));
      ("escaped", monomorphic (arrow TypeRepr.String TypeRepr.String));
      ("contains", monomorphic (arrow TypeRepr.String (arrow TypeRepr.Char TypeRepr.Bool)));
      (
        "concat",
        monomorphic (arrow TypeRepr.String (arrow (TypeRepr.List TypeRepr.String) TypeRepr.String))
      );
      ("to_seq", monomorphic (arrow TypeRepr.String (TypeRepr.Seq TypeRepr.Char)));
      (
        "starts_with",
        monomorphic
          (arrow
            ~label:(TypeRepr.Labelled "prefix")
            TypeRepr.String
            (arrow TypeRepr.String TypeRepr.Bool))
      );
    ]
  );
  (
    "List",
    [
      ("map", polymorphic_list_map);
      ("of_seq", polymorphic_list_of_seq);
      ("rev", polymorphic_list_rev);
    ]
  );
  ("Array", [ ("length", monomorphic (arrow (TypeRepr.Array (TypeRepr.Hole (-1))) TypeRepr.Int)); ]);
  (
    "Buffer",
    [
      ("create", monomorphic (arrow TypeRepr.Int (named "Buffer.t")));
      ("add_char", monomorphic (arrow (named "Buffer.t") (arrow TypeRepr.Char TypeRepr.Unit)));
      ("contents", monomorphic (arrow (named "Buffer.t") TypeRepr.String));
    ]
  );
  (
    "Std",
    [ ("println", monomorphic (arrow TypeRepr.String TypeRepr.Unit)); ("Env.args", runtime_args); ]
  );
  ("Actors", [ ("run", runtime_run); ]);
]
|> List.mapi
  (fun index (module_name, exports) ->
    module_typings ~source_id:(SourceId.of_int ((-1_000) - index)) ~module_name exports)
