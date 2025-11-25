open Std
open ProtofileFormat

module Green = Syn.Ceibo.Green
module SK = Syn.SyntaxKind

(** Helper to create tokens *)
let tok kind text =
  let width = String.length text in
  Green.Token (Green.make_token ~kind ~text ~width)

(** Helper to create nodes *)
let node kind children =
  Green.Node (Green.make_node ~kind ~children:(Array.of_list children))

(** Whitespace helpers *)
let ws () = tok SK.WHITESPACE " "
let nl () = tok SK.WHITESPACE "\n"
let indent n = tok SK.WHITESPACE (String.make (n * 2) ' ')

(** Convert protobuf field type to OCaml type identifier *)
let ocaml_type_of_field_type = function
  | Double -> "float"
  | Float -> "float"
  | Int32 -> "int"
  | Int64 -> "int"
  | Uint32 -> "int"
  | Uint64 -> "int"
  | Sint32 -> "int"
  | Sint64 -> "int"
  | Fixed32 -> "int"
  | Fixed64 -> "int"
  | Sfixed32 -> "int"
  | Sfixed64 -> "int"
  | Bool -> "bool"
  | String -> "string"
  | Bytes -> "bytes"
  | MessageType name -> String.lowercase_ascii name
  | EnumType name -> String.lowercase_ascii name

(** Convert protobuf names to OCaml identifiers *)
let to_lowercase_ident s = String.lowercase_ascii s

(** Build a type constructor node for OCaml types like "int", "string", "list" *)
let build_type_constr name =
  node SK.TYPE_CONSTR [
    tok SK.IDENT_EXPR name
  ]

(** Build a type application like "int list" *)
let build_type_app base_type arg =
  node SK.TYPE_CONSTR [
    base_type;
    ws ();
    tok SK.IDENT_EXPR arg
  ]

(** Generate CST node for an enum type definition *)
let generate_enum_type enum =
  let enum_name = to_lowercase_ident enum.name in

  (* Build variant constructors *)
  let constructors = List.mapi (fun i value ->
    let prefix = if i = 0 then [] else [nl (); indent 1] in
    prefix @ [
      tok SK.TYPE_VARIANT_CONSTR "|";
      ws ();
      tok SK.IDENT_EXPR value.name
    ]
  ) enum.values in
  let all_constructors = List.flatten constructors in

  (* Build type declaration *)
  node SK.TYPE_DECL ([
    tok SK.IDENT_EXPR "type";
    ws ();
    tok SK.IDENT_EXPR enum_name;
    ws ();
    tok SK.TYPE_CONSTR "=";
    nl ();
    indent 1;
  ] @ all_constructors @ [
    nl ()
  ])

(** Build type expression for a field *)
let build_field_type_expr field =
  let base_type_str = ocaml_type_of_field_type field.field_type in
  let base_type = build_type_constr base_type_str in

  match field.label with
  | Some `Repeated ->
      build_type_app base_type "list"
  | Some `None | None ->
      base_type

(** Generate CST node for a message record type definition *)
let rec generate_message_type msg =
  let msg_name = to_lowercase_ident msg.name in

  (* Collect nested types *)
  let nested_types = List.filter_map (fun elem ->
    match elem with
    | NestedEnum enum -> Some (generate_enum_type enum)
    | NestedMessage nested -> Some (generate_message_type nested)
    | _ -> None
  ) msg.elements in

  (* Collect fields *)
  let fields = List.filter_map (fun elem ->
    match elem with
    | Field f -> Some f
    | MapField mf ->
        (* Represent map as (key * value) list *)
        Some {
          label = Some `Repeated;
          field_type = MessageType (Format.sprintf "(%s * %s)"
            (ocaml_type_of_field_type mf.key_type)
            (ocaml_type_of_field_type mf.value_type));
          name = mf.name;
          number = mf.number;
          options = mf.options;
        }
    | _ -> None
  ) msg.elements in

  (* Build record fields *)
  let record_fields = List.mapi (fun i field ->
    let separator = if i = 0 then [] else [tok SK.IDENT_EXPR ";"; nl (); indent 1] in
    separator @ [
      node SK.TYPE_RECORD_FIELD [
        tok SK.IDENT_EXPR (to_lowercase_ident field.name);
        ws ();
        tok SK.IDENT_EXPR ":";
        ws ();
        build_field_type_expr field
      ]
    ]
  ) fields in
  let all_fields = List.flatten record_fields in

  (* Build record type *)
  let record_node = node SK.TYPE_RECORD ([
    tok SK.IDENT_EXPR "{";
    nl ();
    indent 1;
  ] @ all_fields @ [
    nl ();
    tok SK.IDENT_EXPR "}"
  ]) in

  (* Build type declaration *)
  let type_decl = node SK.TYPE_DECL [
    tok SK.IDENT_EXPR "type";
    ws ();
    tok SK.IDENT_EXPR msg_name;
    ws ();
    tok SK.IDENT_EXPR "=";
    ws ();
    record_node;
    nl ()
  ] in

  (* Combine nested types with main type *)
  if List.length nested_types = 0 then
    type_decl
  else
    node SK.STRUCTURE (nested_types @ [nl (); type_decl])

(** Main generation function *)
let generate proto =
  (* Build header comment *)
  let header = [
    tok SK.COMMENT "(* Generated from protobuf definitions *)";
    nl ();
    nl ();
    tok SK.OPEN_STMT "open";
    ws ();
    tok SK.IDENT_EXPR "Std";
    nl ();
    nl ()
  ] in

  (* Process all top-level definitions *)
  let definitions = List.filter_map (fun def ->
    match def with
    | Message msg -> Some (generate_message_type msg)
    | Enum enum -> Some (generate_enum_type enum)
    | Service _ -> None  (* Services handled by Grpc.Codegen *)
    | Option _ -> None
  ) proto.definitions in

  (* Add spacing between definitions *)
  let spaced_definitions = List.map (fun def -> [def; nl ()]) definitions in
  let all_defs = List.flatten spaced_definitions in

  (* Build source file *)
  Green.make_node
    ~kind:SK.SOURCE_FILE
    ~children:(Array.of_list (header @ all_defs))
