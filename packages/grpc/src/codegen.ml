open Std
open Protobuf.ProtofileFormat

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

(** Convert service/method names to OCaml identifiers *)
let to_lowercase_ident s = String.lowercase_ascii s
let to_module_name s = String.capitalize_ascii s

(** Generate a val signature for an RPC method *)
let generate_rpc_signature rpc =
  let method_name = to_lowercase_ident rpc.name in
  let req_type = to_lowercase_ident rpc.input_type in
  let res_type = to_lowercase_ident rpc.output_type in

  let pattern = match (rpc.input_stream, rpc.output_stream) with
    | false, false -> "Unary"
    | false, true -> "Server streaming"
    | true, false -> "Client streaming"
    | true, true -> "Bidirectional streaming"
  in

  (* Build return type based on streaming pattern *)
  let return_type = match (rpc.input_stream, rpc.output_stream) with
    | false, false ->
        (* Unary: request -> (response, error) Result.t *)
        [
          tok SK.IDENT_EXPR "(";
          tok SK.IDENT_EXPR res_type;
          tok SK.IDENT_EXPR ","; ws ();
          tok SK.IDENT_EXPR "Grpc"; tok SK.IDENT_EXPR ".";
          tok SK.IDENT_EXPR "Status"; tok SK.IDENT_EXPR ".";
          tok SK.IDENT_EXPR "t"; ws ();
          tok SK.IDENT_EXPR "*"; ws ();
          tok SK.IDENT_EXPR "string";
          tok SK.IDENT_EXPR ")"; ws ();
          tok SK.IDENT_EXPR "Result"; tok SK.IDENT_EXPR "."; tok SK.IDENT_EXPR "t"
        ]
    | false, true ->
        (* Server streaming: request -> (response MutIterator.t, error) Result.t *)
        [
          tok SK.IDENT_EXPR "(";
          tok SK.IDENT_EXPR res_type; ws ();
          tok SK.IDENT_EXPR "MutIterator"; tok SK.IDENT_EXPR "."; tok SK.IDENT_EXPR "t";
          tok SK.IDENT_EXPR ","; ws ();
          tok SK.IDENT_EXPR "Grpc"; tok SK.IDENT_EXPR ".";
          tok SK.IDENT_EXPR "Status"; tok SK.IDENT_EXPR ".";
          tok SK.IDENT_EXPR "t"; ws ();
          tok SK.IDENT_EXPR "*"; ws ();
          tok SK.IDENT_EXPR "string";
          tok SK.IDENT_EXPR ")"; ws ();
          tok SK.IDENT_EXPR "Result"; tok SK.IDENT_EXPR "."; tok SK.IDENT_EXPR "t"
        ]
    | true, false ->
        (* Client streaming: request MutIterator.t -> (response, error) Result.t *)
        [
          tok SK.IDENT_EXPR "(";
          tok SK.IDENT_EXPR res_type;
          tok SK.IDENT_EXPR ","; ws ();
          tok SK.IDENT_EXPR "Grpc"; tok SK.IDENT_EXPR ".";
          tok SK.IDENT_EXPR "Status"; tok SK.IDENT_EXPR ".";
          tok SK.IDENT_EXPR "t"; ws ();
          tok SK.IDENT_EXPR "*"; ws ();
          tok SK.IDENT_EXPR "string";
          tok SK.IDENT_EXPR ")"; ws ();
          tok SK.IDENT_EXPR "Result"; tok SK.IDENT_EXPR "."; tok SK.IDENT_EXPR "t"
        ]
    | true, true ->
        (* Bidirectional: request MutIterator.t -> (response MutIterator.t, error) Result.t *)
        [
          tok SK.IDENT_EXPR "(";
          tok SK.IDENT_EXPR res_type; ws ();
          tok SK.IDENT_EXPR "MutIterator"; tok SK.IDENT_EXPR "."; tok SK.IDENT_EXPR "t";
          tok SK.IDENT_EXPR ","; ws ();
          tok SK.IDENT_EXPR "Grpc"; tok SK.IDENT_EXPR ".";
          tok SK.IDENT_EXPR "Status"; tok SK.IDENT_EXPR ".";
          tok SK.IDENT_EXPR "t"; ws ();
          tok SK.IDENT_EXPR "*"; ws ();
          tok SK.IDENT_EXPR "string";
          tok SK.IDENT_EXPR ")"; ws ();
          tok SK.IDENT_EXPR "Result"; tok SK.IDENT_EXPR "."; tok SK.IDENT_EXPR "t"
        ]
  in

  (* Build parameter type *)
  let param_type = if rpc.input_stream then
    [tok SK.IDENT_EXPR req_type; ws (); tok SK.IDENT_EXPR "MutIterator"; tok SK.IDENT_EXPR "."; tok SK.IDENT_EXPR "t"]
  else
    [tok SK.IDENT_EXPR req_type]
  in

  [
    nl ();
    indent 1;
    tok SK.COMMENT (Format.sprintf "(** %s: %s -> %s *)" pattern rpc.input_type rpc.output_type);
    nl ();
    indent 1;
    tok SK.IDENT_EXPR "val"; ws ();
    tok SK.IDENT_EXPR method_name; ws ();
    tok SK.IDENT_EXPR ":"; ws ()
  ] @ param_type @ [
    ws ();
    tok SK.IDENT_EXPR "->"; ws ()
  ] @ return_type @ [
    nl ()
  ]

(** Generate client implementation for an RPC *)
let generate_rpc_client_impl service_name rpc =
  let method_name = to_lowercase_ident rpc.name in
  let client_func = match (rpc.input_stream, rpc.output_stream) with
    | false, false -> "call_unary"
    | false, true -> "call_server_streaming"
    | true, false -> "call_client_streaming"
    | true, true -> "call_bidi_streaming"
  in

  (* Parameter name depends on streaming type *)
  let param_name = if rpc.input_stream then "requests" else "request" in

  [
    nl ();
    indent 1;
    tok SK.IDENT_EXPR "let"; ws ();
    tok SK.IDENT_EXPR method_name; ws ();
    tok SK.IDENT_EXPR "conn"; ws ();
    tok SK.IDENT_EXPR param_name; ws ();
    tok SK.IDENT_EXPR "="; nl ();
    indent 2;
    tok SK.IDENT_EXPR "Blink"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "GRPC"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "Client"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR client_func; ws ();
    tok SK.IDENT_EXPR "conn"; nl ();
    indent 3; tok SK.IDENT_EXPR "~service"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" service_name); nl ();
    indent 3; tok SK.IDENT_EXPR "~method_"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" rpc.name); nl ();
    indent 3; tok SK.IDENT_EXPR "~"; tok SK.IDENT_EXPR param_name; nl ();
    indent 3; tok SK.IDENT_EXPR "()"; nl ()
  ]

(** Generate client module for a service *)
let generate_service_client service =
  let module_name = to_module_name service.name ^ "Client" in

  (* Generate client implementations for each RPC *)
  let rpc_impls = List.map (generate_rpc_client_impl service.name) service.rpcs in
  let all_impls = List.flatten rpc_impls in

  (* Build module *)
  node SK.MODULE_DECL ([
    tok SK.IDENT_EXPR "module"; ws ();
    tok SK.IDENT_EXPR module_name; ws ();
    tok SK.IDENT_EXPR "="; ws ();
    tok SK.IDENT_EXPR "struct"; nl ()
  ] @ all_impls @ [
    tok SK.IDENT_EXPR "end"; nl ()
  ])

(** Generate a module type signature for a service *)
let generate_service_signature service =
  let module_name = to_module_name service.name in

  (* Generate val signatures for each RPC *)
  let rpc_sigs = List.map generate_rpc_signature service.rpcs in
  let all_sigs = List.flatten rpc_sigs in

  (* Build module type *)
  node SK.MODULE_TYPE_DECL ([
    tok SK.IDENT_EXPR "module"; ws ();
    tok SK.IDENT_EXPR "type"; ws ();
    tok SK.IDENT_EXPR module_name; ws ();
    tok SK.IDENT_EXPR "="; ws ();
    tok SK.IDENT_EXPR "sig"; nl ()
  ] @ all_sigs @ [
    tok SK.IDENT_EXPR "end"; nl ()
  ])

(** Main generation function *)
let generate proto =
  (* Generate message/enum types using Protobuf.Codegen *)
  let types_tree = Protobuf.Codegen.generate proto in
  let types_children = Array.to_list types_tree.children in

  (* Extract all services from definitions *)
  let services = List.filter_map (fun def ->
    match def with
    | Service svc -> Some svc
    | _ -> None
  ) proto.definitions in

  (* Generate client implementations *)
  let client_header = [
    nl ();
    tok SK.COMMENT "(* Generated gRPC client implementations *)";
    nl ();
    tok SK.COMMENT "(* These modules provide ready-to-use client functions that call Blink.GRPC.Client *)";
    nl ();
    nl ()
  ] in

  let client_modules = List.map generate_service_client services in
  let spaced_clients = List.map (fun client -> [client; nl ()]) client_modules in
  let all_clients = List.flatten spaced_clients in

  (* Generate server signatures *)
  let signature_header = [
    nl ();
    tok SK.COMMENT "(* Generated gRPC service signatures *)";
    nl ();
    tok SK.COMMENT "(* Implement these module types to create gRPC servers *)";
    nl ();
    nl ()
  ] in

  let signature_modules = List.map generate_service_signature services in
  let spaced_signatures = List.map (fun sig_ -> [sig_; nl ()]) signature_modules in
  let all_sigs = List.flatten spaced_signatures in

  (* Combine types, clients, and signatures *)
  let all_children =
    if List.length services = 0 then
      types_children  (* No services, just return types *)
    else
      types_children @ client_header @ all_clients @ signature_header @ all_sigs
  in

  (* Build source file *)
  Green.make_node
    ~kind:SK.SOURCE_FILE
    ~children:(Array.of_list all_children)
