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

(** Generate a comment describing an RPC method *)
let generate_rpc_comment rpc =
  let pattern = match (rpc.input_stream, rpc.output_stream) with
    | false, false -> "Unary"
    | false, true -> "Server streaming"
    | true, false -> "Client streaming"
    | true, true -> "Bidirectional streaming"
  in
  tok SK.COMMENT (Format.sprintf "(** %s RPC: %s -> %s *)" pattern rpc.input_type rpc.output_type)

(** Generate client function for unary RPC *)
let generate_unary_rpc service_name rpc =
  let method_name = to_lowercase_ident rpc.name in
  [
    generate_rpc_comment rpc; nl ();
    tok SK.IDENT_EXPR "let"; ws ();
    tok SK.IDENT_EXPR method_name; ws ();
    tok SK.IDENT_EXPR "conn"; ws ();
    tok SK.IDENT_EXPR "request"; ws ();
    tok SK.IDENT_EXPR "="; nl ();
    indent 1;
    tok SK.IDENT_EXPR "Blink"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "GRPC"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "Client"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "call_unary"; ws ();
    tok SK.IDENT_EXPR "conn"; nl ();
    indent 2; tok SK.IDENT_EXPR "~service"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" service_name); nl ();
    indent 2; tok SK.IDENT_EXPR "~method_"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" rpc.name); nl ();
    indent 2; tok SK.IDENT_EXPR "~request"; ws ();
    tok SK.IDENT_EXPR "()"; nl ()
  ]

(** Generate client function for server streaming RPC *)
let generate_server_streaming_rpc service_name rpc =
  let method_name = to_lowercase_ident rpc.name in
  [
    generate_rpc_comment rpc; nl ();
    tok SK.IDENT_EXPR "let"; ws ();
    tok SK.IDENT_EXPR method_name; ws ();
    tok SK.IDENT_EXPR "conn"; ws ();
    tok SK.IDENT_EXPR "request"; ws ();
    tok SK.IDENT_EXPR "="; nl ();
    indent 1;
    tok SK.IDENT_EXPR "Blink"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "GRPC"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "Client"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "call_server_streaming"; ws ();
    tok SK.IDENT_EXPR "conn"; nl ();
    indent 2; tok SK.IDENT_EXPR "~service"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" service_name); nl ();
    indent 2; tok SK.IDENT_EXPR "~method_"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" rpc.name); nl ();
    indent 2; tok SK.IDENT_EXPR "~request"; ws ();
    tok SK.IDENT_EXPR "()"; nl ()
  ]

(** Generate client function for client streaming RPC *)
let generate_client_streaming_rpc service_name rpc =
  let method_name = to_lowercase_ident rpc.name in
  [
    generate_rpc_comment rpc; nl ();
    tok SK.IDENT_EXPR "let"; ws ();
    tok SK.IDENT_EXPR method_name; ws ();
    tok SK.IDENT_EXPR "conn"; ws ();
    tok SK.IDENT_EXPR "="; nl ();
    indent 1;
    tok SK.IDENT_EXPR "Blink"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "GRPC"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "Client"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "call_client_streaming"; ws ();
    tok SK.IDENT_EXPR "conn"; nl ();
    indent 2; tok SK.IDENT_EXPR "~service"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" service_name); nl ();
    indent 2; tok SK.IDENT_EXPR "~method_"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" rpc.name); nl ();
    indent 2; tok SK.IDENT_EXPR "()"; nl ()
  ]

(** Generate client function for bidirectional streaming RPC *)
let generate_bidi_streaming_rpc service_name rpc =
  let method_name = to_lowercase_ident rpc.name in
  [
    generate_rpc_comment rpc; nl ();
    tok SK.IDENT_EXPR "let"; ws ();
    tok SK.IDENT_EXPR method_name; ws ();
    tok SK.IDENT_EXPR "conn"; ws ();
    tok SK.IDENT_EXPR "="; nl ();
    indent 1;
    tok SK.IDENT_EXPR "Blink"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "GRPC"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "Client"; tok SK.IDENT_EXPR ".";
    tok SK.IDENT_EXPR "call_bidi_streaming"; ws ();
    tok SK.IDENT_EXPR "conn"; nl ();
    indent 2; tok SK.IDENT_EXPR "~service"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" service_name); nl ();
    indent 2; tok SK.IDENT_EXPR "~method_"; tok SK.IDENT_EXPR ":";
    tok SK.STRING_LITERAL (Format.sprintf "\"%s\"" rpc.name); nl ();
    indent 2; tok SK.IDENT_EXPR "()"; nl ()
  ]

(** Generate client function for an RPC based on streaming pattern *)
let generate_rpc_function service_name rpc =
  match (rpc.input_stream, rpc.output_stream) with
  | false, false -> generate_unary_rpc service_name rpc
  | false, true -> generate_server_streaming_rpc service_name rpc
  | true, false -> generate_client_streaming_rpc service_name rpc
  | true, true -> generate_bidi_streaming_rpc service_name rpc

(** Generate a module for a service *)
let generate_service_module service =
  let module_name = to_module_name service.name in

  (* Generate all RPC functions *)
  let rpc_functions = List.map (fun rpc ->
    nl () :: generate_rpc_function service.name rpc
  ) service.rpcs in
  let all_rpcs = List.flatten rpc_functions in

  (* Build module *)
  node SK.MODULE_DECL ([
    tok SK.IDENT_EXPR "module"; ws ();
    tok SK.IDENT_EXPR module_name; ws ();
    tok SK.IDENT_EXPR "="; ws ();
    tok SK.IDENT_EXPR "struct"; nl ()
  ] @ all_rpcs @ [
    tok SK.IDENT_EXPR "end"; nl ()
  ])

(** Main generation function *)
let generate proto =
  (* Generate message/enum types using Protobuf.Codegen *)
  let types_tree = Protobuf.Codegen.generate proto in
  let types_children = Array.to_list types_tree.children in

  (* Build header for services *)
  let service_header = [
    nl ();
    tok SK.COMMENT "(* Generated gRPC service clients *)";
    nl ();
    nl ()
  ] in

  (* Process all service definitions *)
  let services = List.filter_map (fun def ->
    match def with
    | Service svc -> Some (generate_service_module svc)
    | _ -> None
  ) proto.definitions in

  (* Add spacing between services *)
  let spaced_services = List.map (fun svc -> [svc; nl ()]) services in
  let all_svcs = List.flatten spaced_services in

  (* Combine types and services *)
  let all_children =
    if List.length services = 0 then
      types_children  (* No services, just return types *)
    else
      types_children @ service_header @ all_svcs
  in

  (* Build source file *)
  Green.make_node
    ~kind:SK.SOURCE_FILE
    ~children:(Array.of_list all_children)
