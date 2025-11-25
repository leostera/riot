open Std

(** Code generation from protobuf definitions to typed gRPC interfaces

    Generates OCaml type definitions and service signatures from protobuf.
    The generated code is implementation-agnostic - no dependencies on
    Blink (client) or Suri (server). Users implement the signatures using
    their chosen gRPC implementation.

    Builds Syn/Ceibo CST nodes directly without string generation.
*)

(** Generate OCaml CST from protobuf file

    Produces a SOURCE_FILE node containing:
    - Type definitions for all messages and enums
    - Module type signatures for each service
    - Val signatures for each RPC method with correct types:
      - Unary: request -> (response, error) Result.t
      - Server streaming: request -> (response Iter.t, error) Result.t
      - Client streaming: request Iter.t -> (response, error) Result.t
      - Bidirectional: request Iter.t -> (response Iter.t, error) Result.t

    @param proto Protobuf file AST
    @return Ceibo green tree (syntax tree root)
*)
val generate : Protobuf.ProtofileFormat.t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
