open Std

(** Code generation from protobuf definitions to typed gRPC code

    Generates OCaml type definitions, client implementations, and server
    signatures from protobuf definitions.

    For each service, generates:
    1. Client module (e.g., UserServiceClient) with ready-to-use functions
       that call Blink.GRPC.Client - these work out of the box
    2. Server signature (e.g., module type UserService) that servers
       implement - these are implementation-agnostic

    Builds Syn/Ceibo CST nodes directly without string generation.
*)

(** Generate OCaml CST from protobuf file

    Produces a SOURCE_FILE node containing:
    - Type definitions for all messages and enums
    - Client implementation modules for each service (depends on Blink)
      - Each service gets a module named ServiceNameClient
      - Contains concrete implementations calling Blink.GRPC.Client
    - Server signature modules for each service (implementation-agnostic)
      - Each service gets a module type named ServiceName
      - Val signatures for each RPC method with correct types:
        - Unary: request -> (response, error) Result.t
        - Server streaming: request -> (response MutIterator.t, error) Result.t
        - Client streaming: request MutIterator.t -> (response, error) Result.t
        - Bidirectional: request MutIterator.t -> (response MutIterator.t, error) Result.t

    @param proto Protobuf file AST
    @return Ceibo green tree (syntax tree root)
*)
val generate : Protobuf.ProtofileFormat.t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
