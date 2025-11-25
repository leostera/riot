open Std

(** Code generation from protobuf services to typed gRPC clients

    Generates OCaml modules with typed client functions for each service,
    using Blink.GRPC.Client for the underlying protocol implementation.

    Builds Syn/Ceibo CST nodes directly without string generation.
*)

(** Generate OCaml client CST from protobuf file

    Produces a SOURCE_FILE node containing:
    - A module for each service
    - Typed functions for each RPC method
    - Support for all 4 gRPC patterns (unary, server/client/bidi streaming)

    @param proto Protobuf file AST
    @return Ceibo green tree (syntax tree root)
*)
val generate : Protobuf.ProtofileFormat.t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
