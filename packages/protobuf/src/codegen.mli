open Std

(** Code generation from protobuf definitions to OCaml types

    Generates OCaml type definitions and encoder/decoder functions from
    protobuf messages, enums, and nested types.

    Builds Syn/Ceibo CST nodes directly without string generation.
*)

(** Generate OCaml CST from protobuf file

    Produces a SOURCE_FILE node containing:
    - Type definitions for messages (as records)
    - Type definitions for enums (as variants)
    - Encoder functions (to WireFormat.t)
    - Decoder functions (from WireFormat.t)

    @param proto Protobuf file AST
    @return Ceibo green tree (syntax tree root)
*)
val generate : ProtofileFormat.t -> (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
