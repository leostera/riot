open Std

(** gRPC Protocol Implementation

    This package provides protocol-level types and utilities for gRPC.
    It does NOT provide client/server implementations - those are in:
    - blink: Blink.GRPC for gRPC clients
    - suri: Suri.GRPC for gRPC servers
*)

module Status = Status
module Message = Message
module Metadata = Metadata
module Call = Call
