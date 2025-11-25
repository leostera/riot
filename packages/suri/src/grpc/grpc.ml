open Std

(** Create a gRPC service middleware

    Note: This is a simplified implementation that provides the middleware API.
    Full gRPC support requires HTTP/2 integration in the Suri web server layer.

    TODO: Implement full HTTP/2 support including:
    - HTTP/2 frame parsing
    - gRPC message framing (5-byte headers)
    - Stream multiplexing
    - Proper trailers with grpc-status
*)
let service (_path : string) (_impl : (module sig end)) : Middleware.Pipeline.middleware =
  fun conn ->
    (* Check if this is a gRPC request *)
    let content_type = Middleware.Conn.headers conn
      |> List.find_opt (fun (k, _) -> String.lowercase_ascii k = "content-type")
      |> Option.map snd
    in

    match content_type with
    | Some ct when String.starts_with ~prefix:"application/grpc" ct ->
        (* This is a gRPC request *)
        (* TODO: Implement full gRPC protocol handling *)
        (* For now, return an unimplemented status *)
        conn
        |> Middleware.Conn.with_status Net.Http.Status.not_implemented
        |> Middleware.Conn.with_header "content-type" "application/grpc+proto"
        |> Middleware.Conn.with_header "grpc-status" "12"  (* UNIMPLEMENTED *)
        |> Middleware.Conn.with_header "grpc-message" "gRPC middleware requires HTTP/2 support"
        |> Middleware.Conn.send

    | _ ->
        (* Not a gRPC request, pass through *)
        conn
