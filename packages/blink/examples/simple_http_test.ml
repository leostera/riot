open Std

let () =
  Runtime.run
    ~main:(fun ~args:_ ->
      println "Starting HTTP example...";
      (* Build HTTP URL *)
      let uri =
        let builder = Net.Uri.Builder.create () in
        let builder = Net.Uri.Builder.scheme builder "http" in
        let builder = Net.Uri.Builder.host builder "leostera.com" in
        let builder = Net.Uri.Builder.port builder 80 in
        let builder = Net.Uri.Builder.path builder "/" in
        Net.Uri.Builder.build builder |> Result.expect ~msg:"Failed to build URI"
      in
      println ("Connecting to " ^ (Net.Uri.to_string uri) ^ "...");
      (* Connect - will use TCP transport *)
      let conn =
        match Blink.connect uri with
        | Ok conn ->
            println "Connected successfully!";
            conn
        | Error (Blink.Error.NetError Net.Connection_refused) ->
            panic "Connection refused"
        | Error (Blink.Error.NetError Net.Closed) ->
            panic "Connection closed"
        | Error (Blink.Error.NetError (Net.System_error io_err)) ->
            panic ("System error: " ^ IO.error_message io_err)
        | Error (Blink.Error.TlsError Net.TlsStream.Closed) ->
            panic "TLS closed"
        | Error (Blink.Error.TlsError (Net.TlsStream.Handshake_failed msg)) ->
            panic ("TLS handshake failed: " ^ msg)
        | Error (Blink.Error.TlsError (Net.TlsStream.System_error io_err)) ->
            panic ("TLS system error: " ^ IO.error_message io_err)
        | Error (Blink.Error.TlsError (Net.TlsStream.Network_read_failed _err)) ->
            panic "TLS network read failed"
        | Error (Blink.Error.TlsError (Net.TlsStream.Network_write_failed _err)) ->
            panic "TLS network write failed"
        | Error (Blink.Error.TlsError Net.TlsStream.Tls_not_available) ->
            panic "TLS not available"
        | Error (Blink.Error.TlsError Net.TlsStream.Unsupported_vectored_operation) ->
            panic "Unsupported vectored operation"
        | Error (Blink.Error.ParseError msg) ->
            panic ("Parse error: " ^ msg)
        | Error (Blink.Error.ProtocolError msg) ->
            panic ("Protocol error: " ^ msg)
        | Error (Blink.Error.HandshakeFailed msg) ->
            panic ("Handshake failed: " ^ msg)
        | Error Blink.Error.InvalidFrame ->
            panic "Invalid frame"
        | Error Blink.Error.Eof ->
            panic "EOF"
        | Error Blink.Error.Closed ->
            panic "Closed"
      in
      println "Connected! Creating request...";
      (* Create GET request *)
      let req = Net.Http.Request.create Net.Http.Method.Get uri in
      (* Send request *)
      Blink.request conn req () |> Result.expect ~msg:"Request failed";
      println "Request sent! Awaiting response...";
      (* Get full response *)
      let response, body = Blink.await conn |> Result.expect ~msg:"Failed to receive response" in
      let status = Net.Http.Response.status response in
      println
        ("HTTP Status: "
        ^ (Int.to_string (Net.Http.Status.to_int status))
        ^ " "
        ^ (Net.Http.Status.reason_phrase status));
      println ("Body length: " ^ (Int.to_string (String.length body)) ^ " bytes");
      let preview =
        if String.length body > 200 then
          String.sub body 0 200 ^ "..."
        else
          body
      in
      println ("First 200 chars: " ^ preview);
      Blink.close conn;
      println "Connection closed.";
      Ok ())
    ~args:Env.args
    ()
