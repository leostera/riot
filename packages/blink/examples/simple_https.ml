open Std

let () =
  Runtime.run
    ~main:(fun ~args ->
      (* Parse command-line arguments *)
      let cmd = ArgParser.command "simple_https"
      |> ArgParser.about "Simple HTTPS client example"
      |> ArgParser.args
        [
          ArgParser.Arg.option "method"
          |> ArgParser.Arg.long "method"
          |> ArgParser.Arg.value_name "METHOD"
          |> ArgParser.Arg.default "GET"
          |> ArgParser.Arg.help "HTTP method (GET, POST, etc.)";
          ArgParser.Arg.option "url"
          |> ArgParser.Arg.long "url"
          |> ArgParser.Arg.value_name "URL"
          |> ArgParser.Arg.default "https://leostera.com"
          |> ArgParser.Arg.help "URL to request"
        ] in
      let matches =
        match ArgParser.get_matches cmd args with
        | Ok m -> m
        | Error e ->
            ArgParser.print_error e;
            ArgParser.print_help cmd;
            panic "Failed to parse arguments"
      in
      let method_str = ArgParser.get_one matches "method"
      |> Option.unwrap_or ~default:"GET"
      |> String.uppercase_ascii in
      let url_str = ArgParser.get_one matches "url" |> Option.unwrap_or ~default:"https://leostera.com" in
      println ("Starting HTTPS request: " ^ method_str ^ " " ^ url_str);
      (* Parse the URL *)
      let uri = Net.Uri.of_string url_str |> Result.expect ~msg:"Failed to parse URL" in
      (* Parse the HTTP method *)
      let method_ =
        match method_str with
        | "GET" -> Net.Http.Method.Get
        | "POST" -> Net.Http.Method.Post
        | "PUT" -> Net.Http.Method.Put
        | "DELETE" -> Net.Http.Method.Delete
        | "HEAD" -> Net.Http.Method.Head
        | "OPTIONS" -> Net.Http.Method.Options
        | "PATCH" -> Net.Http.Method.Patch
        | other -> panic ("Unsupported HTTP method: " ^ other)
      in
      println ("Connecting to " ^ (Net.Uri.to_string uri) ^ "...");
      (* Connect - will use TLS if scheme is https *)
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
      (* Create HTTP request *)
      let req = Net.Http.Request.create method_ uri in
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
      println ("Response: " ^ preview);
      Blink.close conn;
      println "Connection closed.";
      Ok ())
    ~args:Env.args
    ()
