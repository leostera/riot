open Std

let () =
  Runtime.run
    ~main:(fun ~args:_ ->
      println "Starting HTTPS test with httpbin.org...";
      let uri =
        let builder = Net.Uri.Builder.create () in
        let builder = Net.Uri.Builder.scheme builder "https" in
        let builder = Net.Uri.Builder.host builder "httpbin.org" in
        let builder = Net.Uri.Builder.port builder 443 in
        let builder = Net.Uri.Builder.path builder "/get" in
        Net.Uri.Builder.build builder |> Result.expect ~msg:"Failed to build URI"
      in
      println ("Connecting to " ^ (Net.Uri.to_string uri) ^ "...");
      let conn =
        match Blink.connect uri with
        | Ok conn -> conn
        | Error _ -> panic "Connection failed"
      in
      println "Connected! Sending request...";
      let req = Net.Http.Request.create Net.Http.Method.Get uri in
      Blink.request conn req () |> Result.expect ~msg:"Request failed";
      println "Request sent! Awaiting response...";
      let response, body = Blink.await conn |> Result.expect ~msg:"Failed to receive response" in
      let status = Net.Http.Response.status response in
      println
        ("HTTP Status: "
        ^ (Int.to_string (Net.Http.Status.to_int status))
        ^ " "
        ^ (Net.Http.Status.reason_phrase status));
      println ("Body length: " ^ (Int.to_string (String.length body)) ^ " bytes");
      let preview =
        if String.length body > 300 then
          String.sub body 0 300 ^ "..."
        else
          body
      in
      println ("Response preview: " ^ preview);
      Blink.close conn;
      println "Connection closed.";
      Ok ())
    ~args:Env.args
    ()
