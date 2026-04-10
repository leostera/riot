open Std

let () =
  Runtime.run
    ~main:(fun ~args:_ ->
      let test_uris = [
        "https://api.ipify.org?format=json";
        "https://leostera.com";
        "https://httpbin.org/get";
      ] in
      List.iter
        (fun uri_str ->
          println ("Testing: " ^ uri_str);
          match Net.Uri.of_string uri_str with
          | Ok uri ->
              println "  OK - Parsed successfully";
              println ("  scheme: " ^ (Option.unwrap_or ~default:"none" (Net.Uri.scheme uri)));
              println ("  host: " ^ (Option.unwrap_or ~default:"none" (Net.Uri.host uri)));
              println ("  path: " ^ (Net.Uri.path uri));
              println ("  query: " ^ (Option.unwrap_or ~default:"none" (Net.Uri.query uri)));
          | Error e ->
              println "  ERROR - Parse failed";)
        test_uris;
      Ok ())
    ~args:Env.args
    ()
