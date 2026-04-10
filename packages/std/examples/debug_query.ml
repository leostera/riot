open Std

let () =
  Runtime.run
    ~main:(fun ~args:_ ->
      match Net.Uri.of_string "https://example.com/search?q=hello+world&filter=name%3DJohn" with
      | Ok uri ->
          (
            match Net.Uri.query uri with
            | Some query_str ->
                println ("Raw query string from Uri.query: '" ^ query_str ^ "'");
                let params = Net.Uri.Query.parse query_str in
                println ("Parsed params count: " ^ string_of_int (List.length params));
                List.iter (fun ((k, v)) -> println ("  " ^ k ^ " = '" ^ v ^ "'")) params;
            | None -> println "No query"
          );
          Ok ()
      | Error _ ->
          println "Failed to parse URI";
          Ok ())
    ~args:Env.args
    ()
