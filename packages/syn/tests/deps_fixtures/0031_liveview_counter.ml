open Std
open Suri

(** LiveView Counter Example
    
    A simple interactive counter demonstrating LiveView capabilities:
    - Server-side rendering with Component system
    - Real-time updates over WebSocket
    - No client-side JavaScript framework needed
    
    Run with: riot run suri:liveview_counter
    Then open: http://localhost:4000 *)
module Counter = struct
  let id = LiveView.id "counter"

  open LiveView
  open Component

  type state = {
    count: int;
  }

  type msg =
    Increment
    | Decrement
    | Reset

  type args = unit

  (* No args needed for simple counter *)

  let serialize_args = fun () -> Data.Json.Null

  let deserialize_args = fun _ -> Ok ()

  let init = fun _conn () ->
    Log.info "Counter initialized";
    { count = 0 }

  let update = fun event state ->
    let new_state =
      match event with
      | App Increment -> { count = state.count + 1 }
      | App Decrement -> { count = state.count - 1 }
      | App Reset -> { count = 0 }
      | _ -> state
    in
    Log.info ("Counter: " ^ Int.to_string state.count ^ " -> " ^ Int.to_string new_state.count);
    new_state

  let render = fun ~state () ->
    div
      ~attrs:[ class_ "counter-app" ]
      [
        header
          ~attrs:[ class_ "header" ]
          [
            h1 [ text "LiveView Counter" ];
            p ~attrs:[ class_ "subtitle" ] [ text "Server-side rendering with real-time updates" ];
          ];
        div
          ~attrs:[ class_ "counter-display" ]
          [
            div ~attrs:[ class_ "count-label" ] [ text "Current Count:" ];
            div ~attrs:[ class_ "count-value" ] [ text (Int.to_string state.count) ];
          ];
        div
          ~attrs:[ class_ "controls" ]
          [
            button
              ~attrs:[ class_ "btn btn-decrement"; on_click (fun _ -> Decrement); ]
              [ text "−" ];
            button ~attrs:[ class_ "btn btn-reset"; on_click (fun _ -> Reset); ] [ text "Reset" ];
            button ~attrs:[ class_ "btn btn-increment"; on_click (fun _ -> Increment); ] [ text "+" ];
          ];
        footer
          ~attrs:[ class_ "info" ]
          [
            p
              [
                strong [ text "How it works: " ];
                text "Clicks are sent to the server over WebSocket. ";
                text "The server updates state and sends back only the HTML changes. ";
                text "No client-side framework needed!";
              ];
          ];
      ]
end

(** Page styles *)
let page_styles = {|
  * {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
  }
  
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    min-height: 100vh;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 20px;
  }
  
  #app {
    width: 100%;
    max-width: 500px;
  }
  
  .counter-app {
    background: white;
    border-radius: 16px;
    box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
    padding: 40px;
    text-align: center;
  }
  
  .header {
    margin-bottom: 32px;
  }
  
  .header h1 {
    font-size: 2em;
    color: #333;
    margin-bottom: 8px;
  }
  
  .subtitle {
    color: #666;
    font-size: 0.9em;
  }
  
  .counter-display {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border-radius: 12px;
    padding: 32px;
    margin-bottom: 32px;
  }
  
  .count-label {
    font-size: 0.9em;
    opacity: 0.9;
    margin-bottom: 8px;
    text-transform: uppercase;
    letter-spacing: 1px;
  }
  
  .count-value {
    font-size: 4em;
    font-weight: bold;
    font-variant-numeric: tabular-nums;
  }
  
  .controls {
    display: flex;
    gap: 12px;
    justify-content: center;
    margin-bottom: 32px;
  }
  
  .btn {
    font-size: 1.5em;
    font-weight: bold;
    padding: 16px 24px;
    border: none;
    border-radius: 8px;
    cursor: pointer;
    transition: all 0.2s ease;
    min-width: 80px;
  }
  
  .btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.2);
  }
  
  .btn:active {
    transform: translateY(0);
  }
  
  .btn-decrement {
    background: #dc3545;
    color: white;
  }
  
  .btn-decrement:hover {
    background: #c82333;
  }
  
  .btn-reset {
    background: #6c757d;
    color: white;
  }
  
  .btn-reset:hover {
    background: #5a6268;
  }
  
  .btn-increment {
    background: #28a745;
    color: white;
  }
  
  .btn-increment:hover {
    background: #218838;
  }
  
  .info {
    background: #f8f9fa;
    border-radius: 8px;
    padding: 16px;
    font-size: 0.85em;
    color: #666;
    line-height: 1.6;
  }
  
  .info strong {
    color: #333;
  }
  
  /* Loading state */
  .loading {
    text-align: center;
    color: white;
    font-size: 1.2em;
  }
|}

(** Home page handler with embedded LiveView *)
let home_page = fun conn _req ->
  let open Component in
    let page = html
      [
        head
          [
            meta ~attrs:[ attr "charset" "UTF-8" ] ();
            meta ~attrs:[ attr "viewport" "width=device-width, initial-scale=1.0" ] ();
            title [ text "LiveView Counter" ];
            LiveView.client_script;
            style page_styles;
          ];
        body [ div ~attrs:[ id "app" ] [ LiveView.embed (module Counter) (); ]; ];
      ] in
    conn |> Conn.render_component Net.Http.Status.Ok page

(* Define routes *)

let routes = Middleware.Router.[get "/" home_page;
(* Serve home page with custom styles *)
LiveView.live (module Counter);]

(* App is just a list of middleware! *)

let app = [ Middleware.router routes; ]

let () =
  Actors.run ~args:Env.args ()
    ~main:(fun ~args:_ ->
      Std.Config.load_file (Path.v "packages/suri/examples/conf.toml");
      let _ = Std.Log.start_link () in
      let config = Suri.config ~port:9_999 () in
      match Suri.start_link ~config app with
      | Ok supervisor ->
          Log.info "╔═══════════════════════════════════════════════════╗";
          Log.info "║  LiveView Counter running!                       ║";
          Log.info "║  http://localhost:9999                           ║";
          Log.info "║                                                   ║";
          Log.info "║  Open your browser and watch the magic happen!   ║";
          Log.info "╚═══════════════════════════════════════════════════╝";
          let count = Supervisor.Dynamic.count_children supervisor in
          Log.info ("Started with " ^ Int.to_string count.active ^ " acceptors");
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error `Bind_error ->
          Log.error "Failed to bind to port 9999";
          Error (Failure "Failed to start server"))
