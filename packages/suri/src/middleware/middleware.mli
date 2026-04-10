open Std

(** {1 Middleware Framework}

    Composable middleware system for HTTP request/response processing.
    Build request pipelines that transform, route, log, and respond to HTTP requests.

    {2 Table of Contents}

    - {{!section-why}Why Middleware?}
    - {{!section-quickstart}Quick Start}
    - {{!section-concepts}Core Concepts}
    - {{!section-modules}Modules}
    - {{!section-examples}Examples}

    {2:why Why Middleware?}

    {b ✅ Composability}
    - Chain transformations with [|>] operator
    - Reusable middleware components
    - Easy to test in isolation

    {b ✅ Request Pipeline}
    - Logging, authentication, routing in one flow
    - Early termination with [Conn.halt]
    - Pass data between middleware via [Conn.assign]

    {b ✅ Type-Safe Routing}
    - Pattern matching with parameter extraction
    - [/users/:id] captures [id] parameter
    - Method-specific routes (GET, POST, etc.)

    {2:quickstart Quick Start}

    {3 Simple Middleware Pipeline}

    Middleware can wrap the next handler in the pipeline!

    {[
      open Std
      open Suri

      (* Build routes *)
      let routes = Middleware.Router.[
        get "/" (fun ~conn ~next:_ -> 
          Conn.respond conn ~status:Ok ~body:"Home" |> Conn.send);
        get "/about" (fun ~conn ~next:_ -> 
          Conn.respond conn ~status:Ok ~body:"About" |> Conn.send);
      ]

      (* Pipeline is just a list with built-in logger! *)
      let app = Middleware.[
        logger ();
        router routes;
      ]
    ]}

    {3 Custom Middleware}

    Write middleware that wraps the next handler:

    {[
      (* Add a header to all responses *)
      let add_header ~conn ~next =
        let conn' = next conn in
        Conn.with_header "X-Powered-By" "Suri" conn'

      (* Time requests *)
      let timer ~conn ~next =
        let start = Time.Instant.now () in
        let conn' = next conn in
        let duration = Time.Instant.elapsed start |> Time.Duration.to_millis in
        Log.debug (Printf.sprintf "Request took %.2fms" duration);
        conn'

      (* Authenticate requests *)
      let auth ~conn ~next =
        match Conn.headers conn |> Net.Http.Header.get "Authorization" with
        | Some token when token = "secret" -> next conn
        | _ -> Conn.respond conn ~status:Unauthorized ~body:"Unauthorized" |> Conn.halt

      (* Compose them in a list *)
      let app = Middleware.[
        logger ();
        timer;
        add_header;
        auth;
        router routes;
      ]
    ]}

    {2:concepts Core Concepts}

    {3 Connection (Conn)}

    A {!Conn.t} represents the connection state flowing through the pipeline:
    - Contains the original request
    - Accumulates response data (status, headers, body)
    - Carries middleware-specific state via [assign]
    - Can be halted to stop pipeline execution

    {3 Pipeline}

    A {!Pipeline.t} is simply a list of middleware functions:
    {[
      type middleware = conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t
      type t = middleware list
    ]}

    Each middleware can:
    - Execute code before the handler (call [next conn])
    - Execute code after the handler ([let conn' = next conn in ...])
    - Skip the handler entirely (return without calling [next])
    - Inspect/modify the connection before and after
    - Halt the pipeline early (stops execution)

    {3 Router}

    The {!Router} matches request paths to handler functions:
    - Pattern syntax: [/users/:id/posts/:post_id]
    - Extracts parameters and stores them in [Conn]
    - Method-specific routing (GET, POST, PUT, DELETE)
    - 404 fallback for unmatched routes

    {2:modules Modules}

    - {!Conn} - Connection context with request, response, and state
    - {!Pipeline} - Compose and execute middleware chains
    - {!Router} - Pattern-based routing with parameter extraction
    - {!Logger} - Request/response logging with timing and request IDs

    {2:examples Examples}

    See [packages/suri/examples/]:
    - [routing.ml] - Router with middleware pipeline
    - [json_api.ml] - RESTful API with parameter extraction
    - [middleware_example.ml] - Custom middleware patterns

    Run examples:
    {[
      riot run suri:routing
      riot run suri:json_api
    ]}

    ---

    {1 API Reference} *)

module Conn = Conn

(** {b Connection Context}

    Represents the connection state flowing through middleware.
    
    Contains:
    - Original HTTP request
    - Response data (status, headers, body)
    - Route parameters (from Router)
    - Custom state (via [assign])
    - Halt flag (stop pipeline)

    See {!Conn} for full API. *)

module Pipeline = Pipeline

(** {b Middleware Pipeline}

    A pipeline is just a list of middleware functions.

    {b Type:}
    {[
      type middleware = conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t
      type t = middleware list
    ]}

    {b Usage:}
    {[
      let app = Middleware.[ logger (); router routes ] in
      let conn = Pipeline.run conn app
    ]}

    See {!Pipeline} for full API. *)

module Router = Router

(** {b HTTP Router}

    Pattern-based routing with parameter extraction.

    {b Route patterns:}
    - ["/"] - Exact match
    - ["/users/:id"] - Captures [id] parameter
    - ["/posts/:id/comments/:cid"] - Multiple parameters

    {b Route methods:}
    - [get], [post], [put], [delete], [patch]
    - [any] - Matches all methods

    {b Example:}
    {[
      let routes = [
        Router.get "/" home_handler;
        Router.get "/users/:id" user_handler;
        Router.post "/api/data" create_handler;
      ]

      let router = Router.create routes
    ]}

    See {!Router} for full API. *)

module Logger = Logger

(** {b Request Logger}

    Automatic request/response logging with timing.

    Logs all HTTP requests with:
    - Request method and path
    - Response status code
    - Request duration in milliseconds

    {b Log levels are automatic:}
    - 5xx responses → Error
    - 4xx responses → Warn
    - Slow requests → Warn
    - Normal requests → Info

    {b Example:}
    {[
      let app = Middleware.[
        logger ();
        router routes;
      ]
    ]}

    {b With configuration:}
    {[
      let app = Middleware.[
        logger ~config:Logger.{ default with log_params = true } ();
        router routes;
      ]
    ]}

    See {!Logger} for full API. *)

module Request_id = Request_id

(** {b Request ID Middleware}

    Ensures every request has a unique [x-request-id] header.

    {b Behavior:}
    - Preserves existing [x-request-id] from client
    - Generates UUID v7 if no ID is present
    - Adds ID to both request and response headers

    {b Example:}
    {[
      let app = Middleware.[
        request_id;   (* Ensure request IDs *)
        logger;       (* Logger can use the ID *)
        router routes;
      ]
    ]}

    See {!Request_id} for full API. *)

module Debugger = Debugger

(** {b Visual Debugger Middleware}

    Beautiful error pages for development with source code inspection.

    {b ⚠️ DEVELOPMENT ONLY} - Never use in production!

    {b Features:}
    - 🔥 Beautiful error pages with full stack traces
    - 📚 Source code snippets with syntax highlighting
    - 📨 Complete request/response inspection
    - 📋 Automatic error logging to console

    {b Example:}
    {[
      let app = Middleware.[
        request_id;
        logger;      (* Logs successful requests *)
        debugger;    (* Catches & logs errors, shows error page *)
        router routes;
      ]
    ]}

    {b Production safety:}
    {[
      let is_dev = Env.get "APP_ENV" <> Some "production" in
      let debug = if is_dev then [debugger] else [] in
      let app = Middleware.[request_id; logger] @ debug @ [router routes]
    ]}

    See {!Debugger} for full API and examples. *)
(** {2 Convenience Functions} *)
(** Create router middleware from a list of routes.
    
    This is a convenience alias for [Router.middleware routes].
    Makes middleware pipelines more readable:
    
    {[
      let app = Middleware.[
        logger ();
        router [
          Router.get "/" home;
          Router.get "/about" about;
        ];
      ]
    ]}
    
    Instead of:
    {[
      let app = [
        Middleware.Logger.logger ();
        Middleware.Router.middleware [
          Router.get "/" home;
          Router.get "/about" about;
        ];
      ]
    ]} *)
val router: Router.route list -> Pipeline.middleware

(** Request logger middleware.
    
    Convenience alias for [Logger.logger].
    
    Example:
    {[
      let app = Middleware.[
        logger;
        router routes;
      ]
    ]}
    
    Logs format: [METHOD /path -> STATUS in DURATIONms]
    
    See {!Logger} for full documentation. *)
val logger: Pipeline.middleware

(** Request ID middleware.
    
    Convenience alias for [Request_id.request_id].
    
    Example:
    {[
      let app = Middleware.[
        request_id;  (* Generate/preserve request IDs *)
        logger;      (* Can now log with request ID context *)
        router routes;
      ]
    ]}
    
    Ensures every request has an [x-request-id] header in both
    the request (for handlers) and response (for clients).
    
    See {!Request_id} for full documentation. *)
val request_id: Pipeline.middleware

(** Visual debugger middleware for development.
    
    Convenience alias for [Debugger.debugger].
    
    {b ⚠️ DEVELOPMENT ONLY} - Never use in production!
    
    Example:
    {[
      let app = Middleware.[
        request_id;
        logger;      (* Logs successful requests *)
        debugger;    (* Catches & logs errors, shows error page *)
        router routes;
      ]
    ]}
    
    Shows beautiful error pages with:
    - Full stack traces with source code snippets
    - Request/response inspection
    - File:line error locations
    - Automatic console logging
    
    See {!Debugger} for full documentation and production safety patterns. *)
val debugger: Pipeline.middleware

module Cors = Cors

(** {b CORS Middleware}
    
    Cross-Origin Resource Sharing for APIs and SPAs.
    
    Handles preflight (OPTIONS) and simple CORS requests with
    origin matching (exact strings or wildcard).
    
    {b Quick Start:}
    {[
      (* Development - allow all *)
      let app = Middleware.[
        cors ~origins:["*"] ();
        router routes;
      ]
      
      (* Production - specific origins *)
      let app = Middleware.[
        cors ~origins:["https://app.example.com"] ~credentials:true ();
        router routes;
      ]
    ]}
    
    See {!Cors} for full documentation and examples. *)
(** CORS middleware - simple and direct.
    
    {[
      let app = Middleware.[
        request_id;
        logger;
        cors ~origins:["https://example.com"] ();
        router routes;
      ]
    ]}
    
    {b Parameters:}
    - [origins] - List of allowed origins (use ["*"] for all, or exact matches)
    - [methods] - Allowed HTTP methods beyond GET/HEAD/POST (default: [PUT; PATCH; DELETE])
    - [headers] - Allowed custom headers (default: [] = simple headers only)
    - [credentials] - Allow cookies/auth (default: false, cannot use with wildcard)
    - [expose] - Headers visible to JavaScript (default: [])
    - [max_age] - Preflight cache in seconds (default: none)
    
    {b Security:}
    - Never use ["*"] with [~credentials:true]
    - Be specific with [~headers]
    
     See {!Cors.middleware} for full documentation. *)
val cors: origins:string list ->
  ?methods:Net.Http.Method.t list ->
  ?headers:string list ->
  ?credentials:bool ->
  ?expose:string list ->
  ?max_age:int ->
  unit ->
  Pipeline.middleware

module Session = Session

(** {b Session Middleware}
    
    Secure cookie-based session management.
    
    Stores encrypted session data in HTTP-only cookies.
    No server-side storage required (stateless sessions).
    
    {b Quick Start:}
    {[
      (* Minimal setup *)
      let app = Middleware.[
        session ~secret:"my-secret-key-256-bits" ();
        router routes;
      ]
      
      (* In handlers *)
      let handler ~conn ~next:_ =
        let session = Session.get conn in
        match Session.get_value "user_id" session with
        | Some id -> (* User logged in *)
        | None -> (* Anonymous *)
    ]}
    
    {b Security:}
    - Always use a strong random secret (256 bits)
    - Set [~secure:true] in production (HTTPS only)
    - Default [SameSite=Lax] prevents CSRF
    - Session data encrypted and signed
    
    {b Warning:} Current crypto is a placeholder. Replace with
    AES-256-GCM for production.
    
    See {!Session} for full documentation. *)
(** Session middleware - secure cookie-based sessions.
    
    {[
      let secret = match Env.get "SESSION_SECRET" with
        | Some s -> s
        | None -> failwith "SESSION_SECRET required"
      in
      
      let app = Middleware.[
        request_id;
        logger;
        session ~secret ~secure:true ();
        router routes;
      ]
    ]}
    
    {b Parameters:}
    - [secret] - Encryption/signing key (required, use 256-bit random value)
    - [cookie_name] - Cookie name (default: "_suri_session")
    - [max_age] - Session lifetime in seconds (default: 86400 = 24h)
    - [secure] - Require HTTPS (default: false, {b set true in production!})
    - [same_site] - CSRF protection (default: [Lax])
    
    {b Usage in handlers:}
    {[
      (* Read session *)
      let session = Session.get conn in
      match Session.get_value "user_id" session with
      | Some id -> Printf.printf "User: %s\n" id
      | None -> Printf.printf "Anonymous\n"
      
      (* Write session *)
      Session.put "user_id" "123" session;
      Session.put "username" "alice" session;
      
      (* Logout *)
      Session.clear session
    ]}
    
    See {!Session.middleware} for full documentation. *)
val session: secret:string ->
  ?cookie_name:string ->
  ?max_age:int ->
  ?secure:bool ->
  ?same_site:Http.Http1.Cookie.same_site ->
  unit ->
  Pipeline.middleware

module Csrf = Csrf

(** {b CSRF Protection Middleware}
    
    Cross-Site Request Forgery protection for forms and AJAX requests.
    
    Validates that requests originate from your application by checking
    cryptographic tokens.
    
    {b Quick Start:}
    {[
      let app = Middleware.[
        session ~secret:"secret" ();
        csrf ();
        router routes;
      ]
      
      (* In forms *)
      let form conn =
        "<form method='POST'>" ^
        Csrf.hidden_field conn ^
        "<input name='data'>" ^
        "</form>"
    ]}
    
    {b Security:}
    - Requires Session middleware (must be earlier in pipeline)
    - Tokens are masked to prevent BREACH attacks
    - Returns 403 Forbidden if token missing/invalid
    - Safe methods (GET, HEAD, OPTIONS) skipped by default
    
    See {!Csrf} for full documentation. *)
(** CSRF protection middleware - validates tokens on unsafe requests.
    
    {[
      let app = Middleware.[
        request_id;
        logger;
        session ~secret:"secret" ();
        body_parser ();  (* Parse form data before CSRF! *)
        csrf ();  (* Protects POST, PUT, DELETE, etc. *)
        router routes;
      ]
    ]}
    
    {b Parameters:}
    - [param_name] - Form parameter name (default: "_csrf_token")
    - [header_name] - HTTP header name (default: "x-csrf-token")
    - [skip_safe_methods] - Skip GET/HEAD/OPTIONS (default: true)
    - [skip] - Custom function to skip specific paths
    
    {b Usage in views:}
    {[
      (* HTML forms *)
      Csrf.hidden_field conn  (* Generates <input type="hidden" ...> *)
      
      (* AJAX requests *)
      Csrf.meta_tag conn      (* Generates <meta name="csrf-token" ...> *)
      
      (* Raw token *)
      Csrf.get_token conn     (* Returns token string *)
    ]}
    
    See {!Csrf.middleware} for full documentation. *)
val csrf: ?param_name:string ->
  ?header_name:string ->
  ?skip_safe_methods:bool ->
  ?skip:(Conn.t -> bool) ->
  unit ->
  Pipeline.middleware

module Body_parser = Body_parser

(** {b Body Parser Middleware}
    
    Automatically parses request bodies based on Content-Type.
    
    {b Supported formats:}
    - [application/x-www-form-urlencoded] - HTML forms
    - [application/json] - JSON payloads
    - [multipart/form-data] - File uploads (Phase 2)
    
    {b Quick Start:}
    {[
      let app = Middleware.[
        session ~secret:"secret" ();
        body_parser ();  (* Parse before CSRF! *)
        csrf ();
        router routes;
      ]
      
      (* In handlers *)
      let create_user ~conn ~next:_ =
        let name = List.assoc_opt "name" (Conn.body_params conn) in
        let email = List.assoc_opt "email" (Conn.body_params conn) in
        (* ... *)
    ]}
    
    See {!Body_parser} for full documentation. *)
(** Body parser middleware - parses request bodies automatically.
    
    {[
      (* Basic usage *)
      let app = Middleware.[
        body_parser ();  (* Default: urlencoded + JSON, 10MB limit *)
        router routes;
      ]
      
      (* Custom config *)
      let app = Middleware.[
        body_parser ~config:{
          parsers = [Urlencoded; Json];
          max_body_size = 50 * 1024 * 1024;  (* 50MB *)
        } ();
        router routes;
      ]
    ]}
    
    {b Parameters:}
    - [config.parsers] - List of enabled parsers (default: [Urlencoded; Json])
    - [config.max_body_size] - Max size in bytes (default: 10MB)
    
    {b Important:} Place this middleware {b before} CSRF middleware so
    CSRF tokens in form bodies are accessible.
    
    Bodies exceeding [max_body_size] are not parsed (left empty).
    
    See {!Body_parser.make} for full documentation. *)
val body_parser: ?config:Body_parser.config -> unit -> Pipeline.middleware

module Static = Static

(** {b Static Files Middleware}
    
    Serve static files from a directory with security, caching, and optional
    directory browsing.
    
    {b Quick Start:}
    {[
      let app = Middleware.[
        logger;
        static ~at:"/public" (Path.v "./public") ();
        router routes;
      ]
    ]}
    
    {b Features:}
    - ✅ Security: Path traversal protection, dotfile blocking
    - ✅ Performance: ETag and Last-Modified caching, 304 responses
    - ✅ MIME Types: Automatic detection for 30+ file types
    - ✅ Directory Browsing: Optional HTML listings
    - ✅ Custom Headers: Add security headers, CORS, etc.
    
    {b Security:}
    The middleware prevents common security issues:
    - Path Traversal: Blocks [../../../etc/passwd] attempts
    - Dotfiles: Blocks [.env], [.git/config] by default
    - Symlinks: Follows or denies based on config
    - File Types: Only serves regular files, not special files
    
    See {!Static} for full documentation. *)
(** Serve static files from a directory.
    
    {[
      (* Serve from ./public at /public URL *)
      let app = Middleware.[
        static ~at:"/public" (Path.v "./public") ();
        router routes;
      ]
    ]}
    
    {b Parameters:}
    - [at] - URL prefix to match (e.g., ["/public"], ["/assets"])
    - [root] - Filesystem directory to serve from (e.g., [Path.v "./public"])
    - [config] - Optional configuration (uses {!Static.default_config} if not provided)
    
    {b Examples:}
    
    Custom caching headers:
    {[
      let config = Static.{ default_config with
        cache_control = Some "public, max-age=31536000, immutable";
      } in
      static ~at:"/assets" ~config (Path.v "./dist") ()
    ]}
    
    Enable directory browsing:
    {[
      let config = Static.{ default_config with show_directory = true } in
      static ~at:"/files" ~config (Path.v "./files") ()
    ]}
    
    Multiple static directories:
    {[
      let app = Middleware.[
        static ~at:"/images" (Path.v "./storage/images") ();
        static ~at:"/uploads" (Path.v "./uploads") ();
        static ~at:"/assets" (Path.v "./public") ();
        router routes;
      ]
    ]}
    
    {b Security:}
    - Blocks path traversal (..)
    - Blocks dotfiles by default
    - Validates paths are within root
    
    See {!Static.middleware} for full documentation. *)
val static: ?config:Static.config -> at:string -> Path.t -> unit -> Pipeline.middleware

module Basic_auth = Basic_auth

(** {b HTTP Basic Authentication Middleware}
    
    Simple username/password protection for routes.
    
    {b ⚠️ SECURITY WARNING}: Basic Auth transmits credentials in Base64
    (NOT encryption). {b ALWAYS use HTTPS in production!}
    
    {b Quick Start:}
    {[
      (* Simple protection *)
      let app = Middleware.[
        logger;
        basic_auth ~username:"admin" ~password:"secret" ();
        router routes;
      ]
      
      (* Custom validation *)
      let validate ~username ~password =
        match Database.find_user username with
        | Some user when verify_password user password -> Some user
        | _ -> None
      in
      let app = Middleware.[
        logger;
        basic_auth_with_validation ~validate ~realm:"Member Area" ();
        router routes;
      ]
    ]}
    
    {b Features:}
    - ✅ Constant-time password comparison (timing attack prevention)
    - ✅ Realm sanitization (header injection prevention)
    - ✅ Custom validation support (database, LDAP, etc.)
    - ✅ Skip paths (allow public routes)
    - ✅ RFC 7617 compliant
    
    See {!Basic_auth} for full documentation. *)
(** Basic Auth with static credentials.
    
    {[
      let app = Middleware.[
        logger;
        basic_auth ~username:"admin" ~password:"secret" ();
        router routes;
      ]
    ]}
    
    {b Parameters:}
    - [username] - Expected username
    - [password] - Expected password
    - [realm] - Realm name shown in browser prompt (default: "Restricted Area")
    - [skip] - Function to skip authentication for specific requests
    
    Skip public paths:
    {[
      basic_auth 
        ~username:"admin" 
        ~password:"secret"
        ~skip:(fun conn ->
          String.starts_with (Conn.request_path conn) ~prefix:"/public"
        )
        ()
    ]}
    
    {b Security:}
    - Uses constant-time comparison to prevent timing attacks
    - Sanitizes realm to prevent header injection
    - {b REQUIRES HTTPS in production!}
    
    See {!Basic_auth.middleware} for full documentation. *)
val basic_auth: ?realm:string ->
  ?skip:(Conn.t -> bool) ->
  username:string ->
  password:string ->
  unit ->
  Pipeline.middleware

(** Basic Auth with custom validation function.
    
    Use for database lookups, LDAP, or any custom auth logic.
    
    {[
      let validate ~username ~password =
        match Database.find_user username with
        | Some user when verify_password user password -> Some user
        | _ -> None
      in
      
      let app = Middleware.[
        logger;
        basic_auth_with_validation ~validate ~realm:"Member Area" ();
        router routes;
      ]
    ]}
    
    {b Parameters:}
    - [validate] - Function returning [Some user_data] on success, [None] on failure
    - [realm] - Realm name shown in browser prompt (default: "Restricted Area")
    - [skip] - Function to skip authentication for specific requests
    
    Access authenticated user in handlers:
    {[
      let handler ~conn ~next:_ =
        match Basic_auth.get "basic_auth_user" conn with
        | Some user -> (* user is what validate returned *)
        | None -> (* should never happen if middleware passed *)
    ]}
    
    See {!Basic_auth.middleware_with_validation} for full documentation. *)
val basic_auth_with_validation:
  ?realm:string ->
  ?skip:(Conn.t -> bool) ->
  validate:'a Basic_auth.validation_fn ->
  unit ->
  Pipeline.middleware

module Accepts = Accepts

(** {b Content Negotiation Middleware}
    
    Validate request Accept and Content-Type headers.
    
    Ensures APIs only process requests in supported formats.
    Returns 406 Not Acceptable or 415 Unsupported Media Type for mismatches.
    
    {b Quick Start:}
    {[
      (* JSON-only API *)
      let app = Middleware.[
        logger;
        accepts ["application/json"];
        body_parser ();
        router routes;
      ]
      
      (* Multi-format API *)
      let app = Middleware.[
        logger;
        accepts ["application/json"; "application/xml"; "text/csv"];
        router routes;
      ]
    ]}
    
    {b Features:}
    - ✅ Accept header validation
    - ✅ Content-Type validation (POST/PUT/PATCH)
    - ✅ Wildcard support ([*/*], [text/*])
    - ✅ Quality value parsing ([q=0.8])
    - ✅ Custom rejection handlers
    
    {b Important}: Place {b before} body parsing middleware!
    
    See {!Accepts} for full documentation. *)
(** Content negotiation middleware - validates Accept and Content-Type headers.
    
    {[
      (* Simple usage *)
      let app = Middleware.[
        logger;
        accepts ["application/json"];
        body_parser ();
        router routes;
      ]
    ]}
    
    {b Parameters:}
    - [types] - List of accepted MIME types (supports wildcards)
    - [config] - Optional configuration for advanced use cases
    
    {b Supported types:}
    - Exact: ["application/json"]
    - Wildcard type: ["text/*"] (matches [text/plain], [text/html], etc.)
    - Full wildcard: ["*/*"] (matches anything)
    
    {b Examples:}
    
    Multiple types:
    {[
      accepts ["application/json"; "application/xml"; "text/plain"]
    ]}
    
    With wildcards:
    {[
      accepts ["text/*"; "application/json"]
    ]}
    
    {b Responses:}
    - 406 Not Acceptable - Accept header doesn't match
    - 415 Unsupported Media Type - Content-Type doesn't match
    
    {b Order matters}: Place before body_parser to avoid parsing unsupported content!
    
    See {!Accepts.middleware} for full documentation. *)
val accepts: ?config:Accepts.config -> string list -> Pipeline.middleware

module Head = Head

(** {b HEAD Request Handler}
    
    Automatically handles HEAD requests by stripping response bodies.
    
    {b What it does:}
    - Processes HEAD requests normally through the pipeline
    - Preserves all headers (Content-Length, ETag, etc.)
    - Removes the response body
    - Ensures HTTP/1.1 compliance
    
    {b Quick Start:}
    {[
      let app = Middleware.[
        logger;
        head;  (* Handle HEAD requests *)
        router routes;
      ]
    ]}
    
    This is typically the first middleware in your pipeline.
    No configuration needed - it's completely automatic.
    
    See {!Head} for full documentation. *)
(** HEAD request handler middleware.
    
    {[
      let app = Middleware.[
        head;  (* Automatic HEAD support *)
        router routes;
      ]
    ]}
    
    Automatically strips response bodies for HEAD requests while
    preserving all headers. No configuration required. *)
val head: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

module Runner = Runner

module Runtime = Request_runtime

(** {b Request Timing Middleware}
    
    Adds X-Runtime header with request processing time.
    
    {b Features:}
    - ✅ High-precision timing using Time.Instant
    - ✅ Formatted as seconds (e.g., "0.0234")
    - ✅ Useful for performance monitoring and client-side metrics
    
    {b Quick Start:}
    {[
      let app = Middleware.[
        runner;  (* Add timing header *)
        logger;
        router routes;
      ]
    ]}
    
    Clients receive: [X-Runtime: 0.0234] (seconds)
    
    See {!Runner} for full documentation. *)
(** Request timing middleware.
    
    {[
      let app = Middleware.[
        runner;  (* Time requests *)
        logger;
        router routes;
      ]
    ]}
    
    Adds [X-Runtime] header with processing time in seconds. *)
val runner: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

module Method_override = Method_override

(** {b HTTP Method Override}
    
    Allows HTML forms to use PUT/PATCH/DELETE via _method parameter.
    
    {b Why?} HTML forms only support GET and POST. This middleware
    enables REST APIs with proper HTTP verbs.
    
    {b Quick Start:}
    {[
      let app = Middleware.[
        logger;
        body_parser ();  (* Must parse body first! *)
        method_override;
        router routes;
      ]
      
      (* In HTML forms *)
      <form method="POST" action="/users/123">
        <input type="hidden" name="_method" value="DELETE">
        <button>Delete User</button>
      </form>
    ]}
    
    {b Security:}
    - Only overrides POST requests
    - Only allows PUT, PATCH, DELETE (not GET)
    - Reads from [_method] parameter by default
    
    See {!Method_override} for full documentation. *)
(** Method override middleware for HTML forms (uses default "_method" param).
    
    {[
      let app = Middleware.[
        logger;
        body_parser ();  (* Required! *)
        method_override;  (* Now forms can use PUT/PATCH/DELETE *)
        router routes;
      ]
    ]}
    
    Uses the default form parameter name "_method".
    For custom parameter names, use [Method_override.middleware ~param:"..."].
    
    Place {b after} body_parser so form data is available. *)
val method_override: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

module Remote_ip = Remote_ip

(** {b Real Client IP Extraction}
    
    Extracts the real client IP address when behind proxies.
    
    {b Why?} When behind load balancers or CDNs, [Conn.peer] shows
    the proxy IP, not the real client. This middleware reads
    [X-Forwarded-For] headers securely.
    
    {b Quick Start:}
    {[
      (* Simple setup - trust your proxy *)
      let app = Middleware.[
        logger;
        Remote_ip.middleware ~proxies:["10.0.0.1"];
        router routes;
      ]
      
      (* In handlers, use the corrected IP *)
      let handler ~conn ~next:_ =
        let peer = Conn.peer conn in
        let client_ip = peer.ip in
        Log.info ("Request from: " ^ client_ip);
        Conn.respond conn ~status:Ok ~body:"OK"
    ]}
    
    {b Security:}
    - Only trusts specified proxies (exact IP matching)
    - Validates X-Forwarded-For format
    - Prevents IP spoofing
    
    {b Note:} Current version uses exact IP matching.
    CIDR support planned for future.
    
    See {!Remote_ip} for full documentation. *)

(* No convenience function - use Remote_ip.middleware ~proxies:[...] directly *)

module Etag = Etag

(** {b ETag Generation}
    
    Automatically generates ETags for response bodies.
    
    {b What are ETags?} Unique identifiers for response content.
    Clients send them back to check if content changed (304 responses).
    
    {b Quick Start:}
    {[
      let app = Middleware.[
        logger;
        conditional_get;  (* Check ETags *)
        etag;            (* Generate ETags *)
        router routes;
      ]
    ]}
    
    {b Features:}
    - ✅ SHA256-based hashing
    - ✅ Weak ETags support ([~weak:true])
    - ✅ Skips empty bodies
    - ✅ Doesn't override existing ETags
    
    {b Example Response:}
    {[
      ETag: "a1b2c3d4e5f67890"
    ]}
    
    Pair with {!Conditional_get} for automatic 304 responses.
    
    See {!Etag} for full documentation. *)
(** ETag generation middleware (uses strong ETags by default).
    
    {[
      let app = Middleware.[
        conditional_get;
        etag;  (* Generate ETags from response bodies *)
        router routes;
      ]
    ]}
    
    Generates strong ETags by default.
    For weak ETags, use [Etag.middleware ~weak:true].
    
    Generates ETags using SHA256 hash of response body.
    Place before conditional_get in pipeline. *)
val etag: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

module Conditional_get = Conditional_get

(** {b HTTP Conditional Requests}
    
    Implements 304 Not Modified responses for cached content.
    
    {b What it does:}
    - Checks [If-None-Match] against response ETag
    - Checks [If-Modified-Since] against Last-Modified
    - Returns 304 with empty body if content unchanged
    - Reduces bandwidth and improves performance
    
    {b Quick Start:}
    {[
      let app = Middleware.[
        logger;
        conditional_get;  (* Check cache headers *)
        etag;            (* Generate ETags *)
        router routes;
      ]
    ]}
    
    {b How it works:}
    
    1. Client makes request with caching headers:
       [If-None-Match: "abc123"] or [If-Modified-Since: Wed, 21 Oct 2015 07:28:00 GMT]
    
    2. Middleware processes request and checks response headers
    
    3. If content matches (unchanged):
       - Returns 304 Not Modified
       - Empty body (saves bandwidth)
       - Preserves cache headers
    
    4. If content different:
       - Returns full response (200 OK)
    
    {b Benefits:}
    - Reduces bandwidth
    - Faster responses for cached content
    - Standard HTTP caching
    
    See {!Conditional_get} for full documentation. *)
(** Conditional GET middleware for 304 responses.
    
    {[
      let app = Middleware.[
        conditional_get;  (* Check ETags and dates *)
        etag;            (* Generate ETags *)
        router routes;
      ]
    ]}
    
    Checks If-None-Match and If-Modified-Since headers.
    Returns 304 Not Modified when content hasn't changed.
    
    Only applies to GET and HEAD requests. *)
val conditional_get: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t
