open Std

(** {1 Request Timing Middleware}

    Adds an X-Runtime header to responses showing request processing time.

    {2 Quick Start}

    {[
      let app = Middleware.[
        runner;   (* Adds X-Runtime: 0.0234 *)
        logger;
        router routes;
      ]
    ]}

    {2 Output Format}

    The X-Runtime header contains the request duration in seconds with
    millisecond precision:

    {v X-Runtime: 0.0234 v}  (23.4 milliseconds)

    {2 Use Cases}

    - {b Client monitoring}: Clients can track API response times
    - {b Load balancer metrics}: HAProxy, nginx can log response times
    - {b Debugging}: Quick visibility into slow requests
    - {b APM integration}: Application Performance Monitoring tools

    {2 Example Response}

    {v
    HTTP/1.1 200 OK
    Content-Type: application/json
    X-Runtime: 0.0156
    Content-Length: 42

    {"status": "ok", "data": [...]}
    v}

    {2 Placement}

    Place early in the stack to measure total request time:

    {[
      let app = Middleware.[
        runner;    (* Start timer *)
        logger;     (* This duration is included *)
        cors ~origins:["*"] ();
        router routes;
      ]
    ]}

    Or place later to measure only handler time:

    {[
      let app = Middleware.[
        logger;
        cors ~origins:["*"] ();
        runner;    (* Only measures handler + router *)
        router routes;
      ]
    ]} *)
(** {1 Middleware} *)
(** Request timing middleware.

    Measures request processing time and adds X-Runtime header.

    {[
      let app = Middleware.[
        runner;
        router routes;
      ]
    ]}

    {b Header format}: [X-Runtime: 0.0234] (seconds with 4 decimal places)

    {b Performance}: Minimal overhead (microsecond-level timing) *)
val middleware: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t
