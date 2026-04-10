open Std

(** Runtime header middleware - measures request processing time *)
let middleware = fun ~conn ~next ->
  (* Record start time *)
  let start = Time.Instant.now () in
  (* Process request *)
  let conn' = next conn in
  (* Calculate duration in seconds *)
  let duration = Time.Instant.elapsed start in
  let seconds = Time.Duration.to_secs_float duration in
  (* Format as string with 4 decimal places *)
  let runtime_str =
    let whole = int_of_float seconds in
    let frac = int_of_float ((seconds -. float_of_int whole) *. 10000.0) in
    let frac_str = string_of_int frac in
    (* Pad to 4 digits *)
    let padded =
      let len = String.length frac_str in
      if len >= 4 then
        String.sub frac_str 0 4
      else
        frac_str ^ String.make (4 - len) '0'
    in
    string_of_int whole ^ "." ^ padded
  in
  (* Add X-Runtime header *)
  Conn.with_header "x-runtime" runtime_str conn'
