open Std

type t =
  | Auto
  | Perf
  | Xctrace

type unavailable = {
  profiler: string;
  reason: string;
}

let to_string = fun __tmp1 ->
  match __tmp1 with
  | Auto -> "auto"
  | Perf -> "perf"
  | Xctrace -> "xctrace"

let from_string = fun value ->
  match String.lowercase_ascii value with
  | "auto" -> Ok Auto
  | "perf" -> Ok Perf
  | "xctrace" -> Ok Xctrace
  | other ->
      Error
        ("unknown profiler '"
        ^ other
        ^ "' (expected auto, perf, or xctrace)")

let is_filename_safe_char = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '-'
  | '.' -> true
  | _ -> false

let sanitize_filename_part = fun value ->
  let builder = StringBuilder.create ~size:(String.length value) in
  for index = 0 to String.length value - 1 do
    let char = String.get_unchecked value ~at:index in
    StringBuilder.add_char
      builder
      (
        if is_filename_safe_char char then
          char
        else
          '_'
      )
  done;
  StringBuilder.contents builder

let default_output_path = fun ~binary_name ->
  let timestamp =
    DateTime.now_utc ()
    |> DateTime.to_iso8601
  in
  Path.v (sanitize_filename_part binary_name ^ "_" ^ timestamp ^ ".trace")

let effective = fun profiler ->
  match profiler with
  | Perf
  | Xctrace -> Ok profiler
  | Auto ->
      let host = Riot_model.Riot_dirs.host_target () in
      match host.Riot_model.Target.os with
      | "linux" -> Ok Perf
      | "darwin" -> Ok Xctrace
      | os ->
          Error {
            profiler = "auto";
            reason = "no sampled profiler backend is configured for host OS '" ^ os ^ "'";
          }
