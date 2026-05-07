open Std
open Std.Result.Syntax

module Ser = Serde.Ser

type t = {
  path: Path.t;
  exists: bool;
  format: string option;
  profile: Profile.t option;
}

type error =
  | SummarySystemError of {
      path: Path.t;
      reason: string;
    }

let format_of_path = fun path ->
  let basename = Path.basename path in
  if String.ends_with ~suffix:".trace" basename then
    Some "xctrace"
  else if String.ends_with ~suffix:".perfetto" basename then
    Some "perfetto"
  else if String.ends_with ~suffix:".perf" basename then
    Some "perf.data"
  else
    None

let summarize_profile = fun ~format path ->
  match format with
  | Some "xctrace" ->
      Xctrace.summarize_file path
      |> Result.map_err ~fn:(fun reason -> SummarySystemError { path; reason })
      |> Result.map ~fn:Option.some
  | _ -> Ok None

let summarize = fun path ->
  let* exists =
    Fs.exists path
    |> Result.map_err
      ~fn:(fun err ->
        SummarySystemError {
          path;
          reason = "failed to inspect trace path: " ^ IO.error_message err;
        })
  in
  let format = format_of_path path in
  let* profile =
    if exists then
      summarize_profile ~format path
    else
      Ok None
  in
  Ok { path; exists; format; profile }

let path_serializer = Ser.contramap Path.to_string Ser.string

let ser_list = fun serializer ->
  Ser.contramap Collections.Vector.from_list (Ser.list serializer)

let serializer =
  Ser.record
    (
      Ser.fields [
        Ser.field "type" Ser.string (fun (_summary: t) -> "trace.summary");
        Ser.field "path" path_serializer (fun (summary: t) -> summary.path);
        Ser.field "exists" Ser.bool (fun (summary: t) -> summary.exists);
        Ser.field "format" (Ser.option Ser.string) (fun (summary: t) -> summary.format);
        Ser.field "profile" (Ser.option Profile.serializer) (fun (summary: t) -> summary.profile);
      ]
    )

let table_profile_serializer =
  Ser.record
    (
      Ser.fields [
        Ser.field "sample_count" Ser.int (fun (profile: Profile.t) -> profile.sample_count);
        Ser.field "total_weight_ns" Ser.int (fun (profile: Profile.t) -> profile.total_weight_ns);
        Ser.field
          "total_weight_ms"
          Ser.float
          (fun (profile: Profile.t) -> Profile.weight_ms profile.total_weight_ns);
        Ser.field "top_self" (ser_list Profile.call_cost_serializer) (fun (profile: Profile.t) -> profile.top_self);
        Ser.field
          "top_total"
          (ser_list Profile.call_cost_serializer)
          (fun (profile: Profile.t) -> profile.top_total);
      ]
    )

let call_tree_profile_serializer =
  Ser.record
    (
      Ser.fields [
        Ser.field "sample_count" Ser.int (fun (profile: Profile.t) -> profile.sample_count);
        Ser.field "total_weight_ns" Ser.int (fun (profile: Profile.t) -> profile.total_weight_ns);
        Ser.field
          "total_weight_ms"
          Ser.float
          (fun (profile: Profile.t) -> Profile.weight_ms profile.total_weight_ns);
        Ser.field
          "call_tree"
          (ser_list Profile.call_tree_node_serializer)
          (fun (profile: Profile.t) -> profile.call_tree);
        Ser.field
          "hidden_call_tree_roots"
          Ser.int
          (fun (profile: Profile.t) -> profile.hidden_call_tree_roots);
      ]
    )

let table_serializer =
  Ser.record
    (
      Ser.fields [
        Ser.field "type" Ser.string (fun (_summary: t) -> "trace.summary");
        Ser.field "path" path_serializer (fun (summary: t) -> summary.path);
        Ser.field "exists" Ser.bool (fun (summary: t) -> summary.exists);
        Ser.field "format" (Ser.option Ser.string) (fun (summary: t) -> summary.format);
        Ser.field "profile" (Ser.option table_profile_serializer) (fun (summary: t) -> summary.profile);
      ]
    )

let call_tree_serializer =
  Ser.record
    (
      Ser.fields [
        Ser.field "type" Ser.string (fun (_summary: t) -> "trace.call_tree");
        Ser.field "path" path_serializer (fun (summary: t) -> summary.path);
        Ser.field "exists" Ser.bool (fun (summary: t) -> summary.exists);
        Ser.field "format" (Ser.option Ser.string) (fun (summary: t) -> summary.format);
        Ser.field
          "profile"
          (Ser.option call_tree_profile_serializer)
          (fun (summary: t) -> summary.profile);
      ]
    )

let to_json_string = fun summary -> Serde_json.to_string serializer summary

let to_table_json_string = fun summary -> Serde_json.to_string table_serializer summary

let to_call_tree_json_string = fun summary -> Serde_json.to_string call_tree_serializer summary

let error_message = fun __tmp1 ->
  match __tmp1 with
  | SummarySystemError { path; reason } ->
      "failed to summarize trace '" ^ Path.to_string path ^ "': " ^ reason
