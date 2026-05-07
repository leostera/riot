open Std

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

val format_of_path: Path.t -> string option

val summarize: Path.t -> (t, error) result

val serializer: t Serde.Ser.t

val table_serializer: t Serde.Ser.t

val call_tree_serializer: t Serde.Ser.t

val to_json_string: t -> (string, Serde.error) result

val to_table_json_string: t -> (string, Serde.error) result

val to_call_tree_json_string: t -> (string, Serde.error) result

val error_message: error -> string
