open Std

type kind =
  | File
  | Fragment
  | Generated

type origin =
  | Path of Path.t
  | Label of string

type t = {
  source_id: SourceId.t;
  kind: kind;
  origin: origin;
  text: string;
  revision: int;
}

let make = fun ~source_id ~kind ~origin ~revision ~text ->
  { source_id; kind; origin; text; revision }

let update_text = fun source ~revision ~text ->
  { source with revision; text }

let display_name = fun source ->
  match source.origin with
  | Path path -> Path.to_string path
  | Label label -> label
