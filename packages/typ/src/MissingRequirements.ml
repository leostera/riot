open Std

type requirement =
  | MissingRootSource of { source_id: SourceId.t }
  | MissingModuleSummary of { module_name: string; requested_by: SourceId.t list }

type t = requirement list

let compare_source_ids = fun left right ->
  Int.compare (SourceId.to_int left) (SourceId.to_int right)

let normalize_requesters = fun requested_by ->
  List.sort_uniq compare_source_ids requested_by

let add_module_requirement = fun modules module_name requested_by ->
  let requested_by = normalize_requesters requested_by in
  match List.assoc_opt module_name modules with
  | None -> (module_name, requested_by) :: modules
  | Some existing ->
      let merged_requested_by = normalize_requesters (existing @ requested_by) in
      (module_name, merged_requested_by) :: List.remove_assoc module_name modules

let of_list = fun requirements ->
  let rec loop roots modules = function
    | [] ->
        let root_requirements =
          roots
          |> List.sort_uniq compare_source_ids
          |> List.map (fun source_id -> MissingRootSource { source_id })
        in
        let module_requirements =
          modules
          |> List.sort (fun (left, _) (right, _) -> String.compare left right)
          |> List.map
            (fun (module_name, requested_by) ->
              MissingModuleSummary {
                module_name;
                requested_by = normalize_requesters requested_by;
              })
        in
        root_requirements @ module_requirements
    | MissingRootSource { source_id } :: tail ->
        loop (source_id :: roots) modules tail
    | MissingModuleSummary { module_name; requested_by } :: tail ->
        loop roots (add_module_requirement modules module_name requested_by) tail
  in
  loop [] [] requirements

let requirements = fun missing -> missing

let is_empty = fun missing -> List.is_empty missing

let requirement_to_json = function
  | MissingRootSource { source_id } -> Data.Json.Object [
    ("tag", Data.Json.String "missing_root_source");
    ("source_id", Data.Json.Int (SourceId.to_int source_id));
  ]
  | MissingModuleSummary { module_name; requested_by } -> Data.Json.Object [
    ("tag", Data.Json.String "missing_module_summary");
    ("module_name", Data.Json.String module_name);
    (
      "requested_by",
      Data.Json.Array (requested_by
      |> List.map (fun source_id -> Data.Json.Int (SourceId.to_int source_id)))
    );
  ]

let to_json = fun missing -> Data.Json.Array (List.map requirement_to_json missing)
