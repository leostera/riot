open Std
open Std.Collections

type package_scope =
  | Unknown
  | Build
  | Runtime
  | Dev

type failure = {
  package_name: Riot_model.Package_name.t;
  unit_key: Riot_planner.Build_unit.key;
  reason: failure_reason;
  message: string;
  ocamlc_warnings: string list;
  duration_ms: int;
}

and failure_reason =
  | PackagePlanningFailed of Riot_planner.Planning_error.t
  | PackageExecutionFailed of { message: string }
  | PackageActionFailed of { message: string }
  | PackageActionOutputsNotCreated of {
      missing: Std.Path.t list;
    }
  | PackageActionDependenciesFailed of {
      failed: Std.Graph.SimpleGraph.Node_id.t list;
    }
  | PackageSkipped of { reason: string }
  | UnknownFailure

type package_status =
  | Built of Riot_store.Artifact.t
  | Cached of Riot_store.Artifact.t
  | Skipped of string
  | Failed of string

type package_result = {
  package_name: Riot_model.Package_name.t;
  scope: package_scope;
  status: package_status;
  artifacts: Riot_store.Artifact.t list;
}

type t = {
  packages: package_result list;
}

let package_status_of_build_status = fun __tmp1 ->
  match __tmp1 with
  | Package_builder.Built artifact -> Built artifact
  | Package_builder.Cached artifact -> Cached artifact
  | Package_builder.Skipped { reason } -> Skipped reason
  | Package_builder.Failed error -> Failed (Package_builder.package_error_to_string error)

let artifact_of_status = fun __tmp1 ->
  match __tmp1 with
  | Built artifact
  | Cached artifact -> Some artifact
  | Skipped _
  | Failed _ -> None

let scope_of_unit_key = fun (key: Riot_planner.Build_unit.key) ->
  match key.artifact with
  | Riot_planner.Build_unit.SyntheticTool _ -> Build
  | Library
  | RuntimeBinary _ -> Runtime
  | TestBinary _
  | ExampleBinary _
  | BenchBinary _ -> Dev

let scope_priority = fun __tmp1 ->
  match __tmp1 with
  | Unknown -> 0
  | Build -> 1
  | Runtime -> 2
  | Dev -> 3

let status_priority = fun __tmp1 ->
  match __tmp1 with
  | Failed _ -> 0
  | Skipped _ -> 1
  | Cached _ -> 2
  | Built _ -> 3

let should_prefer_status = fun ~current_scope ~current_status ~incoming_scope ~incoming_status ->
  let scope_comparison =
    Int.compare (scope_priority incoming_scope) (scope_priority current_scope)
  in
  if scope_comparison = Order.GT then
    true
  else if scope_comparison = Order.LT then
    false
  else
    Int.compare (status_priority incoming_status) (status_priority current_status) = Order.GT

let should_prefer = fun current incoming ->
  should_prefer_status
    ~current_scope:current.scope
    ~current_status:current.status
    ~incoming_scope:incoming.scope
    ~incoming_status:incoming.status

let package_result_of_build_result = fun (result: Package_builder.build_result) ->
  let status = package_status_of_build_status result.status in
  {
    package_name = result.package.name;
    scope = scope_of_unit_key result.unit_key;
    status;
    artifacts = Option.to_list (artifact_of_status status);
  }

type package_result_builder = {
  package_name: Riot_model.Package_name.t;
  mutable scope: package_scope;
  mutable status: package_status;
  artifacts: Riot_store.Artifact.t Vector.t;
  artifact_hashes: Crypto.Hash.t HashSet.t;
}

let push_artifact = fun builder artifact ->
  let key = artifact.Riot_store.Artifact.input_hash in
  if HashSet.insert builder.artifact_hashes ~value:key then
    Vector.push builder.artifacts ~value:artifact

let package_result_of_builder = fun builder : package_result -> {
  package_name = builder.package_name;
  scope = builder.scope;
  status = builder.status;
  artifacts =
    builder.artifacts
    |> Vector.to_array
    |> Array.to_list;
}

let from_build_results = fun results -> {
  packages = (
    let builders = HashMap.create () in
    let order = Vector.with_capacity ~size:(List.length results) in
    List.for_each
      results
      ~fn:(fun result ->
        let incoming = package_result_of_build_result result in
        let key = Riot_model.Package_name.to_string incoming.package_name in
        match HashMap.get builders ~key with
        | None ->
            let builder = {
              package_name = incoming.package_name;
              scope = incoming.scope;
              status = incoming.status;
              artifacts = Vector.with_capacity ~size:1;
              artifact_hashes = HashSet.with_capacity ~size:1;
            }
            in
            List.for_each incoming.artifacts ~fn:(push_artifact builder);
            Vector.push order ~value:key;
            ignore (HashMap.insert builders ~key ~value:builder)
        | Some builder ->
            List.for_each incoming.artifacts ~fn:(push_artifact builder);
            if
              should_prefer_status
                ~current_scope:builder.scope
                ~current_status:builder.status
                ~incoming_scope:incoming.scope
                ~incoming_status:incoming.status
            then (
              builder.scope <- incoming.scope;
              builder.status <- incoming.status
            ));
    order
    |> Vector.to_array
    |> Array.to_list
    |> List.filter_map ~fn:(fun key ->
      HashMap.get builders ~key
      |> Option.map ~fn:package_result_of_builder)
  );
}

let packages = fun t -> t.packages

let find_package = fun t name ->
  List.find
    t.packages
    ~fn:(fun pkg -> Riot_model.Package_name.equal pkg.package_name name)

let package_name = fun (t: package_result) -> t.package_name

let package_status = fun (t: package_result) -> t.status

let package_artifact = fun (t: package_result) -> artifact_of_status t.status

let package_artifacts = fun (t: package_result) -> t.artifacts

let rec find_export_in_artifacts = fun artifacts export_name ->
  match artifacts with
  | [] -> None
  | (artifact: Riot_store.Artifact.t) :: rest -> (
      match List.find
        artifact.exports
        ~fn:(fun (entry: Riot_store.Manifest.export_entry) -> String.equal entry.name export_name) with
      | Some entry -> Some entry
      | None -> find_export_in_artifacts rest export_name
    )

let find_export = fun (t: package_result) export_name ->
  find_export_in_artifacts t.artifacts export_name

let failure_reason_of_package_error = fun __tmp1 ->
  match __tmp1 with
  | Package_builder.PlanningFailed planning_error -> PackagePlanningFailed planning_error
  | Package_builder.ExecutionFailed { message } -> PackageExecutionFailed { message }
  | Package_builder.ActionExecutionFailed { message } -> PackageActionFailed { message }
  | Package_builder.ActionOutputsNotCreated { missing } ->
      PackageActionOutputsNotCreated { missing }
  | Package_builder.ActionDependenciesFailed { failed } ->
      PackageActionDependenciesFailed { failed }

let failure_reason_message = fun __tmp1 ->
  match __tmp1 with
  | PackagePlanningFailed planning_error ->
      "Planning failed: " ^ Riot_planner.Planning_error.to_string planning_error
  | PackageExecutionFailed { message } -> "Execution failed: " ^ message
  | PackageActionFailed { message } -> "Action failed: " ^ message
  | PackageActionOutputsNotCreated { missing } ->
      "Outputs not created: " ^ String.concat ", " (List.map missing ~fn:Path.to_string)
  | PackageActionDependenciesFailed { failed } ->
      "Dependencies failed: " ^ Int.to_string (List.length failed) ^ " actions"
  | PackageSkipped { reason } -> "Skipped: " ^ reason
  | UnknownFailure -> "Build failed"

let failure_reason_to_json = fun __tmp1 ->
  match __tmp1 with
  | PackagePlanningFailed planning_error ->
      Data.Json.Object [
        ("type", Data.Json.String "planning_failed");
        ("error", Riot_planner.Planning_error.to_json planning_error);
      ]
  | PackageExecutionFailed { message } ->
      Data.Json.Object [
        ("type", Data.Json.String "execution_failed");
        ("message", Data.Json.String message);
      ]
  | PackageActionFailed { message } ->
      Data.Json.Object [
        ("type", Data.Json.String "action_failed");
        ("message", Data.Json.String message);
      ]
  | PackageActionOutputsNotCreated { missing } ->
      Data.Json.Object [
        ("type", Data.Json.String "outputs_not_created");
        (
          "missing",
          Data.Json.Array (List.map missing ~fn:(fun path -> Data.Json.String (Path.to_string path)))
        );
      ]
  | PackageActionDependenciesFailed { failed } ->
      Data.Json.Object [
        ("type", Data.Json.String "dependencies_failed");
        ("failed_count", Data.Json.Int (List.length failed));
      ]
  | PackageSkipped { reason } ->
      Data.Json.Object [
        ("type", Data.Json.String "skipped");
        ("reason", Data.Json.String reason);
      ]
  | UnknownFailure -> Data.Json.Object [ ("type", Data.Json.String "unknown"); ]

let failure_of_build_result = fun (result: Package_builder.build_result) ->
  let reason =
    match result.status with
    | Package_builder.Failed error -> failure_reason_of_package_error error
    | Package_builder.Skipped { reason } -> PackageSkipped { reason }
    | Package_builder.Built _
    | Package_builder.Cached _ -> UnknownFailure
  in
  let message = failure_reason_message reason in
  {
    package_name = result.package.name;
    unit_key = result.unit_key;
    reason;
    message;
    ocamlc_warnings = result.ocamlc_warnings;
    duration_ms = Int.from_float (Time.Duration.to_secs_float result.duration *. 1_000.0);
  }

let failures_of_build_results = fun results -> List.map results ~fn:failure_of_build_result

let failure_to_json = fun (failure: failure) ->
  Data.Json.Object [
    ("package_name", Data.Json.String (Riot_model.Package_name.to_string failure.package_name));
    ("unit_key", Data.Json.String (Riot_planner.Build_unit.key_to_string failure.unit_key));
    ("reason", failure_reason_to_json failure.reason);
    ("message", Data.Json.String failure.message);
    ("ocamlc_warnings", Data.Json.Array (List.map failure.ocamlc_warnings ~fn:Data.Json.string));
    ("duration_ms", Data.Json.Int failure.duration_ms);
  ]

let failure_message = fun (failure: failure) ->
  Riot_model.Package_name.to_string failure.package_name ^ ": " ^ failure.message
