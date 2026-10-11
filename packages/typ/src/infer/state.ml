module InferenceEnv = Env

open Std
open Std.Collections

type type_param_scope = (string, Ast.Type.t) HashMap.t

type t = {
  mutable next_var: Ast.TypeVar.t;
  mutable variable_levels: (Ast.TypeVar.t, int) HashMap.t;
  mutable level: int;
  mutable env: InferenceEnv.t;
  mutable module_path: string list;
  mutable type_params: type_param_scope option;
  diagnostics: Diagnostics.t;
}

let create () = {
  next_var = Ast.TypeVar.first;
  variable_levels = HashMap.with_capacity ~size:32;
  level = 0;
  diagnostics = Diagnostics.create ();
  env = InferenceEnv.create ();
  module_path = [];
  type_params = None;
}

let env state = state.env

let set_env state env =
  state.env <- env

let add_value state ~name ~scheme =
  state.env <- InferenceEnv.add_value state.env ~name ~scheme

let module_member state name lookup =
  match List.rev (Model.Surface_path.to_segments name) with
  | []
  | [ _ ] -> None
  | member_name :: reversed_module_path ->
      let single_segment name =
        Model.Surface_path.from_parts [ name ]
        |> Result.expect ~msg:"module path segment must be non-empty"
      in
      let module_path =
        reversed_module_path
        |> List.rev
        |> List.map ~fn:single_segment
      in
      let member_name = single_segment member_name in
      (match InferenceEnv.get_module_path state.env ~path:module_path with
      | Some summary -> lookup summary ~name:member_name
      | None -> None)

let get_value state ~name =
  match InferenceEnv.get_value state.env ~name with
  | Some _ as value -> value
  | None -> module_member state name InferenceEnv.module_get_value

let has_value state ~name = Option.is_some (get_value state ~name)

let add_constructor state ~name ~description =
  state.env <- InferenceEnv.add_constructor state.env ~name ~description

let get_constructor state ~name =
  match InferenceEnv.get_constructor state.env ~name with
  | Some _ as constructor -> constructor
  | None -> module_member state name InferenceEnv.module_get_constructor

let has_constructor state ~name = Option.is_some (get_constructor state ~name)

let add_record_field state ~name ~owner ~field =
  let info: InferenceEnv.record_field_info = { owner; field } in
  state.env <- InferenceEnv.add_record_field state.env ~name ~info

let get_record_field state ~name =
  match InferenceEnv.get_record_field state.env ~name with
  | Some _ as field -> field
  | None -> module_member state name InferenceEnv.module_get_record_field

let has_record_field state ~name = Option.is_some (get_record_field state ~name)

let add_type state ~name ~declaration =
  state.env <- InferenceEnv.add_type state.env ~name ~declaration

let get_type state ~name =
  match InferenceEnv.get_type state.env ~name with
  | Some _ as declaration -> declaration
  | None -> module_member state name InferenceEnv.module_get_type

let has_type state ~name = Option.is_some (get_type state ~name)

let get_module state ~name = InferenceEnv.get_module state.env ~name

let get_module_path state ~path =
  let path =
    Model.Surface_path.to_segments path
    |> List.map ~fn:(fun segment ->
      Model.Surface_path.from_parts [ segment ]
      |> Result.expect ~msg:"module path segment must be non-empty")
  in
  InferenceEnv.get_module_path state.env ~path

let open_module state ~path =
  match InferenceEnv.open_module state.env ~path with
  | None -> false
  | Some env ->
      state.env <- env;
      true

let add_module_alias state ~name ~target =
  match InferenceEnv.add_module_alias state.env ~name ~target with
  | None -> false
  | Some env ->
      state.env <- env;
      true

let qualify_current_module state name =
  let parts = state.module_path @ Model.Surface_path.to_segments name in
  Model.Surface_path.from_parts parts
  |> Result.expect ~msg:"qualified type name must not be empty"

let diagnostics state = state.diagnostics

let add_diagnostic state diagnostic = Diagnostics.add state.diagnostics diagnostic

(** Instantiates a fresh type variable at the current HM level. *)
let fresh_var state =
  let id = state.next_var in
  state.next_var <- Ast.TypeVar.next id;
  let _ = HashMap.insert state.variable_levels ~key:id ~value:state.level in
  Ast.Type.Var { id; link = None }

let current_level state = state.level

let variable_level state (variable: Ast.Type.variable) =
  match HashMap.get state.variable_levels ~key:variable.id with
  | Some level -> level
  | None -> 0

let lower_variable_level state variable ~to_level =
  let current = variable_level state variable in
  if Int.(current > to_level) then
    let _ = HashMap.insert state.variable_levels ~key:variable.id ~value:to_level in
    ()

let adjust_levels state ~to_level type_ =
  let rec visit type_ =
    match type_ with
    | Ast.Type.Var ({ link = None; _ } as variable) ->
        lower_variable_level state variable ~to_level
    | Ast.Type.Var { link = Some linked; _ } -> visit linked
    | Ast.Type.Generic _ -> ()
    | Ast.Type.Tuple parts -> List.for_each parts ~fn:visit
    | Ast.Type.Arrow arrow ->
        visit arrow.parameter;
        visit arrow.result
    | Ast.Type.Apply application -> List.for_each application.arguments ~fn:visit
  in
  visit type_

let with_type_params state scope fn =
  let previous = state.type_params in
  state.type_params <- Some scope;
  let result = fn state in
  state.type_params <- previous;
  result

let get_type_param state ~name =
  match state.type_params with
  | None -> None
  | Some scope -> HashMap.get scope ~key:name

let push_scope state =
  state.level <- state.level + 1;
  state.env <- InferenceEnv.push_scope state.env

let pop_scope state =
  state.env <- InferenceEnv.pop_scope state.env;
  state.level <- Int.max 0 (state.level - 1)

let push_module state ~name =
  state.env <- InferenceEnv.push_module state.env ~name;
  state.module_path <- state.module_path @ Model.Surface_path.to_segments name

let pop_module state =
  state.env <- InferenceEnv.pop_module state.env;
  state.module_path <-
    match List.rev state.module_path with
    | [] -> []
    | _ :: rest -> List.rev rest
