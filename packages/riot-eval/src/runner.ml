open Std
open Std.Result.Syntax

let normalize_source = fun source ->
  let source = Std.String.trim source in
  if Std.String.equal source "" || Std.String.ends_with ~suffix:";;" source then
    source
  else
    source ^ ";;"

let eval_phrase_source = fun session ~packages ~source ->
  let* parsed = Prelude.parse source in
  let packages = Context.unique_package_names (packages @ parsed.packages) in
  let* () = Packages.load session ~packages in
  let body = normalize_source parsed.body in
  if Std.String.equal body "" then
    Ok ()
  else
    Phrase.eval session body

let run_string = fun (request: Context.request) ~packages ~source ->
  let* parsed = Prelude.parse source in
  let packages = Context.unique_package_names (packages @ parsed.packages) in
  let* request = Context.with_packages request ~packages in
  Synthetic.run ~on_event:request.on_event request.workspace ~packages ~source:parsed.body ~args:[]

let run_file = fun (request: Context.request) ~path ~packages ~args ->
  let* source =
    Std.Fs.read path
    |> Std.Result.map_err ~fn:(Context.fs_error_message "failed to read script file")
  in
  let* parsed = Script.parse source in
  let dependencies =
    Script.dependencies_of_package_names packages
    @ parsed.dependencies
    |> Script.unique_dependencies
  in
  let* request = Context.with_dependencies request ~dependencies in
  Synthetic.run_script
    ~on_event:request.on_event
    request.workspace
    ~inherit_workspace:false
    ~dependencies
    ~source:parsed.body
    ~args

module Session = struct
  type t = Context.session

  let create = fun ?args (request: Context.request) ->
    Context.create_session ?args request

  let build_workspace = Packages.refresh_workspace

  let load_packages = Packages.load

  let eval_phrase = fun t ~source -> eval_phrase_source t ~packages:[] ~source
end
