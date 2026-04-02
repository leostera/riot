open Std
open Std.Collections
open Std.Iter
open Riot_model

type t = {
  package: Package.t;
  artifact_dir: Path.t;
  depset: t list;
  hash: Crypto.hash;
}

let library_cmxa: t -> Path.t = fun dep ->
  let cmxa = Module_name.(of_string dep.package.name |> cmxa) in
  Path.(dep.artifact_dir / cmxa)

let transitive_closure = fun deps ->
  let seen = HashSet.create () in
  let ordered = vec [] in
  let rec collect dep =
    if HashSet.contains seen dep.package.name then
      ()
    else
      (
        let _ = HashSet.insert seen dep.package.name in
        List.iter collect dep.depset;
        Vector.push ordered dep
      )
  in
  List.iter collect deps;
  Vector.into_iter ordered |> Iterator.to_list

let runtime_closure = fun deps ->
  transitive_closure deps |> List.filter
    (fun dep -> not (Package.is_macro_package dep.package))

let macro_closure = fun deps ->
  transitive_closure deps |> List.filter
    (fun dep -> Package.is_macro_package dep.package)
