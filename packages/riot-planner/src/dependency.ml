open Std
open Std.Collections
open Std.Iter
open Riot_model

type t = {
  package: Package.t;
  artifact_dir: Path.t;
  depset: t list;
  input_hash: Crypto.hash;
  output_hash: Crypto.hash;
}

let library_cmxa: t -> Path.t = fun dep ->
  Path.(dep.artifact_dir / Package_namespace.library_cmxa dep.package)

let transitive_closure = fun deps ->
  let seen = HashSet.create () in
  let ordered = vec [] in
  let rec collect dep =
    if HashSet.contains seen ~value:dep.package.name then
      ()
    else
      (
        let _ = HashSet.insert seen ~value:dep.package.name in
        List.for_each dep.depset ~fn:collect;
        Vector.push ordered ~value:dep
      )
  in
  List.for_each deps ~fn:collect;
  Vector.iter ordered
  |> Iterator.to_list
