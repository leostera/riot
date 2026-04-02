open Stdlib

(** Actors - Minimal OCaml build system

    A self-contained build system that can bootstrap itself and build OCaml
    packages with proper module namespacing and nested library support. *)
(* ===== Main ===== *)

let build_package = fun ~build_results ?(needs_stdlib_and_unix = false) pkg_name pkg_path ->
  Printf.printf "\nBuilding package: %s\n" pkg_name;
  Printf.printf "  Path: %s\n" pkg_path;
  let pkg = Package.read pkg_path in
  (* Override stdlib/unix dependencies if specified *)
  let pkg =
    if needs_stdlib_and_unix then
      { pkg with uses_stdlib = true; uses_unix = true; uses_dynlink = true }
    else
      pkg
  in
  (* Create dependency graph for the package, passing build_results for cross-package deps *)
  let dep_graph = Dep_graph.scan ~root:pkg_path ~package:pkg ~build_results in
  File_scanner.print_tree dep_graph.file_tree;
  (* For now, just print what we would build 
  Printf.printf "\n\nDependency Graph: %s\n" pkg_name;
  Dep_graph.iter
    (fun node ->
      let open Dep_graph in
      let filename =
        match node.value.file with
        | Concrete path -> Filename.basename path
        | Generated { path; _ } -> Filename.basename path ^ " (generated)"
      in
      Printf.printf "  - %s\n%!" filename)
    dep_graph;
*)
  (* Dump graph as dot for debugging *)
  let dot_dir = Printf.sprintf "_build/bootstrap/out/%s" pkg_name in
  Io.mkdir_p dot_dir;
  let dot_file = Printf.sprintf "%s/graph.dot" dot_dir in
  let dot_content = Dep_graph.to_dot dep_graph in
  Io.write_file dot_file dot_content;
  Printf.printf "Dumped graph to %s\n" dot_file;
  let build_plan = Action.from_dep_graph dep_graph in
  (* Printf.printf "\n\nBuild Plan: %s\n" pkg_name;
  Action.print_build_plan build_plan; *)
  Action.execute_build_plan ~build_results build_plan;
  Action.promote_outputs build_plan;
  (* Register this package's outputs for other packages to use *)
  (* IMPORTANT: Only store this package's OWN flags, not accumulated ones! *)
  Dep_graph.Build_results.register
    build_results
    pkg
    build_plan.package_name
    ~outputs:build_plan.outputs
    ~cc_flags:(Package.cc_flags pkg)
    ~ld_flags:(Package.ld_flags pkg)

let () =
  Printf.printf "=== Actors Build System ===\n";
  (* Create build results tracker for cross-package dependencies *)
  let build_results = Dep_graph.Build_results.create () in
  (* Build packages in runtime dependency order for bootstrapping riot-cli. *)
  build_package ~build_results ~needs_stdlib_and_unix:true "kernel" "packages/kernel";
  build_package ~build_results "actors" "packages/actors";
  build_package ~build_results "std" "packages/std";
  build_package ~build_results "colors" "packages/colors";
  build_package ~build_results "tty" "packages/tty";
  build_package ~build_results "ceibo" "packages/ceibo";
  build_package ~build_results "http" "packages/http";
  build_package ~build_results "blink" "packages/blink";
  build_package ~build_results "jsonrpc" "packages/jsonrpc";
  build_package ~build_results "lsp" "packages/lsp";
  build_package ~build_results "pkgs-ml" "packages/pkgs-ml";
  build_package ~build_results "syn" "packages/syn";
  build_package ~build_results "macro" "packages/macro";
  build_package ~build_results "fixme" "packages/fixme";
  build_package ~build_results "krasny" "packages/krasny";
  build_package ~build_results "riot-model" "packages/riot-model";
  build_package ~build_results "riot-store" "packages/riot-store";
  build_package ~build_results "riot-toolchain" "packages/riot-toolchain";
  build_package ~build_results "riot-planner" "packages/riot-planner";
  build_package ~build_results "riot-executor" "packages/riot-executor";
  build_package ~build_results "riot-init" "packages/riot-init";
  build_package ~build_results "riot-fix" "packages/riot-fix";
  build_package ~build_results "riot-fmt" "packages/riot-fmt";
  build_package ~build_results "riot-lsp" "packages/riot-lsp";
  build_package ~build_results "pubgrub" "packages/pubgrub";
  build_package ~build_results "riot-deps" "packages/riot-deps";
  build_package ~build_results "riot-build" "packages/riot-build";
  build_package ~build_results "riot-publish" "packages/riot-publish";
  build_package ~build_results "riot-cli" "packages/riot-cli";
  Printf.printf "\n=== Build complete! ===\n"
