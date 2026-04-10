open Std
open Std.Collections

let bench_vector_push = fun () ->
  let v = Vector.create () in
  Vector.push v 42

let bench_vector_100_pushes = fun () ->
  let v = Vector.create () in
  for i = 1 to 100 do
    Vector.push v i
  done

let bench_hashmap_insert = fun () ->
  let map = HashMap.create () in
  let _ = HashMap.insert map "key" "value" in
  ()

let bench_hashmap_100_inserts = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done

let bench_list_append = fun () ->
  let _result = List.append [ 1; 2; 3 ] [ 4; 5; 6 ] in
  ()

(* Comparison: Array vs Vector for sequential inserts *)

let bench_array_set_100 = fun () ->
  let arr = Array.make 100 0 in
  for i = 0 to 99 do
    Array.set arr i i
  done

let bench_vector_push_100 = fun () ->
  let v = Vector.create () in
  for i = 0 to 99 do
    Vector.push v i
  done

let benchmarks =
  Bench.[
    case "vector single push" bench_vector_push;
    case "vector 100 pushes" bench_vector_100_pushes;
    case "hashmap single insert" bench_hashmap_insert;
    case "hashmap 100 inserts" bench_hashmap_100_inserts;
    with_config ~config:{ iterations = 1_000; warmup = 50 } "list append" bench_list_append;
    skip "skipped benchmark" (fun () -> ());
    compare
      "insert 100 sequential elements"
      [ make_case "Array.set" bench_array_set_100; make_case "Vector.push" bench_vector_push_100; ];
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"Example Benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
