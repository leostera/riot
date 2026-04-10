open Std
open Std.Collections

(* Benchmark: Insert operations *)

let bench_insert_100 = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done

let bench_insert_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done

let bench_insert_100k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done

let bench_insert_1m = fun () ->
  let map = HashMap.create () in
  for i = 1 to 1_000_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done

(* Benchmark: Lookup operations - single lookup from populated map *)

let bench_get_from_100 = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  (* Benchmark: single lookup *)
  let _ = HashMap.get map "key_50" in
  ()

let bench_get_from_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.get map "key_5000" in
  ()

let bench_get_from_100k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.get map "key_50000" in
  ()

let bench_get_from_1m = fun () ->
  let map = HashMap.create () in
  for i = 1 to 1_000_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.get map "key_500000" in
  ()

let bench_get_missing = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  (* Benchmark: lookup non-existing key *)
  let _ = HashMap.get map "missing_key" in
  ()

(* Benchmark: Remove operations - single remove from populated map *)

let bench_remove_from_100 = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.remove map "key_50" in
  ()

let bench_remove_from_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.remove map "key_5000" in
  ()

let bench_remove_from_100k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.remove map "key_50000" in
  ()

(* Benchmark: Iteration *)

let bench_iter_100 = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  HashMap.iter (fun _k _v -> ()) map

let bench_iter_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  HashMap.iter (fun _k _v -> ()) map

let bench_iter_100k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  HashMap.iter (fun _k _v -> ()) map

(* Benchmark: Contains key check *)

let bench_contains_key_from_100 = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.contains_key map "key_50" in
  ()

let bench_contains_key_from_10k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 10_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.contains_key map "key_5000" in
  ()

let bench_contains_key_from_100k = fun () ->
  let map = HashMap.create () in
  for i = 1 to 100_000 do
    let key = "key_" ^ string_of_int i in
    let _ = HashMap.insert map key i in
    ()
  done;
  let _ = HashMap.contains_key map "key_50000" in
  ()

let benchmarks =
  Bench.[
    case "insert: 100 items" bench_insert_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "insert: 10k items" bench_insert_10k;
    with_config ~config:{ iterations = 5; warmup = 1 } "insert: 100k items" bench_insert_100k;
    with_config ~config:{ iterations = 3; warmup = 1 } "insert: 1M items" bench_insert_1m;
    case "get: from 100 items" bench_get_from_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "get: from 10k items" bench_get_from_10k;
    with_config ~config:{ iterations = 20; warmup = 2 } "get: from 100k items" bench_get_from_100k;
    with_config ~config:{ iterations = 10; warmup = 2 } "get: from 1M items" bench_get_from_1m;
    with_config ~config:{ iterations = 20; warmup = 2 } "get: missing from 100k" bench_get_missing;
    case "remove: from 100 items" bench_remove_from_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "remove: from 10k items" bench_remove_from_10k;
    with_config ~config:{ iterations = 20; warmup = 2 } "remove: from 100k items" bench_remove_from_100k;
    case "iter: 100 items" bench_iter_100;
    with_config ~config:{ iterations = 10; warmup = 2 } "iter: 10k items" bench_iter_10k;
    with_config ~config:{ iterations = 5; warmup = 1 } "iter: 100k items" bench_iter_100k;
    case "contains_key: from 100 items" bench_contains_key_from_100;
    with_config ~config:{ iterations = 50; warmup = 5 } "contains_key: from 10k items" bench_contains_key_from_10k;
    with_config ~config:{ iterations = 20; warmup = 2 } "contains_key: from 100k items" bench_contains_key_from_100k;
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Bench.Cli.main ~name:"HashMap Benchmarks" ~benchmarks ~args)
    ~args:Env.args
    ()
