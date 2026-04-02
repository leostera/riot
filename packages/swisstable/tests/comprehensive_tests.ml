open Std

(* Helper to create pre-allocated keys to avoid hash instability *)

let make_keys = fun n ->
  Collections.Array.init n (fun i -> "key" ^ string_of_int i)

let tests = [
  Test.case "zero capacity"
    (fun _ctx ->
      let map = Swisstable.create () in
      Test.assert_true (Swisstable.is_empty map);
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      let map2 = Swisstable.with_capacity 0 in
      Test.assert_true (Swisstable.is_empty map2);
      Ok ());
  Test.case "create capacity zero then insert"
    (fun _ctx ->
      let map = Swisstable.with_capacity 0 in
      let _ = Swisstable.insert map "key1" 1 in
      Test.assert_true (Swisstable.contains_key map "key1");
      Test.assert_false (Swisstable.contains_key map "key0");
      Ok ());
  Test.case "insert basic"
    (fun _ctx ->
      let map = Swisstable.create () in
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:None ~actual:(Swisstable.insert map 1 2);
      Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:None ~actual:(Swisstable.insert map 2 4);
      Test.assert_equal ~expected:2 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.get map 1);
      Test.assert_equal ~expected:(Some 4) ~actual:(Swisstable.get map 2);
      Ok ());
  Test.case "insert and overwrite"
    (fun _ctx ->
      let map = Swisstable.create () in
      Test.assert_equal ~expected:None ~actual:(Swisstable.insert map 1 2);
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.insert map 1 3);
      Test.assert_equal ~expected:(Some 3) ~actual:(Swisstable.insert map 1 4);
      Test.assert_equal ~expected:(Some 4) ~actual:(Swisstable.get map 1);
      Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "is_empty"
    (fun _ctx ->
      let map = Swisstable.create () in
      Test.assert_true (Swisstable.is_empty map);
      let _ = Swisstable.insert map 1 2 in
      Test.assert_false (Swisstable.is_empty map);
      let _ = Swisstable.remove map 1 in
      Test.assert_true (Swisstable.is_empty map);
      Ok ());
  Test.case "empty remove"
    (fun _ctx ->
      let map = Swisstable.create () in
      Test.assert_equal ~expected:None ~actual:(Swisstable.remove map 1);
      Ok ());
  Test.case "lots of insertions (250)"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 250 in
      for i = 0 to 249 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      Test.assert_equal ~expected:250 ~actual:(Swisstable.len map);
      (* Verify all present *)
      let rec verify i =
        if i > 249 then
          Ok ()
        else
          let key = Collections.Array.get keys i in
          match Swisstable.get map key with
          | Some v when v = i -> verify (i + 1)
          | _ -> Error ("key" ^ string_of_int i ^ " missing or wrong value")
      in
      verify 0);
  Test.case "find and contains"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 2 in
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.get map 1);
      Test.assert_true (Swisstable.contains_key map 1);
      Test.assert_false (Swisstable.contains_key map 2);
      Ok ());
  Test.case "remove"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 2 in
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.remove map 1);
      Test.assert_equal ~expected:None ~actual:(Swisstable.remove map 1);
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map 1);
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "iterate"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 10 in
      let _ = Swisstable.insert map 2 20 in
      let _ = Swisstable.insert map 3 30 in
      let sum = Sync.Cell.create 0 in
      Swisstable.iter
        (fun _k v ->
          Sync.Cell.set sum (Sync.Cell.get sum + v))
        map;
      Test.assert_equal ~expected:60 ~actual:(Sync.Cell.get sum);
      Ok ());
  Test.case "keys"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 10 in
      let _ = Swisstable.insert map 2 20 in
      let _ = Swisstable.insert map 3 30 in
      let keys = Swisstable.keys map in
      Test.assert_equal ~expected:3 ~actual:(List.length keys);
      Test.assert_true (List.mem 1 keys);
      Test.assert_true (List.mem 2 keys);
      Test.assert_true (List.mem 3 keys);
      Ok ());
  Test.case "values"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 10 in
      let _ = Swisstable.insert map 2 20 in
      let _ = Swisstable.insert map 3 30 in
      let values = Swisstable.values map in
      Test.assert_equal ~expected:3 ~actual:(List.length values);
      Test.assert_true (List.mem 10 values);
      Test.assert_true (List.mem 20 values);
      Test.assert_true (List.mem 30 values);
      Ok ());
  Test.case "from_iter (of_list)"
    (fun _ctx ->
      let map = Swisstable.of_list [ (1, 10); (2, 20); (3, 30) ] in
      Test.assert_equal ~expected:3 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 10) ~actual:(Swisstable.get map 1);
      Test.assert_equal ~expected:(Some 20) ~actual:(Swisstable.get map 2);
      Test.assert_equal ~expected:(Some 30) ~actual:(Swisstable.get map 3);
      Ok ());
  Test.case "expand (multiple resizes)"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 100 in
      (* Insert 100 elements causing multiple resizes *)
      for i = 0 to 99 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      Test.assert_equal ~expected:100 ~actual:(Swisstable.len map);
      (* Verify all present after multiple resizes *)
      for i = 0 to 99 do
        let key = Collections.Array.get keys i in
        Test.assert_equal ~expected:(Some i) ~actual:(Swisstable.get map key)
      done;
      Ok ());
  Test.case "conflict remove"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 10 in
      for i = 0 to 9 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      (* Remove some entries *)
      for i = 0 to 4 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.remove map key in
        ()
      done;
      Test.assert_equal ~expected:5 ~actual:(Swisstable.len map);
      (* Verify remaining *)
      for i = 5 to 9 do
        let key = Collections.Array.get keys i in
        Test.assert_equal ~expected:(Some i) ~actual:(Swisstable.get map key)
      done;
      Ok ());
  Test.case "insert after remove (tombstone reuse)"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "a" 1 in
      let _ = Swisstable.insert map "b" 2 in
      let _ = Swisstable.insert map "c" 3 in
      (* Remove b *)
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.remove map "b");
      Test.assert_equal ~expected:2 ~actual:(Swisstable.len map);
      (* Insert new element - may reuse tombstone slot *)
      let _ = Swisstable.insert map "d" 4 in
      Test.assert_equal ~expected:3 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 1) ~actual:(Swisstable.get map "a");
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map "b");
      Test.assert_equal ~expected:(Some 3) ~actual:(Swisstable.get map "c");
      Test.assert_equal ~expected:(Some 4) ~actual:(Swisstable.get map "d");
      Ok ());
  Test.case "vacant entry"
    (fun _ctx ->
      let map = Swisstable.create () in
      match Swisstable.entry map "key" with
      | Swisstable.Vacant ->
          let v = Swisstable.or_insert map "key" 42 in
          Test.assert_equal ~expected:42 ~actual:v;
          Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
          Ok ()
      | _ -> Error "Expected vacant entry");
  Test.case "occupied entry"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "key" 10 in
      match Swisstable.entry map "key" with
      | Swisstable.Occupied v ->
          Test.assert_equal ~expected:10 ~actual:v;
          Ok ()
      | _ -> Error "Expected occupied entry");
  Test.case "and_modify"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "counter" 0 in
      Swisstable.and_modify map "counter" (fun x -> x + 1);
      Test.assert_equal ~expected:(Some 1) ~actual:(Swisstable.get map "counter");
      Swisstable.and_modify map "counter" (fun x -> x + 1);
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.get map "counter");
      (* and_modify on non-existent key does nothing *)
      Swisstable.and_modify map "missing" (fun x -> x + 1);
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map "missing");
      Ok ());
  Test.case "clear"
    (fun _ctx ->
      let map = Swisstable.create () in
      for i = 0 to 99 do
        let _ = Swisstable.insert map i (i * 10) in
        ()
      done;
      Test.assert_equal ~expected:100 ~actual:(Swisstable.len map);
      Swisstable.clear map;
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      Test.assert_true (Swisstable.is_empty map);
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map 50);
      Ok ());
  Test.case "fold"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 10 in
      let _ = Swisstable.insert map 2 20 in
      let _ = Swisstable.insert map 3 30 in
      let sum =
        Swisstable.fold (fun _k v acc -> acc + v) map 0
      in
      Test.assert_equal ~expected:60 ~actual:sum;
      let count =
        Swisstable.fold (fun _k _v acc -> acc + 1) map 0
      in
      Test.assert_equal ~expected:3 ~actual:count;
      Ok ());
  Test.case "to_list"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 10 in
      let _ = Swisstable.insert map 2 20 in
      let list = Swisstable.to_list map in
      Test.assert_equal ~expected:2 ~actual:(List.length list);
      Test.assert_true (List.mem (1, 10) list);
      Test.assert_true (List.mem (2, 20) list);
      Ok ());
  Test.case "or_insert with existing key"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "key" 10 in
      let v = Swisstable.or_insert map "key" 20 in
      Test.assert_equal ~expected:10 ~actual:v;
      Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 10) ~actual:(Swisstable.get map "key");
      Ok ());
  Test.case "string keys"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "alice" 30 in
      let _ = Swisstable.insert map "bob" 25 in
      let _ = Swisstable.insert map "charlie" 35 in
      Test.assert_equal ~expected:(Some 30) ~actual:(Swisstable.get map "alice");
      Test.assert_equal ~expected:(Some 25) ~actual:(Swisstable.get map "bob");
      Test.assert_equal ~expected:(Some 35) ~actual:(Swisstable.get map "charlie");
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map "dave");
      Ok ());
  Test.case "int keys, string values"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 "one" in
      let _ = Swisstable.insert map 2 "two" in
      let _ = Swisstable.insert map 3 "three" in
      Test.assert_equal ~expected:(Some "one") ~actual:(Swisstable.get map 1);
      Test.assert_equal ~expected:(Some "two") ~actual:(Swisstable.get map 2);
      Test.assert_equal ~expected:(Some "three") ~actual:(Swisstable.get map 3);
      Ok ());
  Test.case "tuple values"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "point1" (10, 20) in
      let _ = Swisstable.insert map "point2" (30, 40) in
      Test.assert_equal ~expected:(Some (10, 20)) ~actual:(Swisstable.get map "point1");
      Test.assert_equal ~expected:(Some (30, 40)) ~actual:(Swisstable.get map "point2");
      Ok ());
  Test.case "resize under load"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 500 in
      (* Insert 500 elements *)
      for i = 0 to 499 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      Test.assert_equal ~expected:500 ~actual:(Swisstable.len map);
      (* Remove every other element *)
      for i = 0 to 249 do
        let key = Collections.Array.get keys (i * 2) in
        let _ = Swisstable.remove map key in
        ()
      done;
      Test.assert_equal ~expected:250 ~actual:(Swisstable.len map);
      (* Insert new elements *)
      let new_keys = make_keys 250 in
      for i = 0 to 249 do
        let key = "new" ^ Collections.Array.get new_keys i in
        let _ = Swisstable.insert map key (i + 1_000) in
        ()
      done;
      Test.assert_equal ~expected:500 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "alternating insert/remove"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 100 in
      for round = 0 to 9 do
        for i = 0 to 9 do
          let idx = round * 10 + i in
          let key = Collections.Array.get keys idx in
          let _ = Swisstable.insert map key idx in
          ()
        done;
        (* Remove first 5 from this round *)
        for i = 0 to 4 do
          let idx = round * 10 + i in
          let key = Collections.Array.get keys idx in
          let _ = Swisstable.remove map key in
          ()
        done
      done;
      (* Should have 50 elements remaining (5 per round * 10 rounds) *)
      Test.assert_equal ~expected:50 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "capacity growth"
    (fun _ctx ->
      let map = Swisstable.with_capacity 4 in
      let keys = make_keys 20 in
      (* Insert enough to trigger multiple resizes *)
      for i = 0 to 19 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      Test.assert_equal ~expected:20 ~actual:(Swisstable.len map);
      (* All elements should still be accessible *)
      for i = 0 to 19 do
        let key = Collections.Array.get keys i in
        Test.assert_equal ~expected:(Some i) ~actual:(Swisstable.get map key)
      done;
      Ok ());
  Test.case "empty iteration"
    (fun _ctx ->
      let map = Swisstable.create () in
      let count = Sync.Cell.create 0 in
      Swisstable.iter
        (fun _k _v ->
          Sync.Cell.set count (Sync.Cell.get count + 1))
        map;
      Test.assert_equal ~expected:0 ~actual:(Sync.Cell.get count);
      Ok ());
]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"swisstable:comprehensive" ~tests ~args:Env.args)
    ~args:Env.args
    ()
