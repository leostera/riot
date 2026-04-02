open Std

let tests = [
  Test.case "create empty map"
    (fun _ctx ->
      let map = Swisstable.create () in
      Test.assert_true (Swisstable.is_empty map);
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "insert and get"
    (fun _ctx ->
      let map = Swisstable.create () in
      let prev = Swisstable.insert map "alice" 100 in
      Test.assert_equal ~expected:None ~actual:prev;
      Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 100) ~actual:(Swisstable.get map "alice");
      Ok ());
  Test.case "overwrite existing key"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "key" 1 in
      let prev = Swisstable.insert map "key" 2 in
      Test.assert_equal ~expected:(Some 1) ~actual:prev;
      Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.get map "key");
      Ok ());
  Test.case "multiple inserts"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "a" 1 in
      let _ = Swisstable.insert map "b" 2 in
      let _ = Swisstable.insert map "c" 3 in
      Test.assert_equal ~expected:3 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 1) ~actual:(Swisstable.get map "a");
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.get map "b");
      Test.assert_equal ~expected:(Some 3) ~actual:(Swisstable.get map "c");
      Ok ());
  Test.case "remove"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "key" 42 in
      let removed = Swisstable.remove map "key" in
      Test.assert_equal ~expected:(Some 42) ~actual:removed;
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map "key");
      Ok ());
  Test.case "remove non-existent key"
    (fun _ctx ->
      let map = Swisstable.create () in
      let removed = Swisstable.remove map "missing" in
      Test.assert_equal ~expected:None ~actual:removed;
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "contains_key"
    (fun _ctx ->
      let map = Swisstable.create () in
      Test.assert_false (Swisstable.contains_key map "key");
      let _ = Swisstable.insert map "key" 1 in
      Test.assert_true (Swisstable.contains_key map "key");
      let _ = Swisstable.remove map "key" in
      Test.assert_false (Swisstable.contains_key map "key");
      Ok ());
  Test.case "clear"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "a" 1 in
      let _ = Swisstable.insert map "b" 2 in
      let _ = Swisstable.insert map "c" 3 in
      Test.assert_equal ~expected:3 ~actual:(Swisstable.len map);
      Swisstable.clear map;
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      Test.assert_true (Swisstable.is_empty map);
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map "a");
      Ok ());
  Test.case "of_list"
    (fun _ctx ->
      let map = Swisstable.of_list [ ("a", 1); ("b", 2); ("c", 3) ] in
      Test.assert_equal ~expected:3 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 1) ~actual:(Swisstable.get map "a");
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.get map "b");
      Test.assert_equal ~expected:(Some 3) ~actual:(Swisstable.get map "c");
      Ok ());
  Test.case "of_list with duplicates"
    (fun _ctx ->
      let map = Swisstable.of_list [ ("a", 1); ("b", 2); ("a", 3) ] in
      Test.assert_equal ~expected:2 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some 3) ~actual:(Swisstable.get map "a");
      Ok ());
  Test.case "keys"
    (fun _ctx ->
      let map = Swisstable.of_list [ ("a", 1); ("b", 2); ("c", 3) ] in
      let keys = Swisstable.keys map in
      Test.assert_equal ~expected:3 ~actual:(List.length keys);
      Test.assert_true (List.mem "a" keys);
      Test.assert_true (List.mem "b" keys);
      Test.assert_true (List.mem "c" keys);
      Ok ());
  Test.case "values"
    (fun _ctx ->
      let map = Swisstable.of_list [ ("a", 1); ("b", 2); ("c", 3) ] in
      let values = Swisstable.values map in
      Test.assert_equal ~expected:3 ~actual:(List.length values);
      Test.assert_true (List.mem 1 values);
      Test.assert_true (List.mem 2 values);
      Test.assert_true (List.mem 3 values);
      Ok ());
  Test.case "iter"
    (fun _ctx ->
      let map = Swisstable.of_list [ ("a", 1); ("b", 2); ("c", 3) ] in
      let sum = Sync.Cell.create 0 in
      Swisstable.iter
        (fun _ v ->
          Sync.Cell.set sum (Sync.Cell.get sum + v))
        map;
      Test.assert_equal ~expected:6 ~actual:(Sync.Cell.get sum);
      Ok ());
  Test.case "fold"
    (fun _ctx ->
      let map = Swisstable.of_list [ ("a", 1); ("b", 2); ("c", 3) ] in
      let sum =
        Swisstable.fold (fun _ v acc -> acc + v) map 0
      in
      Test.assert_equal ~expected:6 ~actual:sum;
      Ok ());
  Test.case "to_list"
    (fun _ctx ->
      let map = Swisstable.of_list [ ("a", 1); ("b", 2) ] in
      let list = Swisstable.to_list map in
      Test.assert_equal ~expected:2 ~actual:(List.length list);
      Test.assert_true (List.mem ("a", 1) list);
      Test.assert_true (List.mem ("b", 2) list);
      Ok ());
  Test.case "entry - Occupied"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "key" 42 in
      match Swisstable.entry map "key" with
      | Swisstable.Occupied 42 -> Ok ()
      | _ -> Error "Expected Occupied entry with value 42");
  Test.case "entry - Vacant"
    (fun _ctx ->
      let map = Swisstable.create () in
      match Swisstable.entry map "missing" with
      | Swisstable.Vacant -> Ok ()
      | _ -> Error "Expected Vacant entry");
  Test.case "or_insert"
    (fun _ctx ->
      let map = Swisstable.create () in
      let v1 = Swisstable.or_insert map "key" 10 in
      Test.assert_equal ~expected:10 ~actual:v1;
      Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
      let v2 = Swisstable.or_insert map "key" 20 in
      Test.assert_equal ~expected:10 ~actual:v2;
      Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "and_modify"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "count" 5 in
      Swisstable.and_modify map "count" (fun x -> x + 1);
      Test.assert_equal ~expected:(Some 6) ~actual:(Swisstable.get map "count");
      Swisstable.and_modify map "missing" (fun x -> x + 1);
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map "missing");
      Ok ());
  Test.case "with_capacity"
    (fun _ctx ->
      let map = Swisstable.with_capacity 100 in
      Test.assert_true (Swisstable.is_empty map);
      let _ = Swisstable.insert map "key" 1 in
      Test.assert_equal ~expected:1 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "resize with many elements"
    (fun _ctx ->
      let map = Swisstable.create () in
      (* Pre-create keys to ensure consistent hashing *)
      let keys =
        Collections.Array.init 100 (fun i -> "key" ^ string_of_int i)
      in
      for i = 0 to 99 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      Test.assert_equal ~expected:100 ~actual:(Swisstable.len map);
      (* Verify all values are present *)
      let rec verify i =
        if i > 99 then
          Ok ()
        else
          let key = Collections.Array.get keys i in
          match Swisstable.get map key with
          | Some v when v = i -> verify (i + 1)
          | _ -> Error ("Failed at key " ^ key)
      in
      verify 0);
  Test.case "insert after remove"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "a" 1 in
      let _ = Swisstable.insert map "b" 2 in
      let _ = Swisstable.remove map "a" in
      let _ = Swisstable.insert map "c" 3 in
      Test.assert_equal ~expected:2 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:None ~actual:(Swisstable.get map "a");
      Test.assert_equal ~expected:(Some 2) ~actual:(Swisstable.get map "b");
      Test.assert_equal ~expected:(Some 3) ~actual:(Swisstable.get map "c");
      Ok ());
  Test.case "integer keys"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map 1 "one" in
      let _ = Swisstable.insert map 2 "two" in
      let _ = Swisstable.insert map 3 "three" in
      Test.assert_equal ~expected:3 ~actual:(Swisstable.len map);
      Test.assert_equal ~expected:(Some "one") ~actual:(Swisstable.get map 1);
      Test.assert_equal ~expected:(Some "two") ~actual:(Swisstable.get map 2);
      Test.assert_equal ~expected:(Some "three") ~actual:(Swisstable.get map 3);
      Ok ());
  Test.case "complex values"
    (fun _ctx ->
      let map = Swisstable.create () in
      let _ = Swisstable.insert map "point1" (10, 20) in
      let _ = Swisstable.insert map "point2" (30, 40) in
      Test.assert_equal ~expected:(Some (10, 20)) ~actual:(Swisstable.get map "point1");
      Test.assert_equal ~expected:(Some (30, 40)) ~actual:(Swisstable.get map "point2");
      Ok ());
]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"swisstable:basic" ~tests ~args:Env.args)
    ~args:Env.args
    ()
