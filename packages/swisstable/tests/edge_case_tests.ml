open Std

let make_keys = fun n ->
  Collections.Array.init n (fun i -> "key" ^ string_of_int i)

let tests = [
  Test.case "hash collisions"
    (fun _ctx ->
      let map = Swisstable.create () in
      (* Insert 20 elements - some may have hash collisions *)
      for i = 0 to 19 do
        let _ = Swisstable.insert map i (i * 10) in
        ()
      done;
      Test.assert_equal ~expected:20 ~actual:(Swisstable.len map);
      (* Verify all are accessible *)
      for i = 0 to 19 do
        Test.assert_equal ~expected:(Some (i * 10)) ~actual:(Swisstable.get map i)
      done;
      Ok ());
  Test.case "remove/reinsert cycle"
    (fun _ctx ->
      let map = Swisstable.create () in
      for round = 0 to 9 do
        let _ = Swisstable.insert map "key" round in
        Test.assert_equal ~expected:(Some round) ~actual:(Swisstable.get map "key");
        Test.assert_equal ~expected:(Some round) ~actual:(Swisstable.remove map "key");
        Test.assert_equal ~expected:None ~actual:(Swisstable.get map "key")
      done;
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "fill/empty/refill"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 50 in
      (* Fill *)
      for i = 0 to 49 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      Test.assert_equal ~expected:50 ~actual:(Swisstable.len map);
      (* Empty *)
      for i = 0 to 49 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.remove map key in
        ()
      done;
      Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
      (* Refill *)
      for i = 0 to 49 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key (i + 100) in
        ()
      done;
      Test.assert_equal ~expected:50 ~actual:(Swisstable.len map);
      (* Verify new values *)
      for i = 0 to 49 do
        let key = Collections.Array.get keys i in
        Test.assert_equal ~expected:(Some (i + 100)) ~actual:(Swisstable.get map key)
      done;
      Ok ());
  Test.case "overwrite existing"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 20 in
      (* Insert initial values *)
      for i = 0 to 19 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      (* Overwrite all with new values *)
      for i = 0 to 19 do
        let key = Collections.Array.get keys i in
        let old = Swisstable.insert map key (i * 2) in
        Test.assert_equal ~expected:(Some i) ~actual:old
      done;
      Test.assert_equal ~expected:20 ~actual:(Swisstable.len map);
      (* Verify new values *)
      for i = 0 to 19 do
        let key = Collections.Array.get keys i in
        Test.assert_equal ~expected:(Some (i * 2)) ~actual:(Swisstable.get map key)
      done;
      Ok ());
  Test.case "sparse removal"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 100 in
      (* Insert 100 elements *)
      for i = 0 to 99 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      (* Remove every 3rd element *)
      for i = 0 to 33 do
        let key = Collections.Array.get keys (i * 3) in
        let _ = Swisstable.remove map key in
        ()
      done;
      (* Should have 100 - 34 = 66 elements *)
      Test.assert_equal ~expected:66 ~actual:(Swisstable.len map);
      (* Verify removed ones are gone and others remain *)
      for i = 0 to 99 do
        let key = Collections.Array.get keys i in
        if i mod 3 = 0 && i / 3 <= 33 then
          Test.assert_equal ~expected:None ~actual:(Swisstable.get map key)
        else
          Test.assert_equal ~expected:(Some i) ~actual:(Swisstable.get map key)
      done;
      Ok ());
  Test.case "grow then shrink"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 200 in
      (* Grow to 200 elements *)
      for i = 0 to 199 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      Test.assert_equal ~expected:200 ~actual:(Swisstable.len map);
      (* Remove 150 elements *)
      for i = 0 to 149 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.remove map key in
        ()
      done;
      Test.assert_equal ~expected:50 ~actual:(Swisstable.len map);
      (* Verify remaining 50 *)
      for i = 150 to 199 do
        let key = Collections.Array.get keys i in
        Test.assert_equal ~expected:(Some i) ~actual:(Swisstable.get map key)
      done;
      Ok ());
  Test.case "interleaved operations"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 30 in
      (* Insert 10 *)
      for i = 0 to 9 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      (* Remove 5 *)
      for i = 0 to 4 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.remove map key in
        ()
      done;
      (* Insert 10 more *)
      for i = 10 to 19 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      (* Remove 5 more *)
      for i = 5 to 9 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.remove map key in
        ()
      done;
      (* Insert final 10 *)
      for i = 20 to 29 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map key i in
        ()
      done;
      (* Should have 20 elements (10-19 and 20-29) *)
      Test.assert_equal ~expected:20 ~actual:(Swisstable.len map);
      Ok ());
  Test.case "clear and reuse"
    (fun _ctx ->
      let map = Swisstable.create () in
      let keys = make_keys 50 in
      (* Fill, clear, fill again - 3 times *)
      for round = 0 to 2 do
        for i = 0 to 49 do
          let key = Collections.Array.get keys i in
          let _ = Swisstable.insert map key (i + round * 100) in
          ()
        done;
        Test.assert_equal ~expected:50 ~actual:(Swisstable.len map);
        Swisstable.clear map;
        Test.assert_equal ~expected:0 ~actual:(Swisstable.len map);
        Test.assert_true (Swisstable.is_empty map)
      done;
      Ok ());
  Test.case "entry API patterns"
    (fun _ctx ->
      let map = Swisstable.create () in
      (* or_insert on vacant *)
      let v1 = Swisstable.or_insert map "counter" 0 in
      Test.assert_equal ~expected:0 ~actual:v1;
      (* or_insert on occupied *)
      let v2 = Swisstable.or_insert map "counter" 99 in
      Test.assert_equal ~expected:0 ~actual:v2;
      (* and_modify existing *)
      Swisstable.and_modify map "counter" (fun x -> x + 1);
      Test.assert_equal ~expected:(Some 1) ~actual:(Swisstable.get map "counter");
      (* and_modify with chain *)
      for _ = 0 to 9 do
        Swisstable.and_modify map "counter" (fun x -> x + 1)
      done;
      Test.assert_equal ~expected:(Some 11) ~actual:(Swisstable.get map "counter");
      Ok ());
  Test.case "mixed key types"
    (fun _ctx ->
      let map1 = Swisstable.create () in
      let map2 = Swisstable.create () in
      let map3 = Swisstable.create () in
      (* Int keys *)
      for i = 0 to 49 do
        let _ = Swisstable.insert map1 i i in
        ()
      done;
      (* String keys *)
      let keys = make_keys 50 in
      for i = 0 to 49 do
        let key = Collections.Array.get keys i in
        let _ = Swisstable.insert map2 key i in
        ()
      done;
      (* Tuple keys *)
      for i = 0 to 49 do
        let _ = Swisstable.insert map3 (i, i * 2) i in
        ()
      done;
      Test.assert_equal ~expected:50 ~actual:(Swisstable.len map1);
      Test.assert_equal ~expected:50 ~actual:(Swisstable.len map2);
      Test.assert_equal ~expected:50 ~actual:(Swisstable.len map3);
      Ok ());
  Test.case "value patterns"
    (fun _ctx ->
      (* Option values *)
      let map1 = Swisstable.create () in
      let _ = Swisstable.insert map1 "opt_none" ((None: int option)) in
      let _ = Swisstable.insert map1 "opt_some" (Some 42) in
      Test.assert_equal ~expected:(Some None) ~actual:(Swisstable.get map1 "opt_none");
      Test.assert_equal ~expected:(Some (Some 42)) ~actual:(Swisstable.get map1 "opt_some");
      (* Tuple values *)
      let map2 = Swisstable.create () in
      let _ = Swisstable.insert map2 "tuple" (1, 2, 3) in
      Test.assert_equal ~expected:(Some (1, 2, 3)) ~actual:(Swisstable.get map2 "tuple");
      (* Bool values *)
      let map3 = Swisstable.create () in
      let _ = Swisstable.insert map3 "true_val" true in
      let _ = Swisstable.insert map3 "false_val" false in
      Test.assert_equal ~expected:(Some true) ~actual:(Swisstable.get map3 "true_val");
      Test.assert_equal ~expected:(Some false) ~actual:(Swisstable.get map3 "false_val");
      Ok ());
  Test.case "fold patterns"
    (fun _ctx ->
      let map = Swisstable.create () in
      for i = 1 to 10 do
        let _ = Swisstable.insert map i (i * i) in
        ()
      done;
      (* Sum of keys *)
      let sum_keys =
        Swisstable.fold (fun k _v acc -> acc + k) map 0
      in
      Test.assert_equal ~expected:55 ~actual:sum_keys;
      (* Sum of values *)
      let sum_values =
        Swisstable.fold (fun _k v acc -> acc + v) map 0
      in
      Test.assert_equal ~expected:385 ~actual:sum_values;
      (* Count *)
      let count =
        Swisstable.fold (fun _k _v acc -> acc + 1) map 0
      in
      Test.assert_equal ~expected:10 ~actual:count;
      Ok ());
]

let () =
  Miniriot.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"swisstable:edge_case" ~tests ~args:Env.args)
    ~args:Env.args
    ()
