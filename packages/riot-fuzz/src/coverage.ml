open Std

type novelty = { hit_edges: int; new_edges: int }

type t = {
  virgin: bytes;
  mutable total_edges: int;
}

let create = fun () ->
  let virgin = IO.Bytes.create ~size:(Afl.map_size ()) in
  IO.Bytes.fill virgin ~offset:0 ~len:(IO.Bytes.length virgin) ~char:'\255';
  { virgin; total_edges = 0 }

let record = fun t snapshot ->
  let len = Int.min (IO.Bytes.length t.virgin) (IO.Bytes.length snapshot) in
  let hit_edges = ref 0 in
  let new_edges = ref 0 in
  for idx = 0 to len - 1 do
    let hit = IO.Bytes.get_unchecked snapshot ~at:idx in
    if not (Char.equal hit '\000') then (
      hit_edges := !hit_edges + 1;
      if not (Char.equal (IO.Bytes.get_unchecked t.virgin ~at:idx) '\000') then (
        new_edges := !new_edges + 1;
        IO.Bytes.set_unchecked t.virgin ~at:idx ~char:'\000'
      )
    )
  done;
  t.total_edges <- t.total_edges + !new_edges;
  { hit_edges = !hit_edges; new_edges = !new_edges }

let total_edges = fun t -> t.total_edges
