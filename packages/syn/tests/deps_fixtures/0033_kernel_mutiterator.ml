open Global0

module type Intf = sig
  type state
  type item
  val next: state -> item option

  val size: state -> int

  val clone: state -> state
end

type ('item, 'state) iter = (module Intf with type item = 'item and type state = 'state)

type 'item t =
  Iter: (('item, 'state) iter * 'state) -> 'item t

(*************************************************************************************************)

let make: type item state. (item, state) iter -> state -> item t = fun mod_ state ->
  Iter (mod_, state)

let empty = fun (type a) () ->
  let module Empty = struct
    type item = a

    type state = unit

    let next = fun () -> None

    let size = fun () -> 0

    let clone = fun () -> ()
  end in
  make (module Empty) ()

let singleton = fun (type a) (value: a) ->
  let module Singleton = struct
    type item = a

    type state = {
      mutable value: a option;
    }

    let next = fun state ->
      match state.value with
      | Some x ->
          state.value <- None;
          Some x
      | None -> None

    let size = fun state ->
      if Option.is_some state.value then
        1
      else
        0

    let clone = fun state -> { value = state.value }
  end in
  make (module Singleton) { value = Some value }

let next: type item. item t -> item option = fun (Iter ((module Iter), state)) -> Iter.next state

let size: type item. item t -> int = fun (Iter ((module Iter), state)) -> Iter.size state

let clone: type item. item t -> item t = fun (Iter ((module Iter), state)) ->
  let new_state = Iter.clone state in
  Iter ((module Iter), new_state)

(*************************************************************************************************)

let rec collect = fun t acc ->
  match next t with
  | Some item -> collect t (item :: acc)
  | None -> Stdlib.List.rev acc

let to_list = fun t -> collect t []

(*************************************************************************************************)

(* Transformation *)

(*************************************************************************************************)

let iter_next = next

let map: type a b. a t -> fn:(a -> b) -> b t = fun iter ~fn ->
  let module MapIter = struct
    type state = a t

    type item = b

    let next = fun state ->
      Option.map fn (iter_next state)

    let size = fun state -> size state

    let clone = fun state -> clone state
  end in
  make (module MapIter) iter

let filter: type a. a t -> fn:(a -> bool) -> a t = fun iter ~fn ->
  let module FilterIter = struct
    type state = a t

    type item = a

    let rec next = fun state ->
      match iter_next state with
      | Some x when fn x -> Some x
      | Some _ -> next state
      | None -> None

    let size = fun state -> size state

    let clone = fun state -> clone state
  end in
  make (module FilterIter) iter

let filter_map: type a b. a t -> fn:(a -> b option) -> b t = fun iter ~fn ->
  let module FilterMapIter = struct
    type state = a t

    type item = b

    let rec next = fun state ->
      match iter_next state with
      | Some x -> (
          match fn x with
          | Some y -> Some y
          | None -> next state
        )
      | None -> None

    let size = fun state -> size state

    let clone = fun state -> clone state
  end in
  make (module FilterMapIter) iter

let flat_map: type a b. a t -> fn:(a -> b t) -> b t = fun iter ~fn ->
  let module FlatMapIter = struct
    type state = {
      outer: a t;
      mutable current: b t option;
    }

    type item = b

    let rec next = fun state ->
      match state.current with
      | Some inner -> (
          match iter_next inner with
          | Some x -> Some x
          | None ->
              (* Current inner iterator exhausted, get next *)
              state.current <- None;
              next state
        )
      | None -> (
          (* Need a new inner iterator *)
          match iter_next state.outer with
          | Some x ->
              state.current <- Some (fn x);
              next state
          | None -> None
        )

    let size = fun state -> size state.outer

    (* Approximate *)

    let clone = fun state -> { outer = clone state.outer; current = Option.map clone state.current }
  end in
  make (module FlatMapIter) { outer = iter; current = None }

(*************************************************************************************************)

(* Reduction *)

(*************************************************************************************************)

let fold: type a acc. a t -> init:acc -> fn:(a -> acc -> acc) -> acc = fun iter ~init ~fn ->
  let rec loop acc =
    match next iter with
    | Some x -> loop (fn x acc)
    | None -> acc
  in
  loop init

let reduce: type a. a t -> fn:(a -> a -> a) -> a option = fun iter ~fn ->
  match next iter with
  | Some first -> Some (fold iter ~init:first ~fn)
  | None -> None

let count: type a. a t -> int = fun iter -> fold iter ~init:0 ~fn:(fun _ acc -> acc + 1)

(*************************************************************************************************)

(* Search *)

(*************************************************************************************************)

let find: type a. a t -> fn:(a -> bool) -> a option = fun iter ~fn ->
  let rec loop () =
    match next iter with
    | Some x when fn x -> Some x
    | Some _ -> loop ()
    | None -> None
  in
  loop ()

let any: type a. a t -> fn:(a -> bool) -> bool = fun iter ~fn ->
  let rec loop () =
    match next iter with
    | Some x when fn x -> true
    | Some _ -> loop ()
    | None -> false
  in
  loop ()

let all: type a. a t -> fn:(a -> bool) -> bool = fun iter ~fn ->
  let rec loop () =
    match next iter with
    | Some x when fn x -> loop ()
    | Some _ -> false
    | None -> true
  in
  loop ()

(*************************************************************************************************)

(* Combinators *)

(*************************************************************************************************)

let take: type a. a t -> int -> a t = fun iter n ->
  let module TakeIter = struct
    type state = {
      iter: a t;
      mutable remaining: int;
    }

    type item = a

    let next = fun state ->
      if state.remaining <= 0 then
        None
      else (
        state.remaining <- state.remaining - 1;
        iter_next state.iter
      )

    let size = fun state -> min state.remaining (size state.iter)

    let clone = fun state -> { iter = clone state.iter; remaining = state.remaining }
  end in
  make (module TakeIter) { iter; remaining = n }

let drop: type a. a t -> int -> a t = fun iter n ->
  for _ = 1 to n do
    ignore (next iter)
  done;
  iter

let enumerate: type a. a t -> (int * a) t = fun iter ->
  let module EnumIter = struct
    type state = {
      iter: a t;
      mutable index: int;
    }

    type item = int * a

    let next = fun state ->
      match iter_next state.iter with
      | Some x ->
          let idx = state.index in
          state.index <- state.index + 1;
          Some (idx, x)
      | None -> None

    let size = fun state -> size state.iter

    let clone = fun state -> { iter = clone state.iter; index = state.index }
  end in
  make (module EnumIter) { iter; index = 0 }

let zip: type a b. a t -> b t -> (a * b) t = fun iter1 iter2 ->
  let module ZipIter = struct
    type state = {
      iter1: a t;
      iter2: b t;
    }

    type item = a * b

    let next = fun state ->
      match (iter_next state.iter1, iter_next state.iter2) with
      | Some x, Some y -> Some (x, y)
      | _ -> None

    let size = fun state -> min (size state.iter1) (size state.iter2)

    let clone = fun state -> { iter1 = clone state.iter1; iter2 = clone state.iter2 }
  end in
  make (module ZipIter) { iter1; iter2 }

let chain: type a. a t -> a t -> a t = fun iter1 iter2 ->
  let module ChainIter = struct
    type state = {
      first: a t;
      second: a t;
      mutable in_first: bool;
    }

    type item = a

    let rec next = fun state ->
      if state.in_first then
        match iter_next state.first with
        | Some x -> Some x
        | None ->
            state.in_first <- false;
            next state
      else
        iter_next state.second

    let size = fun state ->
      if state.in_first then
        size state.first + size state.second
      else
        size state.second

    let clone = fun state ->
      { first = clone state.first; second = clone state.second; in_first = state.in_first }
  end in
  make (module ChainIter) { first = iter1; second = iter2; in_first = true }

(*************************************************************************************************)

(* Side Effects *)

(*************************************************************************************************)

let for_each: type a. a t -> fn:(a -> unit) -> unit = fun iter ~fn ->
  let rec loop () =
    match next iter with
    | Some x ->
        fn x;
        loop ()
    | None -> ()
  in
  loop ()
