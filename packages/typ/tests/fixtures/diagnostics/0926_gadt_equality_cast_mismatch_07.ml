type (_, _) eq_eta =
  | Refl_eta : ('a, 'a) eq_eta

let cast_bad_eta : type a b. (a, b) eq_eta -> a -> b =
  fun eq x ->
    match eq with
    | Refl_eta -> true
