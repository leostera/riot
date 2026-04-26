type (_, _) eq_gamma =
  | Refl_gamma : ('a, 'a) eq_gamma

let cast_bad_gamma : type a b. (a, b) eq_gamma -> a -> b =
  fun eq x ->
    match eq with
    | Refl_gamma -> true
