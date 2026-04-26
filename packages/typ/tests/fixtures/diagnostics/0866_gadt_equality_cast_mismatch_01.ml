type (_, _) eq_alpha =
  | Refl_alpha : ('a, 'a) eq_alpha

let cast_bad_alpha : type a b. (a, b) eq_alpha -> a -> b =
  fun eq x ->
    match eq with
    | Refl_alpha -> true
