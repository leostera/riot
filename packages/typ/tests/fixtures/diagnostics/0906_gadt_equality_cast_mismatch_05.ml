type (_, _) eq_epsilon =
  | Refl_epsilon : ('a, 'a) eq_epsilon

let cast_bad_epsilon : type a b. (a, b) eq_epsilon -> a -> b =
  fun eq x ->
    match eq with
    | Refl_epsilon -> true
