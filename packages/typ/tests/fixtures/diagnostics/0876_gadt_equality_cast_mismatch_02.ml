type (_, _) eq_beta =
  | Refl_beta : ('a, 'a) eq_beta

let cast_bad_beta : type a b. (a, b) eq_beta -> a -> b =
  fun eq x ->
    match eq with
    | Refl_beta -> true
