type (_, _) eq_theta =
  | Refl_theta : ('a, 'a) eq_theta

let cast_bad_theta : type a b. (a, b) eq_theta -> a -> b =
  fun eq x ->
    match eq with
    | Refl_theta -> true
