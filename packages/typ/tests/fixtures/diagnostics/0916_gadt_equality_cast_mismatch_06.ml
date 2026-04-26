type (_, _) eq_zeta =
  | Refl_zeta : ('a, 'a) eq_zeta

let cast_bad_zeta : type a b. (a, b) eq_zeta -> a -> b =
  fun eq x ->
    match eq with
    | Refl_zeta -> true
