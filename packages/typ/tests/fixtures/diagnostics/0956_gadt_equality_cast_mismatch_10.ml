type (_, _) eq_kappa =
  | Refl_kappa : ('a, 'a) eq_kappa

let cast_bad_kappa : type a b. (a, b) eq_kappa -> a -> b =
  fun eq x ->
    match eq with
    | Refl_kappa -> true
