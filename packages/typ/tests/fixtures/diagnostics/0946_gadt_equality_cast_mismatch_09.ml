type (_, _) eq_iota =
  | Refl_iota : ('a, 'a) eq_iota

let cast_bad_iota : type a b. (a, b) eq_iota -> a -> b =
  fun eq x ->
    match eq with
    | Refl_iota -> true
