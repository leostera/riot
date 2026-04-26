type (_, _) eq_delta =
  | Refl_delta : ('a, 'a) eq_delta

let cast_bad_delta : type a b. (a, b) eq_delta -> a -> b =
  fun eq x ->
    match eq with
    | Refl_delta -> true
