let choose = fun left ~right ->
  left + right

let bad = choose ~other:2 1
