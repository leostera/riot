let make_key = fun ?(kind = 0) ?(mods = 1) code ->
  code + kind + mods

let omitted = make_key 3

let reordered = make_key ~mods:4 3

let explicit = make_key ~kind:5 ~mods:6 7
