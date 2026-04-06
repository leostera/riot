type t = {
  keep_generations: int option;
  max_size_bytes: int option;
}

let default = {
  keep_generations = None;
  max_size_bytes = None;
}
