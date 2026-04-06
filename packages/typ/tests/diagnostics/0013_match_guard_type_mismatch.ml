let input = Some 1

let classify =
  match input with
  | Some n when n -> 1
  | Some _ -> 0
  | None -> 0
