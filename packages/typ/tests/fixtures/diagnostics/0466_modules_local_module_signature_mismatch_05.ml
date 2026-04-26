let _ =
  let module Local_epsilon : sig
    val x : bool
  end = struct
    let x = 4
  end in
  Local_epsilon.x
