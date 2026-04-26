let _ =
  let module Local_alpha : sig
    val x : bool
  end = struct
    let x = 0
  end in
  Local_alpha.x
