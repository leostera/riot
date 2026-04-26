let _ =
  let module Local_gamma : sig
    val x : bool
  end = struct
    let x = 2
  end in
  Local_gamma.x
