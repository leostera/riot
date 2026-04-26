let _ =
  let module Local_theta : sig
    val x : bool
  end = struct
    let x = 7
  end in
  Local_theta.x
