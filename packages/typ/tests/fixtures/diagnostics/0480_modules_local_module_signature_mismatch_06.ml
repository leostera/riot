let _ =
  let module Local_zeta : sig
    val x : bool
  end = struct
    let x = 5
  end in
  Local_zeta.x
