let _ =
  let module Local_kappa : sig
    val x : bool
  end = struct
    let x = 9
  end in
  Local_kappa.x
