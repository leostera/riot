let _ =
  let module Local_eta : sig
    val x : bool
  end = struct
    let x = 6
  end in
  Local_eta.x
