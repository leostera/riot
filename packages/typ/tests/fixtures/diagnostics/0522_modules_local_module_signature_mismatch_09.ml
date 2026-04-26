let _ =
  let module Local_iota : sig
    val x : bool
  end = struct
    let x = 8
  end in
  Local_iota.x
