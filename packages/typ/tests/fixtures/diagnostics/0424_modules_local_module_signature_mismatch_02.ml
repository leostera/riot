let _ =
  let module Local_beta : sig
    val x : bool
  end = struct
    let x = 1
  end in
  Local_beta.x
