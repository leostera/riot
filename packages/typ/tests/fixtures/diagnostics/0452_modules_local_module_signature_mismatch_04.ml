let _ =
  let module Local_delta : sig
    val x : bool
  end = struct
    let x = 3
  end in
  Local_delta.x
