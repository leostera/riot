module U : sig
  type 'a box
  val x : (int, bool) box
end = struct
  type 'a box = Box of 'a
  let x = Box 7
end
