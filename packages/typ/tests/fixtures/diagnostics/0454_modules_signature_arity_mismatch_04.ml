module Q : sig
  type 'a box
  val x : (int, bool) box
end = struct
  type 'a box = Box of 'a
  let x = Box 3
end
