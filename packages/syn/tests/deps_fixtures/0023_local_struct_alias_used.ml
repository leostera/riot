let _ =
  let module X = struct
    module Y = Foo
  end in
  X.Y.value
