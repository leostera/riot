module F (X : S) = struct
  module Y = X

  let _ = Y.value
end
