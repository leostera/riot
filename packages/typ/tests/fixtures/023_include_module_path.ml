module Helpers = struct
  let id x = x
  let wrap value = Some value
end

include Helpers

let answer = wrap (id 1)
