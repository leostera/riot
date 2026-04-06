module Helpers = struct
  let id x = x
  let wrap value = Some value
end

module Util = Helpers

let answer = Util.wrap (Util.id 1)
