module Resource = Luma__resource.Resource
module Asset = Luma__asset.Asset

module Time = struct
  type t = { mutable dt : float; mutable elapsed : float }

  let dt t = t.dt

  module R = Resource.Make (struct
    type inner = t
  end)
end
