module C = Luma__tracked_module.Tracked_module.Make (Luma__id.Id.Component)

module type S = Luma__tracked_module.Tracked_module.S

module Make (B : sig
  type inner
end) =
struct
  module C = C (B)
  include C
end

include Luma__tracked_module.Tracked_module.Packed
