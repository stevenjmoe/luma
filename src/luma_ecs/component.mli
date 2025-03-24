module type S = Luma__tracked_module.Tracked_module.S

module Make (B : sig
  type inner
end) : S with type t = B.inner

include Luma__tracked_module.Tracked_module.Packed
