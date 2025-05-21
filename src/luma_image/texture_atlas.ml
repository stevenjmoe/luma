(* A hidden record type so we can pass it into Asset.Make,
   which produces a nominal type A.t. We then alias t = A.t
   without leaking raw’s internal fields in the public API. *)
type raw = {
  layout : Texture_atlas_layout.t;
  _index : int;
}

module A = Luma__asset.Asset.Make (struct
  type inner = raw
end)

type t = A.t

let from_layout layout = { layout; _index = 0 }
let get_frame t index = Texture_atlas_layout.get_frame t.layout index
let frame_size t = Texture_atlas_layout.frame_size t.layout
