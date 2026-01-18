(*TODO: *)
(* A hidden record type so we can pass it into Asset.Make,
   which produces a nominal type A.t. We then alias t = A.t
   without leaking raw’s internal fields in the public API. *)
type raw = {
  layout : Texture_atlas_layout.t;
  mutable index : int;
}

module A = Luma__asset.Asset.Make (struct
  type inner = raw
end)

type t = A.t

let from_layout ?(index = 0) layout = { layout; index }
let get_frame t = Texture_atlas_layout.get_frame t.layout t.index
let set_index atlas index = atlas.index <- index
let index atlas = atlas.index
