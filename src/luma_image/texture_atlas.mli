open Luma__math
module A : Luma__asset.Asset.S

type t = A.t

val from_layout : Texture_atlas_layout.t -> t
val get_frame : t -> int -> Rect.t option
val frame_size : t -> Vec2.t option
