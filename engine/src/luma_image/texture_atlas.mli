open Luma__math
module A : Luma__asset.Asset.S

type t = A.t

val from_layout : ?index:int -> Texture_atlas_layout.t -> t
val get_frame : t -> Rect.t option
val set_index : t -> int -> unit
val index : t -> int
