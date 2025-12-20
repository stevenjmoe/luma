open Luma__math

type depth
type t

val make : pos:Vec2.t -> size:Vec2.t -> float -> float -> t
(** [make ~pos ~size depth_min depth_max ]*)

val full : int -> int -> t
(** [full w h]*)

val position : t -> Vec2.t
val size : t -> Vec2.t
val x : t -> float
val y : t -> float
val w : t -> float
val h : t -> float
val depth : t -> depth
val clamp_to_window : float -> float -> t -> t
val to_rect : t -> int * int * int * int
val center : t -> Vec2.t
