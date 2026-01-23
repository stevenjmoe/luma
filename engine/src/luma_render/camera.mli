open Luma__math
open Luma__ecs
open Luma__app
module Viewport : module type of Viewport

type t

module C : Component.S with type t = t

val default : unit -> t

val make :
  ?viewport:Viewport.t ->
  ?order:int ->
  offset:Vec2.t ->
  target:Vec2.t ->
  rotation:float ->
  zoom:float ->
  unit ->
  t

val target : t -> Vec2.t
val offset : t -> Vec2.t
val zoom : t -> float
val rotation : t -> float
val viewport : t -> Viewport.t option
val order : t -> int
val active : t -> bool
val set_target : t -> Vec2.t -> unit
val set_offset : t -> Vec2.t -> unit
val set_zoom : t -> float -> unit
val set_rotation : t -> float -> unit
val set_order : t -> int -> unit
val set_active : t -> bool -> unit
val set_viewport : t -> Viewport.t option -> unit
val plugin : bool -> App.t -> App.t
