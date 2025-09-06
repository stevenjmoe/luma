open Luma__math
open Luma__ecs
open Luma__app

module type S = sig
  module Viewport : module type of Viewport

  type camera
  type t

  module C : Component.S with type t = t

  val default : unit -> t

  val make :
    ?viewport:Viewport.t option ->
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
  val camera : t -> camera
  val get_screen_to_world_2d : Vec2.t -> t -> Vec2.t
  val get_world_to_screen_2d : Vec2.t -> t -> Vec2.t
  val set_target : t -> Vec2.t -> unit
  val set_offset : t -> Vec2.t -> unit
  val set_zoom : t -> float -> unit
  val set_rotation : t -> float -> unit
  val plugin : bool -> App.t -> App.t
end

module Make : functor (D : Luma__driver.Driver.S) -> S with type camera = D.camera
