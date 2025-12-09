open Luma__math

module type S = sig
  type t = {
    gravity : Vec2.t;
    bounds : Bounded2d.Aabb2d.t;
    max_step_dt : float;
    debug : bool;
  }

  module R : Luma__resource.Resource.S with type t = t

  val default : unit -> t
end

type t = {
  gravity : Vec2.t;
  bounds : Bounded2d.Aabb2d.t;
  max_step_dt : float;
  debug : bool;
}

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "physics_config"
end)

let default () =
  let gravity = Vec2.create 0. (-9.81) in
  let max_step_dt = 0.016 in
  let min = Vec2.create (-1024.) (-1024.) in
  let max = Vec2.create 1024. 1024. in
  let bounds = Bounded2d.Aabb2d.of_min_max min max in

  { gravity; max_step_dt; bounds; debug = true }

let create ?(debug = false) ~gravity ?(max_step_dt = 0.016) ?(bounds = None) () =
  let bounds =
    match bounds with
    | None ->
        let min = Vec2.create (-1024.) (-1024.) in
        let max = Vec2.create 1024. 1024. in
        Bounded2d.Aabb2d.of_min_max min max
    | Some b -> b
  in
  { gravity; max_step_dt; bounds; debug }
