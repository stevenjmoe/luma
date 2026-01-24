open Luma__ecs
open Luma__math
module Viewport = Viewport

type t = {
  mutable target : Vec2.t;
  mutable rotation : float;
  mutable zoom : float;
  mutable viewport : Viewport.t option;
  mutable order : int;
  mutable active : bool;
}

module C = Component.Make (struct
  type inner = t

  let name = "Camera"
end)

let default () =
  {
    target = Vec2.create 0. 0.;
    rotation = 0.;
    zoom = 1.;
    viewport = None;
    order = 0;
    active = true;
  }

let make ?viewport ?(order = 0) ~target ~rotation ~zoom () =
  { viewport; active = true; order; target; rotation; zoom }

let target c = c.target
let zoom c = c.zoom
let rotation c = c.rotation
let viewport c = c.viewport
let order c = c.order
let active c = c.active
let set_target c target = c.target <- target
let set_zoom c zoom = c.zoom <- zoom
let set_rotation c rotation = c.rotation <- rotation
let set_order c order = c.order <- order
let set_active c active = c.active <- active
let set_viewport c viewport = c.viewport <- viewport
