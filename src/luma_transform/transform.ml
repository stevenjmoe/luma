module Component = Luma__ecs.Component
open Luma__math

type t = { mutable position : Vec3.t; mutable rotation : float; mutable scale : Vec3.t }

(* TODO: I don't particularly like all these getters/setters. Can it be done better? *)
let position_x t = t.position.x
let position_y t = t.position.y
let position_z t = t.position.z
let rotation t = t.rotation
let scale_x t = t.scale.x
let scale_y t = t.scale.y
let scale_z t = t.scale.z
let set_position_x t x = t.position <- Vec3.create x t.position.y t.position.z
let set_position_y t y = t.position <- Vec3.create t.position.x y t.position.z
let set_position_z t z = t.position <- Vec3.create t.position.x t.position.y z

let create ?(position = Vec3.zero ()) ?(rotation = 0.) ?(scale = Vec3.create 1.0 1.0 1.0) () =
  { position; rotation; scale }

module C = Component.Make (struct
  type inner = t
end)
