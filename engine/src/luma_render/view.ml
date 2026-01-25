open Luma__resource
open Luma__camera

type t = {
  camera_entity : Luma__id.Id.Entity.t;
  camera : Camera.t;
  viewport : Viewport.t;
}

let create entity camera viewport = { camera_entity = entity; camera; viewport }
let camera_entity v = v.camera_entity
let camera v = v.camera
let viewport v = v.viewport

module R = Resource.Make (struct
  type inner = t list

  let name = "view_res"
end)
