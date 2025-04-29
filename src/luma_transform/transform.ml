module Component = Luma__ecs.Component
open Luma__math

type t = {
  mutable position : Vec3.t;
  mutable rotation : float;
  mutable scale : Vec3.t;
}

let create ?(position = Vec3.zero ()) ?(rotation = 0.) ?(scale = Vec3.create 1.0 1.0 1.0) () =
  { position; rotation; scale }

module C = Component.Make (struct
  type inner = t

  let name = "Transform"
end)
