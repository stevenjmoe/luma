open Luma__ecs

type t = Rigid_body.t list

module C = Component.Make (struct
  type inner = t

  let name = "Colliders"
end)
