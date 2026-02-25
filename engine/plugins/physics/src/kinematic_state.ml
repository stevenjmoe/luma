type t = {
  mutable on_floor : bool;
  mutable on_wall : bool;
  mutable on_ceiling : bool;
  mutable floor_normal_x : float;
  mutable floor_normal_y : float;
}

let default () =
  {
    on_floor = false;
    on_wall = false;
    on_ceiling = false;
    floor_normal_x = 0.;
    floor_normal_y = 0.;
  }

let clear k =
  k.on_floor <- false;
  k.on_wall <- false;
  k.on_ceiling <- false;
  k.floor_normal_x <- 0.;
  k.floor_normal_y <- 0.

let on_floor k = k.on_floor
let on_wall k = k.on_wall
let on_ceiling k = k.on_ceiling
let floor_normal k = Luma__math.Vec2.create k.floor_normal_x k.floor_normal_y
let set_is_on_floor k on_floor = k.on_floor <- on_floor
let set_is_on_wall k on_wall = k.on_wall <- on_wall
let set_is_on_ceiling k on_ceiling = k.on_ceiling <- on_ceiling
let set_floor_normal_x k normal_x = k.floor_normal_x <- normal_x
let set_floor_normal_y k normal_y = k.floor_normal_y <- normal_y

module C = Luma__ecs.Component.Make (struct
  type inner = t

  let name = "Kinematic_state"
end)
