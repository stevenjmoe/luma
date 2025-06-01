open Luma__math

let make ~(position : Vec2.t) ~(target : Vec2.t) ~rotation ~zoom () =
  let px, py = (Vec2.x position, Vec2.y position) and tx, ty = (Vec2.x target, Vec2.y target) in
  Raylib.Camera2D.create (Raylib.Vector2.create px py) (Raylib.Vector2.create tx ty) rotation zoom

let default () =
  let target = Raylib.Vector2.create 0. 0. in
  let offset = Raylib.Vector2.create 0. 0. in
  Raylib.Camera2D.create offset target 0. 1.

let set_target camera target =
  let x, y = target in
  Raylib.Camera2D.set_target camera (Raylib.Vector2.create x y)
