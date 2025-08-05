open Luma__math

let make ~(offset : Vec2.t) ~(target : Vec2.t) ~rotation ~zoom () =
  let ox, oy = (Vec2.x offset, Vec2.y offset) and tx, ty = (Vec2.x target, Vec2.y target) in
  Raylib.Camera2D.create (Raylib.Vector2.create ox oy) (Raylib.Vector2.create tx ty) rotation zoom

let default () =
  let target = Raylib.Vector2.create 0. 0. in
  let offset = Raylib.Vector2.create 0. 0. in
  Raylib.Camera2D.create offset target 0. 1.

let target camera =
  let target = Raylib.Camera2D.target camera in
  Vec2.create (Raylib.Vector2.x target) (Raylib.Vector2.y target)

let offset camera =
  let offset = Raylib.Camera2D.offset camera in
  Vec2.create (Raylib.Vector2.x offset) (Raylib.Vector2.y offset)

let zoom camera = Raylib.Camera2D.zoom camera
let rotation camera = Raylib.Camera2D.rotation camera

let set_target camera target =
  let target' = Raylib.Vector2.create (Vec2.x target) (Vec2.y target) in
  Raylib.Camera2D.set_target camera target'

let set_offset camera offset =
  let offset' = Raylib.Vector2.create (Vec2.x offset) (Vec2.y offset) in
  Raylib.Camera2D.set_offset camera offset'

let set_rotation camera rotation = Raylib.Camera2D.set_rotation camera rotation
let set_zoom camera rotation = Raylib.Camera2D.set_zoom camera rotation
