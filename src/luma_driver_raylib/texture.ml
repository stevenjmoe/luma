open Raylib
open Luma__math

let width = Raylib.Texture2D.width
let height = Raylib.Texture2D.height

let draw_texture texture source dest origin rotation tint =
  let source =
    Rectangle.create (Rect.x source) (Rect.y source) (Rect.width source) (Rect.height source)
  in
  let dest = Rectangle.create (Rect.x dest) (Rect.y dest) (Rect.width dest) (Rect.height dest) in
  let origin = Vector2.create (Vec2.x origin) (Vec2.y origin) in
  Raylib.draw_texture_pro texture source dest origin rotation tint

let load_texture_from_image = Raylib.load_texture_from_image
