open Luma__math

type draw_plan = {
  sx : float;
  sy : float;
  sw : float;
  sh : float;
  draw_x : float;
  draw_y : float;
  flip_x : bool;
  flip_y : bool;
}

let plan ~(src : Rect.t) ~(dst : Rect.t) ~(origin : Vec2.t) : draw_plan =
  let sx0 = Rect.x src in
  let sy0 = Rect.y src in
  let sw0 = Rect.width src in
  let sh0 = Rect.height src in

  let dw = Rect.width dst in
  let dh = Rect.height dst in

  let flip_x = sw0 < 0. in
  let flip_y = sh0 < 0. in

  let sx = if flip_x && flip_y then sx0 +. sw0 else sx0 in
  let sy = if flip_x && flip_y then sy0 +. sh0 else sy0 in
  let sw = Float.abs sw0 in
  let sh = Float.abs sh0 in

  let draw_x = if flip_x then Vec2.x origin -. dw else -.Vec2.x origin in
  let draw_y = if flip_y then Vec2.y origin -. dh else -.Vec2.y origin in

  { sx; sy; sw; sh; draw_x; draw_y; flip_x; flip_y }

let should_draw (plan : draw_plan) ~(dst : Rect.t) =
  plan.sw > 0. && plan.sh > 0. && Rect.width dst <> 0. && Rect.height dst <> 0.
