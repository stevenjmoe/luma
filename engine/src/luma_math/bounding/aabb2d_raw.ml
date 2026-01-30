let visible_area ~min_x ~max_x ~min_y ~max_y =
  let x = Float.max (max_x -. min_x) 0. in
  let y = Float.max (max_y -. min_y) 0. in
  x *. y

let contains ~min_x1 ~max_x1 ~min_y1 ~max_y1 ~min_x2 ~max_x2 ~min_y2 ~max_y2 =
  min_x2 >= min_x1 && min_y2 >= min_y1 && max_x2 <= max_x1 && max_y2 <= max_y1

let clamp x lo hi = if x < lo then lo else if x > hi then hi else x

let interval_distance a0 a1 b0 b1 =
  (* distance between 1D intervals [a0,a1] and [b0,b1] (0 if overlapping) *)
  if a1 < b0 then b0 -. a1 else if b1 < a0 then a0 -. b1 else 0.0

let aabb_intersects_aabb ~a_min_x ~a_min_y ~a_max_x ~a_max_y ~b_min_x ~b_min_y ~b_max_x ~b_max_y =
  let x_overlaps = a_min_x <= b_max_x && a_max_x >= b_min_x in
  let y_overlaps = a_min_y <= b_max_y && a_max_y >= b_min_y in
  x_overlaps && y_overlaps

let aabb_intersects_circle
    ~aabb_min_x
    ~aabb_min_y
    ~aabb_max_x
    ~aabb_max_y
    ~circle_center_x
    ~circle_center_y
    ~circle_radius =
  let closest_x = max aabb_min_x (min circle_center_x aabb_max_x) in
  let closest_y = max aabb_min_y (min circle_center_y aabb_max_y) in

  let distance_x = circle_center_x -. closest_x in
  let distance_y = circle_center_y -. closest_y in
  let distance_squared = (distance_x *. distance_x) +. (distance_y *. distance_y) in

  distance_squared <= circle_radius *. circle_radius

let circle_intersects_circle ~a_center_x ~a_center_y ~a_radius ~b_center_x ~b_center_y ~b_radius =
  let dx = a_center_x -. b_center_x in
  let dy = a_center_y -. b_center_y in
  let center_distance_squared = (dx *. dx) +. (dy *. dy) in
  let radius_sum = a_radius +. b_radius in
  let radius_sum_squared = radius_sum *. radius_sum in
  center_distance_squared <= radius_sum_squared

let capsule_intersects_circle
    ~cap_center_x
    ~cap_center_y
    ~cap_radius
    ~cap_half_length
    ~circle_center_x
    ~circle_center_y
    ~circle_radius =
  let y1 = cap_center_y -. cap_half_length in
  let y2 = cap_center_y +. cap_half_length in

  let closest_x = cap_center_x in
  let closest_y = clamp circle_center_y y1 y2 in

  let dx = circle_center_x -. closest_x in
  let dy = circle_center_y -. closest_y in
  let dist2 = (dx *. dx) +. (dy *. dy) in
  let r = cap_radius +. circle_radius in
  dist2 <= r *. r

let capsule_intersects_capsule
    ~a_center_x
    ~a_center_y
    ~a_radius
    ~a_half_length
    ~b_center_x
    ~b_center_y
    ~b_radius
    ~b_half_length =
  let ay0 = a_center_y -. a_half_length in
  let ay1 = a_center_y +. a_half_length in
  let by0 = b_center_y -. b_half_length in
  let by1 = b_center_y +. b_half_length in

  let dx = Float.abs (a_center_x -. b_center_x) in
  let dy = interval_distance ay0 ay1 by0 by1 in

  let dist2 = (dx *. dx) +. (dy *. dy) in
  let r = a_radius +. b_radius in
  dist2 <= r *. r

let capsule_intersects_aabb
    ~aabb_min_x
    ~aabb_min_y
    ~aabb_max_x
    ~aabb_max_y
    ~cap_center_x
    ~cap_center_y
    ~cap_radius
    ~cap_half_length =
  let seg_y0 = cap_center_y -. cap_half_length in
  let seg_y1 = cap_center_y +. cap_half_length in

  (* x distance from point cap_center_x to interval [min_x,max_x] *)
  let dx =
    if cap_center_x < aabb_min_x then aabb_min_x -. cap_center_x
    else if cap_center_x > aabb_max_x then cap_center_x -. aabb_max_x
    else 0.0
  in

  (* y distance between intervals [seg_y0,seg_y1] and [aabb_min_y,aabb_max_y] *)
  let dy = interval_distance seg_y0 seg_y1 aabb_min_y aabb_max_y in

  let dist2 = (dx *. dx) +. (dy *. dy) in
  dist2 <= cap_radius *. cap_radius
