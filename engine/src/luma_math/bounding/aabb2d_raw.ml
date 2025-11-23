let visible_area ~min_x ~max_x ~min_y ~max_y =
  let x = Float.max (max_x -. min_x) 0. in
  let y = Float.max (max_y -. min_y) 0. in
  x *. y

let contains ~min_x1 ~max_x1 ~min_y1 ~max_y1 ~min_x2 ~max_x2 ~min_y2 ~max_y2 =
  min_x2 >= min_x1 && min_y2 >= min_y1 && max_x2 <= max_x1 && max_y2 <= max_y1

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
