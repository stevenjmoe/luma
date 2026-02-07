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

let intervals_overlap ~a_min ~a_max ~b_min ~b_max = not (a_max < b_min || b_max < a_min)

let project_aabb_on_axis ~axis_x ~axis_y ~aabb_min_x ~aabb_min_y ~aabb_max_x ~aabb_max_y =
  (* AABB projection via center/extents avoids projecting all 4 corners. *)
  let cx = (aabb_min_x +. aabb_max_x) *. 0.5 in
  let cy = (aabb_min_y +. aabb_max_y) *. 0.5 in
  let ex = (aabb_max_x -. aabb_min_x) *. 0.5 in
  let ey = (aabb_max_y -. aabb_min_y) *. 0.5 in
  let center_proj = (axis_x *. cx) +. (axis_y *. cy) in
  let radius = (Float.abs axis_x *. ex) +. (Float.abs axis_y *. ey) in
  (center_proj -. radius, center_proj +. radius)

let project_convex_polygon_on_axis
    ~axis_x
    ~axis_y
    ~poly_points_x
    ~poly_points_y
    ~poly_offset
    ~poly_count
    ~poly_center_x
    ~poly_center_y
    ~c
    ~s =
  let first = poly_offset in
  let lx = poly_points_x.(first) in
  let ly = poly_points_y.(first) in
  let wx = poly_center_x +. ((lx *. c) -. (ly *. s)) in
  let wy = poly_center_y +. ((lx *. s) +. (ly *. c)) in
  let p0 = (axis_x *. wx) +. (axis_y *. wy) in
  let min_p = ref p0 in
  let max_p = ref p0 in
  for i = 1 to poly_count - 1 do
    let idx = poly_offset + i in
    let lx = poly_points_x.(idx) in
    let ly = poly_points_y.(idx) in
    let wx = poly_center_x +. ((lx *. c) -. (ly *. s)) in
    let wy = poly_center_y +. ((lx *. s) +. (ly *. c)) in
    let p = (axis_x *. wx) +. (axis_y *. wy) in
    if p < !min_p then min_p := p;
    if p > !max_p then max_p := p
  done;
  (!min_p, !max_p)

let separating_axis_exists
    ~axis_x
    ~axis_y
    ~aabb_min_x
    ~aabb_min_y
    ~aabb_max_x
    ~aabb_max_y
    ~poly_points_x
    ~poly_points_y
    ~poly_offset
    ~poly_count
    ~poly_center_x
    ~poly_center_y
    ~c
    ~s =
  let a_min, a_max =
    project_aabb_on_axis ~axis_x ~axis_y ~aabb_min_x ~aabb_min_y ~aabb_max_x ~aabb_max_y
  in
  let p_min, p_max =
    project_convex_polygon_on_axis
      ~axis_x
      ~axis_y
      ~poly_points_x
      ~poly_points_y
      ~poly_offset
      ~poly_count
      ~poly_center_x
      ~poly_center_y
      ~c
      ~s
  in
  not (intervals_overlap ~a_min ~a_max ~b_min:p_min ~b_max:p_max)

let aabb_intersects_convex_polygon_sat
    ~aabb_min_x
    ~aabb_min_y
    ~aabb_max_x
    ~aabb_max_y
    ~poly_points_x
    ~poly_points_y
    ~poly_offset
    ~poly_count
    ~poly_center_x
    ~poly_center_y
    ~poly_angle =
  let x_len = Array.length poly_points_x in
  let y_len = Array.length poly_points_y in
  let last_idx = poly_offset + poly_count - 1 in
  if
    poly_count < 3
    || poly_offset < 0
    || poly_offset >= x_len
    || poly_offset >= y_len
    || last_idx >= x_len
    || last_idx >= y_len
  then false
  else
    let c = Float.cos poly_angle in
    let s = Float.sin poly_angle in
    (* SAT: if any tested axis separates, the shapes do not intersect. *)
    if
      separating_axis_exists
        ~axis_x:1.0
        ~axis_y:0.0
        ~aabb_min_x
        ~aabb_min_y
        ~aabb_max_x
        ~aabb_max_y
        ~poly_points_x
        ~poly_points_y
        ~poly_offset
        ~poly_count
        ~poly_center_x
        ~poly_center_y
        ~c
        ~s
      || separating_axis_exists
           ~axis_x:0.0
           ~axis_y:1.0
           ~aabb_min_x
           ~aabb_min_y
           ~aabb_max_x
           ~aabb_max_y
           ~poly_points_x
           ~poly_points_y
           ~poly_offset
           ~poly_count
           ~poly_center_x
           ~poly_center_y
           ~c
           ~s
    then false
    else
      let eps = 1e-12 in
      let rec loop i =
        if i = poly_count then true
        else
          let j = if i = poly_count - 1 then 0 else i + 1 in
          let i_idx = poly_offset + i in
          let j_idx = poly_offset + j in
          let e_lx = poly_points_x.(j_idx) -. poly_points_x.(i_idx) in
          let e_ly = poly_points_y.(j_idx) -. poly_points_y.(i_idx) in

          (* Rotate edge direction, then take a perpendicular as SAT axis. *)
          let e_wx = (e_lx *. c) -. (e_ly *. s) in
          let e_wy = (e_lx *. s) +. (e_ly *. c) in
          let axis_x = -.e_wy in
          let axis_y = e_wx in

          if ((axis_x *. axis_x) +. (axis_y *. axis_y)) <= eps then loop (i + 1)
          else if
            separating_axis_exists
              ~axis_x
              ~axis_y
              ~aabb_min_x
              ~aabb_min_y
              ~aabb_max_x
              ~aabb_max_y
              ~poly_points_x
              ~poly_points_y
              ~poly_offset
              ~poly_count
              ~poly_center_x
              ~poly_center_y
              ~c
              ~s
          then false
          else loop (i + 1)
      in
      loop 0

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
