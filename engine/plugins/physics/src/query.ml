let sweep_ray_aabb ~origin_x ~origin_y ~dir_x ~dir_y ~min_x ~min_y ~max_x ~max_y =
  let open Float in
  if abs dir_x < epsilon_float && abs dir_y < epsilon_float then None
  else
    (* Slab intersection with an AABB. Returns the earliest hit in [0, +inf). *)
    let inv_dir_x = if abs dir_x < epsilon_float then infinity else 1. /. dir_x in
    let inv_dir_y = if abs dir_y < epsilon_float then infinity else 1. /. dir_y in

    let t1x = (min_x -. origin_x) *. inv_dir_x in
    let t2x = (max_x -. origin_x) *. inv_dir_x in
    let tmin_x = min t1x t2x in
    let tmax_x = max t1x t2x in

    let t1y = (min_y -. origin_y) *. inv_dir_y in
    let t2y = (max_y -. origin_y) *. inv_dir_y in
    let tmin_y = min t1y t2y in
    let tmax_y = max t1y t2y in

    let tmin = max tmin_x tmin_y in
    let tmax = min tmax_x tmax_y in

    if tmax < 0. || tmin > tmax then None
    else
      let toi = max 0. tmin in
      let nx, ny =
        if tmin_x > tmin_y then if dir_x > 0. then (-1., 0.) else (1., 0.)
        else if dir_y > 0. then (0., -1.)
        else (0., 1.)
      in
      Some (toi, nx, ny)

let sweep_circle_circle store shape_store ~row ~other ~delta_x ~delta_y =
  let open Rb_store in
  let ox = pos_x store row in
  let oy = pos_y store row in
  let cx = pos_x store other in
  let cy = pos_y store other in
  let circle_handle = shape_handle store row in
  let other_handle = shape_handle store other in

  (* Relative motion: move `row` by (dx, dy) and treat `other` as fixed. *)
  let combined_radius =
    Shape_store.circle_radius shape_store circle_handle
    +. Shape_store.circle_radius shape_store other_handle
  in
  let a_coeff = (delta_x *. delta_x) +. (delta_y *. delta_y) in

  if a_coeff <= epsilon_float then None
  else
    let rel_x = ox -. cx in
    let rel_y = oy -. cy in
    let b_coeff = 2. *. ((rel_x *. delta_x) +. (rel_y *. delta_y)) in
    let c_coeff = (rel_x *. rel_x) +. (rel_y *. rel_y) -. (combined_radius *. combined_radius) in
    let discriminant = (b_coeff *. b_coeff) -. (4. *. a_coeff *. c_coeff) in
    if discriminant < 0. then None
    else
      let sqrt_disc = sqrt discriminant in
      let t0 = (-.b_coeff -. sqrt_disc) /. (2. *. a_coeff) in
      let t1 = (-.b_coeff +. sqrt_disc) /. (2. *. a_coeff) in

      (* Choose the earliest hit within the sweep [0, 1]. *)
      let t = if t0 >= 0. && t0 <= 1. then t0 else if t1 >= 0. && t1 <= 1. then t1 else infinity in
      if t = infinity then None
      else
        let hit_x = ox +. (delta_x *. t) in
        let hit_y = oy +. (delta_y *. t) in
        let n_x = hit_x -. cx in
        let n_y = hit_y -. cy in
        let len = sqrt ((n_x *. n_x) +. (n_y *. n_y)) in
        let nx, ny = if len < 1e-8 then (0., 0.) else (n_x /. len, n_y /. len) in
        Some (t, nx, ny)

let sweep_aabb_against_aabb store shape_store ~row ~other ~delta_x ~delta_y =
  let open Rb_store in
  let aabb_handle = shape_handle store row in
  let other_handle = shape_handle store other in

  let half_w = Shape_store.aabb_half_width shape_store aabb_handle in
  let half_h = Shape_store.aabb_half_height shape_store aabb_handle in
  let other_hw = Shape_store.aabb_half_width shape_store other_handle in
  let other_hh = Shape_store.aabb_half_height shape_store other_handle in
  let aabb_pos_x = pos_x store row in
  let aabb_pos_y = pos_y store row in
  let other_pos_x = pos_x store other in
  let other_pos_y = pos_y store other in

  (* Expand the target AABB by the moving AABB extents, then raycast the moving
     center through that expanded box. This is equivalent to a Minkowski-sum
     sweep for AABB vs AABB. *)
  let expanded_hw = other_hw +. half_w in
  let expanded_hh = other_hh +. half_h in
  let min_x = other_pos_x -. expanded_hw in
  let max_x = other_pos_x +. expanded_hw in
  let min_y = other_pos_y -. expanded_hh in
  let max_y = other_pos_y +. expanded_hh in

  sweep_ray_aabb ~origin_x:aabb_pos_x ~origin_y:aabb_pos_y ~dir_x:delta_x ~dir_y:delta_y ~min_x
    ~min_y ~max_x ~max_y

let sweep_circle_against_aabb store shape_store ~row ~other ~delta_x ~delta_y =
  let open Rb_store in
  let radius = Shape_store.circle_radius shape_store (shape_handle store row) in
  let origin_x = pos_x store row in
  let origin_y = pos_y store row in

  let aabb_min_x = min_x store other in
  let aabb_max_x = max_x store other in
  let aabb_min_y = min_y store other in
  let aabb_max_y = max_y store other in

  (* Expand the AABB by the circle radius, then raycast the circle center. *)
  let min_x = aabb_min_x -. radius in
  let max_x = aabb_max_x +. radius in
  let min_y = aabb_min_y -. radius in
  let max_y = aabb_max_y +. radius in

  sweep_ray_aabb ~origin_x ~origin_y ~dir_x:delta_x ~dir_y:delta_y ~min_x ~min_y ~max_x ~max_y

let sweep_aabb_against_circle store shape_store ~row ~other ~delta_x ~delta_y =
  let open Rb_store in
  let ox = pos_x store row in
  let oy = pos_y store row in
  let cx = pos_x store other in
  let cy = pos_y store other in
  let circle_hanle = shape_handle store other in
  let aabb_handle = shape_handle store row in

  let circle_radius = Shape_store.circle_radius shape_store circle_hanle in
  let aabb_hw = Shape_store.aabb_half_width shape_store aabb_handle in
  let aabb_hh = Shape_store.aabb_half_height shape_store aabb_handle in

  (* Conservative approximation: treat the moving AABB as a circle with radius
     max(half_w, half_h) and do a circle-circle sweep. *)
  let combined_radius = circle_radius +. max aabb_hw aabb_hh in
  let a_coeff = (delta_x *. delta_x) +. (delta_y *. delta_y) in
  if a_coeff <= epsilon_float then None
  else
    let rel_x = ox -. cx in
    let rel_y = oy -. cy in
    let b_coeff = 2. *. ((rel_x *. delta_x) +. (rel_y *. delta_y)) in
    let c_coeff = (rel_x *. rel_x) +. (rel_y *. rel_y) -. (combined_radius *. combined_radius) in
    let discriminant = (b_coeff *. b_coeff) -. (4. *. a_coeff *. c_coeff) in
    if discriminant < 0. then None
    else
      let sqrt_disc = sqrt discriminant in
      let t0 = (-.b_coeff -. sqrt_disc) /. (2. *. a_coeff) in
      let t1 = (-.b_coeff +. sqrt_disc) /. (2. *. a_coeff) in
      let t = if t0 >= 0. && t0 <= 1. then t0 else if t1 >= 0. && t1 <= 1. then t1 else infinity in
      if t = infinity then None
      else
        let hit_x = ox +. (delta_x *. t) in
        let hit_y = oy +. (delta_y *. t) in
        let n_x = hit_x -. cx in
        let n_y = hit_y -. cy in
        let len = sqrt ((n_x *. n_x) +. (n_y *. n_y)) in
        let nx, ny = if len < 1e-8 then (0., 0.) else (n_x /. len, n_y /. len) in
        Some (t, nx, ny)

let interval_overlap ~a_min ~a_max ~b_min ~b_max = not (a_max < b_min || b_max < a_min)

let interval_sweep ~a_min ~a_max ~b_min ~b_max ~v =
  if Float.abs v < epsilon_float then
    if interval_overlap ~a_min ~a_max ~b_min ~b_max then Some (neg_infinity, infinity) else None
  else
    let t0 = (b_min -. a_max) /. v in
    let t1 = (b_max -. a_min) /. v in

    Some (min t0 t1, max t0 t1)

let polygon_world_points store shape_store poly_row =
  let open Rb_store in
  let poly_handle = shape_handle store poly_row in
  let poly_points_x = Shape_store.polygon_storage_x shape_store in
  let poly_points_y = Shape_store.polygon_storage_y shape_store in
  let poly_offset = Shape_store.polygon_offset shape_store poly_handle in
  let poly_count = Shape_store.polygon_count shape_store poly_handle in
  let poly_pos_x = pos_x store poly_row in
  let poly_pos_y = pos_y store poly_row in
  let poly_angle = angle store poly_row in

  let c = Float.cos poly_angle in
  let s = Float.sin poly_angle in
  let wx = Array.make poly_count 0.0 in
  let wy = Array.make poly_count 0.0 in

  for i = 0 to poly_count - 1 do
    let idx = poly_offset + i in
    let lx = poly_points_x.(idx) in
    let ly = poly_points_y.(idx) in
    wx.(i) <- poly_pos_x +. ((lx *. c) -. (ly *. s));
    wy.(i) <- poly_pos_y +. ((lx *. s) +. (ly *. c))
  done;
  (wx, wy)

let project_polygon_on_axis ~world_x ~world_y ~axis_x ~axis_y =
  let n = Array.length world_x in
  if n = 0 then (0.0, 0.0)
  else
    let p0 = (axis_x *. world_x.(0)) +. (axis_y *. world_y.(0)) in
    let min_p = ref p0 in
    let max_p = ref p0 in

    for i = 1 to n - 1 do
      let p = (axis_x *. world_x.(i)) +. (axis_y *. world_y.(i)) in
      if p < !min_p then min_p := p;
      if p > !max_p then max_p := p
    done;
    (!min_p, !max_p)

let sweep_circle_against_polygon store shape_store ~row ~other ~delta_x ~delta_y =
  let open Rb_store in
  let radius = Shape_store.circle_radius shape_store (shape_handle store row) in
  let origin_x = pos_x store row in
  let origin_y = pos_y store row in
  let world_x, world_y = polygon_world_points store shape_store other in
  let poly_count = Array.length world_x in

  if poly_count < 3 then None
  else
    let t_enter = ref neg_infinity in
    let t_exit = ref infinity in
    let best_nx = ref 0.0 in
    let best_ny = ref 0.0 in
    let best_v = ref 0.0 in
    let has_axis = ref false in
    let separated = ref false in
    let eps = 1e-12 in

    let test_axis axis_x axis_y =
      if not !separated then
        let len_sq = (axis_x *. axis_x) +. (axis_y *. axis_y) in
        if len_sq > eps then
          let inv_len = 1.0 /. Float.sqrt len_sq in
          let nx = axis_x *. inv_len in
          let ny = axis_y *. inv_len in
          let circle_center_proj = (origin_x *. nx) +. (origin_y *. ny) in
          let a_min = circle_center_proj -. radius in
          let a_max = circle_center_proj +. radius in

          let b_min, b_max = project_polygon_on_axis ~world_x ~world_y ~axis_x:nx ~axis_y:ny in
          let v = (delta_x *. nx) +. (delta_y *. ny) in

          match interval_sweep ~a_min ~a_max ~b_min ~b_max ~v with
          | None -> separated := true
          | Some (enter_i, exit_i) ->
              has_axis := true;
              if enter_i > !t_enter then (
                t_enter := enter_i;
                best_nx := nx;
                best_ny := ny;
                best_v := v);
              if exit_i < !t_exit then t_exit := exit_i;
              if !t_enter > !t_exit then separated := true
    in

    (* Polygon edge normals *)
    for i = 0 to poly_count - 1 do
      let j = if i = poly_count - 1 then 0 else i + 1 in
      let ex = world_x.(j) -. world_x.(i) in
      let ey = world_y.(j) -. world_y.(i) in
      test_axis (-.ey) ex
    done;

    (* Extra corner axes for circle-vs-polygon corner impacts. *)
    for i = 0 to poly_count - 1 do
      let ax = world_x.(i) -. origin_x in
      let ay = world_y.(i) -. origin_y in
      test_axis ax ay
    done;

    if !separated || (not !has_axis) || !t_exit < 0.0 || !t_enter > 1.0 then None
    else
      let toi = max 0.0 !t_enter in
      let nx, ny =
        if Float.abs !best_nx < eps && Float.abs !best_ny < eps then
          let move_len = Float.sqrt ((delta_x *. delta_x) +. (delta_y *. delta_y)) in
          if move_len > eps then (-.delta_x /. move_len, -.delta_y /. move_len) else (0.0, 0.0)
        else if !best_v > 0.0 then (-. !best_nx, -. !best_ny)
        else (!best_nx, !best_ny)
      in
      Some (toi, nx, ny)

let sweep_aabb_against_polygon store shape_store ~row ~other ~delta_x ~delta_y =
  let open Rb_store in
  let aabb_handle = shape_handle store row in
  let half_w = Shape_store.aabb_half_width shape_store aabb_handle in
  let half_h = Shape_store.aabb_half_height shape_store aabb_handle in
  let origin_x = pos_x store row in
  let origin_y = pos_y store row in
  let world_x, world_y = polygon_world_points store shape_store other in
  let poly_count = Array.length world_x in
  if poly_count < 3 then None
  else
    let t_enter = ref neg_infinity in
    let t_exit = ref infinity in
    let best_nx = ref 0.0 in
    let best_ny = ref 0.0 in
    let best_v = ref 0.0 in
    let has_axis = ref false in
    let separated = ref false in
    let eps = 1e-12 in

    let test_axis axis_x axis_y =
      if not !separated then
        let len_sq = (axis_x *. axis_x) +. (axis_y *. axis_y) in
        if len_sq > eps then
          let inv_len = 1.0 /. Float.sqrt len_sq in
          let nx = axis_x *. inv_len in
          let ny = axis_y *. inv_len in
          let aabb_center_proj = (origin_x *. nx) +. (origin_y *. ny) in
          let aabb_radius = (Float.abs nx *. half_w) +. (Float.abs ny *. half_h) in
          let a_min = aabb_center_proj -. aabb_radius in
          let a_max = aabb_center_proj +. aabb_radius in
          let b_min, b_max = project_polygon_on_axis ~world_x ~world_y ~axis_x:nx ~axis_y:ny in
          let v = (delta_x *. nx) +. (delta_y *. ny) in
          match interval_sweep ~a_min ~a_max ~b_min ~b_max ~v with
          | None -> separated := true
          | Some (enter_i, exit_i) ->
              has_axis := true;
              if enter_i > !t_enter then (
                t_enter := enter_i;
                best_nx := nx;
                best_ny := ny;
                best_v := v);
              if exit_i < !t_exit then t_exit := exit_i;
              if !t_enter > !t_exit then separated := true
    in

    (* AABB face normals *)
    test_axis 1.0 0.0;
    test_axis 0.0 1.0;

    (* Polygon edge normals *)
    for i = 0 to poly_count - 1 do
      let j = if i = poly_count - 1 then 0 else i + 1 in
      let ex = world_x.(j) -. world_x.(i) in
      let ey = world_y.(j) -. world_y.(i) in
      test_axis (-.ey) ex
    done;

    if !separated || (not !has_axis) || !t_exit < 0.0 || !t_enter > 1.0 then None
    else
      let toi = max 0.0 !t_enter in
      let nx, ny =
        if Float.abs !best_nx < eps && Float.abs !best_ny < eps then
          let move_len = Float.sqrt ((delta_x *. delta_x) +. (delta_y *. delta_y)) in
          if move_len > eps then (-.delta_x /. move_len, -.delta_y /. move_len) else (0.0, 0.0)
        else if !best_v > 0.0 then (-. !best_nx, -. !best_ny)
        else (!best_nx, !best_ny)
      in
      Some (toi, nx, ny)

let kinematic_toi store shape_store ~row ~other ~delta_x ~delta_y =
  let open Rb_store in
  (* Time of impact for moving `row` along (dx, dy) against `other`.
     `shape` encoding: 0 = circle, 1 = AABB, 2 = polygon. *)
  match (shape_kind store row, shape_kind store other) with
  | 1, 1 -> sweep_aabb_against_aabb store shape_store ~row ~other ~delta_x ~delta_y
  | 1, 0 -> sweep_aabb_against_circle store shape_store ~row ~other ~delta_x ~delta_y
  | 0, 1 -> sweep_circle_against_aabb store shape_store ~row ~other ~delta_x ~delta_y
  | 0, 0 -> sweep_circle_circle store shape_store ~row ~other ~delta_x ~delta_y
  | 0, 2 -> sweep_circle_against_polygon store shape_store ~row ~other ~delta_x ~delta_y
  | 1, 2 -> sweep_aabb_against_polygon store shape_store ~row ~other ~delta_x ~delta_y
  | _ -> None
