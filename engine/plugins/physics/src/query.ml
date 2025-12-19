open Grid

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

let sweep_circle_circle store ~row ~other ~dx ~dy =
  let open Rb_store in
  let ox = store.pos_x.(row) in
  let oy = store.pos_y.(row) in
  let cx = store.pos_x.(other) in
  let cy = store.pos_y.(other) in

  (* Relative motion: move `row` by (dx, dy) and treat `other` as fixed. *)
  let combined_radius = store.radius.(row) +. store.radius.(other) in
  let a_coeff = (dx *. dx) +. (dy *. dy) in

  if a_coeff <= epsilon_float then None
  else
    let rel_x = ox -. cx in
    let rel_y = oy -. cy in
    let b_coeff = 2. *. ((rel_x *. dx) +. (rel_y *. dy)) in
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
        let hit_x = ox +. (dx *. t) in
        let hit_y = oy +. (dy *. t) in
        let n_x = hit_x -. cx in
        let n_y = hit_y -. cy in
        let len = sqrt ((n_x *. n_x) +. (n_y *. n_y)) in
        let nx, ny = if len < 1e-8 then (0., 0.) else (n_x /. len, n_y /. len) in
        Some (t, nx, ny)

let sweep_aabb_against_aabb store ~row ~other ~dx ~dy =
  let open Rb_store in
  let half_w = store.box_hw.(row) in
  let half_h = store.box_hh.(row) in
  let other_hw = store.box_hw.(other) in
  let other_hh = store.box_hh.(other) in

  (* Expand the target AABB by the moving AABB extents, then raycast the moving
     center through that expanded box. This is equivalent to a Minkowski-sum
     sweep for AABB vs AABB. *)
  let expanded_hw = other_hw +. half_w in
  let expanded_hh = other_hh +. half_h in
  let min_x = store.pos_x.(other) -. expanded_hw in
  let max_x = store.pos_x.(other) +. expanded_hw in
  let min_y = store.pos_y.(other) -. expanded_hh in
  let max_y = store.pos_y.(other) +. expanded_hh in

  sweep_ray_aabb ~origin_x:store.pos_x.(row) ~origin_y:store.pos_y.(row) ~dir_x:dx ~dir_y:dy ~min_x
    ~min_y ~max_x ~max_y

let sweep_circle_against_aabb store ~row ~other ~dx ~dy =
  let open Rb_store in
  let radius = store.radius.(row) in

  (* Expand the AABB by the circle radius, then raycast the circle center. *)
  let min_x = store.min_x.(other) -. radius in
  let max_x = store.max_x.(other) +. radius in
  let min_y = store.min_y.(other) -. radius in
  let max_y = store.max_y.(other) +. radius in

  sweep_ray_aabb ~origin_x:store.pos_x.(row) ~origin_y:store.pos_y.(row) ~dir_x:dx ~dir_y:dy ~min_x
    ~min_y ~max_x ~max_y

let sweep_aabb_against_circle store ~row ~other ~dx ~dy =
  let open Rb_store in
  let ox = store.pos_x.(row) in
  let oy = store.pos_y.(row) in
  let cx = store.pos_x.(other) in
  let cy = store.pos_y.(other) in

  (* Conservative approximation: treat the moving AABB as a circle with radius
     max(half_w, half_h) and do a circle-circle sweep. *)
  let combined_radius = store.radius.(other) +. max store.box_hw.(row) store.box_hh.(row) in
  let a_coeff = (dx *. dx) +. (dy *. dy) in
  if a_coeff <= epsilon_float then None
  else
    let rel_x = ox -. cx in
    let rel_y = oy -. cy in
    let b_coeff = 2. *. ((rel_x *. dx) +. (rel_y *. dy)) in
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
        let hit_x = ox +. (dx *. t) in
        let hit_y = oy +. (dy *. t) in
        let n_x = hit_x -. cx in
        let n_y = hit_y -. cy in
        let len = sqrt ((n_x *. n_x) +. (n_y *. n_y)) in
        let nx, ny = if len < 1e-8 then (0., 0.) else (n_x /. len, n_y /. len) in
        Some (t, nx, ny)

let kinematic_toi store ~row ~other ~dx ~dy =
  let open Rb_store in
  (* Time of impact for moving `row` along (dx, dy) against `other`.
     `shape` encoding: 0 = circle, 1 = AABB. *)
  match (store.shape.(row), store.shape.(other)) with
  | 1, 1 -> sweep_aabb_against_aabb store ~row ~other ~dx ~dy
  | 1, 0 -> sweep_aabb_against_circle store ~row ~other ~dx ~dy
  | 0, 1 -> sweep_circle_against_aabb store ~row ~other ~dx ~dy
  | 0, 0 -> sweep_circle_circle store ~row ~other ~dx ~dy
  | _ -> None
