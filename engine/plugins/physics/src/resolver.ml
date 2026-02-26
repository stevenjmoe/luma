(* TODO: Kinematic/static bodies should be guaranteed to have an inv_mass of 0, but there should be guards in place here to check *)
open Luma__math

let squared f1 f2 = (f1 *. f1) +. (f2 *. f2)
let dot ~ax ~ay ~bx ~by = (ax *. bx) +. (ay *. by)

let apply_normal_impulse (store : Rb_store.t) ~restitution ~a ~b ~normal_x ~normal_y =
  let open Rb_store in
  let vel_a_x = vel_x store a in
  let vel_a_y = vel_y store a in
  let vel_b_x = vel_x store b in
  let vel_b_y = vel_y store b in
  let inv_mass_a = inv_mass store a in
  let inv_mass_b = inv_mass store b in

  let rel_vel_x = vel_b_x -. vel_a_x in
  let rel_vel_y = vel_b_y -. vel_a_y in

  (* Component along the collision normal. *)
  let relative_speed_along_normal = (rel_vel_x *. normal_x) +. (rel_vel_y *. normal_y) in

  (* Skip if objects are moving apart along the normal. *)
  if relative_speed_along_normal > 0. then ()
  else
    let inv_mass_sum = inv_mass_a +. inv_mass_b in
    if inv_mass_sum <= 0. then ()
    else
      let impulse_scalar = -.(1. +. restitution) *. relative_speed_along_normal /. inv_mass_sum in
      let impulse_x = impulse_scalar *. normal_x in
      let impulse_y = impulse_scalar *. normal_y in

      (* Apply impulse along the normal. *)
      set_vel_x store a (vel_a_x -. (impulse_x *. inv_mass_a));
      set_vel_y store a (vel_a_y -. (impulse_y *. inv_mass_a));
      set_vel_x store b (vel_b_x +. (impulse_x *. inv_mass_b));
      set_vel_y store b (vel_b_y +. (impulse_y *. inv_mass_b))

let resolve_circle_circle
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    ~a
    ~b
    ~restitution
    ~position_correction =
  let open Rb_store in
  (* Only handle circle-circle contacts (shape = 0). *)
  if shape_kind store a <> 0 || shape_kind store b <> 0 then ()
  else
    (* Load positions and radii. *)
    let pos_a_x = pos_x store a in
    let pos_a_y = pos_y store a in
    let pos_b_x = pos_x store b in
    let pos_b_y = pos_y store b in
    let radius_a = Shape_store.circle_radius shape_store (shape_handle store a) in
    let radius_b = Shape_store.circle_radius shape_store (shape_handle store b) in

    let sep_x = pos_b_x -. pos_a_x in
    let sep_y = pos_b_y -. pos_a_y in

    let distance_squared = squared sep_x sep_y in
    let distance = Float.sqrt distance_squared in

    (* Safe normal: degenerate case handled here. *)
    let normal_x = if distance > 1e-8 then sep_x /. distance else 1. in
    let normal_y = if distance > 1e-8 then sep_y /. distance else 0. in

    (* Penetration based on radii. *)
    let total_radius = radius_a +. radius_b in
    let penetration_depth = total_radius -. distance in

    if penetration_depth <= 0. then ()
    else (
      (* Velocity impulse. *)
      apply_normal_impulse ~restitution store ~a ~b ~normal_x ~normal_y;

      (* Positional correction *)
      let inv_mass_a = inv_mass store a in
      let inv_mass_b = inv_mass store b in
      let inv_mass_sum = inv_mass_a +. inv_mass_b in

      if inv_mass_sum <= 0. then ()
      else
        let correction_amount = position_correction *. penetration_depth /. inv_mass_sum in
        let correction_x = correction_amount *. normal_x in
        let correction_y = correction_amount *. normal_y in

        (* Move only non-static bodies (inv_mass > 0). *)
        if inv_mass_a > 0. then (
          let pos_a_x' = pos_a_x -. (correction_x *. inv_mass_a) in
          let pos_a_y' = pos_a_y -. (correction_y *. inv_mass_a) in
          set_pos_x store a pos_a_x';
          set_pos_y store a pos_a_y';
          set_min_x store a (pos_a_x' -. radius_a);
          set_max_x store a (pos_a_x' +. radius_a);
          set_min_y store a (pos_a_y' -. radius_a);
          set_max_y store a (pos_a_y' +. radius_a));

        if inv_mass_b > 0. then (
          let pos_b_x' = pos_b_x +. (correction_x *. inv_mass_b) in
          let pos_b_y' = pos_b_y +. (correction_y *. inv_mass_b) in
          set_pos_x store b pos_b_x';
          set_pos_y store b pos_b_y';
          set_min_x store b (pos_b_x' -. radius_b);
          set_max_x store b (pos_b_x' +. radius_b);
          set_min_y store b (pos_b_y' -. radius_b);
          set_max_y store b (pos_b_y' +. radius_b)))

let resolve_aabb_aabb (store : Rb_store.t) ~a ~b ~restitution ~position_correction =
  let open Rb_store in
  let center_a_x = pos_x store a in
  let center_a_y = pos_y store a in
  let center_b_x = pos_x store b in
  let center_b_y = pos_y store b in

  let min_a_x = min_x store a in
  let max_a_x = max_x store a in
  let min_a_y = min_y store a in
  let max_a_y = max_y store a in

  let min_b_x = min_x store b in
  let max_b_x = max_x store b in
  let min_b_y = min_y store b in
  let max_b_y = max_y store b in

  let half_size_a_x = (max_a_x -. min_a_x) *. 0.5 in
  let half_size_a_y = (max_a_y -. min_a_y) *. 0.5 in
  let half_size_b_x = (max_b_x -. min_b_x) *. 0.5 in
  let half_size_b_y = (max_b_y -. min_b_y) *. 0.5 in

  let delta_x = center_b_x -. center_a_x in
  let delta_y = center_b_y -. center_a_y in

  let overlap_x = half_size_a_x +. half_size_b_x -. Float.abs delta_x in
  let overlap_y = half_size_a_y +. half_size_b_y -. Float.abs delta_y in

  if overlap_x <= 0. || overlap_y <= 0. then ()
  else
    (* Choose axis with smallest overlap. *)
    let use_x_axis = overlap_x < overlap_y in

    let normal_x = if use_x_axis then if delta_x < 0. then -1. else 1. else 0. in
    let normal_y = if use_x_axis then 0. else if delta_y < 0. then -1. else 1. in

    let penetration = if use_x_axis then overlap_x else overlap_y in

    (* Relative velocity v_rel = vB - vA. *)
    let vel_a_x = vel_x store a in
    let vel_a_y = vel_y store a in
    let vel_b_x = vel_x store b in
    let vel_b_y = vel_y store b in

    let rel_vel_x = vel_b_x -. vel_a_x in
    let rel_vel_y = vel_b_y -. vel_a_y in

    (* Component along collision normal. *)
    let normal_velocity = (rel_vel_x *. normal_x) +. (rel_vel_y *. normal_y) in

    let min_velocity = 0.01 in

    if normal_velocity > -.min_velocity then (
      (* Position correction only, but still weighted by inv_mass *)
      let inv_mass_a = inv_mass store a in
      let inv_mass_b = inv_mass store b in
      let inv_mass_sum = inv_mass_a +. inv_mass_b in
      if inv_mass_sum <= 0. then ()
      else
        let correction_amount = position_correction *. penetration /. inv_mass_sum in
        let correction_x = correction_amount *. normal_x in
        let correction_y = correction_amount *. normal_y in

        if inv_mass_a > 0. then (
          let dx_a = correction_x *. inv_mass_a in
          let dy_a = correction_y *. inv_mass_a in
          let new_a_x = center_a_x -. dx_a in
          let new_a_y = center_a_y -. dy_a in

          set_pos_x store a new_a_x;
          set_pos_y store a new_a_y;

          set_min_x store a (min_a_x -. dx_a);
          set_max_x store a (max_a_x -. dx_a);
          set_min_y store a (min_a_y -. dy_a);
          set_max_y store a (max_a_y -. dy_a));

        if inv_mass_b > 0. then (
          let dx_b = correction_x *. inv_mass_b in
          let dy_b = correction_y *. inv_mass_b in
          let new_b_x = center_b_x +. dx_b in
          let new_b_y = center_b_y +. dy_b in

          set_pos_x store b new_b_x;
          set_pos_y store b new_b_y;

          set_min_x store b (min_b_x +. dx_b);
          set_max_x store b (max_b_x +. dx_b);
          set_min_y store b (min_b_y +. dy_b);
          set_max_y store b (max_b_y +. dy_b)))
    else
      (* Full resolution: impulse + positional correction. *)
      let inv_mass_a = inv_mass store a in
      let inv_mass_b = inv_mass store b in
      let inv_mass_sum = inv_mass_a +. inv_mass_b in

      if inv_mass_sum <= 0. then ()
      else
        let relative_speed = Float.sqrt ((rel_vel_x *. rel_vel_x) +. (rel_vel_y *. rel_vel_y)) in
        let restitution = if relative_speed < 2.0 then 0.0 else restitution in

        apply_normal_impulse store ~restitution ~a ~b ~normal_x ~normal_y;

        (* Positional correction weighted by inv_mass. *)
        let correction_amount = position_correction *. penetration /. inv_mass_sum in
        let correction_x = correction_amount *. normal_x in
        let correction_y = correction_amount *. normal_y in

        if inv_mass_a > 0. then (
          let dx_a = correction_x *. inv_mass_a in
          let dy_a = correction_y *. inv_mass_a in

          let new_a_x = center_a_x -. dx_a in
          let new_a_y = center_a_y -. dy_a in

          set_pos_x store a new_a_x;
          set_pos_y store a new_a_y;

          set_min_x store a (min_a_x -. dx_a);
          set_max_x store a (max_a_x -. dx_a);
          set_min_y store a (min_a_y -. dy_a);
          set_max_y store a (max_a_y -. dy_a));

        if inv_mass_b > 0. then (
          let dx_b = correction_x *. inv_mass_b in
          let dy_b = correction_y *. inv_mass_b in

          let new_b_x = center_b_x +. dx_b in
          let new_b_y = center_b_y +. dy_b in

          set_pos_x store b new_b_x;
          set_pos_y store b new_b_y;

          set_min_x store b (min_b_x +. dx_b);
          set_max_x store b (max_b_x +. dx_b);
          set_min_y store b (min_b_y +. dy_b);
          set_max_y store b (max_b_y +. dy_b))

let length vx vy = Float.sqrt ((vx *. vx) +. (vy *. vy))

let update_circle_bounds (store : Rb_store.t) (shape_store : Shape_store.t) circle =
  let open Rb_store in
  let circle_radius = Shape_store.circle_radius shape_store (shape_handle store circle) in
  let circle_pos_x = pos_x store circle in
  let circle_pos_y = pos_y store circle in

  set_min_x store circle (circle_pos_x -. circle_radius);
  set_max_x store circle (circle_pos_x +. circle_radius);
  set_min_y store circle (circle_pos_y -. circle_radius);
  set_max_y store circle (circle_pos_y +. circle_radius)

let update_polygon_bounds (store : Rb_store.t) (shape_store : Shape_store.t) poly =
  let open Rb_store in
  let points = Shape_store.polygon_points shape_store (shape_handle store poly) in
  let iso =
    Isometry.create
      (Rot2.of_radians (angle store poly))
      (Vec2.create (pos_x store poly) (pos_y store poly))
  in
  let aabb = Bounded2d.Bounded_polygon.aabb_2d points iso in
  let min = Bounded2d.Aabb2d.min aabb in
  let max = Bounded2d.Aabb2d.max aabb in

  set_min_x store poly min.x;
  set_min_y store poly min.y;
  set_max_x store poly max.x;
  set_max_y store poly max.y

let update_aabb_bounds (store : Rb_store.t) (shape_store : Shape_store.t) aabb =
  let open Rb_store in
  let cx = pos_x store aabb in
  let cy = pos_y store aabb in
  let half_w = Shape_store.aabb_half_width shape_store (shape_handle store aabb) in
  let half_h = Shape_store.aabb_half_height shape_store (shape_handle store aabb) in

  set_min_x store aabb (cx -. half_w);
  set_max_x store aabb (cx +. half_w);
  set_min_y store aabb (cy -. half_h);
  set_max_y store aabb (cy +. half_h)

let resolve_polygon_circle
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    ~polygon
    ~circle
    ~restitution
    ~position_correction =
  let open Rb_store in
  if shape_kind store polygon <> 2 || shape_kind store circle <> 0 then ()
  else
    let circle_radius = Shape_store.circle_radius shape_store (shape_handle store circle) in
    let circle_pos_x = pos_x store circle in
    let circle_pos_y = pos_y store circle in
    let poly_pos_x = pos_x store polygon in
    let poly_pos_y = pos_y store polygon in
    let poly_angle = angle store polygon in
    let poly_handle = shape_handle store polygon in
    let poly_points_x = Shape_store.polygon_storage_x shape_store in
    let poly_points_y = Shape_store.polygon_storage_y shape_store in
    let poly_offset = Shape_store.polygon_offset shape_store poly_handle in
    let poly_count = Shape_store.polygon_count shape_store poly_handle in

    if poly_count < 3 then ()
    else
      let c = Float.cos poly_angle in
      let s = Float.sin poly_angle in

      (* World circle center -> polygon local. *)
      let rel_x = circle_pos_x -. poly_pos_x in
      let rel_y = circle_pos_y -. poly_pos_y in
      let circle_local_x = (rel_x *. c) +. (rel_y *. s) in
      let circle_local_y = (-.rel_x *. s) +. (rel_y *. c) in

      (* Determine winding to compute inward normals consistently. *)
      let signed_area2 = ref 0.0 in
      for i = 0 to poly_count - 1 do
        let j = if i = poly_count - 1 then 0 else i + 1 in
        let i_idx = poly_offset + i in
        let j_idx = poly_offset + j in
        signed_area2 :=
          !signed_area2
          +. ((poly_points_x.(i_idx) *. poly_points_y.(j_idx))
             -. (poly_points_x.(j_idx) *. poly_points_y.(i_idx)))
      done;
      let ccw = !signed_area2 >= 0.0 in
      let eps = 1e-8 in

      let outside = ref false in
      let min_inward_dist = ref Float.max_float in
      let inside_out_nx = ref 1.0 in
      let inside_out_ny = ref 0.0 in

      let closest_dist_sq = ref Float.max_float in
      let closest_dx = ref 0.0 in
      let closest_dy = ref 0.0 in

      for i = 0 to poly_count - 1 do
        let j = if i = poly_count - 1 then 0 else i + 1 in
        let i_idx = poly_offset + i in
        let j_idx = poly_offset + j in

        let ax = poly_points_x.(i_idx) in
        let ay = poly_points_y.(i_idx) in
        let bx = poly_points_x.(j_idx) in
        let by = poly_points_y.(j_idx) in

        let edge_x = bx -. ax in
        let edge_y = by -. ay in
        let edge_len_sq = (edge_x *. edge_x) +. (edge_y *. edge_y) in

        if edge_len_sq > eps then (
          let edge_len = Float.sqrt edge_len_sq in

          let in_nx, in_ny =
            if ccw then (-.edge_y /. edge_len, edge_x /. edge_len)
            else (edge_y /. edge_len, -.edge_x /. edge_len)
          in

          let dist_in = ((circle_local_x -. ax) *. in_nx) +. ((circle_local_y -. ay) *. in_ny) in

          if dist_in < 0.0 then outside := true;

          if dist_in < !min_inward_dist then (
            min_inward_dist := dist_in;
            (* out normal points from polygon toward circle when center is inside. *)
            inside_out_nx := -.in_nx;
            inside_out_ny := -.in_ny);

          let ap_x = circle_local_x -. ax in
          let ap_y = circle_local_y -. ay in

          let t = ((ap_x *. edge_x) +. (ap_y *. edge_y)) /. edge_len_sq in
          let t = if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t in

          let closest_x = ax +. (edge_x *. t) in
          let closest_y = ay +. (edge_y *. t) in

          let dx = circle_local_x -. closest_x in
          let dy = circle_local_y -. closest_y in
          let dist_sq = (dx *. dx) +. (dy *. dy) in

          if dist_sq < !closest_dist_sq then (
            closest_dist_sq := dist_sq;
            closest_dx := dx;
            closest_dy := dy))
      done;

      let has_collision = ref false in
      let local_nx = ref 1.0 in
      let local_ny = ref 0.0 in
      let penetration = ref 0.0 in

      if !outside then (
        if !closest_dist_sq <= circle_radius *. circle_radius then (
          let dist = Float.sqrt !closest_dist_sq in

          if dist > eps then (
            local_nx := !closest_dx /. dist;
            local_ny := !closest_dy /. dist)
          else (
            local_nx := !inside_out_nx;
            local_ny := !inside_out_ny);
          penetration := circle_radius -. dist;
          has_collision := !penetration > 0.0))
      else (
        local_nx := !inside_out_nx;
        local_ny := !inside_out_ny;
        penetration := circle_radius +. !min_inward_dist;
        has_collision := !penetration > 0.0);

      if not !has_collision then ()
      else
        let normal_x = (!local_nx *. c) -. (!local_ny *. s) in
        let normal_y = (!local_nx *. s) +. (!local_ny *. c) in

        (* polygon -> circle normal *)
        apply_normal_impulse ~restitution store ~a:polygon ~b:circle ~normal_x ~normal_y;

        let poly_inv_mass = inv_mass store polygon in
        let circle_inv_mass = inv_mass store circle in
        let inv_mass_sum = poly_inv_mass +. circle_inv_mass in

        if inv_mass_sum <= 0.0 then ()
        else
          let correction_amount = position_correction *. !penetration /. inv_mass_sum in
          let correction_x = correction_amount *. normal_x in
          let correction_y = correction_amount *. normal_y in

          if poly_inv_mass > 0.0 then (
            set_pos_x store polygon (poly_pos_x -. (correction_x *. poly_inv_mass));
            set_pos_y store polygon (poly_pos_y -. (correction_y *. poly_inv_mass));
            update_polygon_bounds store shape_store polygon);

          if circle_inv_mass > 0.0 then (
            set_pos_x store circle (circle_pos_x +. (correction_x *. circle_inv_mass));
            set_pos_y store circle (circle_pos_y +. (correction_y *. circle_inv_mass));
            update_circle_bounds store shape_store circle)

let resolve_polygon_aabb
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    ~polygon
    ~aabb
    ~restitution
    ~position_correction =
  let open Rb_store in
  if shape_kind store polygon <> 2 || shape_kind store aabb <> 1 then ()
  else
    let poly_pos_x = pos_x store polygon in
    let poly_pos_y = pos_y store polygon in
    let poly_angle = angle store polygon in
    let poly_handle = shape_handle store polygon in

    let poly_points_x = Shape_store.polygon_storage_x shape_store in
    let poly_points_y = Shape_store.polygon_storage_y shape_store in
    let poly_offset = Shape_store.polygon_offset shape_store poly_handle in
    let poly_count = Shape_store.polygon_count shape_store poly_handle in

    let aabb_min_x = min_x store aabb in
    let aabb_max_x = max_x store aabb in
    let aabb_min_y = min_y store aabb in
    let aabb_max_y = max_y store aabb in

    if poly_count < 3 then ()
    else
      let c = Float.cos poly_angle in
      let s = Float.sin poly_angle in
      let aabb_center_x = (aabb_min_x +. aabb_max_x) *. 0.5 in
      let aabb_center_y = (aabb_min_y +. aabb_max_y) *. 0.5 in

      let project_aabb_on_axis ~axis_x ~axis_y =
        let cx = aabb_center_x in
        let cy = aabb_center_y in
        let ex = (aabb_max_x -. aabb_min_x) *. 0.5 in
        let ey = (aabb_max_y -. aabb_min_y) *. 0.5 in
        let center_proj = (axis_x *. cx) +. (axis_y *. cy) in
        let radius = (Float.abs axis_x *. ex) +. (Float.abs axis_y *. ey) in
        (center_proj -. radius, center_proj +. radius)
      in

      let project_polygon_on_axis ~axis_x ~axis_y =
        let first = poly_offset in
        let lx = poly_points_x.(first) in
        let ly = poly_points_y.(first) in

        let wx = poly_pos_x +. ((lx *. c) -. (ly *. s)) in
        let wy = poly_pos_y +. ((lx *. s) +. (ly *. c)) in
        let p0 = (axis_x *. wx) +. (axis_y *. wy) in
        let min_p = ref p0 in
        let max_p = ref p0 in

        for i = 1 to poly_count - 1 do
          let idx = poly_offset + i in
          let lx = poly_points_x.(idx) in
          let ly = poly_points_y.(idx) in
          let wx = poly_pos_x +. ((lx *. c) -. (ly *. s)) in
          let wy = poly_pos_y +. ((lx *. s) +. (ly *. c)) in
          let p = (axis_x *. wx) +. (axis_y *. wy) in
          if p < !min_p then min_p := p;
          if p > !max_p then max_p := p
        done;
        (!min_p, !max_p)
      in

      let separated = ref false in
      let min_overlap = ref Float.max_float in
      let best_nx = ref 1.0 in
      let best_ny = ref 0.0 in
      let eps = 1e-12 in

      let test_axis axis_x axis_y =
        if not !separated then
          let len_sq = (axis_x *. axis_x) +. (axis_y *. axis_y) in
          if len_sq > eps then
            let inv_len = 1.0 /. Float.sqrt len_sq in
            let nx = axis_x *. inv_len in
            let ny = axis_y *. inv_len in

            let a_min, a_max = project_aabb_on_axis ~axis_x:nx ~axis_y:ny in
            let p_min, p_max = project_polygon_on_axis ~axis_x:nx ~axis_y:ny in
            let overlap = Float.min a_max p_max -. Float.max a_min p_min in

            if overlap <= 0.0 then separated := true
            else if overlap < !min_overlap then (
              min_overlap := overlap;
              best_nx := nx;
              best_ny := ny)
      in

      (* AABB face normals *)
      test_axis 1.0 0.0;
      test_axis 0.0 1.0;

      (* Polygon edge normals *)
      for i = 0 to poly_count - 1 do
        let j = if i = poly_count - 1 then 0 else i + 1 in
        let i_idx = poly_offset + i in
        let j_idx = poly_offset + j in
        let e_lx = poly_points_x.(j_idx) -. poly_points_x.(i_idx) in
        let e_ly = poly_points_y.(j_idx) -. poly_points_y.(i_idx) in
        let e_wx = (e_lx *. c) -. (e_ly *. s) in
        let e_wy = (e_lx *. s) +. (e_ly *. c) in
        test_axis (-.e_wy) e_wx
      done;

      if !separated then ()
      else
        let normal_x = ref !best_nx in
        let normal_y = ref !best_ny in
        let center_delta_x = aabb_center_x -. poly_pos_x in
        let center_delta_y = aabb_center_y -. poly_pos_y in

        if (center_delta_x *. !normal_x) +. (center_delta_y *. !normal_y) < 0.0 then (
          normal_x := -. !normal_x;
          normal_y := -. !normal_y);

        (* polygon -> aabb normal *)
        apply_normal_impulse ~restitution store ~a:polygon ~b:aabb ~normal_x:!normal_x
          ~normal_y:!normal_y;

        let poly_inv_mass = inv_mass store polygon in
        let aabb_inv_mass = inv_mass store aabb in
        let inv_mass_sum = poly_inv_mass +. aabb_inv_mass in

        if inv_mass_sum <= 0.0 then ()
        else
          let correction_amount = position_correction *. !min_overlap /. inv_mass_sum in
          let correction_x = correction_amount *. !normal_x in
          let correction_y = correction_amount *. !normal_y in

          if poly_inv_mass > 0.0 then (
            set_pos_x store polygon (poly_pos_x -. (correction_x *. poly_inv_mass));
            set_pos_y store polygon (poly_pos_y -. (correction_y *. poly_inv_mass));
            update_polygon_bounds store shape_store polygon);

          if aabb_inv_mass > 0.0 then (
            set_pos_x store aabb (aabb_center_x +. (correction_x *. aabb_inv_mass));
            set_pos_y store aabb (aabb_center_y +. (correction_y *. aabb_inv_mass));
            update_aabb_bounds store shape_store aabb)

let resolve_circle_aabb
    ?(friction = 0.4)
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    ~circle
    ~aabb
    ~restitution
    ~position_correction =
  let open Rb_store in
  if shape_kind store circle <> 0 || shape_kind store aabb <> 1 then ()
  else
    (* AABB props *)
    let aabb_min_x = min_x store aabb in
    let aabb_min_y = min_y store aabb in
    let aabb_max_x = max_x store aabb in
    let aabb_max_y = max_y store aabb in
    let aabb_vel_x = vel_x store aabb in
    let aabb_vel_y = vel_y store aabb in
    let aabb_inv_mass = inv_mass store aabb in

    (* Circle props *)
    let circle_radius = Shape_store.circle_radius shape_store (shape_handle store circle) in
    let circle_pos_x = pos_x store circle in
    let circle_pos_y = pos_y store circle in
    let circle_vel_x = vel_x store circle in
    let circle_vel_y = vel_y store circle in
    let circle_inv_mass = inv_mass store circle in

    (* Closest point on AABB to circle center *)
    let closest_x = Float.max aabb_min_x (Float.min circle_pos_x aabb_max_x) in
    let closest_y = Float.max aabb_min_y (Float.min circle_pos_y aabb_max_y) in

    let distance_x = circle_pos_x -. closest_x in
    let distance_y = circle_pos_y -. closest_y in
    let distance_mag = length distance_x distance_y in
    let epsilon = 0.0001 in

    (* No collision *)
    if distance_mag > circle_radius then ()
    else (* Inline normal computation, allocation-free *)
      let dx = distance_x in
      let dy = distance_y in
      let dmag = distance_mag in

      (* mutable  vars to populate *)
      let normal_x = ref 0.0 in
      let normal_y = ref 0.0 in
      let dist_x = ref dx in
      let dist_y = ref dy in
      let dist_mag = ref dmag in

      (if dmag < epsilon then (
         let aabb_center_x = (aabb_min_x +. aabb_max_x) *. 0.5 in
         let aabb_center_y = (aabb_min_y +. aabb_max_y) *. 0.5 in

         let dx' = circle_pos_x -. aabb_center_x in
         let dy' = circle_pos_y -. aabb_center_y in
         let dmag' = length dx' dy' in

         dist_x := dx';
         dist_y := dy';
         dist_mag := dmag';

         if dmag' < epsilon then (
           normal_x := 0.0;
           normal_y := 1.0)
         else
           let inv = 1.0 /. dmag' in
           normal_x := dx' *. inv;
           normal_y := dy' *. inv)
       else
         let inv = 1.0 /. dmag in
         normal_x := dx *. inv;
         normal_y := dy *. inv);

      (* Final scalar values *)
      let distance_mag = !dist_mag in
      let normal_x = !normal_x in
      let normal_y = !normal_y in

      let penetration_depth = circle_radius -. distance_mag in
      if penetration_depth <= 0.0 then ()
      else
        let rel_vel_x = circle_vel_x -. aabb_vel_x in
        let rel_vel_y = circle_vel_y -. aabb_vel_y in
        let impact_speed = dot ~ax:rel_vel_x ~ay:rel_vel_y ~bx:normal_x ~by:normal_y in

        let total_inv_mass = circle_inv_mass +. aabb_inv_mass in
        if total_inv_mass <= 0.0 then ()
        else
          let correction_amount = position_correction *. penetration_depth /. total_inv_mass in
          let corr_x = normal_x *. correction_amount in
          let corr_y = normal_y *. correction_amount in

          let new_circle_x =
            if circle_inv_mass > 0.0 then circle_pos_x +. (corr_x *. circle_inv_mass)
            else circle_pos_x
          in
          let new_circle_y =
            if circle_inv_mass > 0.0 then circle_pos_y +. (corr_y *. circle_inv_mass)
            else circle_pos_y
          in

          if circle_inv_mass > 0.0 then (
            set_pos_x store circle new_circle_x;
            set_pos_y store circle new_circle_y);

          if aabb_inv_mass > 0.0 then (
            set_pos_x store aabb (pos_x store aabb -. (corr_x *. aabb_inv_mass));
            set_pos_y store aabb (pos_y store aabb -. (corr_y *. aabb_inv_mass)));

          (* Update bounds using corrected positions *)
          set_min_x store circle (new_circle_x -. circle_radius);
          set_max_x store circle (new_circle_x +. circle_radius);
          set_min_y store circle (new_circle_y -. circle_radius);
          set_max_y store circle (new_circle_y +. circle_radius);

          let new_aabb_x = pos_x store aabb in
          let new_aabb_y = pos_y store aabb in
          let half_w = Shape_store.aabb_half_width shape_store (shape_handle store aabb) in
          let half_h = Shape_store.aabb_half_height shape_store (shape_handle store aabb) in

          set_min_x store aabb (new_aabb_x -. half_w);
          set_max_x store aabb (new_aabb_x +. half_w);
          set_min_y store aabb (new_aabb_y -. half_h);
          set_max_y store aabb (new_aabb_y +. half_h);

          (* Apply impulse/friction only when approaching. *)
          if impact_speed < 0.0 then (
            let normal_impulse_mag = -.(1.0 +. restitution) *. impact_speed /. total_inv_mass in
            let impulse_x = normal_x *. normal_impulse_mag in
            let impulse_y = normal_y *. normal_impulse_mag in

            if circle_inv_mass > 0.0 then (
              set_vel_x store circle (circle_vel_x +. (impulse_x *. circle_inv_mass));
              set_vel_y store circle (circle_vel_y +. (impulse_y *. circle_inv_mass)));
            if aabb_inv_mass > 0.0 then (
              set_vel_x store aabb (aabb_vel_x -. (impulse_x *. aabb_inv_mass));
              set_vel_y store aabb (aabb_vel_y -. (impulse_y *. aabb_inv_mass)));

            (* Friction *)
            let tangent_x = -.normal_y in
            let tangent_y = normal_x in
            let tangent_speed = dot ~ax:rel_vel_x ~ay:rel_vel_y ~bx:tangent_x ~by:tangent_y in

            let friction_impulse =
              let raw = -.tangent_speed /. total_inv_mass in
              let max_friction = friction *. Float.abs normal_impulse_mag in
              if raw > max_friction then max_friction
              else if raw < -.max_friction then -.max_friction
              else raw
            in

            let friction_x = tangent_x *. friction_impulse in
            let friction_y = tangent_y *. friction_impulse in

            if circle_inv_mass > 0.0 then (
              set_vel_x store circle (vel_x store circle +. (friction_x *. circle_inv_mass));
              set_vel_y store circle (vel_y store circle +. (friction_y *. circle_inv_mass)));

            if aabb_inv_mass > 0.0 then (
              set_vel_x store aabb (vel_x store aabb -. (friction_x *. aabb_inv_mass));
              set_vel_y store aabb (vel_y store aabb -. (friction_y *. aabb_inv_mass))))

let resolve
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    ~a
    ~b
    ~restitution
    ~position_correction =
  let open Rb_store in
  let is_sensor_pair = is_sensor store a || is_sensor store b in
  let both_immovable = inv_mass store a = 0. && inv_mass store b = 0. in
  if is_sensor_pair || both_immovable then ()
  else
    match (shape_kind store a, shape_kind store b) with
    | 0, 0 -> resolve_circle_circle ~restitution ~position_correction store shape_store ~a ~b
    | 1, 1 -> resolve_aabb_aabb store ~a ~b ~restitution ~position_correction
    | 0, 1 ->
        resolve_circle_aabb store shape_store ~circle:a ~aabb:b ~restitution ~position_correction
    | 1, 0 ->
        resolve_circle_aabb store shape_store ~circle:b ~aabb:a ~restitution ~position_correction
    | 2, 0 ->
        resolve_polygon_circle store shape_store ~polygon:a ~circle:b ~restitution
          ~position_correction
    | 0, 2 ->
        resolve_polygon_circle store shape_store ~polygon:b ~circle:a ~restitution
          ~position_correction
    | 2, 1 ->
        resolve_polygon_aabb store shape_store ~polygon:a ~aabb:b ~restitution ~position_correction
    | 1, 2 ->
        resolve_polygon_aabb store shape_store ~polygon:b ~aabb:a ~restitution ~position_correction
    | _ -> ()

let resolve_collisions ?(restitution = 0.2) ?(position_correction = 0.8) store shape_store np =
  let ids1, ids2 = Narrow_phase.collisions_view np in
  if Dynarray.length ids1 != Dynarray.length ids2 then ()
  else
    Dynarray.iteri
      (fun idx id ->
        let id2 = Dynarray.get ids2 idx in
        resolve store shape_store ~a:id ~b:id2 ~restitution ~position_correction)
      ids1
