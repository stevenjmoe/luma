(* TODO: Kinematic/static bodies should be guaranteed to have an inv_mass of 0, but there should be guards in place here to check *)
let squared f1 f2 = (f1 *. f1) +. (f2 *. f2)
let dot ~ax ~ay ~bx ~by = (ax *. bx) +. (ay *. by)

let apply_normal_impulse (store : Rb_store.t) ~restitution ~a ~b ~normal_x ~normal_y =
  let vel_a_x = store.vel_x.(a) in
  let vel_a_y = store.vel_y.(a) in
  let vel_b_x = store.vel_x.(b) in
  let vel_b_y = store.vel_y.(b) in
  let inv_mass_a = store.inv_mass.(a) in
  let inv_mass_b = store.inv_mass.(b) in

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
      store.vel_x.(a) <- vel_a_x -. (impulse_x *. inv_mass_a);
      store.vel_y.(a) <- vel_a_y -. (impulse_y *. inv_mass_a);
      store.vel_x.(b) <- vel_b_x +. (impulse_x *. inv_mass_b);
      store.vel_y.(b) <- vel_b_y +. (impulse_y *. inv_mass_b)

let resolve_circle_circle
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    ~a
    ~b
    ~restitution
    ~position_correction =
  (* Only handle circle-circle contacts (shape = 0). *)
  if store.shape.(a) <> 0 || store.shape.(b) <> 0 then ()
  else
    (* Load positions and radii. *)
    let pos_a_x = store.pos_x.(a) in
    let pos_a_y = store.pos_y.(a) in
    let pos_b_x = store.pos_x.(b) in
    let pos_b_y = store.pos_y.(b) in
    let radius_a = Shape_store.circle_radius shape_store store.shape_handle.(a) in
    let radius_b = Shape_store.circle_radius shape_store store.shape_handle.(b) in

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
      let inv_mass_a = store.inv_mass.(a) in
      let inv_mass_b = store.inv_mass.(b) in
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
          store.pos_x.(a) <- pos_a_x';
          store.pos_y.(a) <- pos_a_y';
          store.min_x.(a) <- pos_a_x' -. radius_a;
          store.max_x.(a) <- pos_a_x' +. radius_a;
          store.min_y.(a) <- pos_a_y' -. radius_a;
          store.max_y.(a) <- pos_a_y' +. radius_a);

        if inv_mass_b > 0. then (
          let pos_b_x' = pos_b_x +. (correction_x *. inv_mass_b) in
          let pos_b_y' = pos_b_y +. (correction_y *. inv_mass_b) in
          store.pos_x.(b) <- pos_b_x';
          store.pos_y.(b) <- pos_b_y';
          store.min_x.(b) <- pos_b_x' -. radius_b;
          store.max_x.(b) <- pos_b_x' +. radius_b;
          store.min_y.(b) <- pos_b_y' -. radius_b;
          store.max_y.(b) <- pos_b_y' +. radius_b))

let resolve_aabb_aabb (store : Rb_store.t) ~a ~b ~restitution ~position_correction =
  let center_a_x = store.pos_x.(a) in
  let center_a_y = store.pos_y.(a) in
  let center_b_x = store.pos_x.(b) in
  let center_b_y = store.pos_y.(b) in

  let min_a_x = store.min_x.(a) in
  let max_a_x = store.max_x.(a) in
  let min_a_y = store.min_y.(a) in
  let max_a_y = store.max_y.(a) in

  let min_b_x = store.min_x.(b) in
  let max_b_x = store.max_x.(b) in
  let min_b_y = store.min_y.(b) in
  let max_b_y = store.max_y.(b) in

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
    let vel_a_x = store.vel_x.(a) in
    let vel_a_y = store.vel_y.(a) in
    let vel_b_x = store.vel_x.(b) in
    let vel_b_y = store.vel_y.(b) in

    let rel_vel_x = vel_b_x -. vel_a_x in
    let rel_vel_y = vel_b_y -. vel_a_y in

    (* Component along collision normal. *)
    let normal_velocity = (rel_vel_x *. normal_x) +. (rel_vel_y *. normal_y) in

    let min_velocity = 0.01 in

    if normal_velocity > -.min_velocity then (
      (* Position correction only, but still weighted by inv_mass *)
      let inv_mass_a = store.inv_mass.(a) in
      let inv_mass_b = store.inv_mass.(b) in
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

          store.pos_x.(a) <- new_a_x;
          store.pos_y.(a) <- new_a_y;

          store.min_x.(a) <- min_a_x -. dx_a;
          store.max_x.(a) <- max_a_x -. dx_a;
          store.min_y.(a) <- min_a_y -. dy_a;
          store.max_y.(a) <- max_a_y -. dy_a);

        if inv_mass_b > 0. then (
          let dx_b = correction_x *. inv_mass_b in
          let dy_b = correction_y *. inv_mass_b in
          let new_b_x = center_b_x +. dx_b in
          let new_b_y = center_b_y +. dy_b in

          store.pos_x.(b) <- new_b_x;
          store.pos_y.(b) <- new_b_y;

          store.min_x.(b) <- min_b_x +. dx_b;
          store.max_x.(b) <- max_b_x +. dx_b;
          store.min_y.(b) <- min_b_y +. dy_b;
          store.max_y.(b) <- max_b_y +. dy_b))
    else
      (* Full resolution: impulse + positional correction. *)
      let inv_mass_a = store.inv_mass.(a) in
      let inv_mass_b = store.inv_mass.(b) in
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

          store.pos_x.(a) <- new_a_x;
          store.pos_y.(a) <- new_a_y;

          store.min_x.(a) <- min_a_x -. dx_a;
          store.max_x.(a) <- max_a_x -. dx_a;
          store.min_y.(a) <- min_a_y -. dy_a;
          store.max_y.(a) <- max_a_y -. dy_a);

        if inv_mass_b > 0. then (
          let dx_b = correction_x *. inv_mass_b in
          let dy_b = correction_y *. inv_mass_b in

          let new_b_x = center_b_x +. dx_b in
          let new_b_y = center_b_y +. dy_b in

          store.pos_x.(b) <- new_b_x;
          store.pos_y.(b) <- new_b_y;

          store.min_x.(b) <- min_b_x +. dx_b;
          store.max_x.(b) <- max_b_x +. dx_b;
          store.min_y.(b) <- min_b_y +. dy_b;
          store.max_y.(b) <- max_b_y +. dy_b)

let length vx vy = Float.sqrt ((vx *. vx) +. (vy *. vy))

let resolve_circle_aabb
    ?(friction = 0.4)
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    ~circle
    ~aabb
    ~restitution
    ~position_correction =
  if store.shape.(circle) <> 0 || store.shape.(aabb) <> 1 then ()
  else
    (* AABB props *)
    let aabb_min_x = store.min_x.(aabb) in
    let aabb_min_y = store.min_y.(aabb) in
    let aabb_max_x = store.max_x.(aabb) in
    let aabb_max_y = store.max_y.(aabb) in
    let aabb_vel_x = store.vel_x.(aabb) in
    let aabb_vel_y = store.vel_y.(aabb) in
    let aabb_inv_mass = store.inv_mass.(aabb) in

    (* Circle props *)
    let circle_radius = Shape_store.circle_radius shape_store store.shape_handle.(circle) in
    let circle_pos_x = store.pos_x.(circle) in
    let circle_pos_y = store.pos_y.(circle) in
    let circle_vel_x = store.vel_x.(circle) in
    let circle_vel_y = store.vel_y.(circle) in
    let circle_inv_mass = store.inv_mass.(circle) in

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
        else if impact_speed >= 0.0 then (
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
            store.pos_x.(circle) <- new_circle_x;
            store.pos_y.(circle) <- new_circle_y);

          if aabb_inv_mass > 0.0 then (
            store.pos_x.(aabb) <- store.pos_x.(aabb) -. (corr_x *. aabb_inv_mass);
            store.pos_y.(aabb) <- store.pos_y.(aabb) -. (corr_y *. aabb_inv_mass));

          (* Update circle bounds using new position *)
          store.min_x.(circle) <- new_circle_x -. circle_radius;
          store.max_x.(circle) <- new_circle_x +. circle_radius;
          store.min_y.(circle) <- new_circle_y -. circle_radius;
          store.max_y.(circle) <- new_circle_y +. circle_radius;

          let new_aabb_x = store.pos_x.(aabb) in
          let new_aabb_y = store.pos_y.(aabb) in
          let half_w = Shape_store.aabb_half_width shape_store store.shape_handle.(aabb) in
          let half_h = Shape_store.aabb_half_height shape_store store.shape_handle.(aabb) in

          store.min_x.(aabb) <- new_aabb_x -. half_w;
          store.max_x.(aabb) <- new_aabb_x +. half_w;
          store.min_y.(aabb) <- new_aabb_y -. half_h;
          store.max_y.(aabb) <- new_aabb_y +. half_h)
        else
          let normal_impulse_mag = -.(1.0 +. restitution) *. impact_speed /. total_inv_mass in
          let impulse_x = normal_x *. normal_impulse_mag in
          let impulse_y = normal_y *. normal_impulse_mag in

          if circle_inv_mass > 0.0 then (
            store.vel_x.(circle) <- circle_vel_x +. (impulse_x *. circle_inv_mass);
            store.vel_y.(circle) <- circle_vel_y +. (impulse_y *. circle_inv_mass));
          if aabb_inv_mass > 0.0 then (
            store.vel_x.(aabb) <- aabb_vel_x -. (impulse_x *. aabb_inv_mass);
            store.vel_y.(aabb) <- aabb_vel_y -. (impulse_y *. aabb_inv_mass));

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
            store.vel_x.(circle) <- store.vel_x.(circle) +. (friction_x *. circle_inv_mass);
            store.vel_y.(circle) <- store.vel_y.(circle) +. (friction_y *. circle_inv_mass));

          if aabb_inv_mass > 0.0 then (
            store.vel_x.(aabb) <- store.vel_x.(aabb) -. (friction_x *. aabb_inv_mass);
            store.vel_y.(aabb) <- store.vel_y.(aabb) -. (friction_y *. aabb_inv_mass))

let resolve
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    ~a
    ~b
    ~restitution
    ~position_correction =
  let both_immovable = store.inv_mass.(a) = 0. && store.inv_mass.(b) = 0. in
  if both_immovable then ()
  else
    match (store.shape.(a), store.shape.(b)) with
    | 0, 0 -> resolve_circle_circle ~restitution ~position_correction store shape_store ~a ~b
    | 1, 1 -> resolve_aabb_aabb store ~a ~b ~restitution ~position_correction
    | 0, 1 ->
        resolve_circle_aabb store shape_store ~circle:a ~aabb:b ~restitution ~position_correction
    | 1, 0 ->
        resolve_circle_aabb store shape_store ~circle:b ~aabb:a ~restitution ~position_correction
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
