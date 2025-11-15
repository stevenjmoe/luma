let squared f1 f2 = (f1 *. f1) +. (f2 *. f2)

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

let resolve_circle_circle (store : Rb_store.t) ~a ~b ~restitution ~position_correction =
  (* Only handle circle-circle contacts (shape = 0). *)
  if store.shape.(a) <> 0 || store.shape.(b) <> 0 then ()
  else
    (* Load positions and radii. *)
    let pos_a_x = store.pos_x.(a) in
    let pos_a_y = store.pos_y.(a) in
    let pos_b_x = store.pos_x.(b) in
    let pos_b_y = store.pos_y.(b) in
    let radius_a = store.radius.(a) in
    let radius_b = store.radius.(b) in

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
      (* Objects moving apart or contact too “soft”: only position correction, no impulse. *)
      let half_penetration = penetration *. 0.5 in

      (* Move A opposite normal, B along normal, ignoring inv_mass. *)
      let move_x = normal_x *. half_penetration in
      let move_y = normal_y *. half_penetration in

      let inv_mass_a = store.inv_mass.(a) in
      let inv_mass_b = store.inv_mass.(b) in

      if inv_mass_a > 0. then (
        let new_a_x = center_a_x -. move_x in
        let new_a_y = center_a_y -. move_y in

        store.pos_x.(a) <- new_a_x;
        store.pos_y.(a) <- new_a_y;

        store.min_x.(a) <- min_a_x -. move_x;
        store.max_x.(a) <- max_a_x -. move_x;
        store.min_y.(a) <- min_a_y -. move_y;
        store.max_y.(a) <- max_a_y -. move_y);

      if inv_mass_b > 0. then (
        let new_b_x = center_b_x +. move_x in
        let new_b_y = center_b_y +. move_y in

        store.pos_x.(b) <- new_b_x;
        store.pos_y.(b) <- new_b_y;

        store.min_x.(b) <- min_b_x +. move_x;
        store.max_x.(b) <- max_b_x +. move_x;
        store.min_y.(b) <- min_b_y +. move_y;
        store.max_y.(b) <- max_b_y +. move_y))
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

let resolve (store : Rb_store.t) ~a ~b ~restitution ~position_correction =
  match (store.shape.(a), store.shape.(b)) with
  | 0, 0 -> resolve_circle_circle ~restitution ~position_correction store ~a ~b
  | 1, 1 -> resolve_aabb_aabb store ~a ~b ~restitution ~position_correction
  | _ -> ()

let resolve_collisions ?(restitution = 0.2) ?(position_correction = 0.8) store np =
  let ids1, ids2 = Narrow_phase.collisions_view np in
  if Dynarray.length ids1 != Dynarray.length ids2 then ()
  else
    Dynarray.iteri
      (fun idx id ->
        let id2 = Dynarray.get ids2 idx in
        resolve store ~a:id ~b:id2 ~restitution ~position_correction)
      ids1
