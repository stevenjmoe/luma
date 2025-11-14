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

let resolve_circle_circle
    ?(restitution = 0.2)
    ?(position_correction = 0.4)
    (store : Rb_store.t)
    ~a
    ~b =
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

let resolve (store : Rb_store.t) ~a ~b ~restitution ~position_correction =
  match (store.shape.(a), store.shape.(b)) with
  | 0, 0 -> resolve_circle_circle ~restitution ~position_correction store ~a ~b
  | _ -> ()

let resolve_collisions ?(restitution = 0.2) ?(position_correction = 0.2) store np =
  let ids1, ids2 = Narrow_phase.collisions_view np in
  if Dynarray.length ids1 != Dynarray.length ids2 then ()
  else
    Dynarray.iteri
      (fun idx id ->
        let id2 = Dynarray.get ids2 idx in
        resolve store ~a:id ~b:id2 ~restitution ~position_correction)
      ids1
