module Make (L : Luma.S) = struct
  let get_rigid_body entity index =
    let open Rigid_body in
    match Rb_store.Index.row_of_entity index entity with None -> None | Some row -> Some row

  let derive_kinematic_velocity (store : Rb_store.t) ~row ~curr_x ~curr_y ~dt =
    if dt > 0. then (
      let prev_x = store.prev_pos_x.(row) in
      let prev_y = store.prev_pos_y.(row) in
      store.vel_x.(row) <- (curr_x -. prev_x) /. dt;
      store.vel_y.(row) <- (curr_y -. prev_y) /. dt;
      store.prev_pos_x.(row) <- curr_x;
      store.prev_pos_y.(row) <- curr_y)

  let sync_rigid_bodies () =
    L.System.make_with_resources
      ~components:L.Query.Component.(Required (module Rigid_body.C) & End)
      ~resources:
        L.Query.Resource.(
          Resource (module Rb_store.R)
          & Resource (module Rb_store.Index.R)
          & Resource (module L.Time.R)
          & End)
      "sync_rigid_bodies"
      (fun w _ e (rb_store, (index, (time, _))) ->
        let current = Hashtbl.create 1024 in

        List.iter (fun (entity, (rb, _)) -> Hashtbl.replace current entity true) e;

        (* Add to or update rigid body store *)
        List.iter
          (fun (entity, ((rb : Rigid_body.t), _)) ->
            let open Rigid_body in
            match get_rigid_body entity index with
            | None ->
                let row = Rb_store.add rb_store rb in
                Rb_store.Index.on_add index ~entity ~row
            | Some row -> (
                let open L.Math.Bounded2d in
                rb_store.inv_mass.(row) <- rb.inv_mass;
                rb_store.damping.(row) <- rb.damping;
                rb_store.body_type.(row) <- Rigid_body.encode_body_type rb.body_type;
                rb_store.shape.(row) <- Rigid_body.encode_shape rb.shape;
                let is_kinematic = rb.body_type = Kinematic in

                match rb.shape with
                | Circle c ->
                    let radius = Bounding_circle.radius c in
                    rb_store.radius.(row) <- radius;

                    if is_kinematic then (
                      let curr_x = rb.pos.x in
                      let curr_y = rb.pos.y in
                      let dt = L.Time.dt time in

                      rb_store.pos_x.(row) <- curr_x;
                      rb_store.pos_y.(row) <- curr_y;

                      derive_kinematic_velocity rb_store ~row ~curr_x ~curr_y ~dt);

                    let center_x = rb_store.pos_x.(row) in
                    let center_y = rb_store.pos_y.(row) in

                    rb_store.min_x.(row) <- center_x -. radius;
                    rb_store.min_y.(row) <- center_y -. radius;
                    rb_store.max_x.(row) <- center_x +. radius;
                    rb_store.max_y.(row) <- center_y +. radius
                | Aabb a ->
                    let half_size = Aabb2d.half_size a in
                    rb_store.box_hw.(row) <- half_size.x;
                    rb_store.box_hh.(row) <- half_size.y;

                    if is_kinematic then (
                      let curr_x = rb.pos.x in
                      let curr_y = rb.pos.y in
                      let dt = L.Time.dt time in

                      rb_store.pos_x.(row) <- curr_x;
                      rb_store.pos_y.(row) <- curr_y;

                      derive_kinematic_velocity rb_store ~row ~curr_x ~curr_y ~dt);

                    let center_x = rb_store.pos_x.(row) in
                    let center_y = rb_store.pos_y.(row) in

                    rb_store.min_x.(row) <- center_x -. half_size.x;
                    rb_store.max_x.(row) <- center_x +. half_size.x;
                    rb_store.min_y.(row) <- center_y -. half_size.y;
                    rb_store.max_y.(row) <- center_y +. half_size.y;
                    ()))
          e;

        let i = ref 0 in
        while !i < rb_store.len do
          let entity = index.row_to_ent.(!i) in
          if entity = Utils.sentinel_entity || not (Hashtbl.mem current entity) then (
            let last = rb_store.len - 1 in
            if !i < last then (
              Rb_store.swap_rows rb_store !i last;
              Rb_store.Index.on_swap index ~i:!i ~j:last);

            Rb_store.remove rb_store last;
            Rb_store.Index.on_remove index ~row:last)
          else incr i
        done;

        w)

  let draw_circle store idx queue =
    let open Rb_store in
    if store.active.(idx) = 0 then ()
    else if store.shape.(idx) = 0 then
      let radius = store.radius.(idx) in
      let center_x = store.pos_x.(idx) in
      let center_y = store.pos_y.(idx) in
      let circle = L.Math.Primitives.Circle.create radius (L.Math.Vec2.create center_x center_y) in
      let colour = L.Colour.rgb ~r:255 ~g:0 ~b:0 in

      L.Renderer.push_circle_lines ~z:1000 ~circle colour queue

  let draw_rectangle store idx queue =
    let open Rb_store in
    if store.active.(idx) = 0 then ()
    else if store.shape.(idx) = 1 then
      let pos =
        L.Math.Vec2.create
          (store.pos_x.(idx) -. store.box_hw.(idx))
          (store.pos_y.(idx) -. store.box_hh.(idx))
      in
      let size = L.Math.Vec2.create (store.box_hw.(idx) *. 2.) (store.box_hh.(idx) *. 2.) in
      let rect = L.Math.Rect.create ~pos ~size in
      let colour = L.Colour.rgb ~r:255 ~g:0 ~b:0 in

      L.Renderer.push_rect_lines ~z:1000 ~rect colour queue

  let debug_draw () =
    L.System.make_with_resources ~components:End
      ~resources:
        L.Query.Resource.(Resource (module L.Renderer.Queue.R) & Resource (module Rb_store.R) & End)
      "debug_draw"
      (fun w _ e (queue, (store, _)) ->
        for i = 0 to store.len - 1 do
          if store.shape.(i) = 0 then draw_circle store i queue
          else if store.shape.(i) = 1 then draw_rectangle store i queue
        done;

        w)

  let step () =
    L.System.make_with_resources
      ~components:L.Query.Component.(Required (module Rigid_body.C) & End)
      ~resources:
        L.Query.Resource.(
          Resource (module L.Time.R)
          & Resource (module Config.R)
          & Resource (module Rb_store.R)
          & Resource (module Grid.R)
          & Resource (module Broad_phase.R)
          & Resource (module Narrow_phase.R)
          & Resource (module Collision_event.Collision_events_store.R)
          & Resource (module Rb_store.Index.R)
          & End)
      "step"
      (fun w _ e r ->
        L.Query.Tuple.with8 r (fun time config store grid bp np event_store index ->
            (* Clamp dt to prevent instability *)
            let dt = min (L.Time.dt time) config.max_step_dt in

            if dt > 0. && store.len > 0 then (
              let gx = config.gravity.x and gy = config.gravity.y in

              for row = 0 to store.len - 1 do
                if store.active.(row) = 1 then (
                  Rb_store.apply_gravity_at store ~row ~gx ~gy;
                  Rb_store.integrate_linear_motion_at store ~row ~dt)
              done;

              Broad_phase.update_broad_phase store grid;
              Broad_phase.update_potential_collision_pairs bp grid;
              Narrow_phase.update_actual_collision_pairs np store bp index;
              Resolver.resolve_collisions store np;
              Collision_event.fill_collision_events np event_store index;

              ()));
        w)
end
