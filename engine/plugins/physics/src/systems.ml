module Make (L : Luma.S) = struct
  open Luma__time

  let get_rigid_body entity index =
    match Rb_store.Index.row_of_entity index entity with None -> None | Some row -> Some row

  let sync_to_store () =
    let open Luma__ecs in
    System.make_with_resources
      ~components:L.Ecs.Query.Component.(Required (module Rigid_body.C) & End)
      ~resources:
        L.Ecs.Query.Resource.(
          Resource (module Rb_store.R) & Resource (module Rb_store.Index.R) & End)
      "sync_to_store"
      (fun w _ e (rb_store, (index, _)) ->
        rb_store.current_generation <- rb_store.current_generation + 1;
        let gen = rb_store.current_generation in

        (* Add to or update rigid body store *)
        List.iter
          (fun (entity, ((rb : Rigid_body.t), _)) ->
            let open Rigid_body in
            match get_rigid_body entity index with
            | None -> (
                let row = Rb_store.add rb_store rb in
                Rb_store.Index.on_add index ~entity ~row;
                rb_store.last_seen_generation.(row) <- gen;

                rb_store.inv_mass.(row) <- rb.inv_mass;
                rb_store.damping.(row) <- rb.damping;
                rb_store.body_type.(row) <- Rigid_body.encode_body_type rb.body_type;
                rb_store.shape.(row) <- Rigid_body.encode_shape rb.shape;

                (match rb.body_type with
                | Kinematic ->
                    let c = Kinematic_state.default () in
                    let packed = L.Ecs.Component.pack (module Kinematic_state.C) c in
                    L.Ecs.World.add_component w packed entity
                | _ -> ());

                match rb.shape with
                | Circle radius ->
                    rb_store.radius.(row) <- radius;
                    let cx = rb.pos.x in
                    let cy = rb.pos.y in
                    rb_store.pos_x.(row) <- cx;
                    rb_store.pos_y.(row) <- cy;

                    rb_store.min_x.(row) <- cx -. radius;
                    rb_store.max_x.(row) <- cx +. radius;
                    rb_store.min_y.(row) <- cy -. radius;
                    rb_store.max_y.(row) <- cy +. radius
                | Aabb half_size ->
                    rb_store.box_hw.(row) <- half_size.x;
                    rb_store.box_hh.(row) <- half_size.y;

                    let cx = rb.pos.x in
                    let cy = rb.pos.y in
                    rb_store.pos_x.(row) <- cx;
                    rb_store.pos_y.(row) <- cy;

                    rb_store.min_x.(row) <- cx -. half_size.x;
                    rb_store.max_x.(row) <- cx +. half_size.x;
                    rb_store.min_y.(row) <- cy -. half_size.y;
                    rb_store.max_y.(row) <- cy +. half_size.y)
            | Some row -> rb_store.last_seen_generation.(row) <- gen)
          e;

        (* Cull rows whose entities no longer have a rigid body *)
        let i = ref 0 in
        while !i < rb_store.len do
          let row = !i in
          let entity = index.row_to_ent.(row) in

          if entity = Utils.sentinel_entity || rb_store.last_seen_generation.(row) <> gen then (
            let last = rb_store.len - 1 in

            if row < last then (
              Rb_store.swap_rows rb_store row last;
              Rb_store.Index.on_swap index ~i:row ~j:last);

            Rb_store.remove rb_store last;
            Rb_store.Index.on_remove index ~row:last)
          else incr i
        done;

        w)

  let sync_from_store () =
    L.Ecs.System.make_with_resources
      ~components:L.Ecs.Query.Component.(Required (module Rigid_body.C) & End)
      ~resources:
        L.Ecs.Query.Resource.(
          Resource (module Rb_store.R) & Resource (module Rb_store.Index.R) & End)
      "sync_from_store"
      (fun w _ e (store, (index, _)) ->
        List.iter
          (fun (entity, ((rb : Rigid_body.t), _)) ->
            match get_rigid_body entity index with
            | None -> ()
            | Some row ->
                rb.pos.x <- store.pos_x.(row);
                rb.pos.y <- store.pos_y.(row);
                rb.vel.x <- store.vel_x.(row);
                rb.vel.y <- store.vel_y.(row))
          e;
        w)

  let draw_circle store idx queue =
    let open Rb_store in
    if store.active.(idx) = 0 then ()
    else if store.shape.(idx) = 0 then
      let radius = store.radius.(idx) in
      let center_x = store.pos_x.(idx) in
      let center_y = store.pos_y.(idx) in
      let center = L.Math.Vec2.create center_x center_y in
      let colour = L.Colour.rgb ~r:255 ~g:0 ~b:0 in

      L.Render.Renderer.push_circle_lines ~z:1000 ~radius ~center colour queue

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

      L.Render.Renderer.push_rect_lines ~z:1000 ~rect colour queue

  let debug_draw () =
    L.Ecs.System.make_with_resources ~components:End
      ~resources:
        L.Ecs.Query.Resource.(
          Resource (module L.Render.Renderer.Queue.R) & Resource (module Rb_store.R) & End)
      "debug_draw"
      (fun w _ _ (queue, (store, _)) ->
        for i = 0 to store.len - 1 do
          match store.shape.(i) with
          | 0 -> draw_circle store i queue
          | 1 -> draw_rectangle store i queue
          | _ -> ()
        done;

        w)

  let step () =
    L.Ecs.System.make_with_resources
      ~components:L.Ecs.Query.Component.(Required (module Rigid_body.C) & End)
      ~resources:
        L.Ecs.Query.Resource.(
          Resource (module Time.R)
          & Resource (module Config.R)
          & Resource (module Rb_store.R)
          & Resource (module Grid.R)
          & Resource (module Broad_phase.R)
          & Resource (module Narrow_phase.R)
          & Resource (module Collision_event.Collision_events_store.R)
          & Resource (module Rb_store.Index.R)
          & End)
      "step"
      (fun w _ _ r ->
        L.Ecs.Query.Tuple.with8 r (fun time config store grid bp np event_store index ->
            (* Clamp dt to prevent instability *)
            let dt = min (Time.dt time) config.max_step_dt in

            if dt > 0. && store.len > 0 then (
              let gx = config.gravity.x and gy = config.gravity.y in

              for row = 0 to store.len - 1 do
                if store.active.(row) = 1 then (
                  Rb_store.apply_gravity_at store ~row ~gx ~gy;
                  Rb_store.integrate_linear_motion_at store ~row ~dt)
              done;

              Broad_phase.step store grid bp;

              Narrow_phase.update_actual_collision_pairs np store bp index;
              Resolver.resolve_collisions store np;
              Collision_event.fill_collision_events np event_store));
        w)
end
