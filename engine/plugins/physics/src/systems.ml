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
          Resource (module Rb_store.R)
          & Resource (module Shape_store.R)
          & Resource (module Rb_store.Index.R)
          & End)
      "sync_to_store"
      (fun w _ e (rb_store, (shape_store, (index, _))) ->
        let gen = Rb_store.current_generation rb_store + 1 in
        Rb_store.set_current_generation rb_store gen;

        (* Add to or update rigid body store *)
        List.iter
          (fun (entity, ((rb : Rigid_body.t), _)) ->
            let open Rigid_body in
            match get_rigid_body entity index with
            | None -> (
                let row = Rb_store.add rb_store shape_store rb in
                Rb_store.Index.on_add index ~entity ~row;
                Rb_store.set_last_seen_generation rb_store row gen;

                Rb_store.set_inv_mass rb_store row rb.inv_mass;
                Rb_store.set_damping rb_store row rb.damping;
                Rb_store.set_body_type rb_store row (Rigid_body.encode_body_type rb.body_type);
                Rb_store.set_shape_kind rb_store row (Rigid_body.encode_shape rb.shape);

                (match rb.body_type with
                | Kinematic ->
                    let c = Kinematic_state.default () in
                    let packed = L.Ecs.Component.pack (module Kinematic_state.C) c in
                    L.Ecs.World.add_component w packed entity
                | _ -> ());

                match rb.shape with
                | Circle radius ->
                    Shape_store.set_circle_radius shape_store
                      (Rb_store.shape_handle rb_store row)
                      radius;
                    let cx = rb.pos.x in
                    let cy = rb.pos.y in
                    Rb_store.set_pos_x rb_store row cx;
                    Rb_store.set_pos_y rb_store row cy;

                    Rb_store.set_min_x rb_store row (cx -. radius);
                    Rb_store.set_max_x rb_store row (cx +. radius);
                    Rb_store.set_min_y rb_store row (cy -. radius);
                    Rb_store.set_max_y rb_store row (cy +. radius)
                | Aabb half_size ->
                    Shape_store.set_aabb_half_size shape_store
                      (Rb_store.shape_handle rb_store row)
                      half_size;

                    let cx = rb.pos.x in
                    let cy = rb.pos.y in
                    Rb_store.set_pos_x rb_store row cx;
                    Rb_store.set_pos_y rb_store row cy;

                    Rb_store.set_min_x rb_store row (cx -. half_size.x);
                    Rb_store.set_max_x rb_store row (cx +. half_size.x);
                    Rb_store.set_min_y rb_store row (cy -. half_size.y);
                    Rb_store.set_max_y rb_store row (cy +. half_size.y)
                | Polygon points ->
                    let open Luma__math in
                    let iso = Rigid_body.isometry rb in
                    let aabb = Bounded2d.Bounded_polygon.aabb_2d points iso in
                    let min = Bounded2d.Aabb2d.min aabb in
                    let max = Bounded2d.Aabb2d.max aabb in

                    Rb_store.set_pos_x rb_store row rb.pos.x;
                    Rb_store.set_pos_y rb_store row rb.pos.y;

                    Rb_store.set_min_x rb_store row min.x;
                    Rb_store.set_min_y rb_store row min.y;
                    Rb_store.set_max_x rb_store row max.x;
                    Rb_store.set_max_y rb_store row max.y)
            | Some row -> Rb_store.set_last_seen_generation rb_store row gen)
          e;

        (* Cull rows whose entities no longer have a rigid body *)
        let i = ref 0 in
        while !i < Rb_store.len rb_store do
          let row = !i in
          let entity = Rb_store.Index.entity_at_row index row in

          if entity = Utils.sentinel_entity || Rb_store.last_seen_generation rb_store row <> gen
          then (
            let last = Rb_store.len rb_store - 1 in

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
                rb.pos.x <- Rb_store.pos_x store row;
                rb.pos.y <- Rb_store.pos_y store row;
                rb.vel.x <- Rb_store.vel_x store row;
                rb.vel.y <- Rb_store.vel_y store row)
          e;
        w)

  let draw_circle store shape_store idx queue =
    let open Rb_store in
    if not (is_active store idx) then ()
    else if shape_kind store idx = 0 then
      let radius = Shape_store.circle_radius shape_store (shape_handle store idx) in
      let center_x = pos_x store idx in
      let center_y = pos_y store idx in
      let center = L.Math.Vec2.create center_x center_y in
      let colour = L.Colour.rgb ~r:255 ~g:0 ~b:0 in

      L.Render.Renderer.push_circle_lines ~z:1000 ~radius ~center colour queue

  let draw_rectangle store shape_store idx queue =
    let open Rb_store in
    if not (is_active store idx) then ()
    else if shape_kind store idx = 1 then
      let box_hw = Shape_store.aabb_half_width shape_store (shape_handle store idx) in
      let box_hh = Shape_store.aabb_half_height shape_store (shape_handle store idx) in

      let pos = L.Math.Vec2.create (pos_x store idx -. box_hw) (pos_y store idx -. box_hh) in
      let size = L.Math.Vec2.create (box_hw *. 2.) (box_hh *. 2.) in
      let rect = L.Math.Rect.create ~pos ~size in
      let colour = L.Colour.rgb ~r:255 ~g:0 ~b:0 in

      L.Render.Renderer.push_rect_lines ~z:1000 ~rect colour queue

  let debug_draw () =
    L.Ecs.System.make_with_resources ~components:End
      ~resources:
        L.Ecs.Query.Resource.(
          Resource (module L.Render.Renderer.Queue.R)
          & Resource (module Rb_store.R)
          & Resource (module Shape_store.R)
          & End)
      "debug_draw"
      (fun w _ _ (queue, (store, (shape_store, _))) ->
        for i = 0 to Rb_store.len store - 1 do
          match Rb_store.shape_kind store i with
          | 0 -> draw_circle store shape_store i queue
          | 1 -> draw_rectangle store shape_store i queue
          | 2 -> failwith "TODO draw polygon"
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
          & Resource (module Shape_store.R)
          & Resource (module Grid.R)
          & Resource (module Broad_phase.R)
          & Resource (module Narrow_phase.R)
          & Resource (module Collision_event.Collision_events_store.R)
          & Resource (module Rb_store.Index.R)
          & End)
      "step"
      (fun w _ _ r ->
        L.Ecs.Query.Tuple.with9 r (fun time config store shape_store grid bp np event_store index ->
            (* Clamp dt to prevent instability *)
            let dt = min (Time.dt time) config.max_step_dt in

            if dt > 0. && Rb_store.len store > 0 then (
              let gx = config.gravity.x and gy = config.gravity.y in

              for row = 0 to Rb_store.len store - 1 do
                if Rb_store.is_active store row then (
                  Rb_store.apply_gravity_at store ~row ~gx ~gy;
                  Rb_store.integrate_linear_motion_at store shape_store ~row ~dt)
              done;

              Broad_phase.step store grid bp;

              Narrow_phase.update_actual_collision_pairs np store shape_store bp index;
              Resolver.resolve_collisions store shape_store np;
              Collision_event.fill_collision_events np event_store));
        w)
end
