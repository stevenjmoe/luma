module Make (L : Luma.S) = struct
  let get_rigid_body entity index =
    let open Rigid_body in
    let eid = L.Id.Entity.to_int entity in

    match Rb_store.Index.row_of_entity index eid with None -> None | Some row -> Some row

  let sync_rigid_bodies () =
    L.System.make_with_resources
      ~components:L.Query.Component.(Required (module Rigid_body.C) & End)
      ~resources:
        L.Query.Resource.(Resource (module Rb_store.R) & Resource (module Rb_store.Index.R) & End)
      "sync_rigid_bodies"
      (fun w e (rb_store, (index, _)) ->
        let current = Hashtbl.create 1024 in

        List.iter
          (fun (entity, (rb, _)) -> Hashtbl.replace current (L.Id.Entity.to_int entity) true)
          e;

        (* Add to or update rigid body store *)
        List.iter
          (fun (entity, ((rb : Rigid_body.t), _)) ->
            let open Rigid_body in
            let eid = L.Id.Entity.to_int entity in
            match get_rigid_body entity index with
            | None ->
                let row = Rb_store.add rb_store rb in
                Rb_store.Index.on_add index ~entity:eid ~row
            | Some row -> (
                let open L.Math.Bounded2d in
                rb_store.inv_mass.(row) <- rb.inv_mass;
                rb_store.damping.(row) <- rb.damping;
                rb_store.vel_x.(row) <- rb.vel.x;
                rb_store.vel_y.(row) <- rb.vel.y;
                rb_store.force_acc_x.(row) <- rb.force_accumulator.x;
                rb_store.force_acc_y.(row) <- rb.force_accumulator.y;
                rb_store.body_type.(row) <- Rigid_body.encode_body_type rb.body_type;
                rb_store.shape.(row) <- Rigid_body.encode_shape rb.shape;

                match rb.shape with
                | Circle c ->
                    let radius = Bounding_circle.radius c in
                    rb_store.radius.(row) <- radius;
                    let center_x = rb_store.pos_x.(row) and center_y = rb_store.pos_y.(row) in

                    rb_store.min_x.(row) <- center_x -. radius;
                    rb_store.min_y.(row) <- center_y -. radius;
                    rb_store.max_x.(row) <- center_x +. radius;
                    rb_store.max_y.(row) <- center_y +. radius
                | Aabb a ->
                    let min = Aabb2d.min a in
                    let max = Aabb2d.max a in
                    let half_size = Aabb2d.half_size a in
                    rb_store.min_x.(row) <- min.x;
                    rb_store.min_y.(row) <- min.y;
                    rb_store.max_x.(row) <- max.x;
                    rb_store.max_y.(row) <- max.y;
                    rb_store.box_hw.(row) <- half_size.x;
                    rb_store.box_hh.(row) <- half_size.y;
                    ()))
          e;

        let i = ref 0 in
        while !i < rb_store.len do
          let eid = index.row_to_ent.(!i) in
          if eid = -1 || not (Hashtbl.mem current eid) then (
            let last = rb_store.len - 1 in
            if !i < last then (
              Rb_store.swap_rows rb_store !i last;
              Rb_store.Index.on_swap index ~i:!i ~j:last);
            Rb_store.remove rb_store last;
            Rb_store.Index.on_remove index ~row:last)
          else incr i
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
          & End)
      "step"
      (fun w e (time, (config, (store, (grid, (bp, (np, _)))))) ->
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
          Narrow_phase.update_actual_collision_pairs np store bp;
          Resolver.resolve_collisions store np;
          ());
        w)
end
