module Make (L : Luma.S) (Config : Config.S) (Rb : Rigid_body.S) (Store : Storage.Store) = struct
  let sync_rigid_bodies () =
    L.System.make_with_resources
      ~components:L.Query.Component.(Required (module Rb.C) & End)
      ~resources:
        L.Query.Resource.(Resource (module Store.R) & Resource (module Storage.Rb_index.R) & End)
      "sync_rigid_bodies"
      (fun w e (rb_store, (index, _)) ->
        let current = Hashtbl.create 1024 in

        List.iter
          (fun (entity, (rb, _)) -> Hashtbl.replace current (L.Id.Entity.to_int entity) true)
          e;

        List.iter
          (fun (entity, ((rb : Rigid_body.t), _)) ->
            let open Rigid_body in
            let eid = L.Id.Entity.to_int entity in

            match Storage.Rb_index.row_of_entity index eid with
            | None ->
                let row = Store.add rb_store rb in
                Storage.Rb_index.on_add index ~entity:eid ~row
            | Some row ->
                rb_store.inv_mass.(row) <- rb.inv_mass;
                rb_store.damping.(row) <- rb.damping;
                rb_store.vel_x.(row) <- rb.vel.x;
                rb_store.vel_y.(row) <- rb.vel.y;
                rb_store.force_acc_x.(row) <- rb.force_accumulator.x;
                rb_store.force_acc_y.(row) <- rb.force_accumulator.y;
                rb_store.body_type.(row) <- Rigid_body.encode_body_type rb.body_type;

                ())
          e;

        let i = ref 0 in
        while !i < rb_store.len do
          let eid = index.row_to_ent.(!i) in
          if eid = -1 || not (Hashtbl.mem current eid) then (
            let last = rb_store.len - 1 in
            if !i < last then (
              Store.swap_rows rb_store !i last;
              Storage.Rb_index.on_swap index ~i:!i ~j:last);
            Store.remove rb_store last;
            Storage.Rb_index.on_remove index ~row:last)
          else incr i
        done;

        w)

  let step () =
    L.System.make_with_resources
      ~components:L.Query.Component.(Required (module Rb.C) & End)
      ~resources:
        L.Query.Resource.(
          Resource (module L.Time.R) & Resource (module Config.R) & Resource (module Store.R) & End)
      "step"
      (fun w e (time, (config, (s, _))) ->
        (* Clamp dt to prevent instability *)
        let dt = min (L.Time.dt time) config.max_step_dt in

        if dt > 0. && s.len > 0 then (
          let gx = config.gravity.x and gy = config.gravity.y in

          for row = 0 to s.len - 1 do
            Store.apply_gravity s ~row ~gx ~gy
          done;

          for row = 0 to s.len - 1 do
            Store.integrate_linear_row s ~row ~gx ~gy ~dt
          done);
        w)
end
