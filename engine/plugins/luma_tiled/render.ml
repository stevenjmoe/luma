open Luma.Math

module Make (L : Luma.S) = struct
  open L

  type camera = L.Camera.t

  let render () =
    Luma.Ecs.System.make_with_resources
      ~components:Ecs.Query.Component.(Required (module Camera.C) & End)
      ~resources:
        Ecs.Query.Resource.(
          Resource (module Asset_server.R)
          & Resource (module Registry.R)
          & Resource (module Assets.R)
          & Resource (module Render.Renderer.Queue.R)
          & End)
      "render_tilemap"
      (fun w _ cams res ->
        let draw_plan_for_camera
            (assets : Assets.t)
            (cam : Camera.t)
            (tm : Registry.entry)
            (queue : Render.Renderer.Queue.t) =
          match Registry.render_plan tm with
          | Some plan ->
              let map_origin_world =
                Vec2.(
                  tm.origin
                  |> add
                       (scale
                          (Vec2.create (x plan.map_parallax_origin) (y plan.map_parallax_origin))
                          tm.scale))
              in

              let cam_target = Camera.target cam in
              let d = Vec2.sub cam_target map_origin_world in

              Array.iteri
                (fun i layer ->
                  let meta = plan.meta.(i) in
                  let shift_world =
                    Vec2.create
                      (Vec2.x d *. (1.0 -. Vec2.x meta.parallax))
                      (Vec2.y d *. (1.0 -. Vec2.y meta.parallax))
                  in

                  Array.iter
                    (fun (cmd : Plan.draw_cmd) ->
                      match Image.Texture.Assets.get assets cmd.texture with
                      | None -> ()
                      | Some tex ->
                          let dst_pos_map = Vec2.create (Rect.x cmd.dest) (Rect.y cmd.dest) in
                          let mp_map = Vec2.add dst_pos_map meta.offset in

                          (* map -> world, then parallax shift *)
                          let world_base = Vec2.add tm.origin (Vec2.scale mp_map tm.scale) in
                          let base_world = Vec2.add world_base shift_world in

                          let size_world =
                            Vec2.scale
                              (Vec2.create (Rect.width cmd.dest) (Rect.height cmd.dest))
                              tm.scale
                          in

                          Render.Renderer.push_texture ~z:cmd.z ~tex ~position:base_world
                            ~size:size_world ?src:(Some cmd.source) ~rotation:cmd.rotation
                            ~opacity:meta.opacity ~origin:cmd.origin ~flip_x:cmd.flip.h
                            ~flip_y:cmd.flip.v queue)
                    layer)
                plan.layers
          | None -> ()
        in

        Ecs.Query.Tuple.with4 res
          (fun _server (maps : Registry.t) (assets : Assets.t) (queue : Render.Renderer.Queue.t) ->
            let cams_sorted =
              cams
              |> List.filter_map (fun (_e, (cam, ())) ->
                  if Camera.active cam then Some cam else None)
              |> List.stable_sort (fun a b -> Int.compare (Camera.order a) (Camera.order b))
            in
            List.iter
              (fun cam ->
                Hashtbl.iter (fun _handle tm -> draw_plan_for_camera assets cam tm queue) maps)
              cams_sorted);
        w)
end
