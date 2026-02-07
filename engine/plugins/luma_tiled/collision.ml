(* TODO: Currently only supports rect. Handle other shapes *)
module Collision (L : Luma.S) (Map : Map.S) = struct
  let deg_to_rad d = d *. Float.pi /. 180.

  let get_tile_data_for_object (map : Map.t) (tile : Object.Object_tile_data.t) :
      Tileset.tile_data option =
    match List.nth_opt map.tilesets tile.tileset_location with
    | None -> None
    | Some mts -> Hashtbl.find_opt mts.tiles tile.id

  let source_tile_size
      (map : Map.t)
      (tile_ref : Object.Object_tile_data.t)
      (tile_data : Tileset.tile_data) : float * float =
    if tile_data.width > 0. && tile_data.height > 0. then (tile_data.width, tile_data.height)
    else
      match List.nth_opt map.tilesets tile_ref.tileset_location with
      | Some ts -> (float ts.tile_width, float ts.tile_height)
      | None -> (float map.tile_width, float map.tile_height)

  let safe_scale ~target ~source = if source <= 0. then 1. else target /. source

  (* Applies Tiled gid flip flags in tile-local space.
     Uses normalized [0..1] coordinates so it works for scaled tile objects. *)
  let apply_tile_flip ~tile_w ~tile_h ~flip_h ~flip_v ~flip_d (point : L.Math.Vec2.t) :
      L.Math.Vec2.t =
    let u = if tile_w = 0. then 0. else point.x /. tile_w in
    let v = if tile_h = 0. then 0. else point.y /. tile_h in
    let u, v = if flip_d then (v, u) else (u, v) in
    let u = if flip_h then 1. -. u else u in
    let v = if flip_v then 1. -. v else v in
    L.Math.Vec2.create (u *. tile_w) (v *. tile_h)

  let spawn_rb cmd rb =
    L.Ecs.Command.spawn cmd [ L.Ecs.Component.component (module Luma_physics.Rigid_body.C) rb ]
    |> ignore

  let extract_colliders (map : Map.t) cmd =
    let open Layers.Layer_data in
    List.iter
      (fun l ->
        match l.layer_type with
        | Tiles _ -> ()
        | Objects objects ->
            List.iter
              (fun layer_object ->
                let open Object.Object_data in
                let maybe_tile =
                  let ( let* ) = Option.bind in
                  let* tile_ref = layer_object.tile in
                  let* tile_data = get_tile_data_for_object map tile_ref in
                  Some (tile_ref, tile_data)
                in
                match (layer_object.shape, maybe_tile) with
                | Rect r, Some (tile_ref, tile_data) -> (
                    match tile_data.object_group with
                    | None -> ()
                    | Some object_group ->
                        let src_w, src_h = source_tile_size map tile_ref tile_data in
                        let sx = safe_scale ~target:r.width ~source:src_w in
                        let sy = safe_scale ~target:r.height ~source:src_h in
                        let flip_h, flip_v, flip_d =
                          (tile_ref.flip_h, tile_ref.flip_v, tile_ref.flip_d)
                        in

                        let tile_left = layer_object.x in
                        let tile_top = layer_object.y -. r.height in

                        let local_transform point =
                          apply_tile_flip ~tile_w:r.width ~tile_h:r.height ~flip_h ~flip_v ~flip_d
                            point
                        in

                        Array.iter
                          (fun (tileset_object : Tileset.object_) ->
                            let ox = (object_group.x +. tileset_object.x) *. sx in
                            let oy = (object_group.y +. tileset_object.y) *. sy in

                            match tileset_object.polygon with
                            | None ->
                                let w = tileset_object.width *. sx in
                                let h = tileset_object.height *. sy in
                                if w <= 0. || h <= 0. then ()
                                else
                                  (* Build rect as 4 points, apply flips, then fit AABB.
                                     Keeps rect colliders valid even with diagonal flip. *)
                                  let corners =
                                    [|
                                      L.Math.Vec2.create ox oy;
                                      L.Math.Vec2.create (ox +. w) oy;
                                      L.Math.Vec2.create (ox +. w) (oy +. h);
                                      L.Math.Vec2.create ox (oy +. h);
                                    |]
                                  in

                                  let tcorners = Array.map local_transform corners in
                                  let min_x = ref tcorners.(0).x in
                                  let max_x = ref tcorners.(0).x in
                                  let min_y = ref tcorners.(0).y in
                                  let max_y = ref tcorners.(0).y in

                                  for i = 1 to Array.length tcorners - 1 do
                                    let p = tcorners.(i) in
                                    if p.x < !min_x then min_x := p.x;
                                    if p.x > !max_x then max_x := p.x;
                                    if p.y < !min_y then min_y := p.y;
                                    if p.y > !max_y then max_y := p.y
                                  done;

                                  let width = !max_x -. !min_x in
                                  let height = !max_y -. !min_y in

                                  if width > 0. && height > 0. then
                                    let cx = tile_left +. !min_x +. (width /. 2.) in
                                    let cy = tile_top +. !min_y +. (height /. 2.) in
                                    let pos = L.Math.Vec2.create cx cy in
                                    let size = L.Math.Vec2.create width height in
                                    let rb =
                                      Luma_physics.Rigid_body.create_box
                                        Luma_physics.Rigid_body.Static pos size
                                    in
                                    spawn_rb cmd rb
                            | Some points -> (
                                (* Keep polygon points local to tile object and let rb.pos anchor them. *)
                                let local_points =
                                  Array.map
                                    (fun (point : Luma__math.Vec2.t) ->
                                      let point' =
                                        L.Math.Vec2.create
                                          (ox +. (point.x *. sx))
                                          (oy +. (point.y *. sy))
                                      in
                                      local_transform point')
                                    points
                                in

                                let tile_left = layer_object.x in
                                let tile_top = layer_object.y -. r.height in
                                let pos = L.Math.Vec2.create tile_left tile_top in
                                let angle = deg_to_rad tileset_object.rotation in
                                match
                                  Luma_physics.Rigid_body.create_polygon ~angle
                                    Luma_physics.Rigid_body.Static pos local_points
                                with
                                | Ok rb -> spawn_rb cmd rb
                                | Error _ -> ()))
                          object_group.objects)
                | _ -> ())
              objects.objects)
      map.layers
end
