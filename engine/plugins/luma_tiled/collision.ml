(* TODO: Currently only supports rect. Handle other shapes *)
module Collision (L : Luma.S) (Map : Map.S) = struct
  let get_tile_data_for_object (map : Map.t) (tile : Object.Object_tile_data.t) :
      Tileset.tile_data option =
    (* tileset_location is the index in map.tilesets *)
    match List.nth_opt map.tilesets tile.tileset_location with
    | None -> None
    | Some mts -> Hashtbl.find_opt mts.tiles tile.id

  let extract_colliders (map : Map.t) world cmd =
    let open Layers.Layer_data in
    List.iter
      (fun l ->
        match l.layer_type with
        | Tiles _ -> ()
        | Object object_data ->
            List.iter
              (fun layer_object ->
                let open Object.Object_data in
                let maybe_object_group =
                  let ( let* ) = Option.bind in
                  let* tile_ref = layer_object.tile in
                  let* tile_data = get_tile_data_for_object map tile_ref in
                  Object.Object_tile_data.(tile_data.object_group)
                in

                match maybe_object_group with
                | None -> ()
                | Some og ->
                    Array.iter
                      (fun (tileset_object : Tileset.object_) ->
                        match layer_object.shape with
                        | Rect r ->
                            (* Tile object position comes from the map's object layer. 
                               Tiled stores tile objects with (x, y) at the bottom-left of the tile,
                               while height is the full tile height in map space. *)
                            let tile_left = layer_object.x in
                            let tile_top = layer_object.y -. r.height in

                            (* Collision shapes (tileset_object) are defined in the tileset's 
                               objectgroup with coordinates relative to the *top-left* of the tile. *)
                            let coll_left = tile_left +. tileset_object.x in
                            let coll_top = tile_top +. tileset_object.y in

                            (* Physics expects a box defined by its center and size. 
                               Convert from top-left + width/height to center coordinates. *)
                            let cx = coll_left +. (tileset_object.width /. 2.) in
                            let cy = coll_top +. (tileset_object.height /. 2.) in

                            let size =
                              L.Math.Vec2.create tileset_object.width tileset_object.height
                            in
                            let pos = L.Math.Vec2.create cx cy in

                            let rb = Luma_physics.Rigid_body.create_box Static pos size in

                            L.Command.spawn cmd
                              [ L.Component.component (module Luma_physics.Rigid_body.C) rb ]
                            |> ignore
                        | _ -> ())
                      og.objects)
              object_data.objects)
      map.layers
end
