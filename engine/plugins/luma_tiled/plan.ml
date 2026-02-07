open Luma__asset
open Luma__math
open Types

module type S = sig
  type map

  type flip = {
    h : bool;
    v : bool;
    d : bool;
  }

  type draw_cmd = {
    texture : Assets.handle;
    source : Rect.t;
    dest : Rect.t;
    origin : Vec2.t;
    rotation : float;
    z : int;
    flip : flip;
  }

  type layer_plan = draw_cmd array

  type plan_meta = {
    parallax : Vec2.t;
    offset : Vec2.t;
    opacity : float;
  }

  type t = {
    layers : layer_plan array;
    meta : plan_meta array;
    map_parallax_origin : Vec2.t;
  }

  val make_plan : ?z_base:int -> map -> (int, tileset_loaded) Hashtbl.t -> t
end

module Make (Map : Map.S) : S with type map = Map.t = struct
  type map = Map.t

  type flip = {
    h : bool;
    v : bool;
    d : bool;
  }

  type draw_cmd = {
    texture : Assets.handle;
    source : Rect.t;
    dest : Rect.t;
    origin : Vec2.t;
    rotation : float;
    z : int;
    flip : flip;
  }

  type layer_plan = draw_cmd array

  type plan_meta = {
    parallax : Vec2.t;
    offset : Vec2.t;
    opacity : float;
  }

  type t = {
    layers : layer_plan array;
    meta : plan_meta array;
    map_parallax_origin : Vec2.t;
  }

  let make_plan ?(z_base = 0) (map : Map.t) (tilesets : (int, tileset_loaded) Hashtbl.t) : t =
    let atlas_src ~columns ~spacing ~margin ~cell_w ~cell_h id =
      let col = id mod columns in
      let row = id / columns in
      let x = margin + (col * (int_of_float cell_w + spacing)) |> float in
      let y = margin + (row * (int_of_float cell_h + spacing)) |> float in
      Rect.create ~pos:(Vec2.create x y) ~size:(Vec2.create cell_w cell_h)
    in

    let build_object_layer
        (layer_index : int)
        (objs : Object.Object_data.t list)
        (tilesets : (int, tileset_loaded) Hashtbl.t) : layer_plan =
      let cmds = ref [] in

      let push ?(rotation = 0.) texture ~source ~x ~y ~w ~h ~flip_h ~flip_v ~flip_d =
        let dest = Rect.create ~pos:(Vec2.create x y) ~size:(Vec2.create w h) in
        let origin = Vec2.create 0. h in
        let flip = { h = flip_h; v = flip_v; d = flip_d } in
        let cmd = { texture; source; dest; origin; rotation; z = z_base + layer_index; flip } in
        cmds := cmd :: !cmds
      in

      List.iter
        (fun (object_data : Object.Object_data.t) ->
          if not object_data.visible then ()
          else
            match object_data.tile with
            | None -> ()
            | Some tile -> (
                match Hashtbl.find_opt tilesets tile.tileset_location with
                | None -> ()
                | Some ts_loaded -> (
                    match ts_loaded with
                    | Texture { texture; cell_w; cell_h; columns; spacing; margin } ->
                        let w, h =
                          match object_data.shape with
                          | Object.Object_data.Rect { width; height } when width > 0. && height > 0.
                            ->
                              (width, height)
                          | _ -> (cell_w, cell_h)
                        in
                        let x = object_data.x and y = object_data.y in
                        let source = atlas_src ~columns ~spacing ~margin ~cell_w ~cell_h tile.id in
                        let flip_h, flip_v, flip_d =
                          match object_data.tile with
                          | Some tile -> (tile.flip_h, tile.flip_v, tile.flip_d)
                          | None -> (false, false, false)
                        in
                        push texture ~source ~x ~y ~w ~h ~rotation:object_data.rotation ~flip_h
                          ~flip_v ~flip_d
                    | Textures { texture_by_tile_id } -> (
                        match Hashtbl.find_opt texture_by_tile_id tile.id with
                        | None -> ()
                        | Some { handle; size; pos } ->
                            let native_w, native_h = (Vec2.x size, Vec2.y size) in
                            let w, h =
                              match object_data.shape with
                              | Object.Object_data.Rect { width; height }
                                when width > 0. && height > 0. ->
                                  (width, height)
                              | _ -> (native_w, native_h)
                            in
                            let flip_h, flip_v, flip_d =
                              match object_data.tile with
                              | Some tile -> (tile.flip_h, tile.flip_v, tile.flip_d)
                              | None -> (false, false, false)
                            in
                            let x = object_data.x and y = object_data.y in
                            let source = Rect.create ~pos ~size:(Vec2.create native_w native_h) in
                            push handle ~source ~x ~y ~w ~h ~rotation:object_data.rotation ~flip_h
                              ~flip_v ~flip_d))))
        objs;
      Array.of_list (List.rev !cmds)
    in
    let build_tile_layer layer_index (tile_data : Layers.Tile_data.t) =
      match tile_data with
      | Infinite -> [||]
      | Finite { width; height = _; tiles } ->
          let cmds = ref [] in
          List.iteri
            (fun i (tile_layer_data : Layers.Tile_data.tile_layer_data option) ->
              match tile_layer_data with
              | None -> ()
              | Some { tileset_index; id; flip_h; flip_v; flip_d } -> (
                  match Hashtbl.find_opt tilesets tileset_index with
                  | None -> ()
                  | Some ts_loaded -> (
                      let col = i mod width in
                      let row = i / width in
                      let dx = float_of_int (col * map.tile_width) in
                      let dy = float_of_int (row * map.tile_height) in

                      match ts_loaded with
                      | Texture { texture; cell_w; cell_h; columns; spacing; margin } ->
                          let source = atlas_src ~columns ~spacing ~margin ~cell_w ~cell_h id in
                          let dest =
                            Rect.create ~pos:(Vec2.create dx dy) ~size:(Vec2.create cell_w cell_h)
                          in
                          let flip = { h = flip_h; v = flip_v; d = flip_d } in
                          let cmd =
                            {
                              texture;
                              source;
                              dest;
                              origin = Vec2.zero;
                              rotation = 0.;
                              z = z_base + layer_index;
                              flip;
                            }
                          in
                          cmds := cmd :: !cmds
                      | Textures { texture_by_tile_id } -> (
                          match Hashtbl.find_opt texture_by_tile_id id with
                          | None -> ()
                          | Some { handle; _ } ->
                              let w = float_of_int map.tile_width
                              and h = float_of_int map.tile_height in
                              let dest =
                                Rect.create ~pos:(Vec2.create dx dy) ~size:(Vec2.create w h)
                              in
                              let flip = { h = flip_h; v = flip_v; d = flip_d } in
                              let cmd =
                                {
                                  texture = handle;
                                  source = Rect.create ~pos:Vec2.zero ~size:(Vec2.create w h);
                                  dest;
                                  origin = Vec2.zero;
                                  rotation = 0.;
                                  z = z_base + layer_index;
                                  flip;
                                }
                              in
                              cmds := cmd :: !cmds))))
            tiles;
          Array.of_list (List.rev !cmds)
    in
    let build_layer layer_index (layer : Layers.Layer_data.t) =
      if not layer.visible then
        ([||], { parallax = Vec2.create 1. 1.; offset = Vec2.zero; opacity = layer.opacity })
      else
        let parallax = Vec2.create layer.parallax_x layer.parallax_y in
        let offset = Vec2.create layer.offset_x layer.offset_y in
        let meta = { parallax; offset; opacity = layer.opacity } in

        match layer.layer_type with
        | Objects object_data -> (build_object_layer layer_index object_data.objects tilesets, meta)
        | Tiles tile_data -> (build_tile_layer layer_index tile_data, meta)
    in

    let items = map.layers |> List.mapi build_layer in
    let layers = items |> List.map fst |> Array.of_list in
    let meta = items |> List.map snd |> Array.of_list in
    let map_parallax_origin = map.parallax_origin in

    { layers; meta; map_parallax_origin }
end
