open Luma__math
open Luma__core

module Make (L : Luma.S) = struct
  include Types
  open L

  let log = Luma__core.Log.sub_log "tiled_plugin"

  module Map = Map.Tilemap (L)

  (* Internal assets. Public facing API should only see the final Tilemap type and the resource. *)
  module Tilemap_asset = Asset.Make (struct
    type inner = Map.t
  end)

  module Tilemap_assets = Assets.For (Tilemap_asset)
  module Loader = Loader.Make (L) (Map) (Tilemap_asset)

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

  type plan = {
    layers : layer_plan array;
    meta : plan_meta array;
    map_parallax_origin : Vec2.t;
  }

  type object_tile_data = {
    handle : Assets.handle;  (** Handle for the object texture. *)
    size : Vec2.t;  (** The width and height of the sub-rectangle. *)
    pos : Vec2.t;  (** The position of the sub-rectangle. *)
  }

  type tileset_loaded =
    | Texture of {
        texture : Assets.handle;
        cell_w : float;
        cell_h : float;
        columns : int;
        spacing : int;
        margin : int;
      }
    | Textures of { texture_by_tile_id : (int, object_tile_data) Hashtbl.t }

  type tileset_texture =
    | Image of Assets.handle  (** Single image tileset. *)
    | Collection_of_images of (int, object_tile_data) Hashtbl.t  (** Keyed by tile local id. *)

  type phase =
    | Init
    | Loading_textures of { textures_by_tileset : (int, tileset_texture) Hashtbl.t }
    | Ready of {
        tilesets : (int, tileset_loaded) Hashtbl.t (* tileset index -> loaded tileset *);
        plan : plan;
      }
    | Failed of Error.error

  let make_plan ?(z_base = 0) (map : Map.t) (tilesets : (int, tileset_loaded) Hashtbl.t) : plan =
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
                          | Some { handle; size; pos } ->
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
        | Object object_data -> (build_object_layer layer_index object_data.objects tilesets, meta)
        | Tiles tile_data -> (build_tile_layer layer_index tile_data, meta)
    in

    let items = map.layers |> List.mapi build_layer in
    let layers = items |> List.map fst |> Array.of_list in
    let meta = items |> List.map snd |> Array.of_list in
    let map_parallax_origin = map.parallax_origin in

    { layers; meta; map_parallax_origin }

  type tilemap_res = {
    mutable background_colour : string option;
    origin : Vec2.t; (* world-space top-left of tile (0,0) *)
    scale : float; (* world units per pixel; 1.0 = pixels *)
    layers : string list option; (* None = all TMJ tile layers *)
    z_base : int; (* z for first layer; layers add +1, etc. *)
    mutable phase : phase;
  }

  type t = (Assets.handle, tilemap_res) Hashtbl.t

  module R = Resource.Make (struct
    type inner = t

    let name = "tilemap_res"
  end)

  let create () = Hashtbl.create 16

  (* public functions *)

  let add world path origin scale z tilemaps =
    let ( let* ) = Option.bind in
    let* packed = World.get_resource world Asset_server.R.type_id in
    let* server = Resource.unpack_opt (module Asset_server.R) packed in

    match Asset_server.load (module Tilemap_asset) server path world with
    | Ok handle ->
        let r =
          { origin; scale; layers = None; z_base = z; phase = Init; background_colour = None }
        in
        Hashtbl.add tilemaps handle r;
        Some handle
    | Error e ->
        Log.error (fun log -> log "%a" Luma__core.Error.pp e);
        None

  let tilemap_loaded world handle =
    World.get_resource world R.type_id
    |> Option.fold ~none:false ~some:(fun packed ->
           Resource.unpack_opt (module R) packed
           |> Option.fold ~none:false ~some:(fun tbl ->
                  match Hashtbl.find_opt tbl handle with
                  | Some tilemap -> ( match tilemap.phase with Ready _ -> true | _ -> false)
                  | None -> false))

  let tilemaps_loaded world =
    World.get_resource world R.type_id
    |> Option.fold ~none:false ~some:(fun packed ->
           Resource.unpack_opt (module R) packed
           |> Option.fold ~none:false ~some:(fun res ->
                  Hashtbl.fold
                    (fun _ res acc -> match res.phase with Ready _ -> true | _ -> false)
                    res false))

  (* private functions *)

  let register_map_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Asset_server.R) & End)
      "register_map_loader"
      (fun w _ (server, _) ->
        Asset_server.register_loader server
          (module Loader.Tilemap_loader)
          ~ctx_provider:Asset_loader.Context_provider.no_ctx;
        w)

  let start_loading_textures server (map : Map.t) world =
    let textures_by_tileset = Hashtbl.create (List.length map.tilesets) in
    let load path =
      match Asset_server.load (module Image.Texture.A) server path world with
      | Ok h -> Some h
      | _ -> None
    in

    List.iteri
      (fun ts_idx (ts : Tileset.t) ->
        match ts.image with
        | Some image ->
            Option.iter
              (fun h -> Hashtbl.add textures_by_tileset ts_idx (Image h))
              (load image.source)
        | None ->
            let tile_to_texture = Hashtbl.create (Hashtbl.length ts.tiles) in
            Hashtbl.iter
              (fun id (td : Tileset.tile_data) ->
                match td.image with
                | Some img ->
                    let image_size = Vec2.create (float img.width) (float img.height) in
                    let image_source = Vec2.create td.x td.y in
                    Option.iter
                      (fun handle ->
                        let object_tile_data = { size = image_size; pos = image_source; handle } in
                        Hashtbl.add tile_to_texture id object_tile_data)
                      (load img.source)
                | None -> ())
              ts.tiles;
            Hashtbl.add textures_by_tileset ts_idx (Collection_of_images tile_to_texture))
      map.tilesets;
    textures_by_tileset

  let all_textures_loaded (assets : Assets.t) (by_ts : (int, tileset_texture) Hashtbl.t) : bool =
    Hashtbl.to_seq by_ts
    |> Seq.for_all (function
         | _ts, Image h -> Assets.is_loaded assets h
         | _ts, Collection_of_images id2h ->
             Hashtbl.to_seq id2h
             |> Seq.for_all (fun (_, { size; pos; handle }) -> Assets.is_loaded assets handle))

  let finalize_maps (map : Map.t) (textures_by_tileset : (int, tileset_texture) Hashtbl.t) =
    let finalized_tilesets = Hashtbl.create (List.length map.tilesets) in
    List.iteri
      (fun idx (ts : Tileset.t) ->
        match Hashtbl.find_opt textures_by_tileset idx with
        | Some (Image texture) ->
            Hashtbl.add finalized_tilesets idx
              (Texture
                 {
                   texture;
                   cell_w = float ts.tile_width;
                   cell_h = float ts.tile_height;
                   columns = ts.columns;
                   spacing = ts.spacing;
                   margin = ts.margin;
                 })
        | Some (Collection_of_images tiles) ->
            Hashtbl.add finalized_tilesets idx (Textures { texture_by_tile_id = tiles })
        | None -> ())
      map.tilesets;
    finalized_tilesets

  let resolve () =
    System.make_with_resources ~components:End
      ~resources:
        Query.Resource.(
          Resource (module Assets.R) & Resource (module Asset_server.R) & Resource (module R) & End)
      "resolve_tilemaps"
      (fun w e r ->
        Query.Tuple.with3 r (fun assets server tilemap_map ->
            Hashtbl.iter
              (fun tilemap_handle tilemap_res ->
                match tilemap_res.phase with
                | Init -> (
                    match Tilemap_assets.get assets tilemap_handle with
                    | Some map ->
                        let handles = start_loading_textures server map w in
                        tilemap_res.phase <- Loading_textures { textures_by_tileset = handles };
                        ()
                    | None -> ())
                | Loading_textures { textures_by_tileset } ->
                    if all_textures_loaded assets textures_by_tileset then
                      match Tilemap_assets.get assets tilemap_handle with
                      | Some map ->
                          let tilesets = finalize_maps map textures_by_tileset in
                          let z_base = tilemap_res.z_base in
                          let plan = make_plan ~z_base map tilesets in
                          tilemap_res.background_colour <- map.background_colour;
                          tilemap_res.phase <- Ready { tilesets; plan }
                      | None -> ()
                    else ()
                | _ -> ())
              tilemap_map);
        w)

  let render () =
    System.make_with_resources
      ~components:Query.Component.(Required (module Camera.C) & End)
      ~resources:
        Query.Resource.(
          Resource (module Asset_server.R)
          & Resource (module R)
          & Resource (module Assets.R)
          & Resource (module Renderer.Queue.R)
          & End)
      "render_tilemap"
      (fun w cams res ->
        let draw_plan_for_camera
            (assets : Assets.t)
            (cam : Camera.t)
            (tm : tilemap_res)
            (plan : plan)
            (queue : Renderer.Queue.t) =
          let map_origin_world =
            Vec2.(
              tm.origin
              |> add
                   (scale tm.scale
                      (Vec2.create (x plan.map_parallax_origin) (y plan.map_parallax_origin))))
          in
          let cam_center_world = Camera.center cam in
          let d = Vec2.sub cam_center_world map_origin_world in

          Array.iteri
            (fun i layer ->
              let meta = plan.meta.(i) in
              let shift_world =
                Vec2.create
                  (Vec2.x d *. (1.0 -. Vec2.x meta.parallax))
                  (Vec2.y d *. (1.0 -. Vec2.y meta.parallax))
              in

              Array.iter
                (fun (cmd : draw_cmd) ->
                  match Image.Texture.Assets.get assets cmd.texture with
                  | None -> ()
                  | Some tex ->
                      let dst_pos_map = Vec2.create (Rect.x cmd.dest) (Rect.y cmd.dest) in
                      let mp_map = Vec2.add dst_pos_map meta.offset in

                      (* map -> world, then parallax shift *)
                      let world_base = Vec2.add tm.origin (Vec2.scale tm.scale mp_map) in
                      let base_world = Vec2.add world_base shift_world in

                      let size_world =
                        Vec2.scale tm.scale
                          (Vec2.create (Rect.width cmd.dest) (Rect.height cmd.dest))
                      in

                      Renderer.push_texture ~z:cmd.z ~tex ~position:base_world ~size:size_world
                        ?src:(Some cmd.source) ~rotation:cmd.rotation ~opacity:meta.opacity
                        ~origin:cmd.origin ~flip_x:cmd.flip.h ~flip_y:cmd.flip.v queue ())
                layer)
            plan.layers
        in

        Query.Tuple.with4 res
          (fun _server (maps : t) (assets : Assets.t) (queue : Renderer.Queue.t) ->
            let cams_sorted =
              cams
              |> List.filter_map (fun (_e, (cam, ())) ->
                     if Camera.active cam then Some cam else None)
              |> List.stable_sort (fun a b -> Int.compare (Camera.order a) (Camera.order b))
            in
            List.iter
              (fun cam ->
                Hashtbl.iter
                  (fun _handle tm ->
                    match tm.phase with
                    | Ready { plan; _ } -> draw_plan_for_camera assets cam tm plan queue
                    | _ -> ())
                  maps)
              cams_sorted);
        w)

  let setup_register () =
    System.make ~components:End "setup_tilemap_register" (fun world _ ->
        if World.has_resource R.type_id world then world
        else
          let map = create () in
          let packed = Resource.pack (module R) map in
          World.add_resource R.type_id packed world)

  (** Selects a background colour based on z-index once all maps are finalised*)
  let set_background () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Window_config.R) & Resource (module R) & End)
      "update_background"
      (fun world _ (wc, (maps, _)) ->
        let min_kv =
          Hashtbl.fold
            (fun k p acc ->
              match acc with
              | None -> Some (k, p)
              | Some (_kmin, pmin) -> if p.z_base < pmin.z_base then Some (k, p) else acc)
            maps None
        in
        match min_kv with
        | Some (_, map) -> (
            match map.background_colour with
            | Some bc -> (
                match Colour.from_string bc with
                | Ok c ->
                    wc.colour <- Some c;
                    world
                | Error _ -> world)
            | None -> world)
        | None -> world)

  let plugin (app : App.t) =
    app
    |> App.on Startup (setup_register ())
    |> App.on Startup (register_map_loader ())
    |> App.on Update (resolve ())
    |> App.once Update (set_background ()) ~run_if:tilemaps_loaded
    |> App.on PreRender (render ())
end
