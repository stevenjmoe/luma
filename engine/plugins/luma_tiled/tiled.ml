open Luma__math
open Luma__core
open Luma__resource
open Luma__ecs

module Make (L : Luma.S) = struct
  include Types
  module Map = Map.Tilemap (L.Driver)
  module Plan = Plan.Make (Map)
  module Tiled_render = Render.Make (Plan) (L)
  open Tiled_render

  (* Internal assets. Public facing API should only see the final Tilemap type and the resource. *)
  module Tilemap_asset = Luma__asset.Asset.Make (struct
    type inner = Map.t
  end)

  module Tilemap_assets = Luma__asset.Assets.For (Tilemap_asset)
  module Loader = Loader.Make (L) (Map) (Tilemap_asset)
  module Collision = Collision.Collision (L) (Map)

  type maps = Tiled_render.map_tbl

  module R = Tiled_render.R

  let create () = Hashtbl.create 16

  (* public functions *)

  let add world path origin scale z tilemaps =
    let server =
      Option.bind (World.get_resource world L.Asset_server.R.type_id) (fun p ->
          Resource.unpack_opt (module L.Asset_server.R) p)
    in
    match server with
    | Some server -> (
        match L.Asset_server.load (module Tilemap_asset) server path world with
        | Ok handle ->
            let r =
              { origin; scale; layers = None; z_base = z; phase = Init; background_colour = None }
            in
            Hashtbl.add tilemaps handle r;
            Ok handle
        | Error e -> Error e)
    | None -> Error (Error.resource_not_found "Tiled_plugin.add asset server not found")

  let ( let+ ) o f = match o with Some x -> f x | None -> false

  let tilemap_loaded world handle =
    let+ packed = World.get_resource world R.type_id in
    let+ tbl = Resource.unpack_opt (module R) packed in
    match Hashtbl.find_opt tbl handle with
    | Some tilemap -> ( match tilemap.phase with Ready _ -> true | _ -> false)
    | None -> false

  let tilemaps_loaded world =
    let+ packed = World.get_resource world R.type_id in
    let+ tbl = Resource.unpack_opt (module R) packed in
    Hashtbl.fold
      (fun _ (tm : map_inner) acc -> acc && match tm.phase with Ready _ -> true | _ -> false)
      tbl true

  (* private functions *)

  let register_map_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module L.Asset_server.R) & End)
      "register_map_loader"
      (fun w _ _ (server, _) ->
        L.Asset_server.register_loader server
          (module Loader.Tilemap_loader)
          ~ctx_provider:L.Asset_loader.Context_provider.no_ctx;
        w)

  let start_loading_textures server (map : Map.t) world =
    let textures_by_tileset = Hashtbl.create (List.length map.tilesets) in
    let load path =
      match L.Asset_server.load (module L.Image.Texture.A) server path world with
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

  let all_textures_loaded (assets : L.Assets.t) (by_ts : (int, tileset_texture) Hashtbl.t) : bool =
    Hashtbl.to_seq by_ts
    |> Seq.for_all (function
      | _ts, Image h -> L.Assets.is_loaded assets h
      | _ts, Collection_of_images id2h ->
          Hashtbl.to_seq id2h
          |> Seq.for_all (fun (_, { handle; _ }) -> L.Assets.is_loaded assets handle))

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
          Resource (module L.Assets.R)
          & Resource (module L.Asset_server.R)
          & Resource (module R)
          & End)
      "resolve_tilemaps"
      (fun w cmd _ r ->
        Query.Tuple.with3 r (fun assets server tilemap_map ->
            Hashtbl.iter
              (fun tilemap_handle (render_map : map_inner) ->
                match render_map.phase with
                | Init -> (
                    match Tilemap_assets.get assets tilemap_handle with
                    | Some map ->
                        let handles = start_loading_textures server map w in
                        render_map.phase <- Loading_textures { textures_by_tileset = handles };
                        ()
                    | None -> ())
                | Loading_textures { textures_by_tileset } ->
                    if all_textures_loaded assets textures_by_tileset then
                      match Tilemap_assets.get assets tilemap_handle with
                      | Some map ->
                          let tilesets = finalize_maps map textures_by_tileset in
                          let z_base = render_map.z_base in
                          let plan = Plan.make_plan ~z_base map tilesets in

                          render_map.background_colour <- map.background_colour;
                          Collision.extract_colliders map cmd;

                          render_map.phase <- Ready { tilesets; plan }
                      | None -> ()
                    else ()
                | _ -> ())
              tilemap_map);
        w)

  let setup_register () =
    System.make ~components:End "setup_tilemap_register" (fun world _ _ ->
        if World.has_resource R.type_id world then world
        else
          let map = create () in
          let packed = Resource.pack (module R) map in
          World.add_resource R.type_id packed world)

  (** Selects a background colour based on z-index once all maps are finalised*)
  let set_background () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module L.Window_config.R) & Resource (module R) & End)
      "update_background"
      (fun world _ _ (wc, (maps, _)) ->
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
                match L.Colour.from_string bc with
                | Ok c ->
                    wc.colour <- Some c;
                    world
                | Error _ -> world)
            | None -> world)
        | None -> world)

  (*TODO: check if physics plugin has already been added? (not currently possible though) *)
  let plugin app =
    app
    |> L.App.on Startup (setup_register ())
    |> L.App.on Startup (register_map_loader ())
    |> L.App.on Update (resolve ())
    |> L.App.once Update (set_background ()) ~run_if:tilemaps_loaded
    |> L.App.on PreRender (render ())
end
