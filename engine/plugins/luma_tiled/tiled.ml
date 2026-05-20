open Luma__math
open Luma__ecs
(* TODO: Debugging this plugin is a nightmare *)

module type S = sig
  type maps
  type app

  module R : Luma.Resource.S with type t = maps

  type metadata = {
    source : string;
    width : int;
    height : int;
    tile_width : int;
    tile_height : int;
    orientation : Map.orientation;
    background_colour : string option;
  }

  val add :
    Luma.Ecs.World.t ->
    string ->
    Luma.Math.Vec2.t ->
    float ->
    int ->
    maps ->
    (Luma.Assets.handle, Luma__core__Error.error) result

  val tilemap_loaded : Luma.Ecs.World.t -> Luma.Assets.handle -> bool
  val tilemaps_loaded : Luma.Ecs.World.t -> bool
  val tilemap_renderable : Luma.Ecs.World.t -> Luma.Assets.handle -> bool
  val tilemaps_renderable : Luma.Ecs.World.t -> bool
  val metadata : Luma.Ecs.World.t -> Luma.Assets.handle -> metadata option

  val object_layers :
    Luma.Ecs.World.t -> Luma.Assets.handle -> (string * Object.Object_data.t list) list option

  val object_layer :
    Luma.Ecs.World.t -> Luma.Assets.handle -> string -> Object.Object_data.t list option

  val find_objects :
    ?layer:string ->
    ?name:string ->
    ?type_:string ->
    Luma.Ecs.World.t ->
    Luma.Assets.handle ->
    Object.Object_data.t list option

  val plugin : app -> app
end

module Make (L : Luma.S) : S with type app = L.App.t = struct
  include Types
  open Luma

  let ( let* ) = Result.bind

  type app = L.App.t

  let log = Luma__core.Log.sub_log "tiled_plugin"

  module Map = Map
  module Plan = Plan
  module Tiled_render = Render.Make (L)
  module R = Registry.R

  (* Internal assets. Public facing API should only see the final Tilemap type and the resource. *)
  module Tilemap_source_asset = Luma__asset.Asset.Make (struct
    type inner = Map.source
  end)

  module Tileset_asset = Luma__asset.Asset.Make (struct
    type inner = Tileset.t
  end)

  module Tilemap_source_assets = Luma__asset.Assets.For (Tilemap_source_asset)
  module Tileset_assets = Luma__asset.Assets.For (Tileset_asset)
  module Loader = Loader.Make (L) (Tilemap_source_asset) (Tileset_asset)
  module Collision = Collision.Collision (L)

  type maps = Registry.t

  type metadata = {
    source : string;
    width : int;
    height : int;
    tile_width : int;
    tile_height : int;
    orientation : Map.orientation;
    background_colour : string option;
  }

  let create () = Hashtbl.create 16

  (* public functions *)

  let add world path origin scale z tilemaps =
    let* server_packed =
      World.get_resource world Asset_server.R.type_id
      |> Option.to_result
           ~none:
             (Error.resource_not_found
                (Luma.Id.Resource.to_int Asset_server.R.type_id)
                (Some Asset_server.R.name))
    in
    let* server = Resource.unpack (module Asset_server.R) server_packed in

    match Asset_server.load (module Tilemap_source_asset) server path with
    | Ok handle ->
        let r = Registry.create_entry ~origin ~scale ~z_base:z in
        Hashtbl.add tilemaps handle r;
        Ok handle
    | Error e -> Error e

  let ( let+ ) o f = match o with Some x -> f x | None -> false

  let get_entry world handle =
    let ( let* ) = Option.bind in
    let* packed = World.get_resource world R.type_id in
    let* tbl = Resource.unpack_opt (module R) packed in
    Hashtbl.find_opt tbl handle

  let get_map world handle =
    let ( let* ) = Option.bind in
    let* entry = get_entry world handle in
    Registry.map entry

  let tilemap_loaded world handle =
    let+ entry = get_entry world handle in
    Registry.loaded entry

  let tilemaps_loaded world =
    let+ packed = World.get_resource world R.type_id in
    let+ tbl = Resource.unpack_opt (module R) packed in
    Hashtbl.fold (fun _ entry acc -> acc && Registry.loaded entry) tbl true

  let tilemap_renderable world handle =
    let+ entry = get_entry world handle in
    Registry.renderable entry

  let tilemaps_renderable world =
    let+ packed = World.get_resource world R.type_id in
    let+ tbl = Resource.unpack_opt (module R) packed in
    Hashtbl.fold (fun _ entry acc -> acc && Registry.renderable entry) tbl true

  let metadata world handle =
    Option.map
      (fun (map : Map.t) ->
        {
          source = map.source;
          width = map.width;
          height = map.height;
          tile_width = map.tile_width;
          tile_height = map.tile_height;
          orientation = map.orientation;
          background_colour = map.background_colour;
        })
      (get_map world handle)

  let object_layers_from_map (map : Map.t) =
    map.layers
    |> List.filter_map (fun (layer : Layers.Layer_data.t) ->
        match layer.layer_type with
        | Layers.Layer_data.Objects object_data -> Some (layer.name, object_data.objects)
        | Layers.Layer_data.Tiles _ -> None)

  let object_layers world handle = Option.map object_layers_from_map (get_map world handle)

  let object_layer world handle name =
    let ( let* ) = Option.bind in
    let* layers = object_layers world handle in
    List.assoc_opt name layers

  let find_objects ?layer ?name ?type_ world handle =
    let layer_matches layer_name =
      match layer with None -> true | Some expected -> String.equal expected layer_name
    in

    let object_matches (object_data : Object.Object_data.t) =
      let name_matches =
        match name with None -> true | Some expected -> String.equal expected object_data.name
      in

      let type_matches =
        match type_ with
        | None -> true
        | Some expected -> (
            match object_data.type_ with
            | Some actual -> String.equal expected actual
            | None -> false)
      in
      name_matches && type_matches
    in

    object_layers world handle
    |> Option.map (fun layers ->
        layers
        |> List.filter (fun (layer_name, _) -> layer_matches layer_name)
        |> List.concat_map (fun (_, objects) -> List.filter object_matches objects))

  (* private functions *)

  let register_map_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Asset_server.R) & End)
      "register_map_loader"
      (fun w _ _ (server, _) ->
        Asset_server.register_loader server
          (module Loader.Tilemap_loader)
          ~ctx_provider:Asset_loader.Context_provider.no_ctx;

        Asset_server.register_loader server
          (module Loader.Tileset_loader)
          ~ctx_provider:Asset_loader.Context_provider.no_ctx;
        w)

  let resolve_map_tilesets assets (handles : (int * Assets.handle) list) =
    let* rev =
      List.fold_left
        (fun acc (first_gid, handle) ->
          match acc with
          | Error _ as e -> e
          | Ok rev -> (
              match Tileset_assets.get assets handle with
              | Some tileset -> Ok (Tileset.{ first_gid; tileset } :: rev)
              | None -> Error (Error.asset_load "tileset handle not loaded")))
        (Ok []) handles
    in
    Ok (List.rev rev)

  let start_loading_tilesets server (source : Map.source) =
    let* rev =
      List.fold_left
        (fun acc (first_gid, path) ->
          match acc with
          | Error _ as e -> e
          | Ok rev -> (
              match Asset_server.load (module Tileset_asset) server path with
              | Ok handle -> Ok ((first_gid, handle) :: rev)
              | Error e -> Error e))
        (Ok []) source.tileset_paths_by_gid
    in
    Ok (List.rev rev)

  let all_tilesets_loaded (assets : Assets.t) (tilesets : (int * Assets.handle) list) : bool =
    List.for_all (fun (_, handle) -> Assets.is_loaded assets handle) tilesets

  let start_loading_textures server (map : Map.t) =
    let textures_by_tileset = Hashtbl.create (List.length map.tilesets) in
    let load_texture path =
      match Asset_server.load (module L.Image.Texture.A) server path with
      | Ok h -> Some h
      | _ -> None
    in

    map.tilesets
    |> List.iteri (fun ts_idx (ts : Tileset.t) ->
        match ts.image with
        | Some image ->
            Option.iter
              (fun h -> Hashtbl.add textures_by_tileset ts_idx (Image h))
              (load_texture image.source)
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
                      (load_texture img.source)
                | None -> ())
              ts.tiles;
            Hashtbl.add textures_by_tileset ts_idx (Collection_of_images tile_to_texture));
    textures_by_tileset

  let all_textures_loaded (assets : Assets.t) (by_ts : (int, tileset_texture) Hashtbl.t) : bool =
    Hashtbl.to_seq by_ts
    |> Seq.for_all (function
      | _ts, Image h -> Assets.is_loaded assets h
      | _ts, Collection_of_images id2h ->
          Hashtbl.to_seq id2h
          |> Seq.for_all (fun (_, { handle; _ }) -> Assets.is_loaded assets handle))

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

  let log_error prefix e =
    let msg = Format.asprintf "%s: %a" prefix Error.pp e in
    log.error (fun l -> l "%s" msg)

  let fail_load entry e =
    log_error "Failed to load tilemap" e;
    entry.Registry.load_state <- Load_failed e

  let ensure_loaded_side_effects cmd (entry : Registry.entry) (map : Map.t) =
    if not entry.collision_extracted then (
      Collision.extract_colliders map cmd;
      entry.collision_extracted <- true)

  let update_load_state assets server cmd tilemap_handle (entry : Registry.entry) =
    match entry.load_state with
    | Waiting_source -> (
        match Tilemap_source_assets.get assets tilemap_handle with
        | Some source -> (
            match start_loading_tilesets server source with
            | Ok tileset_handles_by_gid ->
                entry.load_state <- Loading_tilesets { tileset_handles_by_gid }
            | Error e -> fail_load entry e)
        | None -> ())
    | Loading_tilesets { tileset_handles_by_gid } ->
        if all_tilesets_loaded assets tileset_handles_by_gid then
          match Tilemap_source_assets.get assets tilemap_handle with
          | Some source -> (
              match resolve_map_tilesets assets tileset_handles_by_gid with
              | Ok map_tilesets -> (
                  match Map.from_source source map_tilesets with
                  | Ok map ->
                      entry.load_state <- Loaded map;
                      ensure_loaded_side_effects cmd entry map;
                      entry.render_state <-
                        Loading_textures { textures_by_tileset = start_loading_textures server map }
                  | Error e -> fail_load entry e)
              | Error e -> fail_load entry e)
          | None -> ()
        else ()
    | Loaded map -> ensure_loaded_side_effects cmd entry map
    | Load_failed _ -> ()

  let update_render_state assets server (entry : Registry.entry) =
    match (entry.load_state, entry.render_state) with
    | Loaded map, Render_not_started ->
        entry.render_state <-
          Loading_textures { textures_by_tileset = start_loading_textures server map }
    | Loaded map, Loading_textures { textures_by_tileset } ->
        if all_textures_loaded assets textures_by_tileset then
          let tilesets = finalize_maps map textures_by_tileset in
          let plan = Plan.make_plan ~z_base:entry.z_base map tilesets in
          entry.render_state <- Renderable { tilesets; plan }
    | _, Render_failed _ | _, Renderable _ | _, Loading_textures _ | _, Render_not_started -> ()

  let resolve () =
    System.make_with_resources ~components:End
      ~resources:
        Query.Resource.(
          Resource (module Assets.R) & Resource (module Asset_server.R) & Resource (module R) & End)
      "resolve_tilemaps"
      (fun w cmd _ r ->
        Query.Tuple.with3 r (fun assets server tilemap_map ->
            tilemap_map
            |> Hashtbl.iter (fun tilemap_handle (entry : Registry.entry) ->
                update_load_state assets server cmd tilemap_handle entry;
                update_render_state assets server entry));
        w)

  let setup_register app =
    let world = L.App.world app in
    if World.has_resource R.type_id world then app
    else
      let map = create () in
      let packed = Resource.pack (module R) map in
      World.add_resource R.type_id packed world |> ignore;
      app

  (** Selects a background colour based on z-index once all maps are finalised*)
  let set_background () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module L.Window_config.R) & Resource (module R) & End)
      "update_background"
      (fun world _ _ (wc, (maps, _)) ->
        let min_kv =
          Hashtbl.fold
            (fun k entry acc ->
              match Registry.map entry with
              | None -> acc
              | Some map -> (
                  match acc with
                  | None -> Some (k, entry, map)
                  | Some (_kmin, min_entry, _min_map) ->
                      if entry.z_base < min_entry.z_base then Some (k, entry, map) else acc))
            maps None
        in
        match min_kv with
        | Some (_, _, map) -> (
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
    |> setup_register
    |> L.App.on Startup (register_map_loader ())
    |> L.App.on Update (resolve ())
    |> L.App.once Update (set_background ()) ~run_if:tilemaps_loaded
    |> L.App.on PreRender (Tiled_render.render ())
end
