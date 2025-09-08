module Make (L : Luma.S) = struct
  include Types
  open L

  (* Internal assets. Public facing API should only see the final Tilemap type and the resource. *)
  module Tilemap_asset = L.Asset.Make (struct
    type inner = t
  end)

  module Tileset_asset = L.Asset.Make (struct
    type inner = tileset
  end)

  module Loader = Loader.Make (L) (Tilemap_asset) (Tileset_asset)

  type phase =
    | Init
    | Loading_ts of L.Assets.handle list
    | Loading_textures of {
        ts : L.Assets.handle list;
        tex : L.Assets.handle list;
      }
    | Ready
    | Failed of L.Error.error

  type tilemap = {
    map : Assets.handle;
    origin : Luma__math.Vec2.t; (* world-space top-left of tile (0,0) *)
    scale : float; (* world units per pixel; 1.0 = pixels *)
    layers : string list option; (* None = all TMJ tile layers *)
    z_base : int; (* z for first layer; layers add +1, etc. *)
    mutable phase : phase;
  }

  type t = (L.Assets.handle, tilemap) Hashtbl.t

  module R = L.Resource.Make (struct
    type inner = t

    let name = "tilemap_res"
  end)

  let create () = Hashtbl.create 16

  (* public functions *)

  (** [add world path origin scale z] starts loading the tilemap asset. Once finalized, the linked
      tileset files will start loading. Finally, the textures will be loaded. *)
  let add world path origin scale z tilemaps =
    let ( let* ) = Option.bind in
    let* packed = World.get_resource world L.Asset_server.R.type_id in
    let* server = L.Resource.unpack_opt (module L.Asset_server.R) packed in

    match L.Asset_server.load (module Tilemap_asset) server path world with
    | Ok handle ->
        let r = { map = handle; origin; scale; layers = None; z_base = z; phase = Init } in
        Hashtbl.add tilemaps handle r;
        Some r
    | Error e ->
        L.Log.error (fun log -> log "%a" Luma__core.Error.pp e);
        None

  let loaded world handle =
    L.World.get_resource world R.type_id
    |> Option.fold ~none:false ~some:(fun packed ->
           L.Resource.unpack_opt (module R) packed
           |> Option.fold ~none:false ~some:(fun tbl ->
                  match Hashtbl.find_opt tbl handle with
                  | Some tilemap -> ( match tilemap.phase with Ready -> true | _ -> false)
                  | None -> false))

  let orientation t = t.orientation
  let render_order t = t.render_order

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

  let register_set_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Asset_server.R) & End)
      "register_set_loader"
      (fun w _ (server, _) ->
        Asset_server.register_loader server
          (module Loader.Tileset_loader)
          ~ctx_provider:Asset_loader.Context_provider.no_ctx;
        w)

  let resolve () =
    System.make_with_resources ~components:End
      ~resources:
        L.Query.Resource.(
          Resource (module L.Assets.R)
          & Resource (module L.Asset_server.R)
          & Resource (module R)
          & End)
      "resolve_tilemaps"
      (fun w e r ->
        L.Query.Tuple.with3 r (fun assets server tilemap_map ->
            Hashtbl.iter
              (fun _ tilemap ->
                match tilemap.phase with
                | Init -> (
                    match Assets.get (module Tilemap_asset) assets tilemap.map with
                    | Some tilemap_asset -> (
                        let handle_results =
                          List.map
                            (fun ts ->
                              match
                                L.Asset_server.load
                                  (module Tileset_asset)
                                  server
                                  (Filename.concat tilemap_asset.path ts.source)
                                  w
                              with
                              | Ok handle -> Ok handle
                              | Error e -> Error e)
                            tilemap_asset.tilesets
                        in

                        (* Split the successful results from the failed. If all are successful, move to the next phase.
                       If any failed, set phase to failed with the first error *)
                        match
                          List.partition (function Ok _ -> true | _ -> false) handle_results
                        with
                        | oks, [] ->
                            tilemap.phase <-
                              Loading_ts (List.map (function Ok h -> h | _ -> assert false) oks)
                        | _, errs ->
                            (* logging the errors as we filter them for now *)
                            tilemap.phase <-
                              Failed
                                (List.hd
                                   (List.filter_map
                                      (function
                                        | Error e ->
                                            L.Log.error (fun l -> l "%a" L.Error.pp e);
                                            Some e
                                        | _ -> None)
                                      errs)))
                    | None -> ())
                | Loading_ts ts -> (
                    let all_ts_ready =
                      List.for_all
                        (fun tileset_handle -> L.Assets.is_loaded assets tileset_handle)
                        ts
                    in
                    if not all_ts_ready then ()
                    else
                      let tilemap_asset =
                        L.Assets.get (module Tilemap_asset) assets tilemap.map |> Option.get
                      in
                      let ( let* ) = Result.bind in
                      let tex_handles =
                        ts
                        |> List.map (fun th ->
                               let* ts =
                                 L.Assets.get (module Tileset_asset) assets th
                                 |> Option.to_result
                                      ~none:(L.Error.asset_load "Could not get tileset asset")
                               in
                               match ts.image with
                               | None ->
                                   Error
                                     (L.Error.parse_json
                                        (String "Tiled: collection tilesets not supported yet"))
                               | Some rel_path ->
                                   let path = Filename.concat tilemap_asset.path rel_path in
                                   L.Asset_server.load (module L.Image.Texture.A) server path w)
                      in
                      match List.partition (function Ok _ -> true | _ -> false) tex_handles with
                      | oks, [] ->
                          let tex = List.map (function Ok h -> h | _ -> assert false) oks in
                          tilemap.phase <- Loading_textures { ts; tex }
                      | _, errs ->
                          tilemap.phase <-
                            Failed
                              (List.hd
                                 (List.filter_map
                                    (function
                                      | Error e ->
                                          L.Log.error (fun l -> l "%a" L.Error.pp e);
                                          Some e
                                      | _ -> None)
                                    errs)))
                | Loading_textures { ts; tex } ->
                    let all_tex_ready = List.for_all (fun th -> L.Assets.is_loaded assets th) tex in
                    if all_tex_ready then tilemap.phase <- Ready else ()
                | _ -> ())
              tilemap_map);
        w)

  let setup_register () =
    System.make_with_resources ~components:End
      ~resources:L.Query.Resource.(End)
      "setup_tilemap_register"
      (fun world _ res ->
        if World.has_resource R.type_id world then world
        else
          let map = create () in
          let packed = Resource.pack (module R) map in
          World.add_resource R.type_id packed world)

  let any_unresolved_tilemaps w =
    match World.get_resource w R.type_id with
    | None -> false
    | Some packed -> (
        match Resource.unpack (module R) packed with
        | Error _ -> false
        | Ok tilemap ->
            Hashtbl.to_seq tilemap
            |> Seq.exists (fun (_, t) -> match t.phase with Ready -> false | _ -> true))

  let plugin (app : App.t) =
    app
    |> L.App.on Startup (setup_register ())
    |> L.App.on Startup (register_map_loader ())
    |> L.App.on Startup (register_set_loader ())
    |> L.App.on Update (resolve ()) ~run_if:any_unresolved_tilemaps
end
