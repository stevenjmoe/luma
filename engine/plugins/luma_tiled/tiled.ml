module Make (L : Luma.S) = struct
  include Types
  open L

  module Tilemap_asset = L.Asset.Make (struct
    type inner = t
  end)

  module Tileset_asset = L.Asset.Make (struct
    type inner = tileset
  end)

  module Loader = Loader.Make (L) (Tilemap_asset) (Tileset_asset)

  module Tilemap = struct
    open Luma__math

    type ready = t

    type phase =
      | Init
      | Loading_ts of L.Assets.handle list
      | Loading_textures of {
          ts : L.Assets.handle list;
          tex : L.Assets.handle list;
        }
      | Ready
      | Failed of L.Error.error

    type t = {
      map : Assets.handle;
      origin : Vec2.t; (* world-space top-left of tile (0,0) *)
      scale : float; (* world units per pixel; 1.0 = pixels *)
      layers : string list option; (* None = all TMJ tile layers *)
      z_base : int; (* z for first layer; layers add +1, etc. *)
      mutable phase : phase;
    }

    module R = L.Resource.Make (struct
      type inner = t

      let name = "tilemap_res"
    end)

    (** [add world path origin scale z] starts loading the tilemap asset. Once finalized, the linked
        tileset files will start loading. Finally, the textures will be loaded. *)
    let add w path origin scale z =
      let ( let* ) = Option.bind in
      let* packed = World.get_resource w L.Asset_server.R.type_id in
      let* server = L.Resource.unpack_opt (module L.Asset_server.R) packed in

      match L.Asset_server.load (module Tilemap_asset) server path w with
      | Ok handle ->
          let r = { map = handle; origin; scale; layers = None; z_base = z; phase = Init } in
          let packed = L.Resource.pack (module R) r in
          L.World.add_resource R.type_id packed w |> ignore;
          Some r
      | Error e ->
          L.Log.error (fun log -> log "%a" Luma__core.Error.pp e);
          None
  end

  let orientation t = t.orientation
  let render_order t = t.render_order

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
          & Resource (module Tilemap.R)
          & End)
      "resolve_tilemap"
      (fun w e r ->
        let open Tilemap in
        L.Query.Tuple.with3 r (fun assets server tilemap ->
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
                    match List.partition (function Ok _ -> true | _ -> false) handle_results with
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
                  List.for_all (fun tileset_handle -> L.Assets.is_loaded assets tileset_handle) ts
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
            | _ -> ());
        w)

  let tilemap_ready w =
    match World.get_resource w Tilemap.R.type_id with
    | Some packed -> (
        let tilemap = Resource.unpack (module Tilemap.R) packed |> Result.get_ok in
        match tilemap.phase with Failed _ | Ready -> false | _ -> true)
    | None -> false

  let plugin (app : App.t) =
    app
    |> L.App.on Startup (register_map_loader ())
    |> L.App.on Startup (register_set_loader ())
    |> L.App.on Update (resolve ()) ~run_if:tilemap_ready
end
