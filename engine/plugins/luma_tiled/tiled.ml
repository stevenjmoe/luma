open Luma__math

module Make (L : Luma.S) = struct
  include Types
  open L

  (* Internal assets. Public facing API should only see the final Tilemap type and the resource. *)
  module Tilemap_asset = Asset.Make (struct
    type inner = Tilemap.t
  end)

  module Tileset_asset = Asset.Make (struct
    type inner = tileset
  end)

  module Loader = Loader.Make (L) (Tilemap_asset) (Tileset_asset)

  type tileset_rt = {
    texture_handle : Assets.handle;
    tile_size : size;
    columns : int;
    tile_count : int;
    margin : int;
    spacing : int;
    rects : (int, Rect.t) Hashtbl.t option;
    last_local_id : int;
  }

  type gid_span = {
    first : int;
    last : int;
    set_idx : int;
  }

  type runtime = {
    desc : Tilemap_asset.t;
    sets : tileset_rt array;
    spans : gid_span array;
  }

  type phase =
    | Init
    | Loading_ts of Assets.handle list
    | Loading_textures of {
        ts : Assets.handle list;
        tex : Assets.handle list;
      }
    | Ready of runtime
    | Failed of Error.error

  type tilemap = {
    map : Assets.handle;
    origin : Vec2.t; (* world-space top-left of tile (0,0) *)
    scale : float; (* world units per pixel; 1.0 = pixels *)
    layers : string list option; (* None = all TMJ tile layers *)
    z_base : int; (* z for first layer; layers add +1, etc. *)
    mutable phase : phase;
  }

  type t = (Assets.handle, tilemap) Hashtbl.t

  module R = Resource.Make (struct
    type inner = t

    let name = "tilemap_res"
  end)

  let create () = Hashtbl.create 16

  (* public functions *)

  (** [add world path origin scale z] starts loading the tilemap asset. Once finalized, the linked
      tileset files will start loading. Finally, the textures will be loaded. *)
  let add world path origin scale z tilemaps =
    let ( let* ) = Option.bind in
    let* packed = World.get_resource world Asset_server.R.type_id in
    let* server = Resource.unpack_opt (module Asset_server.R) packed in

    match Asset_server.load (module Tilemap_asset) server path world with
    | Ok handle ->
        let r = { map = handle; origin; scale; layers = None; z_base = z; phase = Init } in
        Hashtbl.add tilemaps handle r;
        Some r
    | Error e ->
        Log.error (fun log -> log "%a" Luma__core.Error.pp e);
        None

  let loaded world handle =
    World.get_resource world R.type_id
    |> Option.fold ~none:false ~some:(fun packed ->
           Resource.unpack_opt (module R) packed
           |> Option.fold ~none:false ~some:(fun tbl ->
                  match Hashtbl.find_opt tbl handle with
                  | Some tilemap -> ( match tilemap.phase with Ready _ -> true | _ -> false)
                  | None -> false))

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
        Query.Resource.(
          Resource (module Assets.R) & Resource (module Asset_server.R) & Resource (module R) & End)
      "resolve_tilemaps"
      (fun w e r ->
        Query.Tuple.with3 r (fun assets server tilemap_map ->
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
                                Asset_server.load
                                  (module Tileset_asset)
                                  server
                                  (Filename.concat (Tilemap.path tilemap_asset) ts.source)
                                  w
                              with
                              | Ok handle -> Ok handle
                              | Error e -> Error e)
                            (Tilemap.tilesets tilemap_asset)
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
                                            Log.error (fun l -> l "%a" Error.pp e);
                                            Some e
                                        | _ -> None)
                                      errs)))
                    | None -> ())
                | Loading_ts ts -> (
                    let all_ts_ready =
                      List.for_all (fun tileset_handle -> Assets.is_loaded assets tileset_handle) ts
                    in
                    if not all_ts_ready then ()
                    else
                      let tilemap_asset =
                        Assets.get (module Tilemap_asset) assets tilemap.map |> Option.get
                      in
                      let ( let* ) = Result.bind in
                      let tex_handles =
                        ts
                        |> List.map (fun th ->
                               let* ts =
                                 Assets.get (module Tileset_asset) assets th
                                 |> Option.to_result
                                      ~none:(Error.asset_load "Could not get tileset asset")
                               in
                               match ts.image with
                               | None ->
                                   Error
                                     (Error.parse_json
                                        (String "Tiled: collection tilesets not supported yet"))
                               | Some rel_path ->
                                   let path =
                                     Filename.concat (Tilemap.path tilemap_asset) rel_path
                                   in
                                   Asset_server.load (module Image.Texture.A) server path w)
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
                                          Log.error (fun l -> l "%a" Error.pp e);
                                          Some e
                                      | _ -> None)
                                    errs)))
                | Loading_textures { ts; tex } ->
                    let all_tex_ready = List.for_all (fun th -> Assets.is_loaded assets th) tex in
                    if all_tex_ready then
                      let desc =
                        Assets.get (module Tilemap_asset) assets tilemap.map |> Option.get
                      in

                      let sets =
                        List.map2
                          (fun th texh ->
                            let ts = L.Assets.get (module Tileset_asset) assets th |> Option.get in
                            (* collection vs atlas *)
                            let (rects : (int, Rect.t) Hashtbl.t option), last_local_id =
                              if ts.columns > 0 then (None, ts.tile_count - 1)
                              else
                                let tbl = Hashtbl.create ts.tile_count in
                                Array.iter
                                  (fun t ->
                                    let pos =
                                      Vec2.create (float t.position.x) (float t.position.y)
                                    in
                                    let size = Vec2.create (float t.size.w) (float t.size.h) in
                                    Hashtbl.replace tbl t.id (Rect.create ~pos ~size))
                                  ts.tiles;
                                let max_id =
                                  Array.fold_left
                                    (fun m (t : tile) -> if t.id > m then t.id else m)
                                    0 ts.tiles
                                in
                                (Some tbl, max_id)
                            in
                            {
                              texture_handle = texh;
                              tile_size = ts.tile_size;
                              columns = ts.columns;
                              tile_count = ts.tile_count;
                              margin = ts.margin;
                              spacing = ts.spacing;
                              rects;
                              last_local_id;
                            })
                          ts tex
                        |> Array.of_list
                      in

                      let spans =
                        (* use last_local_id, not tile_count-1, to cover sparse ids *)
                        Tilemap.tilesets desc
                        |> List.mapi (fun i r ->
                               let set = sets.(i) in
                               {
                                 first = r.first_gid;
                                 last = r.first_gid + set.last_local_id;
                                 set_idx = i;
                               })
                        |> List.sort (fun a b -> compare a.first b.first)
                        |> Array.of_list
                      in

                      tilemap.phase <- Ready { desc; sets; spans }
                | _ -> ())
              tilemap_map);
        w)

  let setup_register () =
    System.make ~components:End "setup_tilemap_register" (fun world _ ->
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
            |> Seq.exists (fun (_, t) -> match t.phase with Ready _ -> false | _ -> true))

  (* TODO: This is all over the place. *)
  let gid_lookup (rt : runtime) (gid0 : int) : (tileset_rt * Rect.t) option =
    let gid = gid0 land 0x0FFF_FFFF in
    if gid <= 0 then None
    else
      let refs = Array.of_list (Tilemap.tilesets rt.desc) in
      let n = Int.min (Array.length refs) (Array.length rt.sets) in
      let rec scan i : (tileset_rt * Rect.t) option =
        if i < 0 then None
        else
          let first = refs.(i).first_gid in
          if gid < first then scan (i - 1)
          else
            let set = rt.sets.(i) in
            let local = gid - first in
            match set.rects with
            | Some tbl -> (
                match Hashtbl.find_opt tbl local with Some src -> Some (set, src) | None -> None)
            | None ->
                if set.columns <= 0 then None
                else
                  let col = local mod set.columns in
                  let row = local / set.columns in
                  let x = set.margin + (col * (set.tile_size.w + set.spacing)) in
                  let y = set.margin + (row * (set.tile_size.h + set.spacing)) in
                  let pos = Vec2.create (float x) (float y) in
                  let size = Vec2.create (float set.tile_size.w) (float set.tile_size.h) in
                  Some (set, Rect.create ~pos ~size)
      in
      scan (n - 1)

  let dst_pos_size
      ~(origin : Vec2.t)
      ~(scale : float)
      ~(tile_w : int)
      ~(tile_h : int)
      ~(col : int)
      ~(row : int) : Vec2.t * Vec2.t =
    let w = scale *. float tile_w in
    let h = scale *. float tile_h in
    let x = Vec2.x origin +. (float col *. w) in
    let y = Vec2.y origin +. (float row *. h) in
    (Vec2.create x y, Vec2.create w h)

  let render_tilemaps () =
    let gid_lookup (rt : runtime) (gid0 : int) : (tileset_rt * Rect.t) option =
      let gid = gid0 land 0x0FFF_FFFF in
      if gid <= 0 || Array.length rt.spans = 0 then None
      else
        let rec bs lo hi : (tileset_rt * Rect.t) option =
          if lo > hi then None
          else
            let mid = (lo + hi) lsr 1 in
            let s = rt.spans.(mid) in
            if gid < s.first then bs lo (mid - 1)
            else if gid > s.last then bs (mid + 1) hi
            else if s.set_idx < 0 || s.set_idx >= Array.length rt.sets then None
            else
              let set = rt.sets.(s.set_idx) in
              let local = gid - s.first in
              match set.rects with
              | Some tbl -> (
                  (* collection tileset: direct rect lookup *)
                  match Hashtbl.find_opt tbl local with
                  | None -> None
                  | Some src -> Some (set, src))
              | None ->
                  (* atlas tileset: compute from columns *)
                  if set.columns <= 0 then None
                  else
                    let col = local mod set.columns in
                    let row = local / set.columns in
                    let x = set.margin + (col * (set.tile_size.w + set.spacing)) in
                    let y = set.margin + (row * (set.tile_size.h + set.spacing)) in
                    let pos = Vec2.create (float x) (float y) in
                    let size = Vec2.create (float set.tile_size.w) (float set.tile_size.h) in
                    Some (set, Rect.create ~pos ~size)
        in
        bs 0 (Array.length rt.spans - 1)
    in

    System.make_with_resources ~components:Query.Component.End
      ~resources:
        Query.Resource.(
          Resource (module L.Assets.R)
          & Resource (module Renderer.Queue.R)
          & Resource (module R)
          & End)
      "tilemap_render"
      (fun world _ res ->
        L.Query.Tuple.with3 res (fun assets queue tbl ->
            Hashtbl.iter
              (fun _handle tm ->
                match tm.phase with
                | Ready rt ->
                    List.iteri
                      (fun layer_idx layer ->
                        match layer.payload with
                        | Tile_layer tl -> (
                            let data_opt =
                              match tl.data with
                              | Array_ a -> Some a
                              | String_ _ -> None (* TODO: decode CSV/Base64 during resolve *)
                            in
                            match data_opt with
                            | None -> ()
                            | Some data ->
                                let tile_size = Tilemap.tile_size rt.desc in
                                let tile_w = tile_size.w and tile_h = tile_size.h in
                                let z = tm.z_base + layer_idx in
                                for row = 0 to tl.size.h - 1 do
                                  let base = row * tl.size.w in
                                  for col = 0 to tl.size.w - 1 do
                                    let gid = List.nth data (base + col) in
                                    match gid_lookup rt gid with
                                    | None -> ()
                                    | Some (set, src) -> (
                                        match
                                          L.Assets.get
                                            (module L.Image.Texture.A)
                                            assets set.texture_handle
                                        with
                                        | None -> ()
                                        | Some tex ->
                                            let cell_x =
                                              Vec2.x tm.origin
                                              +. (float_of_int (col * tile_size.w) *. tm.scale)
                                            in
                                            let cell_y =
                                              Vec2.y tm.origin
                                              +. (float_of_int (row * tile_size.h) *. tm.scale)
                                            in
                                            let dw, dh =
                                              if set.columns <= 0 then
                                                ( tm.scale *. Rect.width src,
                                                  tm.scale *. Rect.height src )
                                              else
                                                (tm.scale *. float tile_w, tm.scale *. float tile_h)
                                            in

                                            Renderer.push_texture ~z ~tex
                                              ~position:(Vec2.create cell_x cell_y)
                                              ~size:(Vec2.create dw dh) ~src queue ())
                                  done
                                done)
                        | _ -> ())
                      (Tilemap.layers rt.desc)
                | _ -> ())
              tbl);
        world)

  let plugin (app : App.t) =
    app
    |> App.on Startup (setup_register ())
    |> App.on Startup (register_map_loader ())
    |> App.on Startup (register_set_loader ())
    |> App.on Update (resolve ()) ~run_if:any_unresolved_tilemaps
    |> App.on PreRender (render_tilemaps ())
end
