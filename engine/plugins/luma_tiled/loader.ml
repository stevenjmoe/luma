module Make
    (L : Luma.S)
    (Tilemap_asset : L.Asset.S with type t = Tilemap.t)
    (Tileset_asset : L.Asset.S with type t = Tileset.t) =
struct
  include Types

  (* json helpers *)
  let field name = function
    | `Assoc kv -> ( match List.assoc_opt name kv with Some v -> v | None -> `Null)
    | _ -> `Null

  let fail path msg = Error (L.Error.io_finalize path msg)

  module Tileset_loader :
    L.Asset_loader.LOADER with type t = Tileset.t and type decode = bytes and type ctx = unit =
  struct
    type t = Tileset.t
    type decode = bytes
    type ctx = unit

    module A = Tileset_asset

    let type_id = A.type_id
    let exts = [ ".tsj" ]

    let begin_load path ~k =
      L.IO.read_file path ~k:(function Ok bytes -> k (Ok bytes) | Error e -> k (Error e))

    let finalize (_ctx : ctx) (path : string) (bytes : bytes) =
      let open Luma__serialize.Json_helpers in
      let ( let* ) = Result.bind in

      let int_like key j path =
        match field key j with
        | `Int i -> Ok i
        | `Float f -> Ok (int_of_float f)
        | `Null -> fail path (key ^ " missing")
        | _ -> fail path (key ^ " not an int")
      in

      let int_like_opt ?(default = 0) key j path =
        match field key j with
        | `Int i -> Ok i
        | `Float f -> Ok (int_of_float f)
        | `Null -> Ok default
        | _ -> fail path (key ^ " not an int")
      in

      let parse_grid_opt j =
        match field "grid" j with
        | `Assoc _ as g ->
            let open Yojson.Safe.Util in
            let* width =
              match member "width" g with
              | `Int i -> Ok i
              | `Float f -> Ok (int_of_float f)
              | _ -> Ok 1
            in
            let* height =
              match member "height" g with
              | `Int i -> Ok i
              | `Float f -> Ok (int_of_float f)
              | _ -> Ok 1
            in
            let* orientation =
              match member "orientation" g with
              | `String "orthogonal" -> Ok Orthogonal
              | `String "isometric" -> Ok Isometric
              | `Null | _ -> Ok Orthogonal
            in
            Ok (Some { width; height; orientation })
        | `Null -> Ok None
        | _ -> fail path "grid must be object"
      in

      match Yojson.Safe.from_string (Bytes.unsafe_to_string bytes) with
      | exception _ -> fail path "invalid json"
      | j -> (
          (* guard type *)
          let* () =
            match field "type" j with
            | `String "tileset" -> Ok ()
            | `String other -> fail path (Printf.sprintf "expected type=tileset, got %s" other)
            | _ -> fail path "missing top-level \"type\""
          in

          let* name = parse_string "name" j in
          let* tile_width = int_like "tilewidth" j path in
          let* tile_height = int_like "tileheight" j path in
          let tile_size = { w = tile_width; h = tile_height } in
          let* margin = int_like_opt ~default:0 "margin" j path in
          let* spacing = int_like_opt ~default:0 "spacing" j path in
          let class_ = match field "class" j with `String s -> Some s | _ -> None in
          let* grid = parse_grid_opt j in

          (* atlas *)
          match field "image" j with
          | `String image_path ->
              let* image_w = int_like "imagewidth" j path in
              let* image_h = int_like "imageheight" j path in
              let* columns =
                match field "columns" j with
                | `Int i when i > 0 -> Ok i
                | `Float f when int_of_float f > 0 -> Ok (int_of_float f)
                | _ -> fail path "invalid or missing \"columns\" for atlas tileset"
              in
              let* tile_count = int_like "tilecount" j path in

              let max_cols =
                if tile_width + spacing = 0 then 0
                else (image_w - margin + spacing) / (tile_width + spacing)
              in
              let* () =
                if columns <= max_cols then Ok ()
                else fail path "columns exceed what fits in image (check margin/spacing/tilewidth)"
              in
              let rows = (tile_count + columns - 1) / columns in
              let max_rows =
                if tile_height + spacing = 0 then 0
                else (image_h - margin + spacing) / (tile_height + spacing)
              in
              let* () =
                if rows <= max_rows then Ok ()
                else
                  fail path
                    "tilecount implies more rows than fit in image (check \
                     margin/spacing/tileheight)"
              in

              let image_size = { w = image_w; h = image_h } in
              let tiles =
                Array.init tile_count (fun id ->
                    let col = id mod columns in
                    let row = id / columns in
                    let x = margin + (col * (tile_width + spacing)) in
                    let y = margin + (row * (tile_height + spacing)) in
                    { id; image_path; image_size; size = tile_size; position = { x; y } })
              in
              let r : Tileset.t =
                Tileset.create ~class_ ~image_width:image_w ~image_height:image_h ~name ~tiles
                  ~tile_width ~tile_height ~columns ~image:(Some image_path) ~margin ~tile_count
                  ~spacing ~tiled_version:"" ()
              in
              Ok (L.Asset.pack (module A) r)
          (* shared image with per-tile rects *)
          | _ ->
              let* tiles =
                match field "tiles" j with
                | `List l when l <> [] -> Ok l
                | _ -> fail path "no top-level image and no tiles[]; unsupported tileset shape"
              in
              (* TODO : this is garbage nonsense *)
              let* parsed =
                List.fold_right
                  (fun t acc ->
                    let* acc = acc in
                    let* id =
                      match field "id" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | _ -> fail path "tile.id missing"
                    in

                    let* image_path =
                      match field "image" t with
                      | `String s -> Ok s
                      | _ -> fail path "tile.image missing"
                    in

                    let* image_width =
                      match field "imagewidth" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | _ -> fail path "tile.imagewidth missing"
                    in

                    let* image_height =
                      match field "imageheight" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | _ -> fail path "tile.imageheight missing"
                    in

                    let* x =
                      match field "x" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | `Null -> Ok 0
                      | _ -> fail path "tile.x invalid"
                    in

                    let* y =
                      match field "y" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | `Null -> Ok 0
                      | _ -> fail path "tile.y invalid"
                    in

                    let* tile_width =
                      match field "width" t with
                      | `Int i when i > 0 -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | `Null -> Ok 0
                      | _ -> fail path "tile.width invalid"
                    in

                    let* tile_height =
                      match field "height" t with
                      | `Int i when i > 0 -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | `Null -> Ok 0
                      | _ -> fail path "tile.height invalid"
                    in

                    Ok
                      ((image_path, image_width, image_height, id, x, y, tile_width, tile_height)
                      :: acc))
                  tiles (Ok [])
              in
              (* same image for all tiles *)
              let* shared_path, image_width, image_height =
                match parsed with
                | [] -> fail path "empty tiles[]"
                | (p, iw, ih, _, _, _, tile_width, tile_height) :: rest ->
                    if
                      List.for_all
                        (fun (p', _, _, _, _, _, tile_width, tile_height) -> String.equal p p')
                        rest
                    then Ok (p, iw, ih)
                    else fail path "tiles reference different image files"
              in
              let bounds_ok =
                List.for_all
                  (fun (_p, iw, ih, _id, x, y, tile_width, tile_height) ->
                    x >= 0 && y >= 0 && x + tile_width <= iw && y + tile_height <= ih)
                  parsed
              in
              let* () =
                if bounds_ok then Ok () else fail path "one or more tiles out of atlas bounds"
              in

              let image_size = { w = image_width; h = image_height } in
              let parsed =
                List.sort
                  (fun (_, _, _, id1, _, _, _, _) (_, _, _, id2, _, _, _, _) -> compare id1 id2)
                  parsed
              in
              let tiles =
                parsed
                |> List.map (fun (_p, _iw, _ih, id, x, y, tile_width, tile_height) ->
                       {
                         id;
                         image_path = shared_path;
                         image_size;
                         size = { w = tile_width; h = tile_height };
                         position = { x; y };
                       })
                |> Array.of_list
              in
              let* tile_count =
                match field "tilecount" j with
                | `Int i -> Ok i
                | `Float f -> Ok (int_of_float f)
                | _ -> Ok (Array.length tiles)
              in
              let r : Tileset.t =
                Tileset.create ~class_ ~image_width ~image_height ~name ~tiles ~tile_width
                  ~tile_height ~columns:0 ~image:(Some shared_path) ~margin ~tile_count ~spacing
                  ~tiled_version:"" ()
              in
              Ok (L.Asset.pack (module A) r))
  end

  module Tilemap_loader :
    L.Asset_loader.LOADER with type t = Tilemap.t and type decode = bytes and type ctx = unit =
  struct
    type nonrec t = Tilemap.t
    type decode = bytes
    type ctx = unit

    module A = Tilemap_asset

    let type_id = A.type_id
    let exts = [ ".tmj" ]

    let begin_load path ~k =
      L.IO.read_file path ~k:(function Ok bytes -> k (Ok bytes) | Error e -> k (Error e))

    let finalize ctx path bytes =
      let open Luma__serialize.Json_helpers in
      let pretty_to_string = Yojson.Safe.pretty_to_string in
      let ( let* ) = Result.bind in

      try
        let json = Yojson.Safe.from_string (Bytes.unsafe_to_string bytes) in
        match field "type" json with
        | `String "map" ->
            let* tile_w = parse_int "tilewidth" json in
            let* tile_h = parse_int "tileheight" json in
            let* background_colour = parse_string_opt "backgroundcolor" json in
            let* class_ = parse_string_opt "class" json in
            let* compression_level = parse_int_opt "compressionlevel" json in
            let* infinite = parse_bool "infinite" json in
            let* next_layer_id = parse_int "nextlayerid" json in
            let* next_object_id = parse_int "nextobjectid" json in
            let* parallax_origin_x = parse_float_opt "parallaxoriginx" json in
            let* parallax_origin_y = parse_float_opt "parallaxoriginy" json in
            let* tiled_version = parse_string "tiledversion" json in

            let* orientation =
              match field "orientation" json with
              | `String "orthogonal" -> Ok Orthogonal
              | `String "isometric" -> Ok Isometric
              | `String s -> fail path ("unsupported orientation: " ^ s)
              | _ -> fail path "orientation missing"
            in

            let render_order =
              match field "renderorder" json with
              | `String "right-down" -> Right_down
              | `String "right-up" -> Right_up
              | `String "left-down" -> Left_down
              | `String "left-up" -> Left_up
              | _ -> Right_down
            in

            let* map_size =
              match field "infinite" json with
              | `Int 1 -> Ok Infinite
              | _ ->
                  let* cols = parse_int "width" json in
                  let* rows = parse_int "height" json in
                  Ok (Fixed { rows; columns = cols })
            in

            let rec parse_layer layer =
              let* id = parse_int "id" layer in
              let* name = parse_string "name" layer in
              let* class_ = parse_string_opt "class" layer in
              let* opacity = parse_float_opt "opacity" layer in
              let opacity = Option.value ~default:1. opacity in
              let* visible = parse_bool "visible" layer in
              let* offset_x = parse_float_opt "offsetx" layer in
              let offset_x = Option.value ~default:0. offset_x in
              let* offset_y = parse_float_opt "offsety" layer in
              let offset_y = Option.value ~default:0. offset_y in
              let* parallax_x = parse_float_opt "parallaxx" layer in
              let parallax_x = Option.value ~default:0. parallax_x in
              let* parallax_y = parse_float_opt "parallaxx" layer in
              let parallax_y = Option.value ~default:0. parallax_y in
              let properties = match field "properties" layer with `List xs -> xs | _ -> [] in
              let* start_x = parse_int_opt "startx" layer in
              let* start_y = parse_int_opt "starty" layer in
              let* tint_colour = parse_string_opt "tintcolor" layer in
              let* x = parse_int "x" layer in
              let* y = parse_int "y" layer in
              let common =
                {
                  class_;
                  id;
                  name;
                  opacity;
                  visible;
                  offset_x;
                  offset_y;
                  parallax_x;
                  parallax_y;
                  properties;
                  start_x;
                  start_y;
                  tint_colour;
                  x;
                  y;
                }
              in
              match field "type" layer with
              | `String "tilelayer" ->
                  let* w = parse_int "width" layer in
                  let* h = parse_int "height" layer in
                  let size = { w; h } in
                  let* encoding =
                    match field "encoding" layer with
                    | `String "csv" -> Ok Csv
                    | `String "base64" -> Ok Base64
                    | `Null -> Ok Csv
                    | v -> fail path ("tilelayer.encoding invalid: " ^ pretty_to_string v)
                  in

                  let* compression =
                    match field "compression" layer with
                    | `String "zlib" -> Ok Zlib
                    | `String "gzip" -> Ok Gzip
                    | `String "zstd" -> Ok Zstd
                    | `Null -> Ok None
                    | v -> fail path ("tilelayer.compression invalid: " ^ pretty_to_string v)
                  in

                  let* data =
                    match field "data" layer with
                    | `String s -> Ok (String_ s)
                    | `List xs ->
                        let* rev =
                          List.fold_left
                            (fun acc d ->
                              match acc with
                              | Error _ as e -> e
                              | Ok acc_list -> (
                                  match Yojson.Safe.Util.to_int_option d with
                                  | Some i -> Ok (i :: acc_list)
                                  | None -> fail path "tilelayer.data must be a list of ints"))
                            (Ok []) xs
                        in
                        Ok (Array_ (List.rev rev))
                    | v ->
                        fail path
                          ("tilelayer.data expected string or array, got: " ^ pretty_to_string v)
                  in
                  (* TODO: chunks *)
                  Ok
                    {
                      common;
                      payload = Tile_layer { size; encoding; compression; data; chunks = None };
                    }
              | `String "objectgroup" ->
                  let* draw_order = parse_string_opt "draworder" layer in
                  let draw_order = Option.value ~default:"topdown" draw_order in
                  let objects = match field "objects" layer with `List xs -> xs | _ -> [] in
                  Ok { common; payload = Object_group { draw_order; objects } }
              | `String "imagelayer" ->
                  let* image = parse_string "image" layer in
                  let* iw = parse_int "width" layer in
                  let* ih = parse_int "height" layer in
                  let size = { w = iw; h = ih } in
                  let* repeat_x = parse_bool "repeatx" layer in
                  let* repeat_y = parse_bool "repeaty" layer in
                  Ok { common; payload = Image { image; size; repeat_x; repeat_y } }
              | `String "group" ->
                  let* layers =
                    match field "layers" layer with
                    | `List xs ->
                        let* rev =
                          List.fold_left
                            (fun acc j ->
                              match acc with
                              | Error _ as e -> e
                              | Ok rs ->
                                  let* r = parse_layer j in
                                  Ok (r :: rs))
                            (Ok []) xs
                        in
                        Ok (List.rev rev)
                    | v -> fail path ("group.layers expected list, got: " ^ pretty_to_string v)
                  in
                  Ok { common; payload = Group { layers } }
              | `String other -> fail path ("unsupported layer.type: " ^ other)
              | v -> fail path ("layer.type expected string, got: " ^ pretty_to_string v)
            in
            let* layers =
              match field "layers" json with
              | `List l ->
                  let* rev =
                    List.fold_left
                      (fun acc j ->
                        match acc with
                        | Error _ as e -> e
                        | Ok xs ->
                            let* layer = parse_layer j in
                            Ok (layer :: xs))
                      (Ok []) l
                  in
                  Ok (List.rev rev)
              | j ->
                  fail path
                    ("map.layers expected a list of layers. Got:\n" ^ Yojson.Safe.pretty_to_string j)
            in

            let* tilesets =
              match field "tilesets" json with
              | `List [] -> fail path "Expected a non-empty list of tilesets."
              | `List l ->
                  (List.fold_left (fun acc a ->
                       match acc with
                       | Error _ as e -> e
                       | Ok acc_list -> (
                           match a with
                           | `Assoc _ as v ->
                               let* first_gid = parse_int "firstgid" v in
                               let* source = parse_string "source" v in
                               Ok ({ first_gid; source } :: acc_list)
                           | j -> fail path ("Invalid tileset: " ^ Yojson.Safe.pretty_to_string j))))
                    (Ok []) l
                  |> Result.map List.rev
              | j ->
                  fail path ("Expected a list of tilesets. Got:\n" ^ Yojson.Safe.pretty_to_string j)
            in

            let path = Filename.dirname path ^ "/" in
            let tilemap =
              Tilemap.create ~background_colour ~class_ ?compression_level ?parallax_origin_x
                ?parallax_origin_y ~properties:[] ~render_order ~infinite ~layers ~next_layer_id
                ~next_object_id ~orientation ~tiled_version ~tile_size:{ w = tile_w; h = tile_h }
                ~map_size ~tilesets ~path ()
            in
            Ok (L.Asset.pack (module A) tilemap)
        | `String other ->
            Error
              (L.Error.io_finalize path (Printf.sprintf "expected Tiled type=map, got %s" other))
        | _ -> Error (L.Error.io_finalize path "missing top-level \"type\"")
      with _ -> Error (L.Error.io_finalize path "invalid json")
  end
end
