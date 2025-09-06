module Make
    (L : Luma.S)
    (Tilemap_asset : L.Asset.S with type t = Types.t)
    (Tileset_asset : L.Asset.S with type t = Types.tileset) =
struct
  include Types

  (* json helpers *)
  let field name = function
    | `Assoc kv -> ( match List.assoc_opt name kv with Some v -> v | None -> `Null)
    | _ -> `Null

  let parse_orientation = function
    | "orthogonal" -> Ok Orthogonal
    | "isometric" -> Ok Isometric
    | s -> Error (L.Error.parse_json (String s))

  module Tilemap_loader :
    L.Asset_loader.LOADER with type t = t and type decode = bytes and type ctx = unit = struct
    type nonrec t = t
    type decode = bytes
    type ctx = unit

    module A = Tilemap_asset

    let type_id = A.type_id
    let exts = [ ".tmj" ]

    let begin_load path ~k =
      L.IO.read_file path ~k:(function Ok bytes -> k (Ok bytes) | Error e -> k (Error e))

    let finalize ctx path bytes =
      let open Luma__serialize.Json_helpers in
      let ( let* ) = Result.bind in
      try
        let json = Yojson.Safe.from_string (Bytes.unsafe_to_string bytes) in
        match field "type" json with
        | `String "map" ->
            let* tile_w = parse_int "tilewidth" json in
            let* tile_h = parse_int "tileheight" json in
            let orientation =
              match field "orientation" json with
              | `String "orthogonal" -> Orthogonal
              | `String "isometric" -> Isometric
              | `String s -> failwith ("unsupported orientation: " ^ s)
              | _ -> failwith "orientation missing"
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
            let r =
              L.Asset.pack
                (module A)
                { orientation; render_order; tile_size = { w = tile_w; h = tile_h }; map_size }
            in
            Ok r
        | `String other ->
            Error
              (L.Error.io_finalize path (Printf.sprintf "expected Tiled type=map, got %s" other))
        | _ -> Error (L.Error.io_finalize path "missing top-level \"type\"")
      with _ -> Error (L.Error.io_finalize path "invalid json")
  end

  module Tileset_loader :
    L.Asset_loader.LOADER with type t = tileset and type decode = bytes and type ctx = unit = struct
    type t = tileset
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
      let fail msg = Error (L.Error.io_finalize path msg) in

      let int_like key j =
        match field key j with
        | `Int i -> Ok i
        | `Float f -> Ok (int_of_float f)
        | `Null -> fail (key ^ " missing")
        | _ -> fail (key ^ " not an int")
      in
      let int_like_opt ?(default = 0) key j =
        match field key j with
        | `Int i -> Ok i
        | `Float f -> Ok (int_of_float f)
        | `Null -> Ok default
        | _ -> fail (key ^ " not an int")
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
        | _ -> fail "grid must be object"
      in

      match Yojson.Safe.from_string (Bytes.unsafe_to_string bytes) with
      | exception _ -> fail "invalid json"
      | j -> (
          (* guard type *)
          let* () =
            match field "type" j with
            | `String "tileset" -> Ok ()
            | `String other -> fail (Printf.sprintf "expected type=tileset, got %s" other)
            | _ -> fail "missing top-level \"type\""
          in

          let* name = parse_string "name" j in
          let* tile_w = int_like "tilewidth" j in
          let* tile_h = int_like "tileheight" j in
          let tile_size = { w = tile_w; h = tile_h } in
          let* margin = int_like_opt ~default:0 "margin" j in
          let* spacing = int_like_opt ~default:0 "spacing" j in
          let class_ = match field "class" j with `String s -> Some s | _ -> None in
          let* grid = parse_grid_opt j in

          (* atlas *)
          match field "image" j with
          | `String image_path ->
              let* image_w = int_like "imagewidth" j in
              let* image_h = int_like "imageheight" j in
              let* columns =
                match field "columns" j with
                | `Int i when i > 0 -> Ok i
                | `Float f when int_of_float f > 0 -> Ok (int_of_float f)
                | _ -> fail "invalid or missing \"columns\" for atlas tileset"
              in
              let* tile_count = int_like "tilecount" j in

              let max_cols =
                if tile_w + spacing = 0 then 0 else (image_w - margin + spacing) / (tile_w + spacing)
              in
              let* () =
                if columns <= max_cols then Ok ()
                else fail "columns exceed what fits in image (check margin/spacing/tilewidth)"
              in
              let rows = (tile_count + columns - 1) / columns in
              let max_rows =
                if tile_h + spacing = 0 then 0 else (image_h - margin + spacing) / (tile_h + spacing)
              in
              let* () =
                if rows <= max_rows then Ok ()
                else
                  fail
                    "tilecount implies more rows than fit in image (check \
                     margin/spacing/tileheight)"
              in

              let image_size = { w = image_w; h = image_h } in
              let tiles =
                Array.init tile_count (fun id ->
                    let col = id mod columns in
                    let row = id / columns in
                    let x = margin + (col * (tile_w + spacing)) in
                    let y = margin + (row * (tile_h + spacing)) in
                    { id; image_path; image_size; size = tile_size; position = { x; y } })
              in
              let r : tileset =
                {
                  class_;
                  columns;
                  grid;
                  image = Some image_path;
                  image_size = Some image_size;
                  margin;
                  name;
                  spacing;
                  tile_count;
                  tile_size;
                  tiles;
                }
              in
              Ok (L.Asset.pack (module A) r)
          (* shared image with per-tile rects *)
          | _ ->
              let* tiles_list =
                match field "tiles" j with
                | `List l when l <> [] -> Ok l
                | _ -> fail "no top-level image and no tiles[]; unsupported tileset shape"
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
                      | _ -> fail "tile.id missing"
                    in
                    let* image_path =
                      match field "image" t with
                      | `String s -> Ok s
                      | _ -> fail "tile.image missing"
                    in
                    let* iw =
                      match field "imagewidth" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | _ -> fail "tile.imagewidth missing"
                    in
                    let* ih =
                      match field "imageheight" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | _ -> fail "tile.imageheight missing"
                    in
                    let* x =
                      match field "x" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | `Null -> Ok 0
                      | _ -> fail "tile.x invalid"
                    in
                    let* y =
                      match field "y" t with
                      | `Int i -> Ok i
                      | `Float f -> Ok (int_of_float f)
                      | `Null -> Ok 0
                      | _ -> fail "tile.y invalid"
                    in
                    Ok ((image_path, iw, ih, id, x, y) :: acc))
                  tiles_list (Ok [])
              in
              (* same image for all tiles *)
              let* shared_path, image_w, image_h =
                match parsed with
                | [] -> fail "empty tiles[]"
                | (p, iw, ih, _, _, _) :: rest ->
                    if List.for_all (fun (p', _, _, _, _, _) -> String.equal p p') rest then
                      Ok (p, iw, ih)
                    else fail "tiles reference different image files"
              in
              let bounds_ok =
                List.for_all
                  (fun (_p, iw, ih, _id, x, y) ->
                    x >= 0 && y >= 0 && x + tile_w <= iw && y + tile_h <= ih)
                  parsed
              in
              let* () = if bounds_ok then Ok () else fail "one or more tiles out of atlas bounds" in

              let image_size = { w = image_w; h = image_h } in
              let parsed =
                List.sort (fun (_, _, _, id1, _, _) (_, _, _, id2, _, _) -> compare id1 id2) parsed
              in
              let tiles =
                parsed
                |> List.map (fun (_p, _iw, _ih, id, x, y) ->
                       {
                         id;
                         image_path = shared_path;
                         image_size;
                         size = tile_size;
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
              let r : tileset =
                {
                  class_;
                  columns = 0;
                  grid;
                  image = Some shared_path;
                  image_size = Some image_size;
                  margin;
                  name;
                  spacing;
                  tile_count;
                  tile_size;
                  tiles;
                }
              in
              Ok (L.Asset.pack (module A) r))
  end
end
