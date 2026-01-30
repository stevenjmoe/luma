module Object_tile_data = struct
  type t = {
    tileset_location : int;
    id : int;
    flip_h : bool;
    flip_v : bool;
    flip_d : bool;
  }

  let id o = o.id
  let flip_h o = o.flip_h
  let flip_v o = o.flip_v
  let flip_d o = o.flip_d

  (** [from_bits bits tilesets for_tileset] *)
  let from_bits bits tilesets for_tileset =
    let ( let* ) = Option.bind in
    let flags = bits land Types.all_flip_flags in
    let gid = bits land lnot Types.all_flip_flags in
    let flip_d = flags land Types.flipped_diagonally_flag = Types.flipped_diagonally_flag in
    let flip_h = flags land Types.flipped_horizontally_flag = Types.flipped_horizontally_flag in
    let flip_v = flags land Types.flipped_vertically_flag = Types.flipped_vertically_flag in

    if gid = 0 then None
    else
      let* tileset_location, id =
        match for_tileset with
        | Some _ -> failwith "unsupported for now"
        | None ->
            let* tileset_idx, tileset = Util.get_tileset_for_gid tilesets gid in
            let id = gid - tileset.first_gid in
            Some (tileset_idx, id)
      in
      Some { tileset_location; id; flip_h; flip_v; flip_d }
end

module Object_data = struct
  type horizontal_alignment =
    | Left
    | Center
    | Right
    | Justify

  type vertical_alignment =
    | Top
    | Center
    | Bottom

  type shape_size = {
    width : float;
    height : float;
  }

  type shape_point = {
    x : float;
    y : float;
  }

  type points = shape_point list

  type text = {
    font_family : string;
    pixel_size : int;
    wrap : bool;
    colour : Types.colour;
    bold : bool;
    italic : bool;
    underline : bool;
    strikeout : bool;
    kerning : bool;
    halign : horizontal_alignment;
    valign : vertical_alignment;
    text : string;
    width : float;
    height : float;
  }

  type object_shape =
    | Rect of shape_size
    | Ellipse of shape_size
    | Polyline of points
    | Polygon of points
    | Point of shape_point
    | Text of text

  type t = {
    id : int;
    tile : Object_tile_data.t option;
    name : string;
    user_type : string;
    x : float;
    y : float;
    rotation : float;
    visible : bool;
    shape : object_shape;
    properties : string list (* TODO *);
  }

  let size_from_json json =
    let open Luma__serialize.Json_helpers in
    let ( let* ) = Result.bind in
    let* width = parse_float "width" json in
    let* height = parse_float "height" json in
    Ok { width; height }

  let point_from_json json =
    let open Luma__serialize.Json_helpers in
    let ( let* ) = Result.bind in
    let* x = parse_float "x" json in
    let* y = parse_float "y" json in
    Ok { x; y }

  let polygon_from_json points =
    let ( let* ) = Result.bind in
    let* points =
      List.fold_left
        (fun acc point ->
          match acc with
          | Error _ -> Error (Luma__core.Error.io_finalize "" "")
          | Ok acc ->
              let* point = point_from_json point in
              Ok (point :: acc))
        (Ok []) points
    in
    Ok (Polygon points)

  let polyline_from_json points =
    let ( let* ) = Result.bind in
    let* points =
      List.fold_left
        (fun acc point ->
          match acc with
          | Error _ -> Error (Luma__core.Error.io_finalize "" "")
          | Ok acc ->
              let* point = point_from_json point in
              Ok (point :: acc))
        (Ok []) points
    in
    Ok (Polyline points)

  let from_json json tilesets =
    let open Luma__serialize.Json_helpers in
    let ( let* ) = Result.bind in

    let* id = parse_int_opt "id" json in
    let* gid = parse_int_opt "gid" json in
    let* name = parse_string_opt "name" json in
    let* width = parse_float_opt "width" json in
    let* height = parse_float_opt "height" json in
    let* visible = parse_bool_opt "visible" json in
    let* rotation = parse_float_opt "rotation" json in
    let* x = parse_float_opt "x" json in
    let* y = parse_float_opt "y" json in
    let x = Option.value ~default:0. x in
    let y = Option.value ~default:0. y in
    let tile = Option.bind gid (fun gid -> Object_tile_data.from_bits gid tilesets None) in
    (* TODO *)
    let _template = () in

    let visible = Option.value ~default:true visible in
    let width = Option.value ~default:0. width in
    let height = Option.value ~default:0. height in
    let rotation = Option.value ~default:0. rotation in
    let id = Option.value ~default:0 id in
    let name = Option.value ~default:"" name in

    let ellipse' = "ellipse" in
    let point' = "point" in
    let polygon' = "polygon" in
    let polyline' = "polyline" in

    (* check for each shape type and return when one matches. Default to rect. *)
    let* shape =
      List.fold_left
        (fun s attr ->
          match s with
          | Ok None -> (
              match Yojson.Safe.Util.member attr json with
              | `Bool e when attr = ellipse' && e = true -> Ok (Some (Ellipse { width; height }))
              | `Bool p when attr = point' && p = true -> Ok (Some (Point { x; y }))
              | `List p when attr = polygon' -> (
                  match polygon_from_json p with Error e -> Error e | Ok p -> Ok (Some p))
              | `List p when attr = polyline' -> (
                  match polyline_from_json p with Error e -> Error e | Ok p -> Ok (Some p))
              | _ -> Ok None)
          | Ok s -> Ok s
          | Error e -> Error e)
        (Ok None)
        [ ellipse'; point'; polygon'; polyline' ]
    in
    let shape = Option.value ~default:(Rect { width; height }) shape in

    Ok { id; tile; name; user_type = ""; x; y; rotation; visible; shape; properties = [] }
end
