module type Tilemap = sig
  type t = {
    background_colour : string option;
    version : string;
    source : string;
    orientation : Types.orientation;
    width : int;
    height : int;
    tile_width : int;
    tile_height : int;
    hex_side_length : int option;
    stagger_axis : Types.stagger_axis;
    stagger_index : Types.stagger_index;
    tilesets : Tileset.t list;
  }

  val from_json : Yojson.Safe.t -> string -> (t, Luma__core__Error.error) result
end

module Tilemap (L : Luma.S) : Tilemap = struct
  let ( let* ) = Result.bind

  type t = {
    background_colour : string option;
    version : string;
    source : string;
    orientation : Types.orientation;
    width : int;
    height : int;
    tile_width : int;
    tile_height : int;
    hex_side_length : int option;
    stagger_axis : Types.stagger_axis;
    stagger_index : Types.stagger_index;
    tilesets : Tileset.t list;
  }

  let rec parse_layers layers path infinite tilesets =
    let open Luma__serialize.Json_helpers in
    let* rev =
      List.fold_left
        (fun acc j ->
          match acc with
          | Error _ as e -> e
          | Ok rs ->
              let* r = Layers.Layer_data.from_json j path infinite tilesets in
              Ok (r :: rs))
        (Ok []) layers
    in
    Ok (List.rev rev)

  (* TODO: support embedded tilesets *)
  let parse_tileset_external_references json path =
    let open Luma__serialize.Json_helpers in
    let* rev =
      List.fold_left
        (fun acc j ->
          match acc with
          | Error _ as e -> e
          | Ok r ->
              let* first_gid = parse_int "firstgid" j in
              let* source_path = parse_string "source" j in
              let path = Filename.dirname path in
              let full_path = Filename.concat path source_path in
              let source = L.IO.read_file_blocking full_path |> Yojson.Safe.from_string in
              let* tileset = Tileset.from_json source full_path in
              Ok (({ first_gid; tileset } : Tileset.map_tileset) :: r))
        (Ok []) json
    in
    Ok (List.rev rev)

  let from_json json path =
    let open Luma__serialize.Json_helpers in
    let* colour = parse_string_opt "backgroundcolor" json in
    let* infinite = parse_bool_opt "infinite" json in
    let* user_class = parse_string_opt "class" json in
    let* user_type = parse_string_opt "type" json in
    let* stagger_axis = parse_string_opt "staggeraxis" json in
    let* stagger_index = parse_string_opt "staggerindex" json in
    let* hex_side_length = parse_int_opt "hexsidelength" json in
    let* version = parse_string "version" json in
    let* orientation = parse_string "orientation" json in
    let* width = parse_int "width" json in
    let* height = parse_int "height" json in
    let* tile_width = parse_int "tilewidth" json in
    let* tile_height = parse_int "tileheight" json in

    let* orientation =
      match orientation with
      | "orthogonal" -> Ok Types.Orthogonal
      | "isometric" -> Ok Isometric
      | "staggered" -> Ok Staggered
      | "hexagonal" -> Ok Hexagonal
      | other ->
          Error
            (Luma__core.Error.io_finalize path
               (Printf.sprintf
                  "map.orientation expected one of orthogonal, isometric, staggered, or hexagonal \
                   but got %s."
                  other))
    in
    let* stagger_axis = match stagger_axis with Some "x" -> Ok Types.X | _ -> Ok Types.Y in
    let* stagger_index =
      match stagger_index with Some "even" -> Ok Types.Even | _ -> Ok Types.Odd
    in

    let infinite = Option.value ~default:false infinite in
    let* tilesets =
      match field "tilesets" json with
      | `List l -> parse_tileset_external_references l path
      | other ->
          let s = Yojson.Safe.pretty_to_string other in
          Error
            (Luma__core.Error.io_finalize path
               (Printf.sprintf "Expected a list of tilesets. Got %s" s))
    in
    let* layers =
      match field "layers" json with
      | `List l -> parse_layers l "path" infinite tilesets
      | other ->
          let s = Yojson.Safe.pretty_to_string other in
          Error
            (Luma__core.Error.io_finalize path
               (Printf.sprintf "Expected a list of layers. Got %s" s))
    in
    (* firstgid is no longer needed *)
    let tilesets = List.map (fun (t : Tileset.map_tileset) -> t.tileset) tilesets in

    Ok
      {
        background_colour = colour;
        version;
        source = path;
        orientation;
        width;
        height;
        tile_width;
        tile_height;
        hex_side_length;
        stagger_axis;
        stagger_index;
        tilesets;
      }
end
