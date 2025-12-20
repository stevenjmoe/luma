open Luma__math

module type S = sig
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
    layers : Layers.Layer_data.t list;
    parallax_origin : Vec2.t;
  }

  val from_json : Yojson.Safe.t -> string -> (t, Luma__core__Error.error) result

  module Format : sig
    val pp : Format.formatter -> t -> unit
    val show : 'a Fmt.t -> 'a -> string
  end
end

module Tilemap (L : Luma.S) : S = struct
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
    layers : Layers.Layer_data.t list;
    parallax_origin : Vec2.t;
  }

  let parse_layers layers path infinite tilesets =
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
    let* stagger_axis = parse_string_opt "staggeraxis" json in
    let* stagger_index = parse_string_opt "staggerindex" json in
    let* hex_side_length = parse_int_opt "hexsidelength" json in
    let* version = parse_string "version" json in
    let* orientation = parse_string "orientation" json in
    let* width = parse_int "width" json in
    let* height = parse_int "height" json in
    let* tile_width = parse_int "tilewidth" json in
    let* tile_height = parse_int "tileheight" json in
    let* parallax_origin_x = parse_float_opt "parallaxoriginx" json in
    let parallax_origin_x = Option.value ~default:0. parallax_origin_x in
    let* parallax_origin_y = parse_float_opt "parallaxoriginy" json in
    let parallax_origin_y = Option.value ~default:0. parallax_origin_y in
    let parallax_origin = Vec2.create parallax_origin_x parallax_origin_y in

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
      | `List l -> parse_layers l path infinite tilesets
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
        layers;
        parallax_origin;
      }

  module Format = struct
    module F = Fmt

    let kv name ppv fmt v = F.pf fmt "%s: @[%a@]" name ppv v
    let pp_list pp_elt = F.brackets (F.list ~sep:F.cut pp_elt)
    let pp_option pp_v = F.option pp_v
    let pp_pair ~sep a b = F.pair ~sep:(F.any sep) a b

    let pp_hashtbl_kv pp_k pp_v =
      let binding = pp_pair ~sep:" -> " pp_k pp_v in
      F.brackets (F.hashtbl ~sep:F.cut binding)

    let pp_orientation fmt = function
      | Types.Hexagonal -> F.string fmt "Hexagonal"
      | Types.Isometric -> F.string fmt "Isometric"
      | Types.Orthogonal -> F.string fmt "Orthogonal"
      | Types.Staggered -> F.string fmt "Staggered"

    let pp_axis fmt = function Types.X -> F.string fmt "X" | Types.Y -> F.string fmt "Y"

    let pp_index fmt = function
      | Types.Even -> F.string fmt "Even"
      | Types.Odd -> F.string fmt "Odd"

    let pp_colour fmt (_ : Types.colour) = F.string fmt "<colour>"

    let pp_image fmt (i : Types.image) =
      F.pf fmt "@[<v>%a@,%a@,%a@,%a@]" (kv "source" F.string) i.source (kv "width" F.int) i.width
        (kv "height" F.int) i.height
        (kv "transparent_colour" (pp_option pp_colour))
        i.transparent_colour

    let pp_tile_data fmt (t : Tileset.tile_data) =
      let image_source, image_width, image_height =
        match t.image with Some i -> (Some i.source, i.width, i.height) | None -> (None, 0, 0)
      in
      F.pf fmt "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@]" (kv "id" F.int) t.id
        (kv "image" (pp_option F.string))
        image_source (kv "image_width" F.int) image_width (kv "image_height" F.int) image_height
        (kv "properties" (pp_option F.string))
        t.properties
        (kv "animation" (pp_option F.string))
        t.animation
        (kv "user_type" (pp_option F.string))
        t.user_type (kv "probability" F.float) t.probability

    let pp_tileset fmt (s : Tileset.t) =
      F.pf fmt "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@]" (kv "source" F.string)
        s.source (kv "name" F.string) s.name (kv "tile_width" F.int) s.tile_width
        (kv "tile_height" F.int) s.tile_height (kv "spacing" F.int) s.spacing (kv "margin" F.int)
        s.margin (kv "tile_count" F.int) s.tile_count (kv "columns" F.int) s.columns
        (kv "offset_x" F.int) s.offset_x (kv "offset_y" F.int) s.offset_y
        (kv "image" (pp_option pp_image))
        s.image
        (kv "tiles" (pp_hashtbl_kv F.int pp_tile_data))
        s.tiles
        (kv "wang_sets" (pp_option (pp_list F.string)))
        s.wang_sets

    let pp fmt (m : t) =
      F.pf fmt "@[<v>%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@,%a@]"
        (kv "background_colour" (pp_option F.string))
        m.background_colour (kv "version" F.string) m.version (kv "source" F.string) m.source
        (kv "orientation" pp_orientation) m.orientation (kv "width" F.int) m.width
        (kv "height" F.int) m.height (kv "tile_width" F.int) m.tile_width (kv "tile_height" F.int)
        m.tile_height
        (kv "hex_side_length" (pp_option F.int))
        m.hex_side_length (kv "stagger_axis" pp_axis) m.stagger_axis (kv "stagger_index" pp_index)
        m.stagger_index
        (kv "tilesets" (pp_list pp_tileset))
        m.tilesets

    let show (pp : 'a Fmt.t) (x : 'a) : string = F.to_to_string pp x
  end
end
