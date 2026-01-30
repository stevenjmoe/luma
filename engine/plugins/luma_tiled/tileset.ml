let ( let* ) = Result.bind

type object_ = {
  id : int;
  width : float;
  height : float;
  rotation : float;
  visible : bool;
  point : bool;
  x : float;
  y : float;
}

type object_group = {
  draw_order : string;
  id : int option;
  opacity : float;
  type_ : string;
  visible : bool;
  x : float;
  y : float;
  objects : object_ array;
}

type tile_data = {
  id : int;  (** Local id of the tile. *)
  image : Types.image option;
  properties : string option; (* TODO *)
  animation : string option; (* TODO *)
  user_type : string option; (* TODO *)
  probability : float;  (** Percentage chance this tile is chosen over others in the editor. *)
  x : float;
  y : float;
  width : float;
  height : float;
  object_group : object_group option;
}

type t = {
  source : string;
  name : string;
  tile_width : int;
  tile_height : int;
  spacing : int;
  margin : int;
  tile_count : int;
  columns : int;
  offset_x : int;
  offset_y : int;
  image : Types.image option;
  tiles : (int, tile_data) Hashtbl.t;
  wang_sets : string list option; (*TODO*)
  user_type : string option;
}

type map_tileset = {
  first_gid : int;
  tileset : t;
}

let parse_image json path : (Types.image option, Luma__core.Error.error) result =
  let open Luma__serialize.Json_helpers in
  let ( let* ) = Result.bind in
  let* image = parse_string_opt "image" json in
  match image with
  | Some image ->
      let* width = parse_int_opt "width" json in
      let* image_width = parse_int_opt "imagewidth" json in
      let width = Option.value width ~default:(Option.value image_width ~default:0) in

      let* height = parse_int_opt "height" json in
      let* image_height = parse_int_opt "imageheight" json in
      let height = Option.value height ~default:(Option.value image_height ~default:0) in

      let full_path = Filename.concat path image in
      (* TODO: let* transparent_colour = parse_string_opt "transparentcolor" json in*)

      Ok
        (Some { source = full_path; width; height; transparent_colour = None } : Types.image option)
  | None -> Ok None

let object_from_json json =
  let open Luma__serialize.Json_helpers in
  let* id = parse_int "id" json in
  let* width = parse_float_opt "width" json in
  let* height = parse_float_opt "height" json in
  let* rotation = parse_float_opt "rotation" json in
  let* visible = parse_bool_opt "visible" json in
  let* x = parse_float_opt "x" json in
  let* y = parse_float_opt "y" json in
  let* point = parse_bool_opt "point" json in

  let width = Option.value ~default:0. width in
  let height = Option.value ~default:0. height in
  let rotation = Option.value ~default:0. rotation in
  let visible = Option.value ~default:true visible in
  let point = Option.value ~default:false point in
  let x = Option.value ~default:0. x in
  let y = Option.value ~default:0. y in

  Ok { id; width; height; rotation; visible; point; x; y }

let object_group_from_json json =
  let open Luma__serialize.Json_helpers in
  let parse og_json =
    let* draw_order = parse_string_opt "draworder" og_json in
    let* id = parse_int_opt "id" og_json in
    let* opacity = parse_float_opt "opacity" og_json in
    let* type_ = parse_string_opt "type" og_json in
    let* visible = parse_bool_opt "visible" og_json in
    let* x = parse_float_opt "x" og_json in
    let* y = parse_float_opt "y" og_json in

    let draw_order = Option.value ~default:"index" draw_order in
    let opacity = Option.value ~default:1. opacity in
    let type_ = Option.value ~default:"" type_ in
    let visible = Option.value ~default:true visible in
    let x = Option.value ~default:0. x in
    let y = Option.value ~default:0. y in

    let* objects =
      match field "objects" og_json with
      | `List l ->
          let* rev =
            List.fold_left
              (fun acc j ->
                match acc with
                | Error _ as e -> e
                | Ok rs ->
                    let* o = object_from_json j in
                    Ok (o :: rs))
              (Ok []) l
          in
          Ok (Array.of_list (List.rev rev))
      | other ->
          let other = Yojson.Safe.pretty_to_string other in
          Error
            (Luma__core.Error.io_finalize "tileset.objectgroup"
               (Printf.sprintf "expected list for objects, got %s" other))
    in

    Ok { draw_order; id; opacity; type_; visible; x; y; objects }
  in

  match field "objectgroup" json with
  | `Null -> Ok None
  | `Assoc _ as og_json ->
      let* g = parse og_json in
      Ok (Some g)
  | other ->
      let other = Yojson.Safe.pretty_to_string other in
      Error
        (Luma__core.Error.io_finalize "tileset.tile.objectgroup"
           (Printf.sprintf "expected objectgroup object or null, got %s" other))

let tile_data_from_json json path =
  let open Luma__serialize.Json_helpers in
  let* user_type = parse_string_opt "type" json in
  let* probability = parse_float_opt "probability" json in
  let* id = parse_int "id" json in
  let* image = parse_image json path in
  let* x = parse_int_opt "x" json in
  let* y = parse_int_opt "y" json in
  let x = Option.value ~default:0 x |> float in
  let y = Option.value ~default:0 y |> float in
  let* width = parse_int_opt "width" json in
  let* height = parse_int_opt "height" json in
  let width = Option.value ~default:0 width |> float in
  let height = Option.value ~default:0 height |> float in
  let probability = Option.value ~default:100. probability in
  let* object_group = object_group_from_json json in

  Ok
    {
      id;
      image;
      properties = None;
      animation = None;
      user_type;
      probability;
      x;
      y;
      width;
      height;
      object_group;
    }

let tiles_from_json json path =
  let open Luma__serialize.Json_helpers in
  match field "tiles" json with
  | `List l ->
      let* rev =
        List.fold_left
          (fun acc j ->
            match acc with
            | Error _ as e -> e
            | Ok rs ->
                let* r = tile_data_from_json j path in
                Ok (r :: rs))
          (Ok []) l
      in
      let tbl = Hashtbl.create 16 in
      List.rev rev |> List.iter (fun tile -> Hashtbl.add tbl tile.id tile);
      Ok tbl
  | other ->
      let other = Yojson.Safe.pretty_to_string other in
      Error
        (Luma__core.Error.io_finalize path
           (Printf.sprintf "tileset.tiles expected tile list, got %s" other))

let from_json json path =
  let open Luma__serialize.Json_helpers in
  let path = Filename.dirname path in
  let* spacing = parse_int "spacing" json in
  let* margin = parse_int "margin" json in
  let* columns = parse_int "columns" json in
  let* name = parse_string "name" json in
  let* tile_count = parse_int "tilecount" json in
  let* tile_width = parse_int "tilewidth" json in
  let* tile_height = parse_int "tileheight" json in
  let* tiles = tiles_from_json json path in
  let* image = parse_image json path in

  Ok
    {
      source = path;
      name;
      tile_width;
      tile_height;
      spacing;
      margin;
      tile_count;
      columns;
      offset_x = 0;
      offset_y = 0;
      image;
      tiles;
      wang_sets = None;
      user_type = None;
    }
