let ( let* ) = Result.bind

type tile_data = {
  id : int;
  image : string option;
  image_width : int;
  image_height : int;
  properties : string option; (* TODO *)
  animation : string option; (* TODO *)
  user_type : string option; (* TODO *)
  probability : float;
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
  image : Image.t option;
  tiles : (int, tile_data) Hashtbl.t;
  wang_sets : string list option; (*TODO*)
  user_type : string option;
}

type map_tileset = {
  first_gid : int;
  tileset : t;
}

let tile_data_from_json json =
  let open Luma__serialize.Json_helpers in
  let* user_type = parse_string_opt "type" json in
  let* user_class = parse_string_opt "class" json in
  let* probability = parse_float_opt "probability" json in
  let* id = parse_int "id" json in
  let* image = parse_string_opt "image" json in
  let* image_width = parse_int_opt "image_width" json in
  let* image_height = parse_int_opt "image_height" json in

  let image_width = Option.value ~default:0 image_width in
  let image_height = Option.value ~default:0 image_height in
  let probability = Option.value ~default:100. probability in

  Ok
    {
      id;
      image;
      image_width;
      image_height;
      properties = None;
      animation = None;
      user_type;
      probability;
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
                let* r = tile_data_from_json j in
                Ok (r :: rs))
          (Ok []) l
      in
      let tbl = Hashtbl.create 16 in
      List.rev rev |> List.iter (fun t -> Hashtbl.add tbl t.id t);
      Ok tbl
  | other ->
      let other = Yojson.Safe.pretty_to_string other in
      Error
        (Luma__core.Error.io_finalize path
           (Printf.sprintf "tileset.tiles expected tile list, got %s" other))

let from_json json path =
  let open Luma__serialize.Json_helpers in
  let* spacing = parse_int "spacing" json in
  let* margin = parse_int "margin" json in
  let* columns = parse_int "columns" json in
  let* name = parse_string "name" json in
  let* user_type = parse_string_opt "type" json in
  let* user_class = parse_string_opt "class" json in
  let* tile_count = parse_int "tilecount" json in
  let* tile_width = parse_int "tilewidth" json in
  let* tile_height = parse_int "tileheight" json in
  let* tiles = tiles_from_json json path in

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
      image = None;
      tiles;
      wang_sets = None;
      user_type = None;
    }
