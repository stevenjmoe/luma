type layer_type =
  | Tiles of Tile_data.t
  | Object of Object_data.t

type t = {
  name : string;
  id : int;
  visible : bool;
  offset_x : float;
  offset_y : float;
  parallax_x : float;
  parallax_y : float;
  opacity : float;
  tint_colour : Types.colour option;
  properties : string list option;
  user_type : string option;
  layer_type : layer_type;
}

let ( let* ) = Result.bind

let from_json json path infinite tilesets =
  let open Luma__serialize.Json_helpers in
  let* opacity = parse_float_opt "opacity" json in
  let* visible = parse_bool_opt "visible" json in
  let* offset_x = parse_float_opt "offsetx" json in
  let* offset_y = parse_float_opt "offsety" json in
  let* parallax_x = parse_float_opt "parallaxx" json in
  let* parallax_y = parse_float_opt "parallaxy" json in
  let* name = parse_string "name" json in
  let* id = parse_int_opt "id" json in
  let* user_type = parse_string_opt "type" json in
  let* layer_type = parse_string "type" json in

  let* layer_type =
    match layer_type with
    | "tilelayer" ->
        let* t = Tile_data.from_json json path infinite tilesets in
        Ok (Tiles t)
    | "objectgroup" ->
        let* o = Object_data.from_json json path tilesets in
        Ok (Object o)
    | _ -> Error (Luma__core.Error.io_finalize path "Only tiles and objects currently supported")
  in

  let visible = Option.value ~default:true visible in
  let offset_x = Option.value ~default:0. offset_x in
  let offset_y = Option.value ~default:0. offset_y in
  let parallax_x = Option.value ~default:1. parallax_x in
  let parallax_y = Option.value ~default:1. parallax_y in
  let opacity = Option.value ~default:1. opacity in
  let id = Option.value ~default:0 id in

  Ok
    {
      visible;
      offset_x;
      offset_y;
      parallax_x;
      parallax_y;
      opacity;
      (* TODO:*)
      tint_colour = None;
      name;
      id;
      user_type;
      layer_type;
      (* TODO:*)
      properties = None;
    }
