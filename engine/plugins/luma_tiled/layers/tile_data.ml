open Luma__serialize.Json_helpers

let ( let* ) = Result.bind

type tile_layer_data = {
  tileset_index : int;
  id : int;
  flip_h : bool;
  flip_v : bool;
  flip_d : bool;
}

type finite = {
  width : int;
  height : int;
  tiles : tile_layer_data option list;
}

type t =
  | Finite of finite
  | Infinite

let from_bits bits tilesets =
  let ( let* ) = Option.bind in
  let flags = bits land Types.all_flip_flags in
  let gid = bits land lnot Types.all_flip_flags in
  let flip_d = flags land Types.flipped_diagonally_flag = Types.flipped_diagonally_flag in
  let flip_h = flags land Types.flipped_horizontally_flag = Types.flipped_horizontally_flag in
  let flip_v = flags land Types.flipped_vertically_flag = Types.flipped_vertically_flag in

  if gid = 0 then None
  else
    let* tileset_index, id =
      let* tileset_index, tileset = Util.get_tileset_for_gid tilesets gid in
      let id = gid - tileset.first_gid in
      Some (tileset_index, id)
    in
    Some { tileset_index; id; flip_h; flip_v; flip_d }

let decode_csv csv path tilesets =
  let* rev =
    List.fold_left
      (fun acc d ->
        match acc with
        | Error _ as e -> e
        | Ok acc_list -> (
            match Yojson.Safe.Util.to_int_option d with
            | Some i ->
                let tile = from_bits i tilesets in
                Ok (tile :: acc_list)
            | None -> Util.fail path "tilelayer.data must be a list of ints"))
      (Ok []) csv
  in
  Ok (List.rev rev)

let parse_data_line encoding compression data path tilesets =
  match (encoding, compression) with
  | "csv", None -> decode_csv data path tilesets
  | _ -> Error (Luma__core.Error.io_finalize path "Only csv is currently supported")

let finite_from_json json path width height tilesets =
  let* encoding = parse_string_opt "encoding" json in
  let encoding = Option.value ~default:"csv" encoding in
  let* compression = parse_string_opt "compression" json in

  match field "data" json with
  | `List d ->
      let* tiles = parse_data_line encoding compression d path tilesets in
      Ok (Finite { width; height; tiles })
  | j ->
      let e = Yojson.Safe.pretty_to_string j in
      Error
        (Luma__core.Error.io_finalize path
           (Printf.sprintf "tilelayer.data expected list. Got %s" e))

let from_json json path infinite tilesets =
  let* width, height =
    let* width = parse_int "width" json in
    let* height = parse_int "height" json in
    Ok (width, height)
  in
  if not infinite then finite_from_json json path width height tilesets
  else Error (Luma__core.Error.io_finalize path "Only finite maps supported for now")
