open Luma__serialize.Json_helpers

let ( let* ) = Result.bind

type t = {
  objects : Object.Object_data.t list;
  colour : Types.colour option;
}

let parse_objects objects tilesets =
  let* rev =
    List.fold_left
      (fun acc object_ ->
        match acc with
        | Error _ as e -> e
        | Ok acc_list ->
            let* o = Object.Object_data.from_json object_ tilesets in
            Ok (o :: acc_list))
      (Ok []) objects
  in
  Ok (List.rev rev)

let from_json json path tilesets =
  match field "objects" json with
  | `List objects ->
      let* objects = parse_objects objects tilesets in
      Ok { colour = None; objects }
  | o ->
      let s = Yojson.Safe.pretty_to_string o in
      Error
        (Luma__core.Error.io_finalize path (Printf.sprintf "Expected a list of objects. Got %s" s))
