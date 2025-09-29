let ( let* ) = Result.bind

open Util

module Make (L : Luma.S) (Map : Map.Tilemap) (Tilemap_asset : L.Asset.S with type t = Map.t) =
struct
  include Types

  module Tilemap_loader :
    L.Asset_loader.LOADER with type t = Map.t and type decode = bytes and type ctx = unit = struct
    type nonrec t = Map.t
    type decode = bytes
    type ctx = unit

    module A = Tilemap_asset

    let type_id = A.type_id
    let exts = [ ".tmj" ]

    let begin_load path ~k =
      L.IO.read_file path ~k:(function Ok bytes -> k (Ok bytes) | Error e -> k (Error e))

    let finalize ctx path bytes =
      let open Luma__serialize.Json_helpers in
      try
        let json = Yojson.Safe.from_string (Bytes.unsafe_to_string bytes) in
        match field "type" json with
        | `String "map" ->
            let* tilemap = Map.from_json json path in
            Ok (L.Asset.pack (module A) tilemap)
        | `String other ->
            Error
              (L.Error.io_finalize path (Printf.sprintf "expected Tiled type=map, got %s" other))
        | _ -> Error (L.Error.io_finalize path "missing top-level \"type\"")
      with _ -> Error (L.Error.io_finalize path "invalid json")
  end
end
