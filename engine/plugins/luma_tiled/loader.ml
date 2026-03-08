let ( let* ) = Result.bind

module Make
    (L : Luma.S)
    (Map : Map.S)
    (Tilemap_asset : L.Asset.S with type t = Map.source)
    (Tileset_asset : L.Asset.S with type t = Tileset.t) =
struct
  include Types

  module Tilemap_loader : L.Asset_loader.LOADER with type t = Map.source and type ctx = unit =
  struct
    type nonrec t = Map.source
    type ctx = unit

    module A = Tilemap_asset

    let type_id = A.type_id
    let exts = [ ".tmj" ]
    let begin_load path = L.Driver.IO.read_file path

    let finalize _ path (bytes : bytes) =
      let open Luma__serialize.Json_helpers in
      try
        let json = Yojson.Safe.from_string (Bytes.to_string bytes) in
        match field "type" json with
        | `String "map" ->
            let* tilemap = Map.source_from_json json path in
            Ok (L.Asset.pack (module A) tilemap)
        | `String other ->
            Error
              (L.Error.io_finalize path (Printf.sprintf "expected Tiled type=map, got %s" other))
        | _ -> Error (L.Error.io_finalize path "missing top-level \"type\"")
      with _ -> Error (L.Error.io_finalize path "invalid json")
  end

  module Tileset_loader : L.Asset_loader.LOADER with type t = Tileset.t and type ctx = unit = struct
    type nonrec t = Tileset.t
    type ctx = unit

    module A = Tileset_asset

    let type_id = A.type_id
    let exts = [ ".tsj" ]
    let begin_load path = L.Driver.IO.read_file path

    let finalize _ path (bytes : bytes) =
      try
        let json = Yojson.Safe.from_string (Bytes.to_string bytes) in
        let* tileset = Tileset.from_json json path in
        Ok (L.Asset.pack (module A) tileset)
      with _ -> Error (L.Error.io_finalize path "invalid json")
  end
end
