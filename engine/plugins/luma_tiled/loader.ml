module Make (L : Luma.S) = struct
  include Types

  module Tilemap_loader :
    L.Asset_loader.LOADER with type t = t and type decode = bytes and type ctx = unit = struct
    type nonrec t = t
    type decode = bytes
    type ctx = unit

    module A = L.Asset.Make (struct
      type inner = t
    end)

    let type_id = A.type_id
    let exts = [ "tmj" ]
    let begin_load path ~k = failwith ""

    let finalize ctx path bytes =
      try
        let _ = Yojson.Safe.from_string (Bytes.unsafe_to_string bytes) in
        failwith ""
      with exn -> Error (L.Error.io_finalize path "invalid json")
  end

  module Tileset_loader :
    L.Asset_loader.LOADER with type t = tileset and type decode = bytes and type ctx = unit = struct
    type t = tileset
    type decode = bytes
    type ctx = unit

    module A = L.Asset.Make (struct
      type inner = t
    end)

    let type_id = A.type_id
    let exts = [ "tsj" ]
    let begin_load path ~k = failwith ""

    let finalize ctx path bytes =
      try
        let _ = Yojson.Safe.from_string (Bytes.unsafe_to_string bytes) in
        failwith ""
      with exn -> Error (L.Error.io_finalize path "invalid json")
  end
end
