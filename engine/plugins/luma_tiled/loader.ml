module Make (L : Luma.S) = struct
  include Types

  type ctx = unit

  module Tilemap_loader :
    L.Asset_loader.LOADER with type t = t and type decode = bytes and type ctx = ctx = struct
    type nonrec t = t
    type decode = bytes
    type nonrec ctx = ctx

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
