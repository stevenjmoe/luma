open Luma__asset
open Luma__app
open Luma__ecs
open Luma__core

module type S = sig
  type t

  module A : Luma__asset.Asset.S with type t = t
  module Assets : Luma__asset.Assets.For with type t = Luma__asset.Assets.t and type asset = t

  val add_plugin : App.t -> App.t
end

module Make (D : Luma__driver.Driver.S) : S with type t = D.Texture.t = struct
  type t = D.Texture.t

  module A = Luma__asset.Asset.Make (struct
    type inner = t
  end)

  module Assets = Luma__asset.Assets.For (A)

  module Texture_loader :
    Loader.LOADER with type t = D.Texture.t and type decode = bytes and type ctx = unit = struct
    type t = D.Texture.t
    type decode = bytes
    type ctx = unit

    module A = A

    let exts = [ ".png"; ".jpg" ]
    let type_id = A.type_id

    let begin_load path ~k =
      D.IO.read_file path ~k:(function Ok bytes -> k (Ok bytes) | Error msg -> k (Error msg))

    let finalize _ path data =
      let data = Bytes.to_string data in
      let ext = Filename.extension path in
      try
        let img = D.Image.load_image_from_memory ext data (String.length data) in
        let tex = D.Texture.load_texture_from_image img in
        let packed = Asset.pack (module A) tex in
        Ok packed
      with _exn -> Error (Error.io_finalize path "Could not load texture from file.")
  end

  let register_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Server.R) & End)
      "register_texture_loader"
      (fun w _ _ (server, _) ->
        Server.register_loader server
          (module Texture_loader)
          ~ctx_provider:Loader.Context_provider.no_ctx;
        w)

  let add_plugin app = app |> App.on Startup @@ register_loader ()
end
