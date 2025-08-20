open Luma__asset
open Luma__id
open Luma__app
open Luma__ecs

module type S = sig
  type t

  module A : Luma__asset.Asset.S with type t = t

  val add_plugin : App.t -> App.t
end

module Make (D : Luma__driver.Driver.S) : S with type t = D.Texture.t = struct
  type t = D.Texture.t

  module A = Luma__asset.Asset.Make (struct
    type inner = t
  end)

  module Texture_loader : Loader.LOADER with type t = D.Texture.t and type decode = string = struct
    type t = D.Texture.t
    type decode = string

    module A = A

    let exts = [ ".png"; "jpg" ]
    let type_id = A.type_id

    let begin_load path ~k =
      D.IO.read_file path ~k:(function
        | Ok bytes -> k (Ok (Bytes.unsafe_to_string bytes))
        | Error msg -> k (Error msg))

    let finalize path data =
      let ext = Filename.extension path in
      try
        let img = D.Image.load_image_from_memory ext data (String.length data) in
        let tex = D.Texture.load_texture_from_image img in
        let packed = Asset.pack (module A) tex in
        Ok packed
      with exn -> Error (Printexc.to_string exn)
  end

  let register_loader () =
    System.make_with_resources ~components:End
      ~resources:Query.Resource.(Resource (module Server.R) & End)
      "register_texture_loader"
      (fun w _ (server, _) ->
        Server.register_loader server (Loader.Packed (module Texture_loader));
        w)

  let add_plugin app = app |> App.on Startup @@ register_loader ()
end
