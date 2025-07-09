module type S = sig
  type t

  module A : Luma__asset.Asset.S with type t = t
end

module Make (D : Luma__driver.Driver.S) : S with type t = D.Texture.t = struct
  type t = D.Texture.t

  module A = Luma__asset.Asset.Make (struct
    type inner = t
  end)

  (* TODO: potentially unsafe to load textures here *)
  let () =
    Luma__asset.Server.register_loader_hook (fun server ->
        Luma__asset.Server.register_loader server
          {
            Luma__asset.Loader.exts = [ ".png"; ".jpg" ];
            load =
              (fun path ->
                let image = D.Image.load_image path in
                let texture = D.Texture.load_texture_from_image image in
                Ok (Loaded ((module A), texture)));
            type_id = A.type_id;
          })
end
