type t = Raylib.Texture.t

module A = Luma__asset.Asset.Make (struct
  type inner = t
end)

let () =
  Luma__asset.Server.register_loader_hook (fun server ->
      Luma__asset.Server.register_loader server
        {
          Luma__asset.Loader.exts = [ ".png"; ".jpg" ];
          load =
            (fun path ->
              let image = Raylib.load_image path in
              let texture = Raylib.load_texture_from_image image in
              Ok (Loaded ((module A), texture)));
        })
