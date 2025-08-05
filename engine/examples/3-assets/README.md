## 3-assets

Luma uses an asset server + asset registry pattern. You load assets once using the `Asset_server`, which returns a typed handle, and access them later via the `Assets` resource and that handle.

### 1. Load the Asset with Asset_server

The Asset_server is responsible for loading assets from disk and returning a handle (a simple typed ID). This handle can be stored on components or passed around and used later.
```ocaml
match Asset_server.load asset_server "assets/Player Idle 48x48.png" with
| Ok handle -> (* Do something with the handle *)
| Error _ -> failwith "Could not load texture"
```
>The Asset_server automatically stores the loaded asset in the central Assets table.

### 2. Retrieve the asset

When rendering or animating, you use `Assets.get` to look up an asset by **handle and type**:

```ocaml
match Assets.get (module Texture.A) assets sprite.image with
| Some texture ->
    Renderer.draw_texture texture
      ~position:(Vec2.create 10. 10.)
      ~size:(Vec2.create 500. 500.)
      ~frame_index:animation_config.current_index
      ~texture_atlas:sprite.texture_atlas ()
    |> ignore
| None -> ()
```
>The type witness module (module Texture.A) is necessary because Assets.t is a heterogeneous table — you must specify the type you're expecting.


### 3. Registering a loader

The `Asset_server.load` function doesn't require any type information up front. Instead, it dispatches based on the file extension and a registered loader.

Luma currently includes a loader for image files (".jpg", ".png"), with more to be added soon. You can register your own like so:

```ocaml
let image_loader =
    {
      Luma__asset.Loader.exts = [ ".png"; ".jpg" ];
      load =
        (fun path ->
          let image = Raylib.load_image path in
          let texture = Raylib.load_texture_from_image image in
          Ok (Loaded ((module Luma__image.Image.Texture.A), texture)));
    }
  in
  Luma__asset.Server.register_loader server image_loader
```
>Loaders associate file extensions with typed loading logic, returning the asset along with its type witness ((module Texture.A) in this case).
