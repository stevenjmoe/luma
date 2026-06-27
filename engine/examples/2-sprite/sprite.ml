(* 
   Luma will render entities which have both a Sprite and Transform component.

   dune exec examples/2-sprite/sprite.exe --profile release
*)

module L = Luma.Make (Luma_driver_raylib.Driver)
open L

let setup () =
  Ecs.System.make_with_resources ~components:End
    ~resources:Ecs.Query.Resource.(Resource (module Asset_server.R) & End)
    "setup"
    (fun w cmd _entities (server, _) ->
      let pos_x = L.screen_width () / 2 |> float in
      let pos_y = L.screen_height () / 2 |> float in
      let position = Luma.Math.Vec3.create pos_x pos_y 0. in
      let scale = Luma.Math.Vec3.create 10. 10. 0. in
      let trans = Luma.Transform.create ~position ~rotation:0. ~scale () in
      let sprite_asset =
        Asset_server.load_exn (module L.Image.Texture.A) server "examples/assets/smiley.png"
      in

      let sprite = Sprite.from_image sprite_asset in
      Ecs.Command.spawn ~name:"sprite" cmd
        [
          Ecs.Component.component (module Transform.C) trans;
          Ecs.Component.component (module Sprite.C) sprite;
        ]
      |> ignore;

      let target = Luma.Math.Vec2.create pos_x pos_y in
      let camera = L.Camera.make ~target ~rotation:0. ~zoom:1. () in
      Ecs.Command.spawn ~name:"camera" cmd [ Ecs.Component.component (module L.Camera.C) camera ]
      |> ignore;

      w)

let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.add_plugin Plugin.debug_plugin
  |> App.on Startup (setup ())
  |> App.run
