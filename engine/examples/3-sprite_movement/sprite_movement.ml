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
      let scale = Luma.Math.Vec3.create 10. 10. 1. in
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

let key_axis negative positive =
  let negative_value = if Input.Keyboard.is_key_down negative then 1. else 0. in
  let positive_value = if Input.Keyboard.is_key_down positive then 1. else 0. in

  positive_value -. negative_value

let movement_direction () =
  let open Luma.Math in
  let direction =
    Vec3.create (key_axis Input.Key.A Input.Key.D) (key_axis Input.Key.W Input.Key.S) 0.
  in

  if Vec3.length_squared direction > 1. then Vec3.normalise direction else direction

let move_player () =
  Ecs.System.make_with_resources
    ~components:Ecs.Query.Component.(Required (module Transform.C) & End)
    ~resources:Ecs.Query.Resource.(Resource (module Time.R) & End)
    "move_player"
    (fun world _cmd entities (time, _) ->
      let open Transform in
      let speed = 200. in
      let direction = movement_direction () in
      let distance = speed *. time.dt in
      let dx = direction.x *. distance in
      let dy = direction.y *. distance in

      Ecs.Query.Tuple.iter1
        (fun transform ->
          transform.position.x <- transform.position.x +. dx;
          transform.position.y <- transform.position.y +. dy)
        entities;
      world)

let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.add_plugin Plugin.debug_plugin
  |> App.on Startup (setup ())
  |> App.on Update (move_player ())
  |> App.run
