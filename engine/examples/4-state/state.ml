(*
  This example demonstrates how to use and transition between states in Luma.

  The game is initialised in the `Loading` state and transitions to `InGame` when the assets have finished loading.
  Since there is only one asset to load, which would be imperceptible, it uses an artificial wait of 2 seconds.  
  The `move_player` system shouldn't run until the assets have finished loading. *)

module L = Luma.Make (Luma_driver_raylib.Driver)
open L

module Asset_register_index = struct
  type t = PlayerSprite
end

module Asset_register = struct
  type t = (Asset_register_index.t, L.Assets.handle) Hashtbl.t

  module R = L.Resource.Make (struct
    type inner = t

    let name = "asset_register"
  end)
end

type game_state =
  | Loading
  | InGame

module Game_state = L.State.Make (struct
  type inner = game_state
end)

(* Register and add the asset register to the world. 
   This system needs to be run once before it is accessed by any other system *)
let setup_asset_register () =
  Ecs.System.make ~components:End "setup_asset_register" (fun w _cmd _entities ->
      let res = Hashtbl.create 16 |> Resource.pack (module Asset_register.R) in
      let w = Ecs.World.add_resource Asset_register.R.type_id res w in
      w)

let setup_player () =
  Ecs.System.make_with_resources ~components:End
    ~resources:
      Ecs.Query.Resource.(
        Resource (module Asset_server.R) & Resource (module Asset_register.R) & End)
    "setup"
    (fun w cmd _entities (server, (asset_register, _)) ->
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

      (* Add the player sprite asset handle to the asset register to check later if it has been loaded 
         when transitioning state. *)
      Hashtbl.add asset_register PlayerSprite sprite_asset;

      let target = Luma.Math.Vec2.create pos_x pos_y in
      let camera = L.Camera.make ~target ~rotation:0. ~zoom:1. () in
      Ecs.Command.spawn ~name:"camera" cmd [ Ecs.Component.component (module L.Camera.C) camera ]
      |> ignore;

      w)

let assets_loaded () =
  L.Ecs.System.make_with_resources ~components:End
    ~resources:
      L.Ecs.Query.Resource.(
        Resource (module Asset_register.R)
        & Resource (module Assets.R)
        & Resource (module Time.R)
        & End)
    "assets_laoded"
    (fun w _cmd _entities (asset_register, (assets, (time, _))) ->
      let loaded =
        Hashtbl.to_seq asset_register
        |> Seq.for_all (fun (_key, value) -> Assets.is_loaded assets value)
      in

      print_endline "assets: loading";

      (* artificial wait because loading one asset is too fast *)
      let wait = time.elapsed > 2. in

      if loaded && wait then (
        print_endline "assets: loaded";

        (* Enqueue the next state. The transition will happen on the next frame.
           This will trigger the on_exit hook for the loading state and on_enter for InGame. *)
        Luma.State.queue_state (module Game_state) InGame w)
      else w)

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

(* A couple more systems just to demo the other state hooks *)
let on_enter_loading () =
  Ecs.System.make ~components:End "on_enter" (fun w _cmd _entities ->
      print_endline "on_enter_loading";
      w)

let on_exit_loading () =
  Ecs.System.make ~components:End "on_exit" (fun w _cmd _entities ->
      print_endline "on_exit_loading";
      w)

let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.add_plugin Plugin.debug_plugin
  |> App.init_state (module Game_state) Loading
  |> App.on Startup (setup_asset_register ())
  |> App.on Startup (setup_player ())
  (* `App.on_enter` systems will execute once when the state is transitioned into *)
  |> App.on_enter (module Game_state) Loading (on_enter_loading ())
  (* `App.on_exit` systems will execute once when the state is transitioned out of *)
  |> App.on_exit (module Game_state) Loading (on_exit_loading ())
  (* `App.while_in` systems will execute as long as the game is in the given state. *)
  |> App.while_in (module Game_state) Loading ~stage:PreUpdate ~system:(assets_loaded ())
  |> App.while_in (module Game_state) InGame ~stage:Update ~system:(move_player ())
  |> App.run
