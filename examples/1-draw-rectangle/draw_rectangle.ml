module Driver = Luma__driver_raylib.Driver.Raylib_driver
module Luma = Luma.Make (Driver)
open Luma
module Rectangle = [%component: Math.Rect.t]

let setup_rectangle () =
  let open Math in
  System.make
    ~components:Query.Component.(End)
    "setup_rectangle"
    (fun world entities ->
      let open World in
      let rect = Rect.create ~size:(Vec2.create 100. 100.) ~pos:(Vec2.create 20. 50.) in
      Renderer.draw_rect rect @@ Colour.rgb ~r:100 ~g:100 ~b:100;
      world |> add_entity |> with_component world (module Rectangle.C) rect |> ignore;
      world)

let () =
  App.create ()
  |> Plugin.add_default_plugins
  |> App.on Scheduler.Render (setup_rectangle ())
  |> App.run
