
module D = Luma_driver_raylib.Driver
module L = Luma.Make(D)
open L
open Luma.Math

(* Create an Orbit component that manages the camera over time *)
module Orbit = struct
    type t = {
        mutable angle : float;
        radius : float;
        height : float;
        speed : float;
        target : Vec3.t;
    }

    module C = Ecs.Component.Make (struct
        type inner = t
        let name = "Orbit"
    end)

    let position (o : t) =
        Vec3.create
          (o.target.x +. (o.radius *. cos o.angle))
          o.height
          (o.target.z +. (o.radius *. sin o.angle))

    let camera o = D.Camera3d.perspective ~position:(position o) ~target:o.target ~up:(Vec3.create 0. 1. 0.) ~fov_y_degrees:45.0 ()
end

let orbit_system () =
    Ecs.System.make_with_resources
        ~components:Ecs.Query.Component.(Required (module Orbit.C) & End)
        ~resources:Ecs.Query.Resource.(Resource (module Time.R) & End)
        "orbit"
        (fun world _ entities (time, _) ->
              Ecs.Query.Tuple.iter1
                (fun (o : Orbit.t) -> o.angle <- o.angle +. (o.speed *. time.dt))
                entities;
              world)

let setup () =
    (* Spawn the camera entity *)
    Ecs.System.make ~components:End "setup" (fun world cmd _ ->
        let orbit = { Orbit.angle = 0.; radius = 11.; height = 6.; speed = 0.5;
                  target = Vec3.create 0. 0. 0. } in
        Ecs.Command.spawn ~name:"orbit_camera" cmd
            [Ecs.Component.component (module Orbit.C) orbit]
        |> ignore;
        world
    )

(* print the camera's angle and position every frame *)
let debug_orbit () =
    Ecs.System.make
      ~components:Ecs.Query.Component.(Required (module Orbit.C) & End)
      "debug_orbit"
      (fun world _cmd entities ->
        Ecs.Query.Tuple.iter1
          (fun (o : Orbit.t) ->
            let p = Orbit.position o in
            Printf.printf "angle=%.3f pos=(%.2f, %.2f, %.2f)\n%!" o.angle p.x p.y p.z)
          entities;
        world)

let draw_scene () =
    Ecs.System.make
      ~components:Ecs.Query.Component.(Required (module Orbit.C) & End)
      "draw_scene"
      (fun world _ entities ->
        Ecs.Query.Tuple.iter1
          (fun (o : Orbit.t) ->
          D.Draw3d.with_camera (Orbit.camera o) (fun () ->
              D.Draw3d.grid ~slices:10 ~spacing:1. ))
          entities;
        world)

let () =
    App.create ()
    |> Plugin.add_default_plugins
    |> App.on Startup (setup ())
    |> App.on Update (orbit_system ())
    |> App.on PostUpdate (debug_orbit ())
    |> App.on PostRender (draw_scene ()) (* after 2d drawing, switch to 3d and draw *)
    |> App.run
