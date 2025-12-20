open Luma__ecs
open Luma__math
open Luma__app

module type S = sig
  module Viewport : module type of Viewport

  type camera
  type t

  module C : Luma__ecs.Component.S with type t = t

  val default : unit -> t

  val make :
    ?viewport:Viewport.t option ->
    ?order:int ->
    offset:Vec2.t ->
    target:Vec2.t ->
    rotation:float ->
    zoom:float ->
    unit ->
    t

  val target : t -> Vec2.t
  val offset : t -> Vec2.t
  val zoom : t -> float
  val rotation : t -> float
  val viewport : t -> Viewport.t option
  val order : t -> int
  val active : t -> bool
  val camera : t -> camera
  val center : t -> Vec2.t
  val get_screen_to_world_2d : Vec2.t -> t -> Vec2.t
  val get_world_to_screen_2d : Vec2.t -> t -> Vec2.t
  val set_target : t -> Vec2.t -> unit
  val set_offset : t -> Vec2.t -> unit
  val set_zoom : t -> float -> unit
  val set_rotation : t -> float -> unit
  val plugin : bool -> App.t -> App.t
end

module Make (D : Luma__driver.Driver.S) : S with type camera = D.camera = struct
  module Viewport = Viewport

  type camera = D.camera

  type t = {
    camera : camera;
    viewport : Viewport.t option;
    order : int;
    active : bool;
  }

  module C = Component.Make (struct
    type inner = t

    let name = "Camera"
  end)

  let default () = { camera = D.Camera.default (); viewport = None; order = 0; active = true }

  let make ?(viewport = None) ?(order = 0) ~offset ~target ~rotation ~zoom () =
    { camera = D.Camera.make ~offset ~target ~rotation ~zoom (); viewport; active = true; order }

  let target c = D.Camera.target c.camera
  let offset c = D.Camera.offset c.camera
  let zoom c = D.Camera.zoom c.camera
  let rotation c = D.Camera.rotation c.camera
  let viewport c = c.viewport
  let order c = c.order
  let active c = c.active
  let camera c = c.camera

  let center c =
    match c.viewport with
    | None -> D.Camera.target c.camera
    | Some vp ->
        let screen_center = Viewport.center vp in
        let dx = (Vec2.x screen_center -. Vec2.x (offset c)) /. zoom c in
        let dy = (Vec2.y screen_center -. Vec2.y (offset c)) /. zoom c in
        let rot = -.rotation c in
        let cosr = cos rot and sinr = sin rot in
        let rx = (dx *. cosr) -. (dy *. sinr) in
        let ry = (dx *. sinr) +. (dy *. cosr) in

        Vec2.create (Vec2.x (target c) +. rx) (Vec2.y (target c) +. ry)

  let get_screen_to_world_2d position camera =
    D.Camera.get_screen_to_world_2d position camera.camera

  let get_world_to_screen_2d position camera =
    D.Camera.get_world_to_screen_2d position camera.camera

  let set_target c target = D.Camera.set_target c.camera target
  let set_offset c offset = D.Camera.set_offset c.camera offset
  let set_zoom c zoom = D.Camera.set_zoom c.camera zoom
  let set_rotation c rotation = D.Camera.set_rotation c.camera rotation

  let add_camera default_camera () =
    System.make ~components:End "add_camera" (fun world _ _ ->
        if default_camera then (
          let camera = default () in
          world
          |> World.add_entity ~name:"Camera"
          |> World.with_component world (module C) camera
          |> ignore;
          world)
        else world)

  (*module Camera_serializer =
    Serialize.Make_serializer
      (Serialize.Json_format)
      (struct
        open Json_helpers

        type nonrec t = t

        let normalize s = s |> String.trim |> String.lowercase_ascii

        let to_repr camera =
          let target = of_vec2 "target" @@ target camera in
          let offset = of_vec2 "offset" @@ offset camera in
          let zoom = of_float "zoom" (zoom camera) in
          let rotation = of_float "rotation" (rotation camera) in
          let active = of_bool "active" camera.active in
          `Assoc [ (C.name, `Assoc [ target; offset; zoom; rotation; active ]) ]

        let of_repr repr =
          let ( let* ) = Result.bind in
          match repr with
          | `Assoc [ (name, data) ] when normalize name = normalize C.name ->
              let* target = parse_vec2 "target" data in
              let* offset = parse_vec2 "offset" data in
              let* zoom = parse_float "zoom" data in
              let* rotation = parse_float "rotation" data in
              let* active = parse_bool "active" data in

              Ok (make ~offset ~target ~zoom ~rotation ())
          | _ -> Error (Error.parse_json (Json (Yojson.Safe.pretty_to_string repr)))
      end)

  let register_component app =
    let packed_serializer = Luma__serialize.Serialize.pack_json (module Camera_serializer) in
    App.register_component C.name (module C) [ packed_serializer ] app*)

  let plugin default_camera app =
    app
    |>
    (*register_component |>*)
    App.on PostStartup (add_camera default_camera ())
end
