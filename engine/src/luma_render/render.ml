open Luma__app
open Luma__camera
open Luma__ecs
open Luma__resource
open Luma__math
open Luma__transform
open Luma__asset
open Luma__image

module Projection_pure = struct
  let window_to_viewport_pos ~viewport_pos p = Vec2.sub p viewport_pos
  let viewport_to_window_pos ~viewport_pos p = Vec2.add p viewport_pos
end

module type Renderer = sig
  type texture
  type colour

  module Shape : Shape.S with type colour = colour

  val draw_rect : Luma__math.Rect.t -> colour -> unit
  val draw_rect_lines : Luma__math.Rect.t -> float -> colour -> unit

  val draw_texture :
    size:Luma__math.Vec2.t ->
    position:Vec2.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?src:Luma__math.Rect.t option ->
    ?opacity:float ->
    ?rotation:float ->
    ?origin:Vec2.t ->
    texture ->
    unit

  val draw_circle : center_x:float -> center_y:float -> radius:float -> colour -> unit
  (** [draw_circle center_x center_y radius color] *)

  val draw_circle_lines : center_x:float -> center_y:float -> radius:float -> colour -> unit
  (** [draw_circle_lines center_x center_y radius color] *)

  val plugin : App.t -> App.t

  module Queue : sig
    type sprite_cmd

    type cmd =
      | Rect of Rect.t * colour
      | Rect_lines of Rect.t * float * colour
      | ScreenRect of Rect.t * colour
      | Circle of Luma__math.Primitives.Circle.t * colour
      | Circle_lines of Primitives.Circle.t * colour
      | Sprite of sprite_cmd

    type meta
    type item
    type t = item list ref

    val make : unit -> t

    module R : Luma__resource.Resource.S with type t = t
  end

  module Draw : sig
    val rect : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
    val rect_screen : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
  end

  val push_rect : z:int -> rect:Rect.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit

  val push_rect_lines :
    z:int ->
    rect:Rect.t ->
    ?layers:int64 ->
    ?line_thickness:float ->
    colour ->
    Queue.item list ref ->
    unit

  val push_circle :
    z:int -> circle:Primitives.Circle.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit

  val push_circle_lines :
    z:int -> circle:Primitives.Circle.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit

  val push_texture :
    z:int ->
    tex:texture ->
    position:Vec2.t ->
    size:Vec2.t ->
    ?layers:int64 ->
    ?src:Rect.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?opacity:float ->
    ?rotation:float ->
    ?origin:Vec2.t ->
    Queue.item list ref ->
    unit

  val push_rect_screen :
    z:int -> ?layers:int64 -> rect:Rect.t -> colour -> Queue.item list ref -> unit

  module Projection : sig
    val viewport_to_world_2d : View.t -> Vec2.t -> Vec2.t
    val world_to_viewport_2d : View.t -> Vec2.t -> Vec2.t
    val window_to_world_2d : View.t -> Vec2.t -> Vec2.t
    val world_to_window_2d : View.t -> Vec2.t -> Vec2.t
  end
end

module Make (D : Luma__driver.Driver.S) (Texture : Texture.S with type t = D.texture) :
  Renderer with type texture = D.texture and type colour = D.colour = struct
  open Luma__math
  open Luma__sprite
  module Shape = Shape.Make (D)

  type texture = D.Texture.t
  type colour = D.colour

  let draw_rect rect colour = D.Draw.draw_rect rect colour
  let draw_rect_lines rect line colour = D.Draw.draw_rect_lines rect line colour

  let draw_circle ~center_x ~center_y ~radius colour =
    D.Draw.draw_circle (int_of_float center_x) (int_of_float center_y) radius colour

  let draw_circle_lines ~center_x ~center_y ~radius colour =
    D.Draw.draw_circle_lines (int_of_float center_x) (int_of_float center_y) radius colour

  let draw_texture
      ~size
      ~position
      ?(flip_x = false)
      ?(flip_y = false)
      ?(src = None)
      ?(opacity = 1.)
      ?(rotation = 0.)
      ?(origin = Vec2.(create (size.x *. 0.5) (size.y *. 0.5)))
      texture =
    let full_texture () =
      Rect.create ~pos:(Vec2.create 0. 0.)
        ~size:(Vec2.create (float @@ D.Texture.width texture) (float @@ D.Texture.height texture))
    in

    (* Get with dimensions of the full texture if src is not provided *)
    let src = match src with Some s -> s | None -> full_texture () in

    let x = Rect.x src and y = Rect.y src in
    let w = Rect.width src and h = Rect.height src in

    let src_rect =
      match (flip_x, flip_y) with
      | false, false -> src
      | true, false -> Rect.create ~pos:(Vec2.create x y) ~size:(Vec2.create (-.w) h)
      | false, true -> Rect.create ~pos:(Vec2.create x y) ~size:(Vec2.create w (-.h))
      | true, true ->
          Rect.create ~pos:(Vec2.create (x +. w) (y +. h)) ~size:(Vec2.create (-.w) (-.h))
    in

    let dst = Rect.create ~pos:position ~size in
    let opacity = if opacity < 0. then 0. else if opacity > 1. then 1. else opacity in
    let opacity = int_of_float ((opacity *. 255.0) +. 0.5) in
    let colour = D.Colour.rgba ~r:255 ~b:255 ~g:255 ~a:opacity in

    D.Texture.draw_texture texture src_rect dst origin rotation colour

  module Queue = struct
    open Luma__math

    type sprite_cmd = {
      tex : D.texture;
      pos : Vec2.t;
      size : Vec2.t;
      flip_x : bool;
      flip_y : bool;
      opacity : float;
      rotation : float;
      origin : Vec2.t;
      src : Rect.t option;
    }

    type cmd =
      | Rect of Rect.t * D.colour
      | Rect_lines of Rect.t * float * D.colour
      | ScreenRect of Rect.t * colour
      | Circle of Primitives.Circle.t * colour
      | Circle_lines of Primitives.Circle.t * colour
      | Sprite of sprite_cmd

    type meta = {
      z : int;
      layers : Int64.t;
    }

    type item = {
      meta : meta;
      cmd : cmd;
    }

    type t = item list ref

    let make () = ref []
    let clear q = q := []
    let push q item = q := item :: !q

    let iter_sorted q ~camera_layers ~f =
      !q
      |> List.filter (fun { meta; _ } -> Int64.(logand meta.layers camera_layers <> 0L))
      |> List.stable_sort (fun a b -> compare a.meta.z b.meta.z)
      |> List.iter f

    module R = Resource.Make (struct
      type inner = t

      let name = "render_queue"
    end)
  end

  let push_texture
      ~z
      ~tex
      ~position
      ~size
      ?(layers = 1L)
      ?src
      ?(flip_x = false)
      ?(flip_y = false)
      ?(opacity = 1.)
      ?(rotation = 0.)
      ?(origin = Vec2.(create (size.x *. 0.5) (size.y *. 0.5)))
      q =
    let s = Queue.{ tex; pos = position; size; flip_x; flip_y; src; opacity; rotation; origin } in
    Queue.push q Queue.{ meta = { z; layers }; cmd = Queue.Sprite s }

  let push_rect ~z ~rect ?(layers = 1L) colour q =
    Queue.push q Queue.{ meta = { z; layers }; cmd = Queue.Rect (rect, colour) }

  let push_rect_lines ~z ~rect ?(layers = 1L) ?(line_thickness = 1.) colour q =
    Queue.push q
      Queue.{ meta = { z; layers }; cmd = Queue.Rect_lines (rect, line_thickness, colour) }

  let push_rect_screen ~z ?(layers = 1L) ~rect colour q =
    Queue.push q Queue.{ meta = { z; layers }; cmd = Queue.ScreenRect (rect, colour) }

  let push_circle ~z ~circle ?(layers = 1L) colour q =
    Queue.push q Queue.{ meta = { z; layers }; cmd = Queue.Circle (circle, colour) }

  let push_circle_lines ~z ~circle ?(layers = 1L) colour q =
    Queue.push q Queue.{ meta = { z; layers }; cmd = Queue.Circle_lines (circle, colour) }

  module Draw = struct
    let rect ~rect ~colour q = push_rect ~z:0 ~rect colour q
    let rect_screen ~rect ~colour q = push_rect_screen ~z:0 ~rect colour q
  end

  let to_driver vp c : D.camera =
    let open Camera in
    let vp_pos = Viewport.position vp in
    let vp_size = Viewport.size vp in
    let offset = Vec2.create (vp_pos.x +. (vp_size.x *. 0.5)) (vp_pos.y +. (vp_size.y *. 0.5)) in
    D.Camera.make ~offset ~target:(target c) ~rotation:(rotation c) ~zoom:(zoom c) ()

  let render () =
    System.make_with_resources
      ~components:Query.Component.(Required (module Camera.C) & End)
      ~resources:Query.Resource.(Resource (module Queue.R) & Resource (module View.R) & End)
      "render_cameras"
      (fun world _ _entities (queue, (views, _)) ->
        views
        |> List.iter (fun view ->
            let x, y, w, h = Viewport.to_rect (View.viewport view) in
            D.Window.set_viewport_scissor x y w h;

            Queue.iter_sorted queue ~camera_layers:1L ~f:(fun { cmd; _ } ->
                match cmd with Queue.ScreenRect (r, c) -> draw_rect r c | _ -> ());

            let camera = to_driver (View.viewport view) (View.camera view) in

            D.Window.with_2d camera (fun () ->
                Queue.iter_sorted queue ~camera_layers:1L ~f:(fun { cmd; _ } ->
                    match cmd with
                    | Queue.Sprite s ->
                        draw_texture ~position:s.pos ~size:s.size ~flip_x:s.flip_x ~flip_y:s.flip_y
                          ~src:s.src ~opacity:s.opacity ~rotation:s.rotation ~origin:s.origin s.tex
                    | Queue.Rect (r, colour) -> draw_rect r colour
                    | Queue.Rect_lines (r, line, colour) -> draw_rect_lines r line colour
                    | Queue.Circle (c, colour) ->
                        draw_circle ~center_x:c.center.x ~center_y:c.center.y ~radius:c.radius
                          colour
                    | Queue.Circle_lines (c, colour) ->
                        draw_circle_lines ~center_x:c.center.x ~center_y:c.center.y ~radius:c.radius
                          colour
                    | _ -> ());
                ());
            D.Window.reset_scissor ();
            ());
        world)

  module Projection = struct
    let viewport_to_world_2d (view : View.t) position =
      D.Camera.get_screen_to_world_2d position (to_driver (View.viewport view) (View.camera view))

    let world_to_viewport_2d (view : View.t) position =
      D.Camera.get_world_to_screen_2d position (to_driver (View.viewport view) (View.camera view))

    let window_to_world_2d (view : View.t) pos_window =
      let pos_viewport =
        Projection_pure.window_to_viewport_pos
          ~viewport_pos:(Viewport.position (View.viewport view))
          pos_window
      in
      viewport_to_world_2d view pos_viewport

    let world_to_window_2d (view : View.t) world =
      let pos_viewport = world_to_viewport_2d view world in
      Projection_pure.viewport_to_window_pos
        ~viewport_pos:(Viewport.position (View.viewport view))
        pos_viewport
  end

  (* clear the queue at the beginning of the frame *)
  let begin_frame () =
    System.make_with_resources ~components:Query.Component.End
      ~resources:Query.Resource.(Resource (module Queue.R) & End)
      "render_queue_begin_frame"
      (fun world _ _ (queue, _) ->
        Queue.clear queue;
        world)

  let extract_shapes () =
    System.make_with_resources
      ~components:Query.Component.(Required (module Shape.C) & End)
      ~resources:Query.Resource.(Resource (module Queue.R) & End)
      "extract_sprites"
      (fun world _ entities (queue, _) ->
        let open Shape in
        Query.Tuple.iter1
          (fun shape ->
            match shape with
            | Rect { rect; z; layer; colour; style = Fill; _ } ->
                push_rect ~z ~rect ~layers:layer colour queue
            | Rect { rect; layer; style = Lines l; colour; z; _ } ->
                push_rect_lines ~z ~rect ~layers:layer ~line_thickness:l colour queue
            | Circle { circle; z; layer; colour; style = Fill } ->
                push_circle ~z ~circle ~layers:layer colour queue
            | Circle { circle; z; layer; colour; style = Lines _ } ->
                push_circle_lines ~z ~circle ~layers:layer colour queue)
          entities;
        world)

  let extract_sprite () =
    System.make_with_resources
      ~components:Query.Component.(Required (module Sprite.C) & Required (module Transform.C) & End)
      ~resources:Query.Resource.(Resource (module Assets.R) & Resource (module Queue.R) & End)
      "extract_sprites"
      (fun world _ entities (assets, (queue, _)) ->
        Query.Tuple.iter2
          (fun sprite transform ->
            match Assets.get (module Texture.A) assets (Sprite.image sprite) with
            | None -> ()
            | Some tex ->
                let open Transform in
                let texture_atlas = Sprite.texture_atlas sprite in
                let texture_width = D.Texture.width tex |> float
                and texture_height = D.Texture.height tex |> float in

                let src =
                  match texture_atlas with Some ta -> Texture_atlas.get_frame ta | None -> None
                in

                let size =
                  match src with
                  | Some r -> Vec2.create (Rect.width r) (Rect.height r)
                  | None -> Vec2.create texture_width texture_height
                in

                let flip_x = Sprite.flip_x sprite in
                let flip_y = Sprite.flip_y sprite in
                let z = int_of_float transform.position.z in
                let position = Vec2.create transform.position.x transform.position.y in
                let rotation = transform.rotation in

                push_texture ~z ~tex ~position ~size ~flip_x ~flip_y ?src ~rotation queue)
          entities;
        world)

  let ensure_default_camera () =
    System.make
      ~components:Query.Component.(Required (module Camera.C) & End)
      "add_camera"
      (fun world _ entities ->
        if entities <> [] then world
        else
          let camera = Camera.default () in
          world
          |> World.add_entity ~name:"Camera"
          |> World.with_component world (module Camera.C) camera
          |> ignore;
          world)

  let resolve_views () =
    System.make
      ~components:Query.Component.(Required (module Camera.C) & End)
      "resolve_views"
      (fun w _cmd e ->
        let cams =
          e
          |> List.filter (fun (_, (cam, _)) -> Camera.active cam)
          |> List.sort (fun (e1, (camera1, _)) (e2, (camera2, _)) ->
              compare (Camera.order camera1, e1) (Camera.order camera2, e2))
          |> List.map (fun (entity, (camera, _)) ->
              let viewport =
                let win_w, win_h = (D.Window.screen_width (), D.Window.screen_height ()) in
                match Camera.viewport camera with
                | Some v -> Viewport.clamp_to_window (float win_w) (float win_h) v
                | None -> Viewport.full win_w win_h
              in
              View.create entity camera viewport)
        in

        let packed = Resource.pack (module View.R) cams in
        let w =
          if World.has_resource View.R.type_id w then World.set_resource View.R.type_id packed w
          else World.add_resource View.R.type_id packed w
        in
        w)

  let plugin app =
    World.add_resource Queue.R.type_id
      (Resource.pack (module Queue.R) (Queue.make ()))
      (App.world app)
    |> ignore;

    let app =
      app
      |> App.on PostStartup (ensure_default_camera ())
      |> App.on PreRender (resolve_views ())
      |> App.on PreRender (begin_frame ())
      |> App.on PreRender (extract_sprite ())
      |> App.on PreRender (extract_shapes ())
      |> App.on Render (render ())
    in
    app
end
