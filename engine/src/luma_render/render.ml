open Luma__app
open Luma__ecs
open Luma__resource
open Luma__math
open Luma__image

module Camera_config = struct
  type t = { default_camera : bool }

  let default () = { default_camera = true }
  let make ~default_camera = { default_camera }
  let default_camera c = c.default_camera
end

module type Renderer = sig
  type texture
  type colour

  val draw_rect : Luma__math.Rect.t -> colour -> unit
  val draw_rect_lines : Luma__math.Rect.t -> float -> colour -> unit

  val draw_texture :
    texture ->
    position:Luma__math.Vec2.t ->
    size:Luma__math.Vec2.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?texture_atlas:Luma__image.Texture_atlas.t option ->
    ?src:Rect.t option ->
    ?opacity:float ->
    ?rotation:float ->
    ?origin:Vec2.t ->
    unit ->
    unit

  val draw_circle : int -> int -> float -> colour -> unit
  val plugin : ?camera_config:Camera_config.t -> App.t -> App.t

  module Queue : sig
    type sprite

    type cmd =
      | Rect of Rect.t * colour
      | Rect_lines of Rect.t * float * colour
      | ScreenRect of Rect.t * colour
      | Sprite of sprite

    type meta
    type item
    type t = item list ref

    val make : unit -> 'a list ref
    val clear : 'a list ref -> unit
    val push : 'a list ref -> 'a -> unit
    val iter_sorted : item list ref -> camera_layers:int64 -> f:(item -> unit) -> unit

    module R : Luma__resource.Resource.S with type t = t
  end

  module Draw : sig
    val rect : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
    val rect_screen : rect:Rect.t -> colour:colour -> Queue.item list ref -> unit
  end

  val push_rect : z:int -> rect:Rect.t -> ?layers:int64 -> colour -> Queue.item list ref -> unit

  val push_texture :
    z:int ->
    tex:texture ->
    position:Vec2.t ->
    size:Vec2.t ->
    ?layers:int64 ->
    ?texture_atlas:Texture_atlas.t ->
    ?src:Rect.t ->
    ?flip_x:bool ->
    ?flip_y:bool ->
    ?opacity:float ->
    ?rotation:float ->
    ?origin:Vec2.t ->
    Queue.item list ref ->
    unit ->
    unit

  val push_rect_screen :
    z:int -> ?layers:int64 -> rect:Rect.t -> colour -> Queue.item list ref -> unit

  module Camera : Camera.S
end

module Make (D : Luma__driver.Driver.S) :
  Renderer with type texture = D.texture and type colour = D.colour = struct
  open Luma__math
  open Luma__image

  type texture = D.Texture.t
  type colour = D.colour

  let draw_rect rect colour = D.Draw.draw_rect rect colour
  let draw_rect_lines rect line colour = D.Draw.draw_rect_lines rect line colour

  let draw_circle center_x center_y radius colour =
    D.Draw.draw_circle center_x center_y radius colour

  let draw_texture
      texture
      ~position
      ~size
      ?(flip_x = false)
      ?(flip_y = false)
      ?(texture_atlas = None)
      ?(src = None)
      ?(opacity = 1.)
      ?(rotation = 0.)
      ?(origin = Vec2.zero)
      () =
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

    type sprite = {
      tex : D.texture;
      pos : Vec2.t;
      size : Vec2.t;
      flip_x : bool;
      flip_y : bool;
      opacity : float;
      rotation : float;
      origin : Vec2.t;
      src : Rect.t option;
      atlas : Luma__image.Texture_atlas.t option;
    }

    type cmd =
      | Rect of Luma__math.Rect.t * D.colour
      | Rect_lines of Rect.t * float * D.colour
      | ScreenRect of Rect.t * colour
      | Sprite of sprite

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
      |> List.filter (fun { meta } -> Int64.(logand meta.layers camera_layers <> 0L))
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
      ?texture_atlas
      ?src
      ?(flip_x = false)
      ?(flip_y = false)
      ?(opacity = 1.)
      ?(rotation = 0.)
      ?(origin = Vec2.zero)
      q
      () =
    let s =
      Queue.
        {
          tex;
          pos = position;
          size;
          flip_x;
          flip_y;
          atlas = texture_atlas;
          src;
          opacity;
          rotation;
          origin;
        }
    in
    Queue.push q Queue.{ meta = { z; layers }; cmd = Queue.Sprite s }

  let push_rect ~z ~rect ?(layers = 1L) colour q =
    Queue.push q Queue.{ meta = { z; layers }; cmd = Queue.Rect (rect, colour) }

  let push_rect_screen ~z ?(layers = 1L) ~rect colour q =
    Queue.push q Queue.{ meta = { z; layers }; cmd = Queue.ScreenRect (rect, colour) }

  module Draw = struct
    let rect ~rect ~colour q = push_rect ~z:0 ~rect colour q
    let rect_screen ~rect ~colour q = push_rect_screen ~z:0 ~rect colour q
  end

  (* render specific logic and systems for the camera module *)
  module Camera = struct
    include Camera.Make (D)

    let render_cameras () =
      System.make_with_resources
        ~components:Query.Component.(Required (module C) & End)
        ~resources:Query.Resource.(Resource (module Queue.R) & End)
        "render_cameras"
        (fun world entities (queue, _) ->
          let win_w, win_h = (D.Window.screen_width (), D.Window.screen_height ()) in
          entities
          |> List.filter (fun (_, (cam, _)) -> active cam)
          |> List.sort (fun (e1, (camera1, _)) (e2, (camera2, _)) ->
                 compare (order camera1, e1) (order camera2, e2))
          |> Query.Tuple.iter1 (fun cam ->
                 let vp =
                   match viewport cam with
                   | None -> Viewport.full win_w win_h
                   | Some view ->
                       Viewport.clamp_to_window (float_of_int win_w) (float_of_int win_h) view
                 in
                 let x, y, w, h = Viewport.to_rect vp in
                 D.Window.set_viewport_scissor x y w h;

                 Queue.iter_sorted queue ~camera_layers:1L ~f:(fun { cmd; _ } ->
                     match cmd with Queue.ScreenRect (r, c) -> draw_rect r c | _ -> ());

                 D.Window.with_2d (camera cam) (fun () ->
                     Queue.iter_sorted queue ~camera_layers:1L ~f:(fun { cmd; _ } ->
                         match cmd with
                         | Queue.Sprite s ->
                             draw_texture s.tex ~position:s.pos ~size:s.size ~flip_x:s.flip_x
                               ~flip_y:s.flip_y ~texture_atlas:s.atlas ~src:s.src ~opacity:s.opacity
                               ~rotation:s.rotation ~origin:s.origin ()
                         | Queue.Rect (r, colour) ->
                             draw_rect r colour;
                             ()
                         | _ -> ());
                     ());
                 D.Window.reset_scissor ();
                 ());
          world)

    (* clear the queue at the beginning of the frame *)
    let begin_frame () =
      System.make_with_resources ~components:Query.Component.End
        ~resources:Query.Resource.(Resource (module Queue.R) & End)
        "render_queue_begin_frame"
        (fun world _ (queue, _) ->
          Queue.clear queue;
          world)

    (*TODO: make the mix of combination of render/camera plugins clearer*)
    let plugin default_camera app =
      let app = app |> App.on PreRender @@ begin_frame () |> App.on Render @@ render_cameras () in

      (* apply camera plugin *)
      let app = plugin default_camera app in
      app
  end

  let plugin ?(camera_config = Camera_config.default ()) app =
    let app = Camera.plugin camera_config.default_camera app in
    World.add_resource Queue.R.type_id
      (Resource.pack (module Queue.R) (Queue.make ()))
      (App.world app)
    |> ignore;
    app
end
