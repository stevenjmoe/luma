module Js_driver : Luma__driver.Driver.S = struct
  open Js_of_ocaml
  open Luma__math

  type camera = {
    mutable offset : Vec2.t;
    mutable target : Vec2.t;
    mutable rotation : float;
    mutable zoom : float;
  }

  type colour = {
    r : int;
    g : int;
    b : int;
    a : int;
  }

  type texture = Dom_html.imageElement Js.t
  type sound = unit
  type music = unit

  let canvas_ref : Dom_html.canvasElement Js.t option ref = ref None
  let ctx_ref : Dom_html.canvasRenderingContext2D Js.t option ref = ref None
  let cur_cam : camera option ref = ref None
  let css_rgba { r; g; b; a } = Printf.sprintf "rgba(%d,%d,%d,%.3f)" r g b (float a /. 255.)
  let with_ctx f = match !ctx_ref with None -> () | Some ctx -> f ctx

  (* Frame timing *)
  let dt_ref = ref 0.0
  let last_ts : float option ref = ref None
  let now_s () = ((new%js Js.date_now)##getTime |> Js.float_of_number) /. 1000.

  let compute_dt () =
    let t = now_s () in
    let dt = match !last_ts with None -> 0.0 | Some p -> max 0.0 (t -. p) in
    last_ts := Some t;
    dt_ref := dt

  let get_frame_time () = !dt_ref

  let rect_xywh (rect : Rect.t) =
    ( Rect.x rect |> Js.number_of_float,
      Rect.y rect |> Js.number_of_float,
      Rect.width rect |> Js.number_of_float,
      Rect.height rect |> Js.number_of_float )

  (* World -> screen (rotation ignored for minimal impl) *)
  let to_screen (x : float) (y : float) =
    match !cur_cam with
    | None -> (x, y)
    | Some c ->
        let dx = (x -. Vec2.x c.target) *. c.zoom in
        let dy = (y -. Vec2.y c.target) *. c.zoom in
        (dx +. Vec2.x c.offset, dy +. Vec2.y c.offset)

  let draw_rect (rect : Rect.t) (colour : colour) =
    with_ctx @@ fun ctx ->
    let x, y, w, h = rect_xywh rect in
    ctx##.fillStyle := Js.string (css_rgba colour);
    ctx##fillRect x y w h

  let draw_rect_lines (rect : Rect.t) (line : float) (colour : colour) =
    let line = Js.number_of_float line in
    with_ctx @@ fun ctx ->
    let x, y, w, h = rect_xywh rect in
    ctx##.strokeStyle := Js.string (css_rgba colour);
    ctx##.lineWidth := line;
    ctx##strokeRect x y w h

  let draw_circle center_x center_y radius colour =
    with_ctx @@ fun ctx ->
    let center_x = Int32.of_int center_x |> Js.int32 in
    let center_y = Int32.of_int center_y |> Js.int32 in
    let radius = Js.number_of_float radius in

    ctx##beginPath;
    ctx##arc center_x center_y radius (Js.number_of_float 0.)
      (Js.number_of_float (2. *. Float.pi))
      Js._false;
    ctx##.fillStyle := Js.string (css_rgba colour);
    ctx##fill

  let draw_text text x y size colour =
    with_ctx @@ fun ctx ->
    ctx##.fillStyle := Js.string (css_rgba colour);
    ctx##.font := Js.string (Printf.sprintf "%dpx sans-serif" size);
    ctx##fillText (Js.string text) (Js.number_of_float @@ float x) (Js.number_of_float @@ float y)

  module Camera = struct
    let make ~offset ~target ~rotation ~zoom () = { offset; target; rotation; zoom }

    let default () =
      make ~offset:(Vec2.create 0. 0.) ~target:(Vec2.create 0. 0.) ~rotation:0. ~zoom:1. ()

    let target c = c.target
    let offset c = c.offset
    let zoom c = c.zoom
    let rotation c = c.rotation
    let set_target c v = c.target <- v
    let set_offset c v = c.offset <- v
    let set_zoom c z = c.zoom <- z
    let set_rotation c r = c.rotation <- r
  end

  module Window = struct
    let ensure_canvas () =
      match !canvas_ref with
      | Some c -> c
      | None ->
          let c =
            match Dom_html.getElementById_coerce "game" Dom_html.CoerceTo.canvas with
            | Some c -> c
            | None ->
                let c = Dom_html.createCanvas Dom_html.document in
                c##.id := Js.string "game";
                c##.width := 800;
                c##.height := 600;
                Dom.appendChild Dom_html.document##.body c;
                c
          in
          canvas_ref := Some c;
          c

    let ensure_ctx () =
      match !ctx_ref with
      | Some ctx -> ctx
      | None ->
          let c = ensure_canvas () in
          let ctx = c##getContext Dom_html._2d_ in
          ctx_ref := Some ctx;
          ctx

    let init ~width ~height ~title:_ =
      let c = ensure_canvas () in
      c##.width := width;
      c##.height := height;
      ignore (ensure_ctx ());
      last_ts := None;
      compute_dt ()

    let shutdown () = ()
    let should_close () = false
    let close () = ()
    let is_fullscreen () = false
    let is_hidden () = false
    let is_minimized () = false
    let is_maximized () = false
    let is_focused () = true
    let is_resized () = false
    let toggle_fullscreen () = ()
    let toggle_borderless_windowed () = ()
    let maximize () = ()
    let minimize () = ()
    let restore () = ()
    let get_frame_time = get_frame_time

    let begin_frame () =
      compute_dt ();
      ()

    let end_frame () = ()

    let clear col =
      with_ctx @@ fun ctx ->
      let w = match !canvas_ref with Some c -> float c##.width | None -> 0. in
      let h = match !canvas_ref with Some c -> float c##.height | None -> 0. in
      ctx##.fillStyle := Js.string (css_rgba col);
      ctx##fillRect (Js.number_of_float 0.) (Js.number_of_float 0.) (Js.number_of_float w)
        (Js.number_of_float h)

    let screen_width () = match !canvas_ref with Some c -> c##.width | None -> 0
    let screen_height () = match !canvas_ref with Some c -> c##.height | None -> 0

    let schedule_next_frame f =
      Js_of_ocaml.Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> f ()))
      |> ignore;
      ()

    let begin_2d cam =
      let ctx = ensure_ctx () in
      ctx##save;
      let offset_x =
        float_of_int (screen_width () / 2) -. Vec2.x (Camera.offset cam) |> Js.number_of_float
      in
      let offset_y =
        float_of_int (screen_height () / 2) -. Vec2.y (Camera.offset cam) |> Js.number_of_float
      in
      let zoom = Js.number_of_float @@ Camera.zoom cam in
      let rotate = Js.number_of_float @@ Camera.rotation cam in
      ctx##translate offset_x offset_y;
      ctx##rotate rotate;
      ctx##scale zoom zoom;

      cur_cam := Some cam;
      ()

    let end_2d () = cur_cam := None
  end

  module Colour = struct
    let clamp x = max 0 (min 255 x)
    let rgb ~r ~g ~b = { r = clamp r; g = clamp g; b = clamp b; a = 255 }
    let rgba ~r ~g ~b ~a = { r = clamp r; g = clamp g; b = clamp b; a = clamp a }
    let white = rgb ~r:255 ~g:255 ~b:255
  end

  module Image = struct
    type t = Dom_html.imageElement Js.t

    let load_image src =
      let img = Dom_html.createImg Dom_html.document in
      img##.src := Js.string src;
      img
  end

  module Texture = struct
    type t = texture

    let width (t : t) = t##.width
    let height (t : t) = t##.height

    let draw_texture
        (t : t)
        (src : Rect.t)
        (dst : Rect.t)
        (origin : Vec2.t)
        (_rot : float)
        (tint : colour) =
      with_ctx @@ fun ctx ->
      let sx, sy, sw, sh = rect_xywh src in
      let dx, dy, dw, dh = rect_xywh dst in

      (* apply origin in destination space, ignore rotation for minimal impl *)
      let dx = Js.float_of_number dx -. Vec2.x origin in
      let dy = Js.float_of_number dy -. Vec2.y origin in

      ctx##.globalAlpha := Js.number_of_float (float tint.a /. 255.);
      (* drawImage(image, sx,sy,sw,sh, dx,dy,dw,dh) *)
      Js.Unsafe.fun_call
        (Js.Unsafe.get ctx (Js.string "drawImage"))
        [|
          Js.Unsafe.inject t;
          Js.Unsafe.inject sx;
          Js.Unsafe.inject sy;
          Js.Unsafe.inject sw;
          Js.Unsafe.inject sh;
          Js.Unsafe.inject dx;
          Js.Unsafe.inject dy;
          Js.Unsafe.inject dw;
          Js.Unsafe.inject dh;
        |]
      |> ignore;
      ctx##.globalAlpha := Js.number_of_float 1.

    let load_texture_from_image (img : Image.t) = img
  end

  module Audio = struct
    let init_audio_device () = ()
    let close_audio_device () = ()

    module Music = struct
      type t = music

      let load_music_stream _ = ()
      let is_music_ready _ = true
      let unload_music_stream _ = ()
      let play_music_stream _ = ()
      let is_music_stream_playing _ = false
      let update_music_stream _ = ()
      let stop_music_stream _ = ()
      let pause_music_stream _ = ()
      let resume_music_stream _ = ()
      let seek_music_stream _ _ = ()
      let set_music_volume _ _ = ()
      let set_music_pan _ _ = ()
      let get_music_time_length _ = 0.
      let get_music_time_played _ = 0.
    end

    module Sound = struct
      type t = sound

      let load_sound _ = ()
      let play_sound _ = ()
      let stop_sound _ = ()
      let pause_sound _ = ()
      let resume_sound _ = ()
      let is_sound_playing _ = false
      let set_sound_volume _ _ = ()
      let set_sound_pan _ _ = ()
      let unload_sound _ = ()
    end
  end

  module Text = struct end

  module Input = struct
    open Luma__types.Input_types

    let dom_code_to_key ~(ev : Dom_html.keyboardEvent Js.t) (kc : int) :
        Luma__types.Input_types.Key.t option =
      let open Luma__types.Input_types.Key in
      let loc = ev##.location in
      match kc with
      (* ASCII-ish / printable *)
      | c when c >= 65 && c <= 90 ->
          Some
            (match Char.chr c with
            | 'A' -> A
            | 'B' -> B
            | 'C' -> C
            | 'D' -> D
            | 'E' -> E
            | 'F' -> F
            | 'G' -> G
            | 'H' -> H
            | 'I' -> I
            | 'J' -> J
            | 'K' -> K
            | 'L' -> L
            | 'M' -> M
            | 'N' -> N
            | 'O' -> O
            | 'P' -> P
            | 'Q' -> Q
            | 'R' -> R
            | 'S' -> S
            | 'T' -> T
            | 'U' -> U
            | 'V' -> V
            | 'W' -> W
            | 'X' -> X
            | 'Y' -> Y
            | 'Z' -> Z
            | _ -> assert false)
      | c when c >= 48 && c <= 57 ->
          (* 0-9 row *)
          Some
            (match c with
            | 48 -> Zero
            | 49 -> One
            | 50 -> Two
            | 51 -> Three
            | 52 -> Four
            | 53 -> Five
            | 54 -> Six
            | 55 -> Seven
            | 56 -> Eight
            | 57 -> Nine
            | _ -> assert false)
      (* Punctuation (US layout keyCodes) *)
      | 186 -> Some Semicolon
      | 187 -> Some Equal
      | 188 -> Some Comma
      | 189 -> Some Minus
      | 190 -> Some Period
      | 191 -> Some Slash
      | 192 -> Some Grave
      | 219 -> Some Left_bracket
      | 220 -> Some Backslash
      | 221 -> Some Right_bracket
      | 222 -> Some Apostrophe
      (* Control/navigation *)
      | 8 -> Some Backspace
      | 9 -> Some Tab
      | 13 -> if loc = 3 then Some Kp_enter else Some Enter
      | 16 -> (
          match loc with 2 -> Some Right_shift | 1 -> Some Left_shift | _ -> Some Left_shift)
      | 17 -> (
          match loc with 2 -> Some Right_control | 1 -> Some Left_control | _ -> Some Left_control)
      | 18 -> ( match loc with 2 -> Some Right_alt | 1 -> Some Left_alt | _ -> Some Left_alt)
      | 27 -> Some Escape
      | 32 -> Some Space
      | 33 -> Some Page_up
      | 34 -> Some Page_down
      | 35 -> Some End
      | 36 -> Some Home
      | 37 -> Some Left
      | 38 -> Some Up
      | 39 -> Some Right
      | 40 -> Some Down
      | 45 -> Some Insert
      | 46 -> Some Delete
      | 19 -> Some Pause
      | 20 -> Some Caps_lock
      | 144 -> Some Num_lock
      | 145 -> Some Scroll_lock
      | 44 -> Some Print_screen
      | 91 | 92 -> Some (if kc = 91 then Left_super else Right_super)
      | 93 -> Some Menu
      | c when c >= 112 && c <= 123 ->
          Some
            (match c with
            | 112 -> F1
            | 113 -> F2
            | 114 -> F3
            | 115 -> F4
            | 116 -> F5
            | 117 -> F6
            | 118 -> F7
            | 119 -> F8
            | 120 -> F9
            | 121 -> F10
            | 122 -> F11
            | 123 -> F12
            | _ -> assert false)
      (* Numpad *)
      | c when c >= 96 && c <= 105 ->
          Some
            (match c with
            | 96 -> Kp_0
            | 97 -> Kp_1
            | 98 -> Kp_2
            | 99 -> Kp_3
            | 100 -> Kp_4
            | 101 -> Kp_5
            | 102 -> Kp_6
            | 103 -> Kp_7
            | 104 -> Kp_8
            | 105 -> Kp_9
            | _ -> assert false)
      | 110 -> Some Kp_decimal
      | 111 -> Some Kp_divide
      | 106 -> Some Kp_multiply
      | 109 -> Some Kp_subtract
      | 107 -> Some Kp_add
      | 4 -> Some Back
      | 24 -> Some Volume_up
      | 25 -> Some Volume_down
      | _ -> None

    let keys_down : (Key.t, unit) Hashtbl.t = Hashtbl.create 53
    let keys_pressed : (Key.t, unit) Hashtbl.t = Hashtbl.create 53
    let keys_released : (Key.t, unit) Hashtbl.t = Hashtbl.create 53
    let mouse_down : (int, unit) Hashtbl.t = Hashtbl.create 7
    let mouse_pressed : (int, unit) Hashtbl.t = Hashtbl.create 7
    let mouse_released : (int, unit) Hashtbl.t = Hashtbl.create 7

    let mouse_x = ref 0
    and mouse_y = ref 0

    let last_mouse_x = ref 0
    and last_mouse_y = ref 0

    let press_key k =
      if not (Hashtbl.mem keys_down k) then Hashtbl.replace keys_pressed k ();
      Hashtbl.replace keys_down k ()

    let release_key k =
      if Hashtbl.mem keys_down k then Hashtbl.replace keys_released k ();
      Hashtbl.remove keys_down k

    let () =
      let doc = Dom_html.document in
      Dom_html.addEventListener doc Dom_html.Event.keydown
        (Dom_html.handler (fun ev ->
             let code = ev##.keyCode in
             match dom_code_to_key ~ev code with
             | Some k ->
                 press_key k;
                 Js._true
             | None -> Js._true))
        Js._false
      |> ignore;

      Dom_html.addEventListener doc Dom_html.Event.keyup
        (Dom_html.handler (fun ev ->
             let code = ev##.keyCode in
             match dom_code_to_key ~ev code with
             | Some k ->
                 release_key k;
                 Js._true
             | None -> Js._true))
        Js._false
      |> ignore;

      let canvas =
        match !canvas_ref with
        | Some c -> c
        | None -> (
            let _ = Window.init ~width:800 ~height:600 ~title:"" in
            match !canvas_ref with Some c -> c | None -> assert false)
      in
      Dom_html.addEventListener canvas Dom_html.Event.mousedown
        (Dom_html.handler (fun ev ->
             let b = ev##.button in
             if not (Hashtbl.mem mouse_down b) then Hashtbl.replace mouse_pressed b ();
             Hashtbl.replace mouse_down b ();
             Js._true))
        Js._false
      |> ignore;

      Dom_html.addEventListener canvas Dom_html.Event.mouseup
        (Dom_html.handler (fun ev ->
             let b = ev##.button in
             if Hashtbl.mem mouse_down b then Hashtbl.replace mouse_released b ();
             Hashtbl.remove mouse_down b;
             Js._true))
        Js._false
      |> ignore;

      Dom_html.addEventListener canvas Dom_html.Event.mousemove
        (Dom_html.handler (fun (ev : Dom_html.mouseEvent Js.t) ->
             last_mouse_x := !mouse_x;
             last_mouse_y := !mouse_y;
             mouse_x := int_of_float (Js.float_of_number ev##.offsetX);
             mouse_y := int_of_float (Js.float_of_number ev##.offsetY);
             Js._true))
        Js._false
      |> ignore

    module Keyboard = struct
      let begin_frame () =
        Hashtbl.reset keys_pressed;
        Hashtbl.reset keys_released

      let is_key_down (k : Key.t) = Hashtbl.mem keys_down k
      let is_key_pressed (k : Key.t) = Hashtbl.mem keys_pressed k
      let is_key_released (k : Key.t) = Hashtbl.mem keys_released k
    end

    module Mouse = struct
      let __begin_frame () =
        Hashtbl.reset mouse_pressed;
        Hashtbl.reset mouse_released

      let is_mouse_button_pressed (b : Mouse_button.t) =
        Hashtbl.mem mouse_pressed (Mouse_button.to_int b)

      let is_mouse_button_released (b : Mouse_button.t) =
        Hashtbl.mem mouse_released (Mouse_button.to_int b)

      let is_mouse_button_up (b : Mouse_button.t) =
        not (Hashtbl.mem mouse_down (Mouse_button.to_int b))

      let is_mouse_button_down (b : Mouse_button.t) = Hashtbl.mem mouse_down (Mouse_button.to_int b)
      let get_mouse_x () = !mouse_x
      let get_mouse_y () = !mouse_y
      let get_mouse_position () = Vec2.create (float !mouse_x) (float !mouse_y)

      let get_mouse_delta () =
        Vec2.create (float (!mouse_x - !last_mouse_x)) (float (!mouse_y - !last_mouse_y))

      let set_mouse_position ~x ~y =
        mouse_x := x;
        mouse_y := y

      let set_mouse_offset ~x:_ ~y:_ = ()
      let set_mouse_scale ~x:_ ~y:_ = ()
      let get_mouse_wheel_move () = 0.
    end
  end

  module UI = struct
    open Js_of_ocaml

    type win = {
      x : float;
      y : float;
      w : float;
      h : float;
      mutable cursor_y : float;
      title : string;
    }

    let win_stack : win list ref = ref []
    let window_open : (string, bool) Hashtbl.t = Hashtbl.create 16
    let title_bar_h = 28.0
    let pad_x = 8.0
    let line_h = 18.0
    let line_gap = 2.0
    let overlay : Dom_html.divElement Js.t option ref = ref None

    let ensure_overlay () =
      match !overlay with
      | Some d -> d
      | None ->
          let d = Dom_html.createDiv Dom_html.document in
          d##.style##.position := Js.string "absolute";
          d##.style##.left := Js.string "8px";
          d##.style##.top := Js.string "8px";
          d##.style##.color := Js.string "#fff";
          d##.style##.fontFamily := Js.string "sans-serif";
          Dom.appendChild Dom_html.document##.body d;
          overlay := Some d;
          d

    let begin_window ~title ?pos ?size () =
      let x = Option.map (fun (v : Vec2.t) -> v.x) pos |> Option.value ~default:20. in
      let y = Option.map (fun (v : Vec2.t) -> v.y) pos |> Option.value ~default:20. in
      let w = Option.map (fun (v : Vec2.t) -> v.x) size |> Option.value ~default:360. in
      let h = Option.map (fun (v : Vec2.t) -> v.y) size |> Option.value ~default:220. in
      let active = Option.value (Hashtbl.find_opt window_open title) ~default:true in
      if not active then false
      else
        let d = ensure_overlay () in
        d##.innerHTML := Js.string "";
        let header = Dom_html.createDiv Dom_html.document in
        header##.textContent := Js.some (Js.string title);
        header##.style##.fontWeight := Js.string "bold";
        Dom.appendChild d header;
        let wctx = { x; y; w; h; cursor_y = y +. title_bar_h; title } in
        win_stack := wctx :: !win_stack;
        true

    let end_window () = match !win_stack with _ :: tl -> win_stack := tl | [] -> ()

    let text s =
      match !overlay with
      | None -> ()
      | Some d ->
          let p = Dom_html.createDiv Dom_html.document in
          p##.textContent := Js.some (Js.string s);
          Dom.appendChild d p
  end

  module Debug_draw = struct
    type space =
      [ `World
      | `Screen
      ]

    let line space ~p0 ~p1 ~colour =
      with_ctx @@ fun ctx ->
      let x0, y0 =
        match space with
        | `Screen -> (Js.number_of_float (Vec2.x p0), Js.number_of_float (Vec2.y p0))
        | `World ->
            let i1, i2 = to_screen (Vec2.x p0) (Vec2.y p0) in
            (Js.number_of_float i1, Js.number_of_float i2)
      in
      let x1, y1 =
        match space with
        | `Screen -> (Js.number_of_float (Vec2.x p1), Js.number_of_float (Vec2.y p1))
        | `World ->
            let i1, i2 = to_screen (Vec2.x p1) (Vec2.y p1) in
            (Js.number_of_float i1, Js.number_of_float i2)
      in
      ctx##.strokeStyle := Js.string (css_rgba colour);
      ctx##beginPath;
      ctx##moveTo x0 y0;
      ctx##lineTo x1 y1;
      ctx##stroke
  end
end

include Js_driver
