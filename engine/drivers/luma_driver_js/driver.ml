module Js_driver : Luma__driver.Driver.S = struct
  open Js_of_ocaml
  open Luma__math
  open Luma__core

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

  type sound = {
    element : Dom_html.audioElement Js.t;
    mutable sound_pan : float;
  }

  type music = {
    element : Dom_html.audioElement Js.t;
    mutable music_pan : float;
  }

  let canvas_ref : Dom_html.canvasElement Js.t option ref = ref None
  let ctx_ref : Dom_html.canvasRenderingContext2D Js.t option ref = ref None
  let cur_cam : camera option ref = ref None
  let cam_depth = ref 0
  let scissor_depth = ref 0
  let input_handlers_installed = ref false
  let should_close_ref = ref false
  let is_hidden_ref = ref false
  let is_focused_ref = ref true
  let is_resized_ref = ref false
  let mouse_offset_x = ref 0
  let mouse_offset_y = ref 0
  let mouse_scale_x = ref 1.0
  let mouse_scale_y = ref 1.0
  let wheel_accum = ref 0.
  let wheel_frame = ref 0.
  let dt_ref = ref 0.0
  let last_ts : float option ref = ref None
  let clamp_int lo hi n = max lo (min hi n)
  let clamp01 x = max 0.0 (min 1.0 x)
  let now_s () = Js.float_of_number (Js.Unsafe.pure_js_expr "Date.now()") /. 1000.

  let compute_dt () =
    let t = now_s () in
    let dt = match !last_ts with None -> 0.0 | Some p -> max 0.0 (t -. p) in
    last_ts := Some t;
    dt_ref := dt

  let get_frame_time () = !dt_ref
  let with_ctx f = match !ctx_ref with None -> () | Some ctx -> f ctx

  let css_rgba { r; g; b; a } =
    Printf.sprintf "rgba(%d,%d,%d,%.3f)" (clamp_int 0 255 r) (clamp_int 0 255 g) (clamp_int 0 255 b)
      (float_of_int (clamp_int 0 255 a) /. 255.)

  let rect_xywh (rect : Rect.t) = (Rect.x rect, Rect.y rect, Rect.width rect, Rect.height rect)

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
              c##.style##.display := Js.string "block";
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

  let screen_width () = match !canvas_ref with Some c -> c##.width | None -> 0
  let screen_height () = match !canvas_ref with Some c -> c##.height | None -> 0

  let rotate x y angle =
    let ca = cos angle and sa = sin angle in
    ((x *. ca) -. (y *. sa), (x *. sa) +. (y *. ca))

  let world_to_screen (p : Vec2.t) (c : camera) =
    let x = Vec2.x p -. Vec2.x c.target in
    let y = Vec2.y p -. Vec2.y c.target in
    let x = x *. c.zoom and y = y *. c.zoom in
    let x, y = rotate x y c.rotation in
    Vec2.create (x +. Vec2.x c.offset) (y +. Vec2.y c.offset)

  let screen_to_world (p : Vec2.t) (c : camera) =
    let x = Vec2.x p -. Vec2.x c.offset in
    let y = Vec2.y p -. Vec2.y c.offset in
    let x, y = rotate x y (-.c.rotation) in
    let x = x /. c.zoom and y = y /. c.zoom in
    Vec2.create (x +. Vec2.x c.target) (y +. Vec2.y c.target)

  let to_screen x y =
    match !cur_cam with
    | None -> (x, y)
    | Some c ->
        let p = world_to_screen (Vec2.create x y) c in
        (Vec2.x p, Vec2.y p)

  let parse_hex_byte s i = int_of_string ("0x" ^ String.sub s i 2)

  let parse_colour_string raw =
    let s =
      if String.length raw > 0 && raw.[0] = '#' then String.sub raw 1 (String.length raw - 1)
      else raw
    in
    match String.lowercase_ascii s with
    | "white" -> Ok { r = 255; g = 255; b = 255; a = 255 }
    | "black" -> Ok { r = 0; g = 0; b = 0; a = 255 }
    | "red" -> Ok { r = 255; g = 0; b = 0; a = 255 }
    | "green" -> Ok { r = 0; g = 255; b = 0; a = 255 }
    | "blue" -> Ok { r = 0; g = 0; b = 255; a = 255 }
    | _ -> (
        try
          match String.length s with
          | 8 ->
              let a = parse_hex_byte s 0
              and r = parse_hex_byte s 2
              and g = parse_hex_byte s 4
              and b = parse_hex_byte s 6 in
              Ok { r; g; b; a }
          | 6 ->
              let r = parse_hex_byte s 0 and g = parse_hex_byte s 2 and b = parse_hex_byte s 4 in
              Ok { r; g; b; a = 255 }
          | _ -> Error (Error.hexcode raw)
        with _ -> Error (Error.hexcode raw))

  module Draw = struct
    let draw_rect (rect : Rect.t) (colour : colour) =
      with_ctx @@ fun ctx ->
      let x, y, w, h = rect_xywh rect in
      ctx##.fillStyle := Js.string (css_rgba colour);
      ctx##fillRect (Js.number_of_float x) (Js.number_of_float y) (Js.number_of_float w)
        (Js.number_of_float h)

    let draw_rect_lines (rect : Rect.t) (line : float) (colour : colour) =
      with_ctx @@ fun ctx ->
      let x, y, w, h = rect_xywh rect in
      ctx##.strokeStyle := Js.string (css_rgba colour);
      ctx##.lineWidth := Js.number_of_float line;
      ctx##strokeRect (Js.number_of_float x) (Js.number_of_float y) (Js.number_of_float w)
        (Js.number_of_float h)

    let draw_circle center_x center_y radius colour =
      with_ctx @@ fun ctx ->
      ctx##beginPath;
      ctx##arc
        (Js.number_of_float (float_of_int center_x))
        (Js.number_of_float (float_of_int center_y))
        (Js.number_of_float radius) (Js.number_of_float 0.)
        (Js.number_of_float (2. *. Float.pi))
        Js._false;
      ctx##.fillStyle := Js.string (css_rgba colour);
      ctx##fill

    let draw_circle_lines center_x center_y radius colour =
      with_ctx @@ fun ctx ->
      ctx##beginPath;
      ctx##arc
        (Js.number_of_float (float_of_int center_x))
        (Js.number_of_float (float_of_int center_y))
        (Js.number_of_float radius) (Js.number_of_float 0.)
        (Js.number_of_float (2. *. Float.pi))
        Js._false;
      ctx##.strokeStyle := Js.string (css_rgba colour);
      ctx##stroke

    let draw_line ~start_pos_x ~start_pos_y ~end_pos_x ~end_pos_y colour =
      with_ctx @@ fun ctx ->
      ctx##.strokeStyle := Js.string (css_rgba colour);
      ctx##beginPath;
      ctx##moveTo
        (Js.number_of_float (float_of_int start_pos_x))
        (Js.number_of_float (float_of_int start_pos_y));
      ctx##lineTo
        (Js.number_of_float (float_of_int end_pos_x))
        (Js.number_of_float (float_of_int end_pos_y));
      ctx##stroke

    let draw_capsule2d center ~half_length ~radius colour =
      with_ctx @@ fun ctx ->
      let cx = Vec2.x center in
      let cy = Vec2.y center in
      ctx##.fillStyle := Js.string (css_rgba colour);

      ctx##fillRect
        (Js.number_of_float (cx -. radius))
        (Js.number_of_float (cy -. half_length))
        (Js.number_of_float (radius *. 2.))
        (Js.number_of_float (half_length *. 2.));

      ctx##beginPath;
      ctx##arc (Js.number_of_float cx)
        (Js.number_of_float (cy -. half_length))
        (Js.number_of_float radius) (Js.number_of_float 0.)
        (Js.number_of_float (2. *. Float.pi))
        Js._false;
      ctx##fill;

      ctx##beginPath;
      ctx##arc (Js.number_of_float cx)
        (Js.number_of_float (cy +. half_length))
        (Js.number_of_float radius) (Js.number_of_float 0.)
        (Js.number_of_float (2. *. Float.pi))
        Js._false;
      ctx##fill

    let draw_capsule2d_wires center ~half_length ~radius colour =
      with_ctx @@ fun ctx ->
      let cx = Vec2.x center in
      let cy = Vec2.y center in
      ctx##.strokeStyle := Js.string (css_rgba colour);

      ctx##strokeRect
        (Js.number_of_float (cx -. radius))
        (Js.number_of_float (cy -. half_length))
        (Js.number_of_float (radius *. 2.))
        (Js.number_of_float (half_length *. 2.));

      ctx##beginPath;
      ctx##arc (Js.number_of_float cx)
        (Js.number_of_float (cy -. half_length))
        (Js.number_of_float radius) (Js.number_of_float 0.)
        (Js.number_of_float (2. *. Float.pi))
        Js._false;
      ctx##stroke;

      ctx##beginPath;
      ctx##arc (Js.number_of_float cx)
        (Js.number_of_float (cy +. half_length))
        (Js.number_of_float radius) (Js.number_of_float 0.)
        (Js.number_of_float (2. *. Float.pi))
        Js._false;
      ctx##stroke

    let draw_text text x y size colour =
      with_ctx @@ fun ctx ->
      ctx##.fillStyle := Js.string (css_rgba colour);
      ctx##.font := Js.string (Printf.sprintf "%dpx sans-serif" size);
      ctx##fillText (Js.string text)
        (Js.number_of_float (float_of_int x))
        (Js.number_of_float (float_of_int y))
  end

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
    let set_zoom c z = c.zoom <- max 0.0001 z
    let set_rotation c r = c.rotation <- r
    let get_world_to_screen_2d p c = world_to_screen p c
    let get_screen_to_world_2d p c = screen_to_world p c
  end

  module IO = struct
    type path = string

    let run_io_loop () = ()
    let blocking_cache : (path, string) Hashtbl.t = Hashtbl.create 128

    let bytes_of_binary_string (s : string) : bytes =
      Bytes.init (String.length s) (fun i -> Char.chr (Char.code s.[i] land 0xFF))

    let xhr_read_bytes_async (path : path) ~(k : (bytes, Error.error) result -> unit) : unit =
      let xhr = Js.Unsafe.new_obj (Js.Unsafe.pure_js_expr "XMLHttpRequest") [||] in
      ignore
        (Js.Unsafe.meth_call xhr "open"
           [|
             Js.Unsafe.inject (Js.string "GET");
             Js.Unsafe.inject (Js.string path);
             Js.Unsafe.inject Js._true;
           |]);
      ignore
        (Js.Unsafe.meth_call xhr "overrideMimeType"
           [| Js.Unsafe.inject (Js.string "text/plain; charset=x-user-defined") |]);
      Js.Unsafe.set xhr "onload"
        (Js.wrap_callback (fun _ ->
             let status = int_of_float (Js.float_of_number (Js.Unsafe.get xhr "status")) in
             if (status >= 200 && status < 300) || status = 0 then (
               let txt = Js.to_string (Js.Unsafe.get xhr "responseText") in
               let b = bytes_of_binary_string txt in
               Hashtbl.replace blocking_cache path txt;
               k (Ok b))
             else
               k
                 (Error
                    (Error.io_read path (Printf.sprintf "HTTP request failed. status=%d" status)))));
      Js.Unsafe.set xhr "onerror"
        (Js.wrap_callback (fun _ ->
             k (Error (Error.io_read path "Network error while loading asset."))));
      ignore (Js.Unsafe.meth_call xhr "send" [| Js.Unsafe.inject Js.null |])

    let read_file_blocking (path : path) =
      try Sys_js.read_file ~name:path
      with _ -> (
        match Hashtbl.find_opt blocking_cache path with
        | Some s -> s
        | None ->
            raise
              (Error.Engine_error
                 (Error.io_read path
                    "Blocking IO unavailable for browser HTTP assets. Use async IO.read_file first.")))

    let read_file path ~k =
      try
        let s = read_file_blocking path in
        k (Ok (bytes_of_binary_string s))
      with Error.Engine_error _ -> xhr_read_bytes_async path ~k

    let write_file path bytes = Sys_js.create_file ~name:path ~content:(Bytes.to_string bytes)
  end

  module Window = struct
    let install_window_handlers () =
      if !input_handlers_installed then ()
      else (
        input_handlers_installed := true;
        Dom_html.addEventListener Dom_html.window Dom_html.Event.focus
          (Dom_html.handler (fun _ ->
               is_focused_ref := true;
               Js._true))
          Js._false
        |> ignore;
        Dom_html.addEventListener Dom_html.window Dom_html.Event.blur
          (Dom_html.handler (fun _ ->
               is_focused_ref := false;
               Js._true))
          Js._false
        |> ignore;
        Dom_html.addEventListener Dom_html.window Dom_html.Event.resize
          (Dom_html.handler (fun _ ->
               is_resized_ref := true;
               Js._true))
          Js._false
        |> ignore;
        ())

    let init ~width ~height ~title ~resizable =
      let c = ensure_canvas () in
      c##.width := width;
      c##.height := height;
      c##.title := Js.string title;
      c##.style##.overflow := Js.string (if resizable then "auto" else "hidden");
      ignore (ensure_ctx ());
      install_window_handlers ();
      should_close_ref := false;
      last_ts := None;
      compute_dt ()

    let shutdown () =
      ctx_ref := None;
      canvas_ref := None;
      cur_cam := None

    let should_close () = !should_close_ref
    let close () = should_close_ref := true

    let is_fullscreen () =
      Js.Optdef.test
        (Js.Unsafe.get Dom_html.document (Js.string "fullscreenElement")
          : Dom_html.element Js.t Js.optdef)

    let is_hidden () =
      is_hidden_ref := Js.to_bool Dom_html.document##.hidden;
      !is_hidden_ref

    let is_minimized () = false
    let is_maximized () = false
    let is_focused () = !is_focused_ref

    let is_resized () =
      let v = !is_resized_ref in
      is_resized_ref := false;
      v

    let toggle_fullscreen () =
      if is_fullscreen () then ignore (Js.Unsafe.meth_call Dom_html.document "exitFullscreen" [||])
      else
        let canvas = ensure_canvas () in
        ignore (Js.Unsafe.meth_call canvas "requestFullscreen" [||])

    let toggle_borderless_windowed () = ()
    let maximize () = ()
    let minimize () = ()
    let restore () = ()
    let get_frame_time = get_frame_time

    let begin_frame () =
      compute_dt ();
      wheel_frame := !wheel_accum;
      wheel_accum := 0.

    let end_frame () = ()

    let begin_2d cam =
      let ctx = ensure_ctx () in
      ctx##save;
      ctx##translate
        (Js.number_of_float (Vec2.x (Camera.offset cam)))
        (Js.number_of_float (Vec2.y (Camera.offset cam)));
      ctx##rotate (Js.number_of_float (Camera.rotation cam));
      ctx##scale (Js.number_of_float (Camera.zoom cam)) (Js.number_of_float (Camera.zoom cam));
      ctx##translate
        (Js.number_of_float (-.Vec2.x (Camera.target cam)))
        (Js.number_of_float (-.Vec2.y (Camera.target cam)));
      cur_cam := Some cam;
      incr cam_depth

    let end_2d () =
      with_ctx (fun ctx -> if !cam_depth > 0 then ctx##restore);
      if !cam_depth > 0 then decr cam_depth;
      if !cam_depth = 0 then cur_cam := None

    let with_2d cam f =
      begin_2d cam;
      Fun.protect ~finally:end_2d f

    let set_viewport_scissor x y w h =
      with_ctx @@ fun ctx ->
      ctx##save;
      ctx##beginPath;
      ctx##rect
        (Js.number_of_float (float_of_int x))
        (Js.number_of_float (float_of_int y))
        (Js.number_of_float (float_of_int w))
        (Js.number_of_float (float_of_int h));
      ctx##clip;
      incr scissor_depth

    let reset_scissor () =
      with_ctx (fun ctx -> if !scissor_depth > 0 then ctx##restore);
      if !scissor_depth > 0 then decr scissor_depth

    let clear col =
      with_ctx @@ fun ctx ->
      let w = float_of_int (screen_width ()) in
      let h = float_of_int (screen_height ()) in
      ctx##.fillStyle := Js.string (css_rgba col);
      ctx##fillRect (Js.number_of_float 0.) (Js.number_of_float 0.) (Js.number_of_float w)
        (Js.number_of_float h)

    let screen_width = screen_width
    let screen_height = screen_height

    let schedule_next_frame f =
      ignore (Dom_html.window##requestAnimationFrame (Js.wrap_callback (fun _ -> f ())))
  end

  module Colour = struct
    let rgb ~r ~g ~b =
      { r = clamp_int 0 255 r; g = clamp_int 0 255 g; b = clamp_int 0 255 b; a = 255 }

    let rgba ~r ~g ~b ~a =
      { r = clamp_int 0 255 r; g = clamp_int 0 255 g; b = clamp_int 0 255 b; a = clamp_int 0 255 a }

    let white = rgb ~r:255 ~g:255 ~b:255
    let from_string = parse_colour_string
  end

  module Image = struct
    type t = Dom_html.imageElement Js.t

    let base64_table =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    let base64_encode_bytes (s : string) (len : int) : string =
      let len = min len (String.length s) in
      let out = Buffer.create (((len + 2) / 3) * 4) in
      let rec loop i =
        if i >= len then ()
        else
          let b0 = Char.code s.[i] in
          let b1 = if i + 1 < len then Char.code s.[i + 1] else 0 in
          let b2 = if i + 2 < len then Char.code s.[i + 2] else 0 in
          let n = (b0 lsl 16) lor (b1 lsl 8) lor b2 in
          Buffer.add_char out base64_table.[(n lsr 18) land 0x3F];
          Buffer.add_char out base64_table.[(n lsr 12) land 0x3F];
          if i + 1 < len then Buffer.add_char out base64_table.[(n lsr 6) land 0x3F]
          else Buffer.add_char out '=';
          if i + 2 < len then Buffer.add_char out base64_table.[n land 0x3F]
          else Buffer.add_char out '=';
          loop (i + 3)
      in
      loop 0;
      Buffer.contents out

    let load_image src =
      let img = Dom_html.createImg Dom_html.document in
      img##.src := Js.string src;
      img

    let load_image_from_memory file_type file_data data_size =
      let img = Dom_html.createImg Dom_html.document in
      let mime =
        match String.lowercase_ascii file_type with
        | ".png" | "png" -> "image/png"
        | ".jpg" | ".jpeg" | "jpg" | "jpeg" -> "image/jpeg"
        | ".webp" | "webp" -> "image/webp"
        | _ -> "application/octet-stream"
      in
      let payload = base64_encode_bytes file_data data_size in
      img##.src := Js.string (Printf.sprintf "data:%s;base64,%s" mime payload);
      img
  end

  module Texture = struct
    type t = texture

    let width (t : t) =
      let n = int_of_float (Js.float_of_number (Js.Unsafe.get t "naturalWidth")) in
      if n > 0 then n else t##.width

    let height (t : t) =
      let n = int_of_float (Js.float_of_number (Js.Unsafe.get t "naturalHeight")) in
      if n > 0 then n else t##.height

    let draw_texture
        (t : t)
        (src : Rect.t)
        (dst : Rect.t)
        (origin : Vec2.t)
        (rot : float)
        (tint : colour) =
      with_ctx @@ fun ctx ->
      let complete = Js.to_bool (Js.Unsafe.get t "complete") in
      let natural_width = int_of_float (Js.float_of_number (Js.Unsafe.get t "naturalWidth")) in
      if (not complete) || natural_width <= 0 then ()
      else
        let sx, sy, sw, sh = rect_xywh src in
        let dx, dy, dw, dh = rect_xywh dst in

        ctx##save;
        ctx##.globalAlpha := Js.number_of_float (float_of_int tint.a /. 255.);
        ctx##translate
          (Js.number_of_float (dx +. Vec2.x origin))
          (Js.number_of_float (dy +. Vec2.y origin));
        ctx##rotate (Js.number_of_float rot);

        ignore
          (Js.Unsafe.meth_call ctx "drawImage"
             [|
               Js.Unsafe.inject t;
               Js.Unsafe.inject (Js.number_of_float sx);
               Js.Unsafe.inject (Js.number_of_float sy);
               Js.Unsafe.inject (Js.number_of_float sw);
               Js.Unsafe.inject (Js.number_of_float sh);
               Js.Unsafe.inject (Js.number_of_float (-.Vec2.x origin));
               Js.Unsafe.inject (Js.number_of_float (-.Vec2.y origin));
               Js.Unsafe.inject (Js.number_of_float dw);
               Js.Unsafe.inject (Js.number_of_float dh);
             |]);

        ctx##.globalAlpha := Js.number_of_float 1.;
        ctx##restore

    let load_texture_from_image (img : Image.t) = img
  end

  module Audio = struct
    let audio_enabled = ref false
    let init_audio_device () = audio_enabled := true
    let close_audio_device () = audio_enabled := false

    let create_audio path =
      let a = Dom_html.createAudio Dom_html.document in
      a##.src := Js.string path;
      a##.preload := Js.string "auto";
      a

    let play_audio (a : Dom_html.audioElement Js.t) = ignore (Js.Unsafe.meth_call a "play" [||])
    let pause_audio (a : Dom_html.audioElement Js.t) = ignore (Js.Unsafe.meth_call a "pause" [||])

    let set_current_time (a : Dom_html.audioElement Js.t) t =
      Js.Unsafe.set a "currentTime" (Js.Unsafe.inject (Js.number_of_float t))

    let get_current_time (a : Dom_html.audioElement Js.t) =
      Js.float_of_number (Js.Unsafe.get a "currentTime")

    let get_duration (a : Dom_html.audioElement Js.t) =
      let d = Js.float_of_number (Js.Unsafe.get a "duration") in
      if Float.is_finite d then d else 0.

    let set_volume (a : Dom_html.audioElement Js.t) v =
      Js.Unsafe.set a "volume" (Js.Unsafe.inject (Js.number_of_float (clamp01 v)))

    let is_playing (a : Dom_html.audioElement Js.t) =
      let paused = Js.to_bool (Js.Unsafe.get a "paused") in
      let ended = Js.to_bool (Js.Unsafe.get a "ended") in
      (not paused) && not ended

    module Music = struct
      type t = music

      let load_music_stream path : t = { element = create_audio path; music_pan = 0. }

      let is_music_ready (t : t) =
        let state = int_of_float (Js.float_of_number (Js.Unsafe.get t.element "readyState")) in
        state > 0

      let unload_music_stream (t : t) =
        pause_audio t.element;
        t.element##.src := Js.string ""

      let play_music_stream (t : t) =
        ignore t.music_pan;
        if !audio_enabled then play_audio t.element

      let is_music_stream_playing (t : t) = is_playing t.element
      let update_music_stream _ = ()

      let stop_music_stream (t : t) =
        pause_audio t.element;
        set_current_time t.element 0.

      let pause_music_stream (t : t) = pause_audio t.element
      let resume_music_stream (t : t) = if !audio_enabled then play_audio t.element
      let seek_music_stream (t : t) seconds = set_current_time t.element (max 0. seconds)
      let set_music_volume (t : t) volume = set_volume t.element volume
      let set_music_pan (t : t) pan = t.music_pan <- max (-1.) (min 1. pan)
      let get_music_time_length (t : t) = get_duration t.element
      let get_music_time_played (t : t) = get_current_time t.element
    end

    module Sound = struct
      type t = sound

      let load_sound path : t = { element = create_audio path; sound_pan = 0. }

      let play_sound (t : t) =
        ignore t.sound_pan;
        if !audio_enabled then (
          set_current_time t.element 0.;
          play_audio t.element)

      let stop_sound (t : t) =
        pause_audio t.element;
        set_current_time t.element 0.

      let pause_sound (t : t) = pause_audio t.element
      let resume_sound (t : t) = if !audio_enabled then play_audio t.element
      let is_sound_playing (t : t) = is_playing t.element
      let set_sound_volume (t : t) volume = set_volume t.element volume
      let set_sound_pan (t : t) pan = t.sound_pan <- max (-1.) (min 1. pan)

      let unload_sound (t : t) =
        pause_audio t.element;
        t.element##.src := Js.string ""
    end
  end

  module Text = struct end

  module Input = struct
    open Luma__types.Input_types

    let key_down : (Key.t, unit) Hashtbl.t = Hashtbl.create 97
    let key_pressed : (Key.t, unit) Hashtbl.t = Hashtbl.create 97
    let key_released : (Key.t, unit) Hashtbl.t = Hashtbl.create 97
    let mouse_down : (Mouse_button.t, unit) Hashtbl.t = Hashtbl.create 7
    let mouse_pressed : (Mouse_button.t, unit) Hashtbl.t = Hashtbl.create 7
    let mouse_released : (Mouse_button.t, unit) Hashtbl.t = Hashtbl.create 7
    let raw_mouse_x = ref 0
    let raw_mouse_y = ref 0
    let last_raw_mouse_x = ref 0
    let last_raw_mouse_y = ref 0

    let map_dom_mouse_button (button : int) : Mouse_button.t option =
      match button with
      | 0 -> Some Mouse_button.Left
      | 1 -> Some Mouse_button.Middle
      | 2 -> Some Mouse_button.Right
      | 3 -> Some Mouse_button.Back
      | 4 -> Some Mouse_button.Forward
      | _ -> None

    let dom_code_to_key ~(ev : Dom_html.keyboardEvent Js.t) (kc : int) : Key.t option =
      let open Key in
      let loc = ev##.location in
      match kc with
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
      | 91 -> Some Left_super
      | 92 -> Some Right_super
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

    let install_handlers () =
      if !input_handlers_installed then ()
      else
        let () =
          input_handlers_installed := true;
          ignore (ensure_canvas ());
          ignore (ensure_ctx ())
        in
        let doc = Dom_html.document in
        let canvas = ensure_canvas () in

        Dom_html.addEventListener doc Dom_html.Event.keydown
          (Dom_html.handler (fun ev ->
               let code = ev##.keyCode in
               (match dom_code_to_key ~ev code with
               | Some k ->
                   if not (Hashtbl.mem key_down k) then Hashtbl.replace key_pressed k ();
                   Hashtbl.replace key_down k ()
               | None -> ());
               Js._true))
          Js._false
        |> ignore;

        Dom_html.addEventListener doc Dom_html.Event.keyup
          (Dom_html.handler (fun ev ->
               let code = ev##.keyCode in
               (match dom_code_to_key ~ev code with
               | Some k ->
                   if Hashtbl.mem key_down k then Hashtbl.replace key_released k ();
                   Hashtbl.remove key_down k
               | None -> ());
               Js._true))
          Js._false
        |> ignore;

        Dom_html.addEventListener canvas Dom_html.Event.mousedown
          (Dom_html.handler (fun (ev : Dom_html.mouseEvent Js.t) ->
               let b = ev##.button in
               (match map_dom_mouse_button b with
               | Some btn ->
                   if not (Hashtbl.mem mouse_down btn) then Hashtbl.replace mouse_pressed btn ();
                   Hashtbl.replace mouse_down btn ()
               | None -> ());
               Js._true))
          Js._false
        |> ignore;

        Dom_html.addEventListener canvas Dom_html.Event.mouseup
          (Dom_html.handler (fun (ev : Dom_html.mouseEvent Js.t) ->
               let b = ev##.button in
               (match map_dom_mouse_button b with
               | Some btn ->
                   if Hashtbl.mem mouse_down btn then Hashtbl.replace mouse_released btn ();
                   Hashtbl.remove mouse_down btn
               | None -> ());
               Js._true))
          Js._false
        |> ignore;

        Dom_html.addEventListener canvas Dom_html.Event.mousemove
          (Dom_html.handler (fun (ev : Dom_html.mouseEvent Js.t) ->
               last_raw_mouse_x := !raw_mouse_x;
               last_raw_mouse_y := !raw_mouse_y;
               raw_mouse_x := int_of_float (Js.float_of_number ev##.offsetX);
               raw_mouse_y := int_of_float (Js.float_of_number ev##.offsetY);
               Js._true))
          Js._false
        |> ignore;

        Dom_html.addEventListener canvas Dom_html.Event.mousewheel
          (Dom_html.handler (fun (ev : Dom_html.mousewheelEvent Js.t) ->
               let dy = Js.float_of_number ev##.deltaY in
               wheel_accum := !wheel_accum +. (-.dy /. 120.);
               Js._true))
          Js._false
        |> ignore

    let begin_frame_common () =
      Hashtbl.reset key_pressed;
      Hashtbl.reset key_released;
      Hashtbl.reset mouse_pressed;
      Hashtbl.reset mouse_released

    let scaled_mouse_x () =
      int_of_float (float_of_int (!raw_mouse_x + !mouse_offset_x) *. !mouse_scale_x)

    let scaled_mouse_y () =
      int_of_float (float_of_int (!raw_mouse_y + !mouse_offset_y) *. !mouse_scale_y)

    let scaled_last_mouse_x () =
      int_of_float (float_of_int (!last_raw_mouse_x + !mouse_offset_x) *. !mouse_scale_x)

    let scaled_last_mouse_y () =
      int_of_float (float_of_int (!last_raw_mouse_y + !mouse_offset_y) *. !mouse_scale_y)

    let () = install_handlers ()

    module Keyboard = struct
      let begin_frame () = begin_frame_common ()
      let is_key_down (k : Key.t) = Hashtbl.mem key_down k
      let is_key_pressed (k : Key.t) = Hashtbl.mem key_pressed k
      let is_key_released (k : Key.t) = Hashtbl.mem key_released k
    end

    module Mouse = struct
      let is_mouse_button_pressed (b : Mouse_button.t) = Hashtbl.mem mouse_pressed b
      let is_mouse_button_released (b : Mouse_button.t) = Hashtbl.mem mouse_released b
      let is_mouse_button_up (b : Mouse_button.t) = not (Hashtbl.mem mouse_down b)
      let is_mouse_button_down (b : Mouse_button.t) = Hashtbl.mem mouse_down b
      let get_mouse_x () = scaled_mouse_x ()
      let get_mouse_y () = scaled_mouse_y ()

      let get_mouse_position () =
        Vec2.create (float_of_int (get_mouse_x ())) (float_of_int (get_mouse_y ()))

      let get_mouse_delta () =
        Vec2.create
          (float_of_int (scaled_mouse_x () - scaled_last_mouse_x ()))
          (float_of_int (scaled_mouse_y () - scaled_last_mouse_y ()))

      let set_mouse_position ~x ~y =
        raw_mouse_x := x;
        raw_mouse_y := y

      let set_mouse_offset ~x ~y =
        mouse_offset_x := x;
        mouse_offset_y := y

      let set_mouse_scale ~x ~y =
        mouse_scale_x := x;
        mouse_scale_y := y

      let get_mouse_wheel_move () = !wheel_frame
    end
  end

  module UI = struct
    type _win = {
      title : string;
      x : float;
      y : float;
      w : float;
      h : float;
      mutable cursor_y : float;
    }

    let win_stack : _win list ref = ref []
    let overlay : Dom_html.divElement Js.t option ref = ref None
    let line_h = 18.0

    let ensure_overlay () =
      match !overlay with
      | Some d -> d
      | None ->
          let d = Dom_html.createDiv Dom_html.document in
          d##.style##.position := Js.string "absolute";
          d##.style##.left := Js.string "0px";
          d##.style##.top := Js.string "0px";
          d##.style##.pointerEvents := Js.string "none";
          d##.style##.fontFamily := Js.string "sans-serif";
          d##.style##.fontSize := Js.string "14px";
          d##.style##.color := Js.string "#ffffff";
          Dom.appendChild Dom_html.document##.body d;
          overlay := Some d;
          d

    let begin_window ~title ?pos ?size () =
      let x = Option.map (fun (v : Vec2.t) -> v.x) pos |> Option.value ~default:20. in
      let y = Option.map (fun (v : Vec2.t) -> v.y) pos |> Option.value ~default:20. in
      let w = Option.map (fun (v : Vec2.t) -> v.x) size |> Option.value ~default:360. in
      let h = Option.map (fun (v : Vec2.t) -> v.y) size |> Option.value ~default:220. in

      let root = ensure_overlay () in
      let wnd = Dom_html.createDiv Dom_html.document in
      wnd##.style##.position := Js.string "absolute";
      wnd##.style##.left := Js.string (Printf.sprintf "%.0fpx" x);
      wnd##.style##.top := Js.string (Printf.sprintf "%.0fpx" y);
      wnd##.style##.width := Js.string (Printf.sprintf "%.0fpx" w);
      wnd##.style##.minHeight := Js.string (Printf.sprintf "%.0fpx" h);
      wnd##.style##.padding := Js.string "8px";
      wnd##.style##.border := Js.string "1px solid #aaaaaa";
      wnd##.style##.background := Js.string "rgba(0,0,0,0.45)";

      let header = Dom_html.createDiv Dom_html.document in
      header##.textContent := Js.some (Js.string title);
      header##.style##.fontWeight := Js.string "bold";
      header##.style##.marginBottom := Js.string "8px";
      Dom.appendChild wnd header;
      Dom.appendChild root wnd;

      win_stack := { title; x; y; w; h; cursor_y = y +. 24. } :: !win_stack;
      true

    let end_window () = match !win_stack with _ :: tl -> win_stack := tl | [] -> ()

    let text s =
      match !win_stack with
      | [] -> ()
      | _ ->
          let root = ensure_overlay () in
          let p = Dom_html.createDiv Dom_html.document in
          p##.textContent := Js.some (Js.string s);
          p##.style##.lineHeight := Js.string (Printf.sprintf "%.0fpx" line_h);
          Dom.appendChild root p
  end

  module Debug_draw = struct
    type space =
      [ `World
      | `Screen
      ]

    let line space ~p0 ~p1 ~colour =
      with_ctx @@ fun ctx ->
      let p0 =
        match space with
        | `Screen -> p0
        | `World ->
            let x, y = to_screen (Vec2.x p0) (Vec2.y p0) in
            Vec2.create x y
      in
      let p1 =
        match space with
        | `Screen -> p1
        | `World ->
            let x, y = to_screen (Vec2.x p1) (Vec2.y p1) in
            Vec2.create x y
      in
      ctx##.strokeStyle := Js.string (css_rgba colour);
      ctx##beginPath;
      ctx##moveTo (Js.number_of_float (Vec2.x p0)) (Js.number_of_float (Vec2.y p0));
      ctx##lineTo (Js.number_of_float (Vec2.x p1)) (Js.number_of_float (Vec2.y p1));
      ctx##stroke
  end
end

include Js_driver
