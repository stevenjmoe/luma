open Luma__ecs
open Luma__id
open Luma__serialize
open Luma__core
open Luma__app

module type S = sig
  val toggle_overlay : unit -> ('a, unit) System.t
  val draw_overlay : unit -> (World.t, unit) System.t
  val setup_state : unit -> (World.t, unit) System.t
  val add_plugin : App.t -> App.t
end

let rec take_n n items =
  match (n, items) with
  | n, _ when n <= 0 -> []
  | _, [] -> []
  | n, x :: tl -> x :: take_n (n - 1) tl

let page_slice ~page ~per_page items = List.drop (page * per_page) items |> List.take per_page

let abbrev_name (s : string) : string =
  let len = String.length s in
  if len = 0 then "?"
  else
    let rec caps i acc =
      if i >= len then acc
      else
        let c = s.[i] in
        let acc' = if i = 0 then c :: acc else if c >= 'A' && c <= 'Z' then c :: acc else acc in
        caps (i + 1) acc'
    in
    match List.rev (caps 0 []) with
    | [] -> if len <= 3 then String.uppercase_ascii s else String.uppercase_ascii (String.sub s 0 3)
    | [ c ] ->
        if len >= 2 then String.uppercase_ascii (String.make 1 c ^ String.make 1 s.[1])
        else String.uppercase_ascii (String.make 1 c)
    | cs ->
        let n = min 3 (List.length cs) in
        String.uppercase_ascii (String.init n (fun i -> List.nth cs i))

let component_label ~expanded world e cid : string =
  match World.Introspect.get_component_packed world e cid with
  | Some (Luma__ecs.Component.Packed ((module C), _)) ->
      if expanded then C.name else abbrev_name C.name
  | None -> "?"

let preview_components max_chars world e : string =
  let cids = World.Introspect.entity_components world e |> Luma__id.Id.ComponentSet.to_list in
  let labels = List.map (component_label ~expanded:false world e) cids in
  let total = List.length labels in
  let rec loop rem shown acc = function
    | [] ->
        if shown = total then String.concat ", " (List.rev acc)
        else String.concat ", " (List.rev acc) ^ " +" ^ string_of_int (total - shown)
    | l :: tl ->
        let add = if acc = [] then String.length l else 2 + String.length l in
        if add <= rem then loop (rem - add) (shown + 1) (l :: acc) tl
        else String.concat ", " (List.rev acc) ^ " +" ^ string_of_int (total - shown)
  in
  loop max_chars 0 [] labels

(* wrap a list of labels to multiple comma-separated lines within [chars_per_line] *)
let wrap_labels_to_lines ~chars_per_line labels =
  let flush acc line = if line = [] then acc else String.concat ", " (List.rev line) :: acc in
  let rec loop current_len current_line acc = function
    | [] -> List.rev (flush acc current_line)
    | l :: tl ->
        let added = if current_line = [] then String.length l else 2 + String.length l in
        if current_len + added <= chars_per_line then
          loop (current_len + added) (l :: current_line) acc tl
        else loop (String.length l) [ l ] (flush acc current_line) tl
  in
  loop 0 [] [] labels

(* bottom-right resize handle hit-test  *)
let handle_hit ~x ~y ~w ~h (mx, my) : bool =
  let hs = 14. in
  let hx = x +. w -. hs and hy = y +. h -. hs in
  let fx = float_of_int mx and fy = float_of_int my in
  fx >= hx && fx <= x +. w && fy >= hy && fy <= y +. h

let clamp_window_size ~min_w ~min_h w h =
  let w' = if w < min_w then min_w else w in
  let h' = if h < min_h then min_h else h in
  (w', h')

module Make (D : Luma__driver.Driver.S) = struct
  open State

  let current_panel_rect state : Luma__math.Rect.t * float * float * float * float =
    if state.full then
      let sw, sh =
        (D.Window.screen_width () |> float_of_int, D.Window.screen_height () |> float_of_int)
      in
      let margin = 8. in
      let x = margin and y = margin in
      let w = sw -. (2. *. margin) and h = sh -. (2. *. margin) in
      ( Luma__math.Rect.create ~pos:(Luma__math.Vec2.create x y) ~size:(Luma__math.Vec2.create w h),
        x,
        y,
        w,
        h )
    else
      ( Luma__math.Rect.create
          ~pos:(Luma__math.Vec2.create state.x state.y)
          ~size:(Luma__math.Vec2.create state.w state.h),
        state.x,
        state.y,
        state.w,
        state.h )

  let toggle_overlay () =
    System.make_with_resources ~components:Query.Component.End
      ~resources:Query.Resource.(Resource (module State.R) & End)
      "debug_toggle_overlay"
      (fun w _ _ (state, _) ->
        if D.Input.Keyboard.is_key_pressed Luma__types.Input_types.Key.F1 then (
          state.open_ <- not state.open_;
          if state.open_ then state.page <- 0);
        w)

  let setup_state () =
    System.make ~components:End "" (fun w _ _ ->
        let state = State.default () in
        (* Ensure initial compact size is large enough to fit headings and a short preview. *)
        if state.w < 460. then state.w <- 460.;
        if state.h < 320. then state.h <- 320.;
        let packed = Luma__resource.Resource.pack (module State.R) state in
        World.add_resource State.R.type_id packed w)

  let draw_overlay () =
    System.make_with_resources ~components:Query.Component.End
      ~resources:Query.Resource.(Resource (module State.R) & End)
      "debug.draw_overlay"
      (fun world _ _ (state, _) ->
        if not state.open_ then world
        else (
          if D.Input.Keyboard.is_key_pressed Luma__types.Input_types.Key.F2 then
            state.full <- not state.full;

          (* Refresh cache only when the world's structure changes *)
          let rev = World.Introspect.revision world in
          if rev <> state.cached_rev then (
            state.cached_entities <- World.Introspect.entities_seq world |> List.of_seq;
            state.cached_rev <- rev;
            state.page <- 0);

          (* Ensure a sensible minimum compact size every frame (covers initial too) *)
          if not state.full then (
            let mw, mh = (460., 320.) in
            if state.w < mw then state.w <- mw;
            if state.h < mh then state.h <- mh);

          (* Compute rect from state *)
          let rect, x, y, w, h = current_panel_rect state in

          (* Compact-mode resize with bottom-right handle *)
          if not state.full then (
            let m = D.Input.Mouse.get_mouse_position () in
            let mx = Luma__math.Vec2.x m |> int_of_float
            and my = Luma__math.Vec2.y m |> int_of_float in
            let over_handle = handle_hit ~x ~y ~w ~h (mx, my) in

            if
              over_handle
              && D.Input.Mouse.is_mouse_button_pressed Luma__types.Input_types.Mouse_button.Left
            then state.resizing <- true;

            if
              state.resizing
              && D.Input.Mouse.is_mouse_button_down Luma__types.Input_types.Mouse_button.Left
            then (
              let fx = float_of_int mx and fy = float_of_int my in
              let new_w = fx -. x and new_h = fy -. y in
              let new_w, new_h = clamp_window_size ~min_w:260. ~min_h:160. new_w new_h in
              state.w <- new_w;
              state.h <- new_h);

            if
              state.resizing
              && D.Input.Mouse.is_mouse_button_released Luma__types.Input_types.Mouse_button.Left
            then state.resizing <- false);

          (* Layout and paging *)
          let line_h = 20 in
          let header_h = 34 in
          let usable_h = h -. float_of_int header_h -. 8. in
          let rows_fit =
            let n = int_of_float (usable_h /. float_of_int line_h) in
            if n < 1 then 1 else n
          in
          let per_page = if state.per_page < rows_fit then state.per_page else rows_fit in
          let total = List.length state.cached_entities in
          let pages = if total <= 0 then 1 else (total + per_page - 1) / per_page in
          let page =
            if state.page < 0 then 0 else if state.page >= pages then pages - 1 else state.page
          in
          state.page <- page;

          let entity_slice = page_slice ~page ~per_page state.cached_entities in

          (* Panel *)
          let title =
            Printf.sprintf "Entities (%d)  page %d/%d" total (page + 1)
              (if pages <= 0 then 1 else pages)
          in
          D.Draw.draw_rect rect (D.Colour.rgb ~r:32 ~g:32 ~b:32);
          D.Draw.draw_text title
            (int_of_float x + 8)
            (int_of_float y + 6)
            18
            (D.Colour.rgb ~r:220 ~g:220 ~b:220);

          (* Headings *)
          let y_head = int_of_float y + 34 in
          D.Draw.draw_text "entity"
            (int_of_float x + 8)
            y_head 16
            (D.Colour.rgb ~r:160 ~g:160 ~b:160);
          D.Draw.draw_text "components"
            (int_of_float x + 150)
            y_head 16
            (D.Colour.rgb ~r:160 ~g:160 ~b:160);

          let div_rect =
            Luma__math.Rect.create
              ~pos:(Luma__math.Vec2.create (x +. 8.) (float_of_int (y_head + 18)))
              ~size:(Luma__math.Vec2.create (w -. 16.) 1.)
          in
          D.Draw.draw_rect div_rect (D.Colour.rgb ~r:96 ~g:96 ~b:96);

          let chars_per_line =
            let avail = w -. 170. in
            let c = int_of_float (avail /. 7.) in
            if c < 8 then 8 else if c > 256 then 256 else c
          in

          let expanded = state.full || w >= 700. in

          (* Rows: entity id + components (wrapped full names if expanded, compact preview otherwise) *)
          List.fold_left
            (fun y_row e ->
              let entity_metadata = World.entity_metadata world e in
              let entity_label =
                if String.length entity_metadata.name = 0 then Id.Entity.to_int e |> string_of_int
                else entity_metadata.name
              in
              D.Draw.draw_text entity_label
                (int_of_float x + 8)
                y_row 16
                (D.Colour.rgb ~r:220 ~g:220 ~b:220);

              if expanded then
                let labels =
                  World.Introspect.entity_components world e
                  |> Luma__id.Id.ComponentSet.to_list
                  |> List.map (component_label ~expanded:true world e)
                in

                let lines = wrap_labels_to_lines ~chars_per_line labels in

                let rec draw_lines y0 = function
                  | [] -> y0
                  | s :: tl ->
                      D.Draw.draw_text s
                        (int_of_float x + 150)
                        y0 16
                        (D.Colour.rgb ~r:220 ~g:220 ~b:220);
                      let y1 = y0 + line_h in
                      if float_of_int y1 < y +. h -. 8. then draw_lines y1 tl else y0
                in

                let next_y = draw_lines y_row lines in
                if float_of_int next_y < y +. h -. 8. then next_y else y_row
              else
                let preview = preview_components chars_per_line world e in

                D.Draw.draw_text preview
                  (int_of_float x + 150)
                  y_row 16
                  (D.Colour.rgb ~r:220 ~g:220 ~b:220);
                let next_y = y_row + line_h in
                if float_of_int next_y < y +. h -. 8. then next_y else y_row)
            (y_head + 24) entity_slice
          |> ignore;

          (if not state.full then
             let hs = 14. in
             let handle_rect =
               Luma__math.Rect.create
                 ~pos:(Luma__math.Vec2.create (x +. w -. hs) (y +. h -. hs))
                 ~size:(Luma__math.Vec2.create hs hs)
             in
             D.Draw.draw_rect handle_rect (D.Colour.rgb ~r:80 ~g:80 ~b:80));

          (* Page controls *)
          if D.Input.Keyboard.is_key_pressed Luma__types.Input_types.Key.Page_up then
            state.page <- (if page <= 0 then 0 else page - 1);
          if D.Input.Keyboard.is_key_pressed Luma__types.Input_types.Key.Page_down then
            state.page <- (if page + 1 >= pages then pages - 1 else page + 1);

          world))

  let add_plugin app =
    let open Luma__app in
    app
    |> App.on Startup @@ setup_state ()
    |> App.on PreUpdate @@ toggle_overlay ()
    |> App.on Overlay @@ draw_overlay ()
end
