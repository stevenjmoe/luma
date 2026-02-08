open Luma__math
(* TODO: remove hashtbls *)

type t = {
  ids1 : int Dynarray.t;
  ids2 : int Dynarray.t;
  mutable prev_pairs : (int64, unit) Hashtbl.t;
  mutable curr_pairs : (int64, unit) Hashtbl.t;
}

let create ?(pairs_cap = 0) ?(set_cap = 16384) () =
  let ids1 = Dynarray.create () in
  let ids2 = Dynarray.create () in
  if pairs_cap > 0 then (
    Dynarray.ensure_capacity ids1 pairs_cap;
    Dynarray.ensure_capacity ids2 pairs_cap);

  { ids1; ids2; prev_pairs = Hashtbl.create set_cap; curr_pairs = Hashtbl.create set_cap }

let clear c =
  Hashtbl.clear c.curr_pairs;
  Dynarray.clear c.ids1;
  Dynarray.clear c.ids2

let aabb_aabb_collision (store : Rb_store.t) idx1 idx2 =
  let open Rb_store in
  if shape_kind store idx1 <> 1 || shape_kind store idx2 <> 1 then
    failwith "Expected aabb aabb pair";

  let a_min_x = min_x store idx1 in
  let a_max_x = max_x store idx1 in
  let a_min_y = min_y store idx1 in
  let a_max_y = max_y store idx1 in
  let b_min_x = min_x store idx2 in
  let b_max_x = max_x store idx2 in
  let b_min_y = min_y store idx2 in
  let b_max_y = max_y store idx2 in

  Aabb2d_raw.aabb_intersects_aabb ~a_min_x ~a_min_y ~a_max_x ~a_max_y ~b_min_x ~b_min_y ~b_max_x
    ~b_max_y

let aabb_circle_collision (store : Rb_store.t) (shape_store : Shape_store.t) idx1 idx2 =
  let open Rb_store in
  if shape_kind store idx1 <> 1 || shape_kind store idx2 <> 0 then
    failwith "Expected aabb circle pair";

  let circle_radius = Shape_store.circle_radius shape_store (shape_handle store idx2) in
  let aabb_min_x = min_x store idx1 in
  let aabb_max_x = max_x store idx1 in
  let aabb_min_y = min_y store idx1 in
  let aabb_max_y = max_y store idx1 in

  let circle_center_x = pos_x store idx2 in
  let circle_center_y = pos_y store idx2 in

  Aabb2d_raw.aabb_intersects_circle ~aabb_min_x ~aabb_min_y ~aabb_max_x ~aabb_max_y ~circle_center_x
    ~circle_center_y ~circle_radius

let circle_circle_collision (store : Rb_store.t) (shape_store : Shape_store.t) idx1 idx2 =
  let open Rb_store in
  if shape_kind store idx1 <> 0 || shape_kind store idx2 <> 0 then
    failwith "Expected circle circle pair";

  let circle_radius_a = Shape_store.circle_radius shape_store (shape_handle store idx1) in
  let circle_radius_b = Shape_store.circle_radius shape_store (shape_handle store idx2) in

  let a_center_x = pos_x store idx1 in
  let a_center_y = pos_y store idx1 in
  let b_center_x = pos_x store idx2 in
  let b_center_y = pos_y store idx2 in

  Aabb2d_raw.circle_intersects_circle ~a_center_x ~a_center_y ~a_radius:circle_radius_a ~b_center_x
    ~b_center_y ~b_radius:circle_radius_b

let aabb_polygon_collision (store : Rb_store.t) (shape_store : Shape_store.t) idx1 idx2 =
  let open Rb_store in
  if shape_kind store idx1 <> 1 || shape_kind store idx2 <> 2 then
    failwith "Expected aabb polygon pair";

  let aabb_min_x = min_x store idx1 in
  let aabb_max_x = max_x store idx1 in
  let aabb_min_y = min_y store idx1 in
  let aabb_max_y = max_y store idx1 in

  let poly_handle = shape_handle store idx2 in
  let poly_points_x = Shape_store.polygon_storage_x shape_store in
  let poly_points_y = Shape_store.polygon_storage_y shape_store in
  let poly_offset = Shape_store.polygon_offset shape_store poly_handle in
  let poly_count = Shape_store.polygon_count shape_store poly_handle in
  let poly_center_x = pos_x store idx2 in
  let poly_center_y = pos_y store idx2 in
  let poly_angle = angle store idx2 in

  Aabb2d_raw.aabb_intersects_convex_polygon_sat ~aabb_min_x ~aabb_min_y ~aabb_max_x ~aabb_max_y
    ~poly_points_x ~poly_points_y ~poly_offset ~poly_count ~poly_center_x ~poly_center_y ~poly_angle

let polygon_circle_collision (store : Rb_store.t) (shape_store : Shape_store.t) poly_idx circle_idx
    =
  let open Rb_store in
  if shape_kind store poly_idx <> 2 || shape_kind store circle_idx <> 0 then
    failwith "Expected polygon circle pair";

  let circle_radius = Shape_store.circle_radius shape_store (shape_handle store circle_idx) in
  let circle_center_x = pos_x store circle_idx in
  let circle_center_y = pos_y store circle_idx in

  let poly_handle = shape_handle store poly_idx in
  let poly_points_x = Shape_store.polygon_storage_x shape_store in
  let poly_points_y = Shape_store.polygon_storage_y shape_store in
  let poly_offset = Shape_store.polygon_offset shape_store poly_handle in
  let poly_count = Shape_store.polygon_count shape_store poly_handle in
  let poly_center_x = pos_x store poly_idx in
  let poly_center_y = pos_y store poly_idx in
  let poly_angle = angle store poly_idx in

  let x_len = Array.length poly_points_x in
  let y_len = Array.length poly_points_y in
  let last_idx = poly_offset + poly_count - 1 in
  if
    poly_count < 3
    || poly_offset < 0
    || poly_offset >= x_len
    || poly_offset >= y_len
    || last_idx >= x_len
    || last_idx >= y_len
  then false
  else
    let c = Float.cos poly_angle in
    let s = Float.sin poly_angle in

    (* Circle center in polygon-local space: apply inverse transform. *)
    let rel_x = circle_center_x -. poly_center_x in
    let rel_y = circle_center_y -. poly_center_y in
    let circle_local_x = (rel_x *. c) +. (rel_y *. s) in
    let circle_local_y = (-.rel_x *. s) +. (rel_y *. c) in
    let radius_sq = circle_radius *. circle_radius in

    (* Check if inside convex polygon *)
    let eps = 1e-12 in
    let sign = ref 0 in
    let inside = ref true in
    let i = ref 0 in

    while !inside && !i < poly_count do
      let j = if !i = poly_count - 1 then 0 else !i + 1 in
      let i_idx = poly_offset + !i in
      let j_idx = poly_offset + j in
      let ax = poly_points_x.(i_idx) in
      let ay = poly_points_y.(i_idx) in
      let bx = poly_points_x.(j_idx) in
      let by = poly_points_y.(j_idx) in
      let edge_x = bx -. ax in
      let edge_y = by -. ay in
      let to_p_x = circle_local_x -. ax in
      let to_p_y = circle_local_y -. ay in
      let cross = (edge_x *. to_p_y) -. (edge_y *. to_p_x) in
      if Float.abs cross > eps then (
        let cur = if cross > 0. then 1 else -1 in
        if !sign = 0 then sign := cur else if cur <> !sign then inside := false;
        incr i)
    done;

    if !inside then true
    else
      (* edge distance check *)
      let intersects = ref false in
      let i = ref 0 in
      while (not !intersects) && !i < poly_count do
        let j = if !i = poly_count - 1 then 0 else !i + 1 in
        let i_idx = poly_offset + !i in
        let j_idx = poly_offset + j in
        let ax = poly_points_x.(i_idx) in
        let ay = poly_points_y.(i_idx) in
        let bx = poly_points_x.(j_idx) in
        let by = poly_points_y.(j_idx) in

        let edge_x = bx -. ax in
        let edge_y = by -. ay in
        let edge_len_sq = (edge_x *. edge_x) +. (edge_y *. edge_y) in

        (if edge_len_sq > eps then
           let ap_x = circle_local_x -. ax in
           let ap_y = circle_local_y -. ay in
           let t = ((ap_x *. edge_x) +. (ap_y *. edge_y)) /. edge_len_sq in
           let t = if t < 0. then 0. else if t > 1. then 1. else t in
           let closest_x = ax +. (edge_x *. t) in
           let closest_y = ay +. (edge_y *. t) in
           let dx = circle_local_x -. closest_x in
           let dy = circle_local_y -. closest_y in
           let dist_sq = (dx *. dx) +. (dy *. dy) in
           if dist_sq <= radius_sq then intersects := true);
        incr i
      done;
      !intersects

let check_collision (store : Rb_store.t) (shape_store : Shape_store.t) ~row_a ~row_b =
  let open Rb_store in
  let shape_a = shape_kind store row_a in
  let shape_b = shape_kind store row_b in

  (* 0: Cirlce, 1: Aabb *)
  match (shape_a, shape_b) with
  (* Aabb Aabb *)
  | 1, 1 -> aabb_aabb_collision store row_a row_b
  (* Circle Circle *)
  | 0, 0 -> circle_circle_collision store shape_store row_a row_b
  (* Aabb Circle *)
  | 1, 0 -> aabb_circle_collision store shape_store row_a row_b
  (* Circle Aabb *)
  | 0, 1 -> aabb_circle_collision store shape_store row_b row_a
  (* Polygon Circle *)
  | 2, 0 -> polygon_circle_collision store shape_store row_a row_b
  (* Circle Polygon *)
  | 0, 2 -> polygon_circle_collision store shape_store row_b row_a
  (* Polygon Aabb *)
  | 2, 1 -> aabb_polygon_collision store shape_store row_b row_a
  (* Aabb Polygon *)
  | 1, 2 -> aabb_polygon_collision store shape_store row_a row_b
  | _ -> false

let pair_key_of_pairs ~entity_a ~entity_b =
  let open Luma__id in
  let id1 = Id.Entity.to_int entity_a in
  let id2 = Id.Entity.to_int entity_b in
  let a = Int64.of_int (min id1 id2) in
  let b = Int64.of_int (max id1 id2) in
  Int64.logor (Int64.shift_left a 32) b

let entities_of_pair_key key =
  let a = Int64.(to_int (shift_right_logical key 32)) |> Luma__id.Id.Entity.of_int in
  let b = Int64.(to_int (logand key 0xFFFF_FFFFL)) |> Luma__id.Id.Entity.of_int in
  (a, b)

let update_actual_collision_pairs
    c
    (store : Rb_store.t)
    (shape_store : Shape_store.t)
    (bp : Broad_phase.t)
    (index : Rb_store.Index.t) =
  clear c;
  let { curr_pairs; ids1; ids2; _ } = c in

  let bp_ids1, bp_ids2 = Broad_phase.pairs_view bp in

  for i = 0 to Dynarray.length bp_ids1 - 1 do
    let row_a = Dynarray.get bp_ids1 i in
    let row_b = Dynarray.get bp_ids2 i in
    let entity_a = Rb_store.Index.entity_at_row index row_a in
    let entity_b = Rb_store.Index.entity_at_row index row_b in
    let pair_key = pair_key_of_pairs ~entity_a ~entity_b in

    if check_collision store shape_store ~row_a ~row_b && not (Hashtbl.mem curr_pairs pair_key) then (
      Hashtbl.add curr_pairs pair_key ();
      Dynarray.add_last ids1 row_a;
      Dynarray.add_last ids2 row_b)
  done

let collisions_view c = (c.ids1, c.ids2)

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "narrow_phase"
end)
