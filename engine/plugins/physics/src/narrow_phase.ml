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
