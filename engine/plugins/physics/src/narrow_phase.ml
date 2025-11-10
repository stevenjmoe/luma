open Luma__math

type t = {
  added_pairs : (int64, unit) Hashtbl.t;
  ids1 : int Dynarray.t;
  ids2 : int Dynarray.t;
}

let create ?(pairs_cap = 0) ?(set_cap = 16384) () =
  let ids1 = Dynarray.create () in
  let ids2 = Dynarray.create () in
  if pairs_cap > 0 then (
    Dynarray.ensure_capacity ids1 pairs_cap;
    Dynarray.ensure_capacity ids2 pairs_cap);

  { added_pairs = Hashtbl.create set_cap; ids1; ids2 }

let clear c =
  Hashtbl.clear c.added_pairs;
  Dynarray.clear c.ids1;
  Dynarray.clear c.ids2

let aabb_aabb_collision (s : Rb_store.t) idx1 idx2 =
  Aabb2d_raw.aabb_intersects_aabb ~a_min_x:s.min_x.(idx1) ~a_min_y:s.min_y.(idx1)
    ~a_max_x:s.max_x.(idx1) ~a_max_y:s.max_y.(idx1) ~b_min_x:s.min_x.(idx2) ~b_min_y:s.min_y.(idx2)
    ~b_max_x:s.max_x.(idx2) ~b_max_y:s.max_y.(idx2)

let aabb_circle_collision (s : Rb_store.t) idx1 idx2 =
  Aabb2d_raw.aabbb_intersects_circle ~aabb_min_x:s.min_x.(idx1) ~aabb_min_y:s.min_y.(idx1)
    ~aabb_max_x:s.max_x.(idx1) ~aabb_max_y:s.max_y.(idx1) ~circle_center_x:s.pos_x.(idx2)
    ~circle_center_y:s.pos_y.(idx2) ~circle_radius:s.radius.(idx2)

let circle_circle_collision (s : Rb_store.t) idx1 idx2 =
  Aabb2d_raw.circle_intersects_circle ~a_center_x:s.pos_x.(idx1) ~a_center_y:s.pos_y.(idx1)
    ~a_radius:s.radius.(idx1) ~b_center_x:s.pos_x.(idx2) ~b_center_y:s.pos_y.(idx2)
    ~b_radius:s.radius.(idx2)

let check_collision (s : Rb_store.t) ~idx1 ~idx2 =
  let open Bounded2d.Aabb2d in
  let a_body_type = s.shape.(idx1) in
  let b_body_type = s.shape.(idx2) in

  (* 0: Cirlce, 1: Aabb *)
  match (a_body_type, b_body_type) with
  (* Aabb Aabb *)
  | 1, 1 -> aabb_aabb_collision s idx1 idx2
  (* Circle Circle *)
  | 0, 0 -> circle_circle_collision s idx1 idx2
  (* Aabb Circle *)
  | 1, 0 -> aabb_circle_collision s idx1 idx2
  (* Circle Aabb *)
  | 0, 1 -> aabb_circle_collision s idx2 idx1
  | _ -> false

let update_actual_collision_pairs c (s : Rb_store.t) (bp : Broad_phase.t) =
  clear c;
  let { added_pairs; ids1; ids2 } = c in

  let pair_key_of_pairs id1 id2 =
    let a = Int64.of_int (min id1 id2) and b = Int64.of_int (max id1 id2) in
    Int64.logor (Int64.shift_left a 32) b
  in

  let bp_ids1, bp_ids2 = Broad_phase.pairs_view bp in

  for i = 0 to Dynarray.length bp_ids1 - 1 do
    let idx1 = Dynarray.get bp_ids1 i in
    let idx2 = Dynarray.get bp_ids2 i in
    let pair_key = pair_key_of_pairs idx1 idx2 in

    if check_collision s ~idx1 ~idx2 && not (Hashtbl.mem added_pairs pair_key) then (
      Hashtbl.add added_pairs pair_key ();
      Dynarray.add_last ids1 idx1;
      Dynarray.add_last ids2 idx2)
  done

let collisions_view c = (c.ids1, c.ids2)

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "narrow_phase"
end)
