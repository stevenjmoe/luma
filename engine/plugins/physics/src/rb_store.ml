open Luma__math
open Rigid_body

(** The rigid body Store is used to represent a rigid body's values in a way that prevents boxed
    values in hot paths.

    Each body is represented by the same index in each array. The index should be the *)

type rigid_body = Rigid_body.t

type t = {
  mutable len : int;
  mutable cap : int;
  mutable body_type : int array;
  mutable pos_x : float array;
  mutable pos_y : float array;
  mutable vel_x : float array;
  mutable vel_y : float array;
  mutable force_acc_x : float array;
  mutable force_acc_y : float array;
  mutable inv_mass : float array;
  mutable damping : float array;
  mutable angle : float array;
  mutable shape : int array;
  mutable radius : float array;
  mutable box_hw : float array;
  mutable box_hh : float array;
  mutable min_x : float array;
  mutable min_y : float array;
  mutable max_x : float array;
  mutable max_y : float array;
  mutable active : int array;
}

module R = Luma__resource.Resource.Make (struct
  type inner = t

  let name = "body_store"
end)

let create ?(initial = 128) () =
  let f v = Array.make initial v in
  {
    len = 0;
    cap = initial;
    body_type = f 0;
    pos_x = f 0.;
    pos_y = f 0.;
    vel_x = f 0.;
    vel_y = f 0.;
    force_acc_x = f 0.;
    force_acc_y = f 0.;
    inv_mass = f 0.;
    damping = f 1.;
    angle = f 0.;
    shape = f 0;
    radius = f 0.;
    box_hw = f 0.;
    box_hh = f 0.;
    min_x = f 0.;
    min_y = f 0.;
    max_x = f 0.;
    max_y = f 0.;
    active = f 1;
  }

let ensure_capacity s need =
  if need <= s.cap then ()
  else
    let new_cap = max need (s.cap * 2) in
    let grow_float a =
      let b = Array.make new_cap 0. in
      Array.blit a 0 b 0 s.len;
      b
    in
    let grow_int a =
      let b = Array.make new_cap 0 in
      Array.blit a 0 b 0 s.len;
      b
    in
    s.cap <- new_cap;
    s.body_type <- grow_int s.body_type;
    s.pos_x <- grow_float s.pos_x;
    s.pos_y <- grow_float s.pos_y;
    s.vel_x <- grow_float s.vel_x;
    s.vel_y <- grow_float s.vel_y;
    s.force_acc_x <- grow_float s.force_acc_x;
    s.force_acc_y <- grow_float s.force_acc_y;
    s.inv_mass <- grow_float s.inv_mass;
    s.damping <- grow_float s.damping;
    s.angle <- grow_float s.angle;
    s.active <- grow_int s.active;
    s.radius <- grow_float s.radius;
    s.box_hw <- grow_float s.box_hw;
    s.box_hh <- grow_float s.box_hh;
    s.min_x <- grow_float s.min_x;
    s.min_y <- grow_float s.min_y;
    s.max_x <- grow_float s.max_x;
    s.max_y <- grow_float s.max_y

(** [swap_rows store row1 row2] *)
let swap_rows s i j =
  if i = j then ()
  else
    let swap a =
      let t = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- t
    in
    swap s.body_type;
    swap s.pos_x;
    swap s.pos_y;
    swap s.vel_x;
    swap s.vel_y;
    swap s.force_acc_x;
    swap s.force_acc_y;
    swap s.inv_mass;
    swap s.damping;
    swap s.angle;
    swap s.active;
    swap s.shape;
    swap s.radius;
    swap s.box_hw;
    swap s.box_hh;
    swap s.min_x;
    swap s.min_y;
    swap s.max_x;
    swap s.max_y

(** [add store rigid_body] adds the body to the store and returns the index. *)
let add s (rb : rigid_body) =
  ensure_capacity s (s.len + 1);
  let i = s.len in
  s.body_type.(i) <- Rigid_body.encode_body_type rb.body_type;
  s.inv_mass.(i) <- rb.inv_mass;
  s.pos_x.(i) <- rb.pos.x;
  s.pos_y.(i) <- rb.pos.y;
  s.vel_x.(i) <- rb.vel.x;
  s.vel_y.(i) <- rb.vel.y;
  s.force_acc_x.(i) <- rb.force_accumulator.x;
  s.force_acc_y.(i) <- rb.force_accumulator.y;
  s.inv_mass.(i) <- rb.inv_mass;
  s.damping.(i) <- rb.damping;
  s.angle.(i) <- rb.angle;
  s.active.(i) <- (if rb.active then 1 else 0);
  s.len <- i + 1;

  (match rb.shape with
  | Circle c -> s.radius.(i) <- Bounded2d.Bounding_circle.radius c
  | Aabb a ->
      let half_size = Bounded2d.Aabb2d.half_size a in
      s.box_hw.(i) <- half_size.x;
      s.box_hh.(i) <- half_size.y;
      s.min_x.(i) <- (Bounded2d.Aabb2d.min a).x;
      s.min_y.(i) <- (Bounded2d.Aabb2d.min a).y;
      s.max_x.(i) <- (Bounded2d.Aabb2d.max a).x;
      s.max_y.(i) <- (Bounded2d.Aabb2d.max a).y);
  i

(** [remove store row] *)
let remove s row =
  let last = s.len - 1 in
  if row < 0 || row >= s.len then invalid_arg "Rigid_body_store.remove";
  if row < last then swap_rows s row last;
  s.len <- last

(** [set_velocity store row vx vy] *)
let set_velocity s row vx vy =
  s.vel_x.(row) <- vx;
  s.vel_y.(row) <- vy

(** [add_force store row fx fy] *)
let add_force s row fx fy =
  s.force_acc_x.(row) <- s.force_acc_x.(row) +. fx;
  s.force_acc_y.(row) <- s.force_acc_y.(row) +. fy

let iter_active s f =
  for i = 0 to s.len - 1 do
    if s.active.(i) = 1 then f i
  done

let clear s = s.len <- 0

(* Helpers *)
let is_active s row = s.active.(row) = 1
let is_static s row = s.body_type.(row) = 0 (* Static=0, Dynamic=1, Kinematic=2 *)

let apply_impulse_at s ~row ~ix ~iy =
  if is_active s row && not (is_static s row) then (
    let inv = s.inv_mass.(row) in
    s.vel_x.(row) <- s.vel_x.(row) +. (ix *. inv);
    s.vel_y.(row) <- s.vel_y.(row) +. (iy *. inv))

let clear_forces_at s ~row =
  s.force_acc_x.(row) <- 0.;
  s.force_acc_y.(row) <- 0.

let apply_force_at s ~row ~fx ~fy =
  if is_active s row && not (is_static s row) then (
    s.force_acc_x.(row) <- s.force_acc_x.(row) +. fx;
    s.force_acc_y.(row) <- s.force_acc_y.(row) +. fy)

let apply_gravity_at s ~row ~gx ~gy =
  if is_active s row && not (is_static s row) then
    let inv = s.inv_mass.(row) in
    if inv > 0. then
      let m = 1. /. inv in
      let gravity_force_x = gx *. m in
      let gravity_force_y = gy *. m in
      apply_force_at s ~row ~fx:gravity_force_x ~fy:gravity_force_y

(* Semi-implicit Euler with damping expressed per-second: vel *= damping ** dt *)
let integrate_linear_motion_at s ~row ~dt =
  if dt > 0. && is_active s row && not (is_static s row) then (
    let inv = s.inv_mass.(row) in

    let ax = s.force_acc_x.(row) *. inv in
    let ay = s.force_acc_y.(row) *. inv in

    s.vel_x.(row) <- s.vel_x.(row) +. (ax *. dt);
    s.vel_y.(row) <- s.vel_y.(row) +. (ay *. dt);

    (* damping (frame-rate independent) *)
    let damp = s.damping.(row) ** dt in
    s.vel_x.(row) <- s.vel_x.(row) *. damp;
    s.vel_y.(row) <- s.vel_y.(row) *. damp;

    s.pos_x.(row) <- s.pos_x.(row) +. (s.vel_x.(row) *. dt);
    s.pos_y.(row) <- s.pos_y.(row) +. (s.vel_y.(row) *. dt);

    (* done with forces this step *)
    s.force_acc_x.(row) <- 0.;
    s.force_acc_y.(row) <- 0.;

    (* 0: Circle. 1: Box *)
    (if s.shape.(row) = 0 then (
       let r = s.radius.(row) in
       s.min_x.(row) <- s.pos_x.(row) -. r;
       s.max_x.(row) <- s.pos_x.(row) +. r;
       s.min_y.(row) <- s.pos_y.(row) -. r;
       s.max_y.(row) <- s.pos_y.(row) +. r;
       ())
     else
       let half_w = s.box_hw.(row) in
       let half_h = s.box_hh.(row) in

       s.min_x.(row) <- s.pos_x.(row) -. half_w;
       s.max_x.(row) <- s.pos_x.(row) +. half_w;

       s.min_y.(row) <- s.pos_y.(row) -. half_h;
       s.max_y.(row) <- s.pos_y.(row) +. half_h;
       ());

    ())

module Index = struct
  type t = {
    ent_to_row : (int, int) Hashtbl.t;
    mutable row_to_ent : int array;
    mutable len : int;
    mutable cap : int;
  }

  module R = Luma__resource.Resource.Make (struct
    type inner = t

    let name = "rb_index"
  end)

  let create ~initial =
    {
      ent_to_row = Hashtbl.create (initial * 2);
      row_to_ent = Array.make initial (-1);
      len = 0;
      cap = initial;
    }

  let ensure_capacity s need =
    if need <= s.cap then ()
    else
      let cap = max need (max 8 (s.cap * 2)) in
      let r = Array.make cap (-1) in
      Array.blit s.row_to_ent 0 r 0 s.len;
      s.row_to_ent <- r;
      s.cap <- cap

  let on_add s ~entity ~row =
    ensure_capacity s (row + 1);
    s.row_to_ent.(row) <- entity;
    Hashtbl.replace s.ent_to_row entity row;
    if row >= s.len then s.len <- row + 1

  let on_swap s ~i ~j =
    if i = j then ()
    else
      let ei = s.row_to_ent.(i) and ej = s.row_to_ent.(j) in
      s.row_to_ent.(i) <- ej;
      s.row_to_ent.(j) <- ei;
      if ej <> -1 then Hashtbl.replace s.ent_to_row ej i;
      if ei <> -1 then Hashtbl.replace s.ent_to_row ei j

  let on_remove s ~row =
    let last = s.len - 1 in
    if row < 0 || row >= s.len then invalid_arg "Rb_index.on_remove";

    let e_removed = s.row_to_ent.(row) in
    let e_last = s.row_to_ent.(last) in

    if row < last then (
      Hashtbl.replace s.ent_to_row e_last row;
      (* Move last's entry into removed slot *)
      s.row_to_ent.(row) <- e_last);

    (* Clean up last entry and removed entity *)
    Hashtbl.remove s.ent_to_row e_removed;
    s.row_to_ent.(last) <- -1;
    s.len <- last

  let row_of_entity s entity = Hashtbl.find_opt s.ent_to_row entity
end
