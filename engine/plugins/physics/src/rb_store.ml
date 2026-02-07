open Luma__math
open Rigid_body
open Utils

type rigid_body = Rigid_body.t

type t = {
  mutable len : int;
  mutable cap : int;
  mutable shape_handle : int array;
  mutable body_type : int array;
  mutable pos_x : float array;
  mutable pos_y : float array;
  mutable prev_pos_x : float array;
  mutable prev_pos_y : float array;
  mutable vel_x : float array;
  mutable vel_y : float array;
  mutable force_acc_x : float array;
  mutable force_acc_y : float array;
  mutable inv_mass : float array;
  mutable damping : float array;
  mutable angle : float array;
  mutable shape : int array;
  mutable min_x : float array;
  mutable min_y : float array;
  mutable max_x : float array;
  mutable max_y : float array;
  mutable active : int array;
  mutable last_seen_generation : int array;
  mutable current_generation : int;
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
    shape_handle = f 0;
    body_type = f 0;
    pos_x = f 0.;
    pos_y = f 0.;
    prev_pos_x = f 0.;
    prev_pos_y = f 0.;
    vel_x = f 0.;
    vel_y = f 0.;
    force_acc_x = f 0.;
    force_acc_y = f 0.;
    inv_mass = f 0.;
    damping = f 1.;
    angle = f 0.;
    shape = f 0;
    min_x = f 0.;
    min_y = f 0.;
    max_x = f 0.;
    max_y = f 0.;
    active = f 1;
    last_seen_generation = f 0;
    current_generation = 0;
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
    s.shape_handle <- grow_int s.shape_handle;
    s.body_type <- grow_int s.body_type;
    s.pos_x <- grow_float s.pos_x;
    s.pos_y <- grow_float s.pos_y;
    s.prev_pos_x <- grow_float s.prev_pos_x;
    s.prev_pos_y <- grow_float s.prev_pos_y;
    s.vel_x <- grow_float s.vel_x;
    s.vel_y <- grow_float s.vel_y;
    s.force_acc_x <- grow_float s.force_acc_x;
    s.force_acc_y <- grow_float s.force_acc_y;
    s.inv_mass <- grow_float s.inv_mass;
    s.damping <- grow_float s.damping;
    s.angle <- grow_float s.angle;
    s.active <- grow_int s.active;
    s.shape <- grow_int s.shape;
    s.min_x <- grow_float s.min_x;
    s.min_y <- grow_float s.min_y;
    s.max_x <- grow_float s.max_x;
    s.max_y <- grow_float s.max_y;
    s.last_seen_generation <- grow_int s.last_seen_generation

(** [swap_rows store row1 row2] *)
let swap_rows s i j =
  if i = j then ()
  else
    let swap a =
      let t = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- t
    in
    swap s.shape_handle;
    swap s.body_type;
    swap s.pos_x;
    swap s.pos_y;
    swap s.prev_pos_x;
    swap s.prev_pos_y;
    swap s.vel_x;
    swap s.vel_y;
    swap s.force_acc_x;
    swap s.force_acc_y;
    swap s.inv_mass;
    swap s.damping;
    swap s.angle;
    swap s.active;
    swap s.shape;
    swap s.min_x;
    swap s.min_y;
    swap s.max_x;
    swap s.max_y;
    swap s.last_seen_generation

(** [add store rigid_body] adds the body to the store and returns the index. *)
let add store (shape_store : Shape_store.t) (rb : rigid_body) =
  let new_len = store.len + 1 in
  ensure_capacity store new_len;
  let i = store.len in
  store.shape_handle.(i) <- Shape_store.add shape_store rb.shape;
  store.body_type.(i) <- Rigid_body.encode_body_type rb.body_type;
  store.inv_mass.(i) <- rb.inv_mass;
  store.pos_x.(i) <- rb.pos.x;
  store.pos_y.(i) <- rb.pos.y;
  store.prev_pos_x.(i) <- rb.pos.x;
  store.prev_pos_y.(i) <- rb.pos.y;
  store.vel_x.(i) <- rb.vel.x;
  store.vel_y.(i) <- rb.vel.y;
  store.force_acc_x.(i) <- rb.force_accumulator.x;
  store.force_acc_y.(i) <- rb.force_accumulator.y;
  store.inv_mass.(i) <- rb.inv_mass;
  store.damping.(i) <- rb.damping;
  store.angle.(i) <- rb.angle;
  store.active.(i) <- (if rb.active then 1 else 0);
  store.shape.(i) <- Rigid_body.encode_shape rb.shape;
  store.len <- i + 1;
  store.current_generation <- 0;

  (match rb.shape with
  | Circle radius ->
      (* circle aabb *)
      let cx = store.pos_x.(i) in
      let cy = store.pos_y.(i) in
      store.min_x.(i) <- cx -. radius;
      store.min_y.(i) <- cy -. radius;
      store.max_x.(i) <- cx +. radius;
      store.max_y.(i) <- cy +. radius;
      ()
  | Aabb half_size ->
      let cx = store.pos_x.(i) in
      let cy = store.pos_y.(i) in
      let min_x = cx -. half_size.x in
      let max_x = cx +. half_size.x in
      let min_y = cy -. half_size.y in
      let max_y = cy +. half_size.y in

      store.min_x.(i) <- min_x;
      store.min_y.(i) <- min_y;
      store.max_x.(i) <- max_x;
      store.max_y.(i) <- max_y
  | Polygon points ->
      let iso = Rigid_body.isometry rb in
      let aabb = Bounded2d.Bounded_polygon.aabb_2d points iso in
      let min = Bounded2d.Aabb2d.min aabb in
      let max = Bounded2d.Aabb2d.max aabb in

      store.min_x.(i) <- min.x;
      store.min_y.(i) <- min.y;
      store.max_x.(i) <- max.x;
      store.max_y.(i) <- max.y);
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
let is_dynamic s row = s.body_type.(row) = 1
let is_kinematic s row = s.body_type.(row) = 2

let apply_impulse_at s ~row ~ix ~iy =
  if is_active s row && is_dynamic s row then (
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
  if is_active s row && is_dynamic s row then
    let inv = s.inv_mass.(row) in
    if inv > 0. then
      let m = 1. /. inv in
      let gravity_force_x = gx *. m in
      let gravity_force_y = gy *. m in
      apply_force_at s ~row ~fx:gravity_force_x ~fy:gravity_force_y

(* Semi-implicit Euler with damping expressed per-second: vel *= damping ** dt *)
let integrate_linear_motion_at store (shape_store : Shape_store.t) ~row ~dt =
  if dt > 0. && is_active store row && is_dynamic store row then (
    let inv = store.inv_mass.(row) in

    let ax = store.force_acc_x.(row) *. inv in
    let ay = store.force_acc_y.(row) *. inv in

    store.vel_x.(row) <- store.vel_x.(row) +. (ax *. dt);
    store.vel_y.(row) <- store.vel_y.(row) +. (ay *. dt);

    (* damping (frame-rate independent) *)
    let damp = store.damping.(row) ** dt in
    store.vel_x.(row) <- store.vel_x.(row) *. damp;
    store.vel_y.(row) <- store.vel_y.(row) *. damp;

    store.pos_x.(row) <- store.pos_x.(row) +. (store.vel_x.(row) *. dt);
    store.pos_y.(row) <- store.pos_y.(row) +. (store.vel_y.(row) *. dt);

    (* done with forces this step *)
    store.force_acc_x.(row) <- 0.;
    store.force_acc_y.(row) <- 0.;

    (* 0: Circle. 1: Box *)
    match store.shape.(row) with
    | 0 ->
        let radius = Shape_store.circle_radius shape_store store.shape_handle.(row) in
        store.min_x.(row) <- store.pos_x.(row) -. radius;
        store.max_x.(row) <- store.pos_x.(row) +. radius;
        store.min_y.(row) <- store.pos_y.(row) -. radius;
        store.max_y.(row) <- store.pos_y.(row) +. radius
    | 1 ->
        let half_size = Shape_store.aabb_half_size shape_store store.shape_handle.(row) in

        store.min_x.(row) <- store.pos_x.(row) -. half_size.x;
        store.max_x.(row) <- store.pos_x.(row) +. half_size.x;

        store.min_y.(row) <- store.pos_y.(row) -. half_size.y;
        store.max_y.(row) <- store.pos_y.(row) +. half_size.y
    | 2 ->
        let points = Shape_store.polygon_points shape_store store.shape_handle.(row) in
        let iso =
          Isometry.create
            (Rot2.of_radians store.angle.(row))
            (Vec2.create store.pos_x.(row) store.pos_y.(row))
        in
        let aabb = Bounded2d.Bounded_polygon.aabb_2d points iso in
        let min = Bounded2d.Aabb2d.min aabb in
        let max = Bounded2d.Aabb2d.max aabb in

        store.min_x.(row) <- min.x;
        store.min_y.(row) <- min.y;
        store.max_x.(row) <- max.x;
        store.max_y.(row) <- max.y
    | _ -> ())

(** [bounding_box store idx] returns the [Bounded2d.Aabb2d] representation of the raw store values
    from the given index. *)
let bounding_box store i =
  let min_x = store.min_x.(i) in
  let min_y = store.min_y.(i) in
  let max_x = store.max_x.(i) in
  let max_y = store.max_y.(i) in
  let min = Vec2.create min_x min_y in
  let max = Vec2.create max_x max_y in
  Bounded2d.Aabb2d.of_min_max min max

module Index = struct
  open Luma__id

  type t = {
    ent_to_row : (Id.Entity.t, int) Hashtbl.t;
    mutable row_to_ent : Id.Entity.t array;
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
      row_to_ent = Array.make initial sentinel_entity;
      len = 0;
      cap = initial;
    }

  let ensure_capacity s need =
    if need <= s.cap then ()
    else
      let cap = max need (max 8 (s.cap * 2)) in
      let r = Array.make cap sentinel_entity in
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
      let ei = s.row_to_ent.(i) in
      let ej = s.row_to_ent.(j) in
      s.row_to_ent.(i) <- ej;
      s.row_to_ent.(j) <- ei;
      if ej <> sentinel_entity then Hashtbl.replace s.ent_to_row ej i;
      if ei <> sentinel_entity then Hashtbl.replace s.ent_to_row ei j

  let on_remove s ~row =
    let last = s.len - 1 in
    if row < 0 || row >= s.len then invalid_arg "Rb_index.on_remove";

    let e_removed = s.row_to_ent.(row) in
    let e_last = s.row_to_ent.(last) in

    if row < last && e_last <> sentinel_entity then (
      Hashtbl.replace s.ent_to_row e_last row;
      (* Move last's entry into removed slot *)
      s.row_to_ent.(row) <- e_last);

    (* Clean up last entry and removed entity *)
    Hashtbl.remove s.ent_to_row e_removed;
    s.row_to_ent.(last) <- sentinel_entity;
    s.len <- last

  let row_of_entity s entity = Hashtbl.find_opt s.ent_to_row entity
end
