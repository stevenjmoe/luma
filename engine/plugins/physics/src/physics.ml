open Luma__core
open Luma__math
open Luma__resource
open Luma__id

module type S = sig
  open Luma__ecs
  module Rigid_body = Rigid_body
  module Colliders = Colliders

  val pos : Luma__ecs.World.t -> Luma__id.Id.EntitySet.elt -> Vec2.t option

  val move_and_collide :
    World.t -> Id.Entity.t -> velocity:Vec2.t -> dt:float -> Kinematic_collision2d.t option

  val move_and_slide : ?max_iterations:int -> World.t -> Id.Entity.t -> Vec2.t -> float -> bool
  val plugin : ?world_config:Config.t -> Luma__app.App.t -> Luma__app.App.t
end

module Make (L : Luma.S) : S = struct
  module Rigid_body = Rigid_body
  module Colliders = Colliders
  module Plugin = Plugin.Make (L)

  let plugin = Plugin.plugin

  let update_bounds store row =
    let open Rb_store in
    if store.shape.(row) = 0 then (
      let r = store.radius.(row) in
      let x = store.pos_x.(row) in
      let y = store.pos_y.(row) in

      store.min_x.(row) <- x -. r;
      store.max_x.(row) <- x +. r;
      store.min_y.(row) <- y -. r;
      store.max_y.(row) <- y +. r)
    else
      let hw = store.box_hw.(row) in
      let hh = store.box_hh.(row) in
      let x = store.pos_x.(row) in
      let y = store.pos_y.(row) in

      store.min_x.(row) <- x -. hw;
      store.max_x.(row) <- x +. hw;
      store.min_y.(row) <- y -. hh;
      store.max_y.(row) <- y +. hh

  (* Public *)

  let pos world entity =
    let ( let* ) = Option.bind in
    let* store_packed = Luma__ecs.World.get_resource world Rb_store.R.type_id in
    let* store = Resource.unpack_opt (module Rb_store.R) store_packed in

    let* index_packed = Luma__ecs.World.get_resource world Rb_store.Index.R.type_id in
    let* index = Resource.unpack_opt (module Rb_store.Index.R) index_packed in

    match Rb_store.Index.row_of_entity index entity with
    | Some row -> Some (Vec2.create store.pos_x.(row) store.pos_y.(row))
    | None -> None

  let get_resource (type a) (module R : Resource.S with type t = a) world =
    let packed_opt = Luma__ecs.World.get_resource world R.type_id in
    if Option.is_none packed_opt then failwith (Printf.sprintf "%s resource is missing" R.name);
    let packed = Option.get packed_opt in

    let r = Resource.unpack_opt (module R) packed in
    if Option.is_none r then failwith (Printf.sprintf "%s resource is missing" R.name);

    Option.get r

  let move_and_collide world entity ~(velocity : Luma__math.Vec2.t) ~dt =
    let store = get_resource (module Rb_store.R) world in
    let grid = get_resource (module Grid.R) world in
    let index = get_resource (module Rb_store.Index.R) world in

    match Rb_store.Index.row_of_entity index entity with
    | None -> None
    | Some row -> (
        (* ensure grid reflects current positions *)
        Broad_phase.update_broad_phase store grid |> ignore;

        let dx = velocity.x *. dt in
        let dy = velocity.y *. dt in
        let move_length = Float.sqrt ((dx *. dx) +. (dy *. dy)) in

        if move_length <= Float.epsilon then (
          store.vel_x.(row) <- 0.;
          store.vel_y.(row) <- 0.;
          None)
        else
          let start_min_x = store.min_x.(row) in
          let start_min_y = store.min_y.(row) in
          let start_max_x = store.max_x.(row) in
          let start_max_y = store.max_y.(row) in

          let end_min_x = start_min_x +. dx in
          let end_min_y = start_min_y +. dy in
          let end_max_x = start_max_x +. dx in
          let end_max_y = start_max_y +. dy in

          let sweep_min_x = Float.min start_min_x end_min_x in
          let sweep_min_y = Float.min start_min_y end_min_y in
          let sweep_max_x = Float.max start_max_x end_max_x in
          let sweep_max_y = Float.max start_max_y end_max_y in

          let earliest_collision_fraction = ref 1. in
          let collision_normal_x = ref 0. in
          let collision_normal_y = ref 0. in
          let collided_row = ref None in

          Query.iter_aabb grid ~min_x:sweep_min_x ~min_y:sweep_min_y ~max_x:sweep_max_x
            ~max_y:sweep_max_y ~f:(fun other ->
              if other <> row && store.active.(other) = 1 && not (Rb_store.is_dynamic store other)
              then
                match Query.kinematic_toi store ~row ~other ~dx ~dy with
                | None -> ()
                | Some (collision_fraction, nx, ny) ->
                    if collision_fraction >= 0. && collision_fraction < !earliest_collision_fraction
                    then (
                      earliest_collision_fraction := collision_fraction;
                      collision_normal_x := nx;
                      collision_normal_y := ny;
                      collided_row := Some other));

          let contact_slop = 1e-4 in
          let safe_travel_fraction =
            if !earliest_collision_fraction < 1. then
              Float.max 0. (!earliest_collision_fraction -. (contact_slop /. move_length))
            else 1.
          in

          let actual_dx = dx *. safe_travel_fraction in
          let actual_dy = dy *. safe_travel_fraction in

          store.prev_pos_x.(row) <- store.pos_x.(row);
          store.prev_pos_y.(row) <- store.pos_y.(row);
          store.pos_x.(row) <- store.pos_x.(row) +. actual_dx;
          store.pos_y.(row) <- store.pos_y.(row) +. actual_dy;
          store.vel_x.(row) <- actual_dx /. dt;
          store.vel_y.(row) <- actual_dy /. dt;
          update_bounds store row;

          match !collided_row with
          | None -> None
          | Some other ->
              let collider = index.row_to_ent.(other) in
              let normal = L.Math.Vec2.create !collision_normal_x !collision_normal_y in
              let travel = Vec2.create !collision_normal_x !collision_normal_y in
              let remainder = Vec2.create (dx -. actual_dx) (dy -. actual_dy) in
              let position =
                let x = store.prev_pos_x.(row) +. (dx *. !earliest_collision_fraction) in
                let y = store.prev_pos_y.(row) +. (dy *. !earliest_collision_fraction) in
                Vec2.create x y
              in

              Some
                (Kinematic_collision2d.create ~collider ~angle:0. ~collider_velocity:Vec2.zero
                   ~depth:0. ~normal ~position ~remainder ~travel))

  let move_and_slide ?(max_iterations = 4) world entity velocity dt =
    let open Luma__math.Vec2 in
    let remaining_x = ref (velocity.x *. dt) in
    let remaining_y = ref (velocity.y *. dt) in
    let remaining_dt = ref dt in
    let iter = ref 0 in
    let epsilon = 1e-4 in
    let collided = ref false in

    while !iter < max_iterations && !remaining_dt > 0. do
      let rem_len = Float.sqrt ((!remaining_x *. !remaining_x) +. (!remaining_y *. !remaining_y)) in
      if rem_len <= epsilon then iter := max_iterations
      else
        let vel_step =
          L.Math.Vec2.create (!remaining_x /. !remaining_dt) (!remaining_y /. !remaining_dt)
        in
        match move_and_collide world entity ~velocity:vel_step ~dt:!remaining_dt with
        | None -> iter := max_iterations
        | Some col ->
            collided := true;

            let normal = Kinematic_collision2d.normal col in
            let remainder = Kinematic_collision2d.remainder col in

            (* Slide the remainder along the collision plane *)
            let dot_rest = (remainder.x *. normal.x) +. (remainder.y *. normal.y) in
            let slide_x = remainder.x -. (dot_rest *. normal.x) in
            let slide_y = remainder.y -. (dot_rest *. normal.y) in

            remaining_x := slide_x;
            remaining_y := slide_y;

            let slide_len = Float.sqrt ((slide_x *. slide_x) +. (slide_y *. slide_y)) in
            remaining_dt :=
              if rem_len <= Float.epsilon then 0. else !remaining_dt *. (slide_len /. rem_len);

            iter := !iter + 1
    done;
    !collided
end
