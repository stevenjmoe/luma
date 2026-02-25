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

  module Kinematic_state : sig
    type t

    val on_floor : t -> bool
    val on_wall : t -> bool
    val on_ceiling : t -> bool
    val floor_normal : t -> Luma__math__Vec2.t

    module C : Component.S with type t = t
  end
end

module Make (L : Luma.S) : S = struct
  module Rigid_body = Rigid_body
  module Colliders = Colliders
  module Plugin = Plugin.Make (L)

  let plugin = Plugin.plugin

  let update_bounds store shape_store row =
    let open Rb_store in
    match shape_kind store row with
    | 0 ->
        let radius = Shape_store.circle_radius shape_store (shape_handle store row) in
        let x = pos_x store row in
        let y = pos_y store row in

        set_min_x store row (x -. radius);
        set_max_x store row (x +. radius);
        set_min_y store row (y -. radius);
        set_max_y store row (y +. radius)
    | 1 ->
        let aabb_handle = shape_handle store row in
        let hw = Shape_store.aabb_half_width shape_store aabb_handle in
        let hh = Shape_store.aabb_half_height shape_store aabb_handle in

        let x = pos_x store row in
        let y = pos_y store row in

        set_min_x store row (x -. hw);
        set_max_x store row (x +. hw);
        set_min_y store row (y -. hh);
        set_max_y store row (y +. hh)
    | 2 -> failwith "TODO"
    | other -> invalid_arg (Printf.sprintf "Physics.update_bounds %d" other)

  (* Public *)

  let pos world entity =
    let ( let* ) = Option.bind in
    let* store_packed = Luma__ecs.World.get_resource world Rb_store.R.type_id in
    let* store = Resource.unpack_opt (module Rb_store.R) store_packed in

    let* index_packed = Luma__ecs.World.get_resource world Rb_store.Index.R.type_id in
    let* index = Resource.unpack_opt (module Rb_store.Index.R) index_packed in

    match Rb_store.Index.row_of_entity index entity with
    | Some row -> Some (Vec2.create (Rb_store.pos_x store row) (Rb_store.pos_y store row))
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
    let shape_store = get_resource (module Shape_store.R) world in
    let grid = get_resource (module Grid.R) world in
    let index = get_resource (module Rb_store.Index.R) world in

    match Rb_store.Index.row_of_entity index entity with
    | None -> None
    | Some row -> (
        (* ensure grid reflects current positions *)
        Broad_phase.update_broad_phase store grid |> ignore;

        let delta_x = velocity.x *. dt in
        let delta_y = velocity.y *. dt in
        let move_length = Float.sqrt ((delta_x *. delta_x) +. (delta_y *. delta_y)) in

        if move_length <= Float.epsilon then (
          Rb_store.set_vel_x store row 0.;
          Rb_store.set_vel_y store row 0.;
          None)
        else
          let start_min_x = Rb_store.min_x store row in
          let start_min_y = Rb_store.min_y store row in
          let start_max_x = Rb_store.max_x store row in
          let start_max_y = Rb_store.max_y store row in

          let end_min_x = start_min_x +. delta_x in
          let end_min_y = start_min_y +. delta_y in
          let end_max_x = start_max_x +. delta_x in
          let end_max_y = start_max_y +. delta_y in

          let sweep_min_x = Float.min start_min_x end_min_x in
          let sweep_min_y = Float.min start_min_y end_min_y in
          let sweep_max_x = Float.max start_max_x end_max_x in
          let sweep_max_y = Float.max start_max_y end_max_y in

          let earliest_collision_fraction = ref 1. in
          let collision_normal_x = ref 0. in
          let collision_normal_y = ref 0. in
          let collided_row = ref None in

          Grid.iter_aabb grid ~min_x:sweep_min_x ~min_y:sweep_min_y ~max_x:sweep_max_x
            ~max_y:sweep_max_y ~f:(fun other ->
              if
                other <> row
                && Rb_store.is_active store other
                && not (Rb_store.is_dynamic store other)
              then
                match Query.kinematic_toi store shape_store ~row ~other ~delta_x ~delta_y with
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

          let actual_dx = delta_x *. safe_travel_fraction in
          let actual_dy = delta_y *. safe_travel_fraction in

          Rb_store.set_prev_pos_x store row (Rb_store.pos_x store row);
          Rb_store.set_prev_pos_y store row (Rb_store.pos_y store row);
          Rb_store.set_pos_x store row (Rb_store.pos_x store row +. actual_dx);
          Rb_store.set_pos_y store row (Rb_store.pos_y store row +. actual_dy);
          Rb_store.set_vel_x store row (actual_dx /. dt);
          Rb_store.set_vel_y store row (actual_dy /. dt);
          update_bounds store shape_store row;

          match !collided_row with
          | None -> None
          | Some other ->
              let collider = Rb_store.Index.entity_at_row index other in
              let normal = L.Math.Vec2.create !collision_normal_x !collision_normal_y in
              let travel = Vec2.create !collision_normal_x !collision_normal_y in
              let remainder = Vec2.create (delta_x -. actual_dx) (delta_y -. actual_dy) in
              let position =
                let x =
                  Rb_store.prev_pos_x store row +. (delta_x *. !earliest_collision_fraction)
                in
                let y =
                  Rb_store.prev_pos_y store row +. (delta_y *. !earliest_collision_fraction)
                in
                Vec2.create x y
              in

              Some (Kinematic_collision2d.create ~collider ~normal ~position ~remainder ~travel))

  let move_and_slide ?(max_iterations = 4) world entity velocity dt =
    let open Luma__math.Vec2 in
    let remaining_x = ref (velocity.x *. dt) in
    let remaining_y = ref (velocity.y *. dt) in
    let remaining_dt = ref dt in
    let iter = ref 0 in
    let epsilon = 1e-4 in
    let collided = ref false in

    let k_state =
      match L.Ecs.World.get_component world (module Kinematic_state.C) entity with
      | Some s -> s
      | None ->
          L.Log.error (fun l ->
              l
                "move_and_slide: missing Kinematic_state. Expected Kinematic_state to exist before \
                 any call to move_and_slide. Ensure movement logic does not run before PreUpdate");
          failwith "move_and_slide: missing Kinematic_state"
    in
    Kinematic_state.clear k_state;

    (* Classification constants *)
    let up = Vec2.up in
    let floor_min_dot = 0.7 in
    let ceiling_min_dot = 0.7 in
    let wall_max_abs_dot = 0.2 in

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

            (* Update kinematic state flags *)
            let d = (normal.x *. up.x) +. (normal.y *. up.y) in
            if d >= floor_min_dot then Kinematic_state.set_is_on_floor k_state true;
            if d <= -.ceiling_min_dot then Kinematic_state.set_is_on_ceiling k_state true;
            if Float.abs d <= wall_max_abs_dot then Kinematic_state.set_is_on_wall k_state true;

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

  module Kinematic_state = struct
    type t = Kinematic_state.t

    let on_floor = Kinematic_state.on_floor
    let on_wall = Kinematic_state.on_wall
    let on_ceiling = Kinematic_state.on_ceiling
    let floor_normal = Kinematic_state.floor_normal

    module C = Kinematic_state.C
  end
end
