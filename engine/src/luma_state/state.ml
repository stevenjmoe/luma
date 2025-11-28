type base = ..

let log = Luma__core.Log.sub_log "state"

module Key : sig
  type 'a t = Luma__id.Id.State.t
  type (_, _) eq = Eq : ('a, 'a) eq

  val make : unit -> 'a t
  val witness : 'a t -> 'b t -> ('a, 'b) eq option
end = struct
  type 'a t = Luma__id.Id.State.t
  type (_, _) eq = Eq : ('a, 'a) eq

  let make () = Luma__id.Id.State.next ()

  (*TODO: try to find another solution. Maybe Type_equal. *)
  (* Obj.magic should be safe here because of the type_id check but I'm not completely satisfied with this solution. *)
  let witness k1 k2 = if Luma__id.Id.State.eq k1 k2 then Some (Obj.magic Eq) else None
end

module type STATE = sig
  type t

  val key : t Key.t
  val eq : t -> t -> bool
  val of_base : base -> t
  val to_base : t -> base
end

module Make (S : sig
  type inner
end) : STATE with type t = S.inner = struct
  include S

  type t = inner
  type base += T of t

  let key = Key.make ()
  let eq = ( = )
  let to_base t = T t

  let of_base = function
    | T t -> t
    | _ ->
        Luma__core.Error.unpacked_unexpected_base_type_exn (Luma__id.Id.State.to_int key)
          "Unexpected value wrapped in 'T' constructor"

  let of_base_opt = function T t -> Some t | _ -> None
end

type state = State : (module STATE with type t = 'a) * 'a -> state

let pack : type a. (module STATE with type t = a) -> a -> state =
 fun state state_value -> State (state, state_value)

let unpack : type a. (module STATE with type t = a) -> state -> (a, Luma__core.Error.error) result =
 fun (module S) (State ((module S'), value)) ->
  if not @@ Luma__id.Id.State.eq S.key S'.key then
    Error
      (Luma__core.Error.unpacked_type_mismatch (Luma__id.Id.State.to_int S.key)
         (Luma__id.Id.State.to_int S'.key) "")
  else Ok (S.of_base (S'.to_base value))

let cast : type a b. (a, b) Key.eq -> a -> b = function Key.Eq -> fun x -> x

let eq_state : type a b. state -> state -> bool =
 fun a b ->
  match (a, b) with
  | State ((module A), s1), State ((module B), s2) -> (
      match Key.witness A.key B.key with None -> false | Some proof -> A.eq s1 (cast proof s2))

type transition_result =
  | NoChange
  | Transitioned of {
      from : state;
      to_ : state;
    }

type state_resource = {
  previous : state option;
  current : state option;
  next : state option;
  last_result : transition_result;
}

module State_res = struct
  type t = state_resource

  let create s = { current = Some s; next = None; previous = None; last_result = NoChange }
  let previous s = s.previous
  let current s = s.current
  let next s = s.next
  let last_result s = s.last_result

  module R = Luma__resource.Resource.Make (struct
    type inner = t

    let name = "state_res"
  end)
end

let just_entered (type s) (module S : STATE with type t = s) expected_state res =
  match res.last_result with
  | Transitioned { to_ } ->
      let expected = State ((module S), expected_state) in
      eq_state to_ expected
  | _ -> false

let just_exited (type s) (module S : STATE with type t = s) expected_state res =
  match res.last_result with
  | Transitioned { from } ->
      let expected = State ((module S), expected_state) in
      eq_state from expected
  | _ -> false

let queue_state (type s) (module S : STATE with type t = s) (v : s) (world : Luma__ecs.World.t) =
  let open Luma__ecs in
  let pending = State ((module S), v) in
  match World.get_resource world State_res.R.type_id with
  | Some packed -> (
      match Luma__resource.Resource.unpack_opt (module State_res.R) packed with
      | Some sr ->
          (* TODO: perform some checks of the existing resource. eg. Don't overwrite next if it's some *)
          let next = State_res.{ sr with next = Some pending } in
          let packed = Luma__resource.Resource.pack (module State_res.R) next in
          World.set_resource State_res.R.type_id packed world
      | None ->
          log.warn (fun l -> l "queue_state: Failed to unpack state resource.");
          world)
  | None ->
      let packed =
        State_res.{ next = Some pending; current = None; previous = None; last_result = NoChange }
        |> Luma__resource.Resource.pack (module State_res.R)
      in
      World.set_resource State_res.R.type_id packed world

let is (type a) (module S : STATE with type t = a) (v : a) (st : state) : bool =
  eq_state st (State ((module S), v))

let transition_system () =
  let open Luma__ecs in
  let open Luma__resource in
  Luma__ecs.System.make ~components:End "transition_system" (fun w _ e ->
      let ( >>= ) = Option.bind in
      match
        World.get_resource w State_res.R.type_id >>= fun s ->
        Resource.unpack_opt (module State_res.R) s
      with
      | Some { current = Some curr; next = Some next_; _ } ->
          if eq_state curr next_ then w
          else
            let s =
              State_res.
                {
                  next = None;
                  previous = Some curr;
                  current = Some next_;
                  last_result = Transitioned { from = curr; to_ = next_ };
                }
            in
            let new_packed = Resource.pack (module State_res.R) s in
            World.set_resource State_res.R.type_id new_packed w
      | Some ({ next = None; last_result = Transitioned _ } as r) ->
          (* Clear last_result one frame after a transition, if no new transition is queued. *)
          let cleared = { r with last_result = NoChange } in
          let packed = Resource.pack (module State_res.R) cleared in
          World.set_resource State_res.R.type_id packed w
      | _ -> w)
