type base = ..

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
  let of_base = function T t -> t | _ -> failwith ""
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

let rec eq_state : type a b. state -> state -> bool =
 fun a b ->
  match (a, b) with
  | State ((module A), s1), State ((module B), s2) -> (
      match Key.witness A.key B.key with None -> false | Some proof -> A.eq s1 (cast proof s2))

module State_res = struct
  type t = state

  module R = Luma__resource.Resource.Make (struct
    type inner = t

    let name = "state_res"
  end)
end

module Next_state_res = struct
  type t = state option

  module R = Luma__resource.Resource.Make (struct
    type inner = t

    let name = "next_state_res"
  end)
end

let queue_state (type s) (module S : STATE with type t = s) (v : s) (world : Luma__ecs.World.t) =
  let pending = State ((module S), v) in
  let packed = Luma__resource.Resource.pack (module Next_state_res.R) (Some pending) in
  Luma__ecs.World.set_resource Next_state_res.R.type_id packed world

let is (type a) (module S : STATE with type t = a) (v : a) (st : state) : bool =
  eq_state st (State ((module S), v))

let transition_system () =
  Luma__ecs.System.make ~components:End "transition_system" (fun w e ->
      let ( >>= ) = Option.bind in
      match
        Luma__ecs.World.get_resource w Next_state_res.R.type_id >>= fun r ->
        Luma__resource.Resource.unpack_opt (module Next_state_res.R) r >>= fun next_state_res ->
        next_state_res
      with
      | None -> w
      | Some next_state -> (
          match
            Luma__ecs.World.get_resource w State_res.R.type_id >>= fun s ->
            Luma__resource.Resource.unpack_opt (module State_res.R) s
          with
          | Some state ->
              if eq_state state next_state then w
              else
                let new_packed = Luma__resource.Resource.pack (module State_res.R) next_state in
                let old_packed = Luma__resource.Resource.pack (module Next_state_res.R) None in
                Luma__ecs.World.set_resource State_res.R.type_id new_packed w
                |> Luma__ecs.World.set_resource Next_state_res.R.type_id old_packed
                |> ignore;
                w
          | None -> w))
