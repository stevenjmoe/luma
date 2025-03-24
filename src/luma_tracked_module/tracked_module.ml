type base = ..

(* TODO: do this better *)
type error = [ `Not_found of Luma__id.Id.id | `Type_mismatch of Luma__id.Id.id ]

let error_to_string = function
  | `Not_found id -> Printf.sprintf "Tracked module with ID %d not found" id
  | `Type_mismatch id -> Printf.sprintf "Tracked module with ID %d does not match" id

module type S = sig
  type t

  val id : Luma__id.Id.id
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make
    (Id : Luma__id.Id.S)
    (B : sig
      type inner
    end) : S with type t = B.inner = struct
  include B

  type t = inner
  type base += T of t

  let id = Id.next ()
  let of_base = function T t -> t | _ -> failwith ""
  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

module type Packed = sig
  type packed = Packed : (module S with type t = 'a) * 'a -> packed

  val pack : (module S with type t = 'a) -> 'a -> packed
  val unpack : (module S with type t = 'a) -> packed -> ('a, error) result
  val id : packed -> int
end

module Packed : Packed = struct
  type packed = Packed : (module S with type t = 'a) * 'a -> packed

  let pack : type a. (module S with type t = a) -> a -> packed =
   fun component value -> Packed (component, value)

  let unpack : type a. (module S with type t = a) -> packed -> (a, error) result =
   fun (module M) (Packed ((module M'), value)) ->
    if M.id = M'.id then
      match M.of_base_opt (M'.to_base value) with
      | Some v -> Ok v
      | None -> Error (`Type_mismatch M.id)
    else
      Error (`Type_mismatch M.id)

  let id : packed -> Luma__id.Id.id = function Packed ((module R), _) -> R.id
end
