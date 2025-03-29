type base = ..

module type S = sig
  type t

  val id : Luma__id.Id.Asset.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make (B : sig
  type inner
end) : S with type t = B.inner = struct
  include B

  type t = inner
  type base += T of t

  let id = Luma__id.Id.Asset.next ()
  let of_base = function T t -> t | _ -> failwith ""
  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

(* TODO: stop copy/pasting this stuff man *)
type error = [ `Not_found of Luma__id.Id.id | `Type_mismatch of Luma__id.Id.id ]
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
