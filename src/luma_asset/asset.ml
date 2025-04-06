type base = ..

module type S = sig
  type t

  val type_id : Luma__id.Id.Asset_type.t
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

  let type_id = Luma__id.Id.Asset_type.next ()
  let of_base = function T t -> t | _ -> failwith ""
  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

(* TODO: stop copy/pasting this stuff man *)
type error = [ `Not_found of Luma__id.Id.Asset_type.t | `Type_mismatch of Luma__id.Id.Asset_type.t ]
type packed = Packed : (module S with type t = 'a) * 'a -> packed

let pack : type a. (module S with type t = a) -> a -> packed =
 fun component value -> Packed (component, value)

let unpack : type a. (module S with type t = a) -> packed -> (a, error) result =
 fun (module M) (Packed ((module M'), value)) ->
  if M.type_id = M'.type_id then
    match M.of_base_opt (M'.to_base value) with
    | Some v -> Ok v
    | None -> Error (`Type_mismatch M.type_id)
  else
    Error (`Type_mismatch M.type_id)

let type_id : packed -> Luma__id.Id.Asset_type.t = function Packed ((module R), _) -> R.type_id
