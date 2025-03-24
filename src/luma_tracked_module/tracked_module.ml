type base = ..

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

module Packed = struct
  type packed = Packed : (module S with type t = 'a) * 'a -> packed

  let pack : type a. (module S with type t = a) -> a -> packed =
   fun component value -> Packed (component, value)

  let unpack : type a. (module S with type t = a) -> packed -> a option =
   fun (module M) (Packed ((module M'), value)) ->
    if M.id = M'.id then
      M.of_base_opt (M'.to_base value)
    else
      None

  let id : packed -> Luma__id.Id.Resource.t = function Packed ((module R), _) -> R.id
end
