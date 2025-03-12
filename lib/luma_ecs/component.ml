type base = ..

module type S = sig
  type t

  val id : Id.Component.t
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

  let id = Id.Component.next ()
  let of_base = function T t -> t | _ -> failwith ""
  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

type packed = Packed : (module S with type t = 'a) * 'a -> packed

let pack : type a. (module S with type t = a) -> a -> packed =
 fun component value -> Packed (component, value)

let unpack : type a. (module S with type t = a) -> packed -> a option =
 fun (module C) (Packed ((module C'), value)) ->
  if C.id = C'.id then
    C.of_base_opt (C'.to_base value)
  else
    None

let id : packed -> Id.Component.t = function Packed ((module C), _) -> C.id
