type base = ..

module type S = sig
  type t

  val id : Luma__id.Id.Component.t
  val name : string
  val pp : t Fmt.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

module Make (B : sig
  type inner

  val name : string
end) : S with type t = B.inner = struct
  include B

  type t = inner
  type base += T of t

  let id = Luma__id.Id.Component.next ()
  let name = B.name
  let pp fmt _ = Fmt.pf fmt "<%s #%d>" name (Luma__id.Id.Component.to_int id)
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

let id : packed -> Luma__id.Id.Component.t = function Packed ((module C), _) -> C.id
