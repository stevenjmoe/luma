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

  let of_base = function
    | T t -> t
    | _ ->
        Luma__core.Error.unpacked_unexpected_base_type_exn (Luma__id.Id.Component.to_int id)
          "Unexpected value wrapped in 'T' constructor"

  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

type packed = Packed : (module S with type t = 'a) * 'a -> packed

let pack : type a. (module S with type t = a) -> a -> packed =
 fun component value -> Packed (component, value)

let unpack : type a. (module S with type t = a) -> packed -> (a, Luma__core.Error.error) result =
 fun (module C) (Packed ((module C'), value)) ->
  let open Luma__id.Id.Component in
  if not @@ Luma__id.Id.Component.eq C.id C'.id then
    Error
      (Luma__core.Error.unpacked_type_mismatch (to_int C.id) (to_int C'.id)
         "Component type mismatch while unpacking")
  else
    match C.of_base_opt (C'.to_base value) with
    | Some v -> Ok v
    | None ->
        Error
          (Luma__core.Error.unpacked_unexpected_base_type (to_int C.id)
             "Invalid component base type conversion while unpacking")

let unpack_opt : type a. (module S with type t = a) -> packed -> a option =
 fun (module C) packed -> match unpack (module C) packed with Ok v -> Some v | Error _ -> None

let id : packed -> Luma__id.Id.Component.t = function Packed ((module C), _) -> C.id
let pp_packed fmt (Packed ((module C), value)) = Format.fprintf fmt "%a" C.pp value
let show packed = Luma__core.Print.show_of_pp pp_packed packed
