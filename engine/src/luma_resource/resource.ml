type base = ..

module type S = sig
  type t

  val type_id : Luma__id.Id.Resource.t
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

  let type_id = Luma__id.Id.Resource.next ()
  let name = B.name
  let pp fmt _ = Fmt.pf fmt "<%s #%d>" name (Luma__id.Id.Resource.to_int type_id)

  let of_base = function
    | T t -> t
    | _ ->
        Luma__core.Error.unpacked_unexpected_base_type_exn
          (Luma__id.Id.Resource.to_int type_id)
          "Unexpected value wrapped in 'T' constructor"

  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

type packed = Packed : (module S with type t = 'a) * 'a -> packed

let pack : type a. (module S with type t = a) -> a -> packed =
 fun component value -> Packed (component, value)

let unpack : type a. (module S with type t = a) -> packed -> (a, Luma__core.Error.error) result =
 fun (module M) (Packed ((module M'), value)) ->
  let open Luma__id.Id.Resource in
  if not @@ eq M.type_id M'.type_id then
    Error
      (Luma__core.Error.unpacked_type_mismatch (to_int M.type_id) (to_int M'.type_id)
         "Resource type mismatch while unpacking")
  else
    match M.of_base_opt (M'.to_base value) with
    | Some v -> Ok v
    | None ->
        Error
          (Luma__core.Error.unpacked_unexpected_base_type (to_int M.type_id)
             "Invalid resource base type conversion while unpacking")

let unpack_opt : type a. (module S with type t = a) -> packed -> a option =
 fun (module M) p -> match unpack (module M) p with Ok v -> Some v | Error _ -> None

let type_id : packed -> Luma__id.Id.Resource.t = function Packed ((module R), _) -> R.type_id
let name : packed -> string = function Packed ((module R), _) -> R.name
let pp_packed fmt (Packed ((module R), value)) = Fmt.pf fmt "%a" R.pp value
let show packed = Luma__core.Print.show_of_pp pp_packed packed
