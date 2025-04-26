(* TODO: better error reporting. *)
type base = ..

type error =
  [ `Not_found of Luma__id.Id.Resource.t
  | `Type_mismatch of Luma__id.Id.Resource.t
  ]

let error_to_string = function
  | `Not_found id ->
      Printf.sprintf "Error: No resource found with ID %d." (Luma__id.Id.Resource.to_int id)
  | `Type_mismatch id ->
      Printf.sprintf
        "Error: Type mismatch for resource with ID %d. The provided value is incompatible with the \
         expected type."
        (Luma__id.Id.Resource.to_int id)

module type S = sig
  type t

  val id : Luma__id.Id.Resource.t
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

  let id = Luma__id.Id.Resource.next ()
  let of_base = function T t -> t | _ -> failwith ""
  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

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

let id : packed -> Luma__id.Id.Resource.t = function Packed ((module R), _) -> R.id

module Query = struct end
