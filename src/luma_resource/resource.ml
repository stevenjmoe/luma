(* TODO: better error reporting. *)
type base = ..
type error = [ `Not_found of Luma__id.Id.id | `Type_mismatch of Luma__id.Id.id ]

let error_to_string = function
  | `Not_found id -> Printf.sprintf "Error: No resource found with ID %d." id
  | `Type_mismatch id ->
      Printf.sprintf
        "Error: Type mismatch for resource with ID %d. The provided value is incompatible with the \
         expected type."
        id

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

let id : packed -> Luma__id.Id.id = function Packed ((module R), _) -> R.id

module Query = struct
  type _ term = Resource : (module S with type t = 'a) -> 'a term
  type _ t = Res : ('a term * 'b t) -> ('a * 'b) t | End : unit t

  let ( & ) term rest = Res (term, rest)

  let evaluate : type a. a t -> (Luma__id.Id.Resource.t, packed) Hashtbl.t -> (a, error) result =
   fun query store ->
    let rec fetch : type a. a t -> (Luma__id.Id.Resource.t, packed) Hashtbl.t -> (a, error) result =
     fun query store ->
      match query with
      | End -> Ok ()
      | Res (Resource (module R), rest) -> (
          match Hashtbl.find_opt store R.id with
          | Some packed -> (
              match unpack (module R) packed with
              | Ok result -> (
                  match fetch rest store with Ok rest -> Ok (result, rest) | Error e -> Error e)
              | Error e -> Error e)
          | None -> Error (`Not_found R.id))
    in
    fetch query store
end
