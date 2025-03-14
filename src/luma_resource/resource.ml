module Id = Luma__id.Id

type base = ..

module type S = sig
  type t

  val id : Id.Resource.t
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

  let id = Id.Resource.next ()
  let of_base = function T t -> t | _ -> failwith ""
  let of_base_opt = function T t -> Some t | _ -> None
  let to_base t = T t
end

type packed = Packed : (module S with type t = 'a) * 'a -> packed

let pack : type a. (module S with type t = a) -> a -> packed =
 fun component value -> Packed (component, value)

(*TODO: Add an exn throwing version *)
let unpack : type a. (module S with type t = a) -> packed -> a option =
 fun (module R) (Packed ((module R'), value)) ->
  if R.id = R'.id then
    R.of_base_opt (R'.to_base value)
  else
    None

let id : packed -> Id.Resource.t = function Packed ((module R), _) -> R.id

(* TODO: Error handling *)
module Query = struct
  type _ term = Resource : (module S with type t = 'a) -> 'a term
  type _ t = Res : ('a term * 'b t) -> ('a * 'b) t | End : unit t

  let ( & ) term rest = Res (term, rest)

  let evaluate : type a. a t -> (Id.Resource.t, packed) Hashtbl.t -> a =
   fun query store ->
    let rec fetch : type a. a t -> (Id.Resource.t, packed) Hashtbl.t -> a =
     fun query store ->
      match query with
      | End -> ()
      | Res (Resource (module R), rest) -> (
          match Hashtbl.find_opt store R.id with
          | Some packed -> (
              match unpack (module R) packed with
              | Some v -> (v, fetch rest store)
              | None -> failwith "TODO")
          | None -> failwith "TODO")
    in
    fetch query store
end
