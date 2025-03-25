module C = Luma__tracked_module.Tracked_module.Make (Luma__id.Id.Resource)

type error = Luma__tracked_module.Tracked_module.error

module type S = Luma__tracked_module.Tracked_module.S

module Make (B : sig
  type inner
end) =
struct
  module R = C (B)
  include R
end

include Luma__tracked_module.Tracked_module.Packed

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
