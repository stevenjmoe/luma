module Resource = Luma__resource.Resource
module Id = Luma__id.Id

(* TODO:
   - Asset should be a type constructor that accepts a type with a certain signature

*)
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

module Assets = struct
  type t

  let get (module S : S) = ()

  module R = Resource.Make (struct
    type inner = t
  end)
end

module Asset_server = struct
  type t = int

  let load_asset (module S : S) path = ()

  module R = Resource.Make (struct
    type inner = t
  end)
end
