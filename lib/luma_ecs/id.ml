module type S = sig
  type t = int

  val next : unit -> t
  val compare : t -> t -> int
  val of_int : int -> t
  val to_int : t -> int
end

module Make () : S = struct
  type t = int

  let current = ref 0

  let next () =
    incr current;
    !current

  let compare = compare
  let of_int i = i
  let to_int i = i
end

module Entity = Make ()
module EntitySet = Set.Make (Entity)
module Component = Make ()
module ComponentSet = Set.Make (Component)
module Resource = Make ()
module ResourceSet = Set.Make (Resource)
