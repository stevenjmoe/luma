open Containers

type 'a t

val create : ?max_size:int -> unit -> 'a t
val dense : 'a t -> 'a Vector.vector
val sparse : 'a t -> int option Vector.vector Vector.vector
val max_size : 'a t -> int
val set : 'a t -> int -> 'a -> unit
val get : 'a t -> int -> 'a option
val delete : 'a t -> int -> unit
val length : 'a t -> int
