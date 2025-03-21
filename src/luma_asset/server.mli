type t

val create : unit -> t

module R : Luma__resource.Resource.S with type t = t
