type load_error = Unsupported_extension of string | Loader_error of string
type t

val create : Assets.t -> t
val register_loader : t -> Loader.t -> unit
val find_loader : t -> string -> Loader.t option
val load : t -> string -> ('a Assets.handle, load_error) result

module R : Luma__resource.Resource.S with type t = t
