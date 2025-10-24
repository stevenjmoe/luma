type t

val make : ?uuid:Uuidm.t option -> string -> t
val id : t -> Luma__id.Id.Entity.t
val uuid : t -> Uuidm.t
val name : t -> string
