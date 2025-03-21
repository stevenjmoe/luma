type load_state = Loading | Loaded of Asset.asset | Failed of string
type t

val create : unit -> t

val insert : t -> string -> load_state -> unit
(**[insert t id asset] Inserts the asset identified by the given Id. If it already exists, it will
   be replaced. *)

val get : t -> string -> load_state option
(**  *)

val contains : t -> string -> bool
(** Checks if an asset with the given [id] exists. *)

module R : Luma__resource.Resource.S with type t = t
