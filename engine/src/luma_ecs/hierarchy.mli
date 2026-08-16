open Luma__id

module ChildOf : sig
  type t

  module C : Component.S with type t = t

  val create : Id.Entity.t -> t
  val parent : t -> Id.Entity.t
end

module Children : sig
  type t

  module C : Component.S with type t = t
end

val child_of_hooks  : Component_info.Component_hooks.t
