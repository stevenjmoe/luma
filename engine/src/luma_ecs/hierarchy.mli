open Luma__id

module ChildOf : sig
  type t

  module C : Component.S

  val create : Id.Entity.t -> t
  val parent : t -> Id.Entity.t
end

module Children : sig
  type t

  module C : Component.S
end
