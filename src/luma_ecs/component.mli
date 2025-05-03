type base

module type S = sig
  type t

  val id : Luma__id.Id.Component.t
  (** Gets the unique identifier for the component type. *)

  val name : string
  val pp : t Fmt.t
  val of_base : base -> t
  val of_base_opt : base -> t option
  val to_base : t -> base
end

(** Creates a component module for a given type [B.inner]. The resulting module implements the [S]
    signature, allowing [B.inner] to be added as a component to the game world. Example:
    {[
      module Velocity = struct
        type t = Math.Vec2.t

        module C = Component.Make (struct
          type inner = t
        end)
      end
    ]}
    There is a preprocessor available to reduce boilerplate somewhat. The same module can be defined
    like this:
    {[
      module Velocity = [%component: Math.Vec2.t]
    ]}
    Or, if some helper functions are required:
    {[
      [%%component
      module Velocity = struct
        type t = Math.Vec2.t

        let create x y = Math.Vec2.create x y
        let zero () = Math.Vec2.zero
        let x t = Math.Vec2.x t
        let y t = Math.Vec2.y t
        let set_x t x = Math.Vec2.set_x t x
        let set_y t y = Math.Vec2.set_y t y
      end]
    ]}
    These will both generate the functor, and the second example will include the helper functions.
    Note that the preprocessor requires a type with the name t. *)
module Make (B : sig
  type inner

  val name : string
end) : S with type t = B.inner

(** A packed component, which combines a component module and its value. This allows components of
    different types to be stored together. *)
type packed = Packed : (module S with type t = 'a) * 'a -> packed

val pack : 'a. (module S with type t = 'a) -> 'a -> packed
(** Packs a component value with its module into a [packed] value. This allows you to store and
    access the component's ID and data together. Example:
    {[
      module MyComponent = Component.Make (struct
        type inner = int
      end)

      let packed = Component.pack (module MyComponent) 42
    ]} *)

val unpack : 'a. (module S with type t = 'a) -> packed -> 'a option
(** Attempts to extract a component value from a [packed] value. Returns [Some value] if successful,
    otherwise [None]. Example:
    {[
      match Component.unpack (module MyComponent) packed with
      | Some value -> (* Use the value *)
      | None -> (* Handle mismatch *)
    ]} *)

val id : packed -> Luma__id.Id.Component.t
(** Gets the [Component] id from the packed component. *)

val pp_packed : Format.formatter -> packed -> unit
(** Print a packed component using its own pretty-printer. *)

val show : packed -> string
(** [show packed] takes a packed component and returns a formatted string based on the component's
    pretty printer. *)
