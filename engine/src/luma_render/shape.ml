open Luma__math

module type S = sig
  type colour

  type style =
    | Fill
    | Lines of float

  type space =
    | World
    | Screen

  type t =
    | Rect of {
        rect : Rect.t;
        colour : colour;
        style : style;
        space : space;
        z : int;
        layer : Int64.t;
      }
    | Circle of {
        radius : float;
        center : Vec2.t;
        colour : colour;
        style : style;
        z : int;
        layer : Int64.t;
      }
    | Capsule of {
        capsule : Primitives.Capsule2d.t;
        colour : colour;
        style : style;
        z : int;
        layer : Int64.t;
      }

  val rect : ?layer:int64 -> Rect.t -> colour -> style -> space -> int -> t
  val circle : ?layer:int64 -> radius:float -> center:Vec2.t -> colour -> style -> int -> t

  module C : Luma__ecs.Component.S with type t = t
end

module Make (D : Luma__driver.Driver.S) : S with type colour = D.colour = struct
  type colour = D.colour

  type style =
    | Fill
    | Lines of float

  type space =
    | World
    | Screen

  type t =
    | Rect of {
        rect : Rect.t;
        colour : colour;
        style : style;
        space : space;
        z : int;
        layer : Int64.t;
      }
    | Circle of {
        radius : float;
        center : Vec2.t;
        colour : colour;
        style : style;
        z : int;
        layer : Int64.t;
      }
    | Capsule of {
        capsule : Primitives.Capsule2d.t;
        colour : colour;
        style : style;
        z : int;
        layer : Int64.t;
      }

  let rect ?(layer = 1L) rect colour style space z = Rect { rect; colour; style; space; z; layer }

  let circle ?(layer = 1L) ~radius ~center colour style z =
    Circle { radius; center; colour; style; z; layer }

  module C = Luma__ecs.Component.Make (struct
    type inner = t

    let name = "render_shape"
  end)
end
