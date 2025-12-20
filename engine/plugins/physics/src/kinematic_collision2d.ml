open Luma__math
open Luma__id.Id

(* TODO: angle, depth, collider_velocity are not currently calculated. *)

type t = {
  collider : Entity.t;
  normal : Vec2.t;
  position : Vec2.t;
  remainder : Vec2.t;
  travel : Vec2.t;
}

let create ~collider ~normal ~position ~remainder ~travel =
  { collider; normal; position; remainder; travel }

let collider k = k.collider
let normal k = k.normal
let position k = k.position
let remainder k = k.remainder
let travel k = k.travel
