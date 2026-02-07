open Luma_physics
open Luma__math
open Luma__id

let ( -: ) n f = Alcotest.test_case n `Quick f
let e = Float.epsilon

let mk_body f =
  (* positions scale with f so we can verify SoA slots directly *)
  Rigid_body.create_box Dynamic (Vec2.create (10. *. f) (10. *. f)) (Vec2.create 20. 20.) ~mass:20.

let fill_store ?(n = 100) store shape_store index =
  for i = 0 to n - 1 do
    let entity = Id.Entity.of_int i in
    let f = float_of_int (i + 1) in
    let rb = mk_body f in
    let row = Rb_store.add store shape_store rb in
    Rb_store.Index.on_add index ~entity ~row
  done
