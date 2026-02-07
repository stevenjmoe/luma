open Utils
open Luma_physics
open Luma__math
open Alcotest
open Luma__id

let eoi i = Id.Entity.of_int i
let ioe e = Id.Entity.to_int e

let test_create_and_basic_ops () =
  let gravity = Vec2.create 0. (-9.81) in
  let max_step_dt = 0.0016 in

  let bounds = Bounded2d.Aabb2d.of_min_max (Vec2.create 50. 50.) (Vec2.create 50. 50.) in
  let _cfg = Config.create ~gravity ~max_step_dt ~bounds:(Some bounds) in
  let index = Rb_store.Index.create ~initial:99 in
  let store = Rb_store.create ~initial:99 () in
  let shape_store = Shape_store.create ~initial:99 () in

  fill_store ~n:100 store shape_store index;

  check int "len = 100 after adds" 100 (Rb_store.len store);
  check (float e) "pos_x[0] = 10." 10. (Rb_store.pos_x store 0);
  check (float e) "pos_y[0] = 10." 10. (Rb_store.pos_y store 0);
  check (float e) "pos_x[1] = 20." 20. (Rb_store.pos_x store 1);
  check (float e) "pos_x[57] = 580." 580. (Rb_store.pos_x store 57);

  (* remove tail: no swap expected *)
  Rb_store.remove store 99;
  Rb_store.Index.on_remove index ~row:99;

  check int "len = 99 after tail remove" 99 (Rb_store.len store);
  check (option int) "row_of_entity 99 -> None" None
    (Rb_store.Index.row_of_entity index (Id.Entity.of_int 99));

  (* still present *)
  check (option int) "row_of_entity 50 -> Some 50" (Some 50)
    (Rb_store.Index.row_of_entity index (Id.Entity.of_int 50))

let test_capacity_growth_and_data_stability () =
  (* initial < total inserts to force growth *)
  let initial = 4 in
  let total = 10 in
  let index = Rb_store.Index.create ~initial in
  let store = Rb_store.create ~initial () in
  let shape_store = Shape_store.create ~initial () in

  fill_store ~n:total store shape_store index;

  check int "len after growth" total (Rb_store.len store);

  (* spot-check a few after growth to ensure arrays preserved values *)
  let shape0 = Rb_store.shape_handle store 0 in
  check (float e) "pos_x[0] = 10." 10. (Rb_store.pos_x store 0);
  check (float e) "max_x[0] = 20." 20. (Rb_store.max_x store 0);
  check (float e) "max_y[0] = 20." 20. (Rb_store.max_y store 0);
  check (float e) "min_x[0] = 0." 0. (Rb_store.min_x store 0);
  check (float e) "min_y[0] = 0." 0. (Rb_store.min_y store 0);
  check (float e) "box_hw[0] = 10." 10. (Shape_store.aabb_half_width shape_store shape0);
  check (float e) "box_hh[0] = 10." 10. (Shape_store.aabb_half_height shape_store shape0);
  check (float e) "radius[0] = 0." 0. (Shape_store.circle_radius shape_store shape0);

  let shape3 = Rb_store.shape_handle store 3 in
  check (float e) "pos_x[3] = 40." 40. (Rb_store.pos_x store 3);
  check (float e) "max_x[3] = 50." 50. (Rb_store.max_x store 3);
  check (float e) "max_y[3] = 50." 50. (Rb_store.max_y store 3);
  check (float e) "min_x[3] = 30." 30. (Rb_store.min_x store 3);
  check (float e) "min_y[3] = 30." 30. (Rb_store.min_y store 3);
  check (float e) "box_hw[3] = 10." 10. (Shape_store.aabb_half_width shape_store shape3);
  check (float e) "box_hh[3] = 10." 10. (Shape_store.aabb_half_height shape_store shape3);
  check (float e) "radius[3] = 0." 0. (Shape_store.circle_radius shape_store shape3);

  check (float e) "pos_x[9] = 100." 100. (Rb_store.pos_x store 9);
  ()

let test_index_roundtrip_after_many_ops () =
  let index = Rb_store.Index.create ~initial:16 in
  let store = Rb_store.create ~initial:16 () in
  let shape_store = Shape_store.create ~initial:16 () in

  (* add 16 *)
  fill_store ~n:16 store shape_store index;

  (* remove a few scattered rows (tail, head, middle) *)
  let removals = [ 15; 0; 7 ] in
  List.iter
    (fun row ->
      Rb_store.remove store row;
      Rb_store.Index.on_remove index ~row)
    removals;

  check int "len after scattered removals" 13 (Rb_store.len store);

  (* Swap-pop consequences: 15 truncated; 14 -> row 0; 13 -> row 7; removed entities cleared *)
  check (option int) "entity 15 removed" None
    (Rb_store.Index.row_of_entity index (Id.Entity.of_int 15));
  check (option int) "entity 0 removed" None
    (Rb_store.Index.row_of_entity index (Id.Entity.of_int 0));
  check (option int) "entity 7 removed" None
    (Rb_store.Index.row_of_entity index (Id.Entity.of_int 7));
  check int "entity 14 now at row 0" 0
    (Option.get (Rb_store.Index.row_of_entity index (Id.Entity.of_int 14)));
  check int "entity 13 now at row 7" 7
    (Option.get (Rb_store.Index.row_of_entity index (Id.Entity.of_int 13)));
  check (float e) "pos_x[0] == 14's value (150.)" 150. (Rb_store.pos_x store 0);
  check (float e) "pos_x[7] == 13's value (140.)" 140. (Rb_store.pos_x store 7);

  (* re-add 3 new bodies; entities continue from 16,17,18 *)
  for ent = 16 to 18 do
    let rb = mk_body (float_of_int (ent + 1)) in
    let row = Rb_store.add store shape_store rb in
    Rb_store.Index.on_add index ~entity:(Id.Entity.of_int ent) ~row
  done;

  check int "len after re-adds" 16 (Rb_store.len store);

  (* lookups for some stable entities exist *)
  List.iter
    (fun ent ->
      let ent = eoi ent in
      match Rb_store.Index.row_of_entity index ent with
      | None -> fail (Printf.sprintf "expected Some _ for entity %d" (ioe ent))
      | Some _ -> ())
    [ 1; 2; 3; 8; 12 ];

  (* new entities are present *)
  List.iter
    (fun ent ->
      let ent = eoi ent in
      match Rb_store.Index.row_of_entity index ent with
      | None -> fail (Printf.sprintf "missing row for new entity %d" (ioe ent))
      | Some r ->
          let expected = float_of_int (ioe ent + 1) *. 10. in
          check (float e) (Printf.sprintf "pos_x[%d] written" r) expected (Rb_store.pos_x store r))
    [ 16; 17; 18 ]

let tests =
  ( "storage",
    [
      "create & basic ops" -: test_create_and_basic_ops;
      "capacity growth stable" -: test_capacity_growth_and_data_stability;
      "index roundtrip" -: test_index_roundtrip_after_many_ops;
    ] )
