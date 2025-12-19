open Utils
open Luma_physics
open Luma__math
open Alcotest

(* helpers *)

let aabb_of_index store i = Rb_store.bounding_box store i

let naive_pairs store =
  let n = Rb_store.(store.len) in
  let acc = ref [] in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      let a = aabb_of_index store i in
      let b = aabb_of_index store j in
      if Bounded2d.Aabb2d.intersects_aabb a b then acc := (i, j) :: !acc
    done
  done;
  List.sort_uniq compare !acc

let canonical_pair (i, j) = if i <= j then (i, j) else (j, i)

let broad_phase_pairs bp =
  let a, b = Broad_phase.pairs_view bp in
  let len = min (Dynarray.length a) (Dynarray.length b) in
  let acc = ref [] in
  for i = 0 to len - 1 do
    let i0 = Dynarray.get a i in
    let i1 = Dynarray.get b i in
    acc := canonical_pair (i0, i1) :: !acc
  done;
  !acc |> List.sort_uniq compare

let setup () =
  let gravity = Vec2.create 0. (-9.81) in
  let max_step_dt = 0.0016 in
  let bounds = Bounded2d.Aabb2d.of_min_max (Vec2.create 0. 0.) (Vec2.create 1040. 1040.) in
  let cfg = Config.create ~gravity ~max_step_dt ~bounds:(Some bounds) () in
  let index = Rb_store.Index.create ~initial:100 in
  let store = Rb_store.create ~initial:100 () in
  let bp = Broad_phase.create ~max_bodies:1000 () in
  let grid = Grid.create cfg.bounds 32. in

  fill_store ~n:100 store index;

  (cfg, index, store, grid, bp)

(* tests *)

let test_broad_phase_has_no_false_negatives () =
  let _cfg, _index, store, grid, bp = setup () in
  Broad_phase.step store grid bp;

  let expected = naive_pairs store in
  let actual = broad_phase_pairs bp in

  let missing = List.filter (fun p -> not (List.mem p actual)) expected in
  check bool "all intersecting pairs present" true (missing = []);
  ()

let test_fill_store_bounding_box_sanity () =
  let _cfg, _index, store, _grid, _bp = setup () in
  let b = Rb_store.bounding_box store 0 in
  let b2 = Rb_store.bounding_box store 2 in
  let b3 = Rb_store.bounding_box store 5 in

  check (float epsilon_float) "bounding_box store 2 max.y" 20. (Bounded2d.Aabb2d.max b).y;
  check (float epsilon_float) "bounding_box store 2 max.y" 40. (Bounded2d.Aabb2d.max b2).y;
  check (float epsilon_float) "bounding_box store 2 max.y" 70. (Bounded2d.Aabb2d.max b3).y;
  ()

let tests =
  ( "broad_phase",
    [
      "" -: test_broad_phase_has_no_false_negatives;
      "fill_store_sanity" -: test_fill_store_bounding_box_sanity;
    ] )
