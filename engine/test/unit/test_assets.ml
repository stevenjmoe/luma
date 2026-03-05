open Alcotest
open Luma__asset

let ( -: ) n f = Alcotest.test_case n `Quick f

module Asset_a = Asset.Make (struct
  type inner = unit
end)

module Asset_b = Asset.Make (struct
  type inner = unit
end)

let not_loaded () =
  let assets = Assets.create () in
  let handle = Assets.add_pending (module Asset_a) assets in
  let loaded = Assets.is_loaded assets handle in

  check bool "Check the asset has not been flagged as loaded" false loaded;

  ()

let loaded () =
  let assets = Assets.create () in
  let handle = Assets.add (module Asset_a) assets () in

  let loaded = Assets.is_loaded assets handle in

  check bool "Check the asset has been flagged as loaded" true loaded;
  ()

let all_loaded () =
  (* Asset_a assets*)
  let assets = Assets.create () in
  let _handle1 = Assets.add (module Asset_a) assets () in
  let _handle2 = Assets.add (module Asset_a) assets in
  let _handle3 = Assets.add (module Asset_a) assets () in
  let _handle4 = Assets.add (module Asset_a) assets () in
  let _handle5 = Assets.add (module Asset_a) assets () in

  (* Asset_b assets*)
  let _handle6 = Assets.add_pending (module Asset_b) assets in

  let all_loaded = Assets.all_loaded_by_type (module Asset_a) assets in

  check bool "Check that all Asset_a assets are loaded" true all_loaded;
  ()

let all_not_loaded () =
  (* Asset_a assets*)
  let assets = Assets.create () in
  let _handle1 = Assets.add (module Asset_a) assets () in
  let _handle2 = Assets.add (module Asset_a) assets in
  let _handle3 = Assets.add (module Asset_a) assets () in
  let _handle4 = Assets.add_pending (module Asset_a) assets in
  let _handle5 = Assets.add (module Asset_a) assets () in

  (* Asset_b assets*)
  let _handle6 = Assets.add_pending (module Asset_b) assets in

  let all_not_loaded = Assets.all_loaded_by_type (module Asset_a) assets in

  check bool "Check that all Asset_a assets are not loaded" false all_not_loaded;
  ()

let tests =
  ( "assets",
    [
      "not_loaded" -: not_loaded;
      "loaded" -: loaded;
      "all_loaded" -: all_loaded;
      "all_not_loaded" -: all_not_loaded;
    ] )
