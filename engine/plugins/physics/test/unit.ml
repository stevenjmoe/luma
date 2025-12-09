open Alcotest
open Luma_physics
open Luma__math
open Utils

let () = Alcotest.run "Physics" [ Storage_tests.tests; Broad_phase_tests.tests ]
