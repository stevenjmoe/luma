(lang dune 3.17)

(name luma)

(generate_opam_files true)

(source
 (github stevenjmoe/luma))
(authors "Steven")
(maintainers "Steven")

(package
 (name luma)
 (synopsis "Does this need to exist?")
 (description "Does this need to exist?")
 (depends 
   (alcotest :with-test) 
   containers
   dune
   fmt
   logs
   lwt
   lwt_ppx
   ocaml 
   ppxlib
   ppx_inline_test
   raylib
   raygui
   ptime
   fmt)
 (tags
  ("game engine" "ecs")))

(map_workspace_root false)
