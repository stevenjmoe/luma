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
   ocaml 
   dune
   (alcotest :with-test) 
   raylib
   containers)
 (tags
  ("game engine" "ecs")))
