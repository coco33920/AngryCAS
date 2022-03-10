(lang dune 3.0)
(using menhir 2.1)
(name angrycas)

(generate_opam_files true)

(source
 (github coco33920/AngryCAS))

(authors "Charlotte THOMAS")

(license LICENSE)

(documentation "Charlotte THOMAS")

(package
 (name angrycas)
 (synopsis "A Really Angry CAS")
 (description "Ever wanted a Computer Algebra System that kicks your ass when you're doing simple algebra? No? Well this exist
 either way...")
 (depends ocaml dune ocamllex menhir))