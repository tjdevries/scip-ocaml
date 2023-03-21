(lang dune 3.7)

(name scip_ocaml)

(generate_opam_files true)

(source
 (github tjdevries/scip-ocaml))

(authors "TJ DeVries")

(maintainers "TJ DeVries")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name scip_ocaml)
 (synopsis "Emit SCIP for OCaml")
 (description "A SCIP Indexer for OCaml (powered by Merlin)")
 (depends
   ocaml ocaml-compiler-libs dune
   stdune
   utop
   ocaml-protoc
   (merlin-lib (>= 4.8)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
