(lang dune 3.12)

(name life)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "James Meyers")
(maintainers "James Meyers")

(license MIT)

(package
 (name life)
 (synopsis "AoC 20234 Solutions")
 (description "My solutions for the 2024 Advent of Code challenge")
 (depends ocaml dune alcotest yaml)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
