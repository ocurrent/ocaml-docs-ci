(lang dune 1.11)
(name example)

(generate_opam_files true)

; Edit these to give your project's URL, authors and maintainers:
(source (github ocurrent/ocurrent-skeleton))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")

(package
 (name example)
 (synopsis "An OCurrent pipeline")
 (description "A longer description")
 (depends
  (current (>= 0.3))
  current_web
  current_git
  fmt
  lwt
  cmdliner
  (ocaml (>= 4.08.0))))
