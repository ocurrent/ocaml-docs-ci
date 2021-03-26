(lang dune 2.0)
(name docs-ci)



(generate_opam_files true)

; Edit these to give your project's URL, authors and maintainers:
(source (github TheLortex/docs-ci))
(authors "lucas@tarides.com")
(maintainers "lucas@tarides.com")

(package
 (name docs-ci)
 (synopsis "An OCurrent pipeline")
 (description "A longer description")
 (depends
  (current (>= 0.3))
  current_web
  current_git
  opam-0install
  capnp-rpc-unix
  fmt
  lwt
  cmdliner
  (ocaml (>= 4.08.0))))
