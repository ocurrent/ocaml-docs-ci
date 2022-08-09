(lang dune 2.8)

(name ocaml-docs-ci)
(formatting disabled)
(generate_opam_files true)

; Edit these to give your project's URL, authors and maintainers:

(source
 (github ocurrent/ocaml-docs-ci))

(authors "lucas@tarides.com")

(maintainers "lucas@tarides.com")

(package
 (name ocaml-docs-ci)
 (synopsis "An OCurrent pipeline")
 (description "A longer description")
 (depends
  (ocaml
   (>= 4.11.0))
  (current
   (>= 0.3))
  current_web
  current_git
  current_github
  opam-0install
  capnp-rpc-unix
  fmt
  lwt
  cmdliner
  git-unix
  dockerfile
  graphql-cohttp
  graphql-lwt
  conf-libev
  ocaml-version
  docker_hub))
