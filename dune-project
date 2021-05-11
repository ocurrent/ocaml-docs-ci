(lang dune 2.8)

(name ocaml-docs-ci)

(generate_opam_files true)

; Edit these to give your project's URL, authors and maintainers:

(source
 (github ocurrent/ocaml-docs-ci))

(authors "lucas@tarides.com")

(maintainers "lucas@tarides.com")

(package
 (name docs-ci)
 (synopsis "An OCurrent pipeline")
 (description "A longer description")
 (depends
  (current
   (>= 0.3))
  current_web
  current_git
  opam-0install
  capnp-rpc-unix
  fmt
  lwt
  cmdliner
  git-unix
  dockerfile
  graphql-cohttp
  graphql-lwt
  (ocaml
   (>= 4.08.0))))

(package
 (name docs-ci-web)
 (synopsis "A frontend for the docs CI")
 (description "")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  dream
  git-unix
  tyxml-jsx
  lambdasoup
  reason
  omd
  graphql_ppx))
