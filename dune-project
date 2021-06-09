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
  conf-libev
  (ocaml
   (>= 4.08.0))))

(package
 (name docs-ci-tools)
 (synopsis "Tools")
 (description "")
 (depends cmdliner git-unix httpaf-lwt-unix))

(package
 (name docs-ci-web)
 (synopsis "A frontend for the docs CI")
 (description "")
 (depends
  (ocaml
   (>= 4.08.0))
  dune
  (dream
   (>= 1.0.0~alpha))
  (git-unix
   (>= 3.3.3))
  (tyxml-jsx
   (>= 4.4.0))
  (lambdasoup
   (>= 0.7.2))
  (reason
   (>= 3.7.0))
  (omd
   (>= 2.0.0~alpha))
  (graphql_ppx
   (>= 1.0.1))))
