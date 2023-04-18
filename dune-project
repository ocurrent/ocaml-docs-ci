(lang dune 3.6)

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
 (synopsis "OCurrent pipeline for building documentation")
 (description "OCurrent pipeline for building HTML documentation for the ocaml.org website.")
 (depends
  (ocaml
   (>= 4.14.0))
  (alcotest (and (>= 1.7.0) :with-test))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
  (current
   (>= 0.6))
  (cstruct (< 6.1.0))
  current_web
  current_git
  current_github
  current_ocluster
  opam-0install
  (capnp-rpc-unix (>= 1.2))
  (lwt (>= 5.6.1))
  (fmt (>= 0.8.9))
  (cmdliner (>= 1.1.0))
  git-unix
  dockerfile
  conf-libev
  (ocaml-version (>= 3.6.1))
  (obuilder-spec (>= 0.5.1))
  (docker_hub (>= 0.1.1))))

(package
 (name ocaml-docs-ci-client)
 (synopsis "Command-line client for ocaml-docs-ci")
 (depends
  (ocaml (>= 4.14))
  (logs (>= 0.7.0))
  (fmt (>= 0.8.9))
  current_rpc
  (capnp-rpc-unix (>= 1.2))
  dockerfile
  (timedesc (>= 0.9.0))))