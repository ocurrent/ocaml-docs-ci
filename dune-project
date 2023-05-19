(lang dune 3.6)

(name ocaml-docs-ci)
(generate_opam_files true)

(source
 (github ocurrent/ocaml-docs-ci))

(authors "lucas@tarides.com")
(maintainers "navin@tarides.com" "timmcgil@gmail.com")

(package
 (name ocaml-docs-ci)
 (synopsis "OCurrent pipeline for building documentation")
 (description "OCurrent pipeline for building HTML documentation for the ocaml.org website.")
 (depends
  (ocaml (>= 4.14.1))
  (current (>= 0.6))
   current_web
   current_git
   current_github
   current_ocluster
  (cstruct (< 6.1.0))
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
  (docker_hub (>= 0.1.1))

  (memtrace (>= 0.1.1)) ; required for memory profiling
  (alcotest (and (>= 1.7.0) :with-test))
  (alcotest-lwt (and (>= 1.7.0) :with-test))))
