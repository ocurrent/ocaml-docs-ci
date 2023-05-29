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
  (opam-format (>= 2.1.4))
  (current (>= 0.6))
   current_web
   current_git
   current_github
   current_ocluster
  (cstruct (< 6.1.0))
  (opam-0install (>= 0.4.3))
  (capnp-rpc-unix (>= 1.2))
  (capnp-rpc-lwt (>= 1.2.3))
  (lwt (>= 5.6.1))
  (fmt (>= 0.8.9))
  (cmdliner (>= 1.1.0))
  (dockerfile (>= 8.2.1))
  (astring (>= 0.8.5))
  (digestif (>= 1.1.4))
  (logs (>= 0.7.0))
  (opam-file-format (>= 2.1.6))
  (ppx_deriving (>= 5.2.1))
  (ppx_deriving_yojson (>= 3.7.0))
  git-unix
  (dockerfile (>= 8.2.1))
  conf-libev
  (ocaml-version (>= 3.6.1))
  (obuilder-spec (>= 0.5.1))
  (docker_hub (>= 0.1.1))
  (ocolor (>= 1.3.0))
  (memtrace (>= 0.1.1)) ; required for memory profiling
  (alcotest (and (>= 1.7.0) :with-test))
  (alcotest-lwt (and (>= 1.7.0) :with-test))
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
