(* Repro: how does x-extra-doc-deps augmentation interact with the
   conflicts formula in opam-0install?

   Cases:
   - tgta: ONE extra, no conflicts field   (the eio_linux.1.3 shape)
   - tgtb: THREE extras, no conflicts      (the odoc.3.2.1 shape)
   - tgtc: ONE extra + a real conflicts    (escape-branch hypothesis)
   - tgtd: TWO extras, no conflicts        (the eio.1.3 shape minus seq) *)

let opam_of_string s =
  OpamFile.OPAM.read_from_string s

let mk_pins entries =
  List.fold_left (fun acc (name, version, body) ->
    OpamPackage.Name.Map.add
      (OpamPackage.Name.of_string name)
      (OpamPackage.Version.of_string version, opam_of_string body)
      acc)
    OpamPackage.Name.Map.empty entries

let pins = mk_pins [
  ("ocaml-base-compiler", "5.0.0", {|
opam-version: "2.0"
|});
  ("extraa", "1", {|opam-version: "2.0"|});
  ("e1", "1", {|opam-version: "2.0"|});
  ("e2", "1", {|opam-version: "2.0"|});
  ("e3", "1", {|opam-version: "2.0"|});
  ("seqq", "1", {|opam-version: "2.0"|});
  ("tgta", "1", {|
opam-version: "2.0"
x-extra-doc-deps: [ "extraa" {= version} ]
|});
  ("tgtb", "1", {|
opam-version: "2.0"
x-extra-doc-deps: [ "e1" {= version} "e2" "e3" ]
|});
  ("tgtc", "1", {|
opam-version: "2.0"
conflicts: [ "seqq" {< "0.3"} ]
x-extra-doc-deps: [ "extraa" {= version} ]
|});
  ("tgtd", "1", {|
opam-version: "2.0"
x-extra-doc-deps: [ "extraa" {= version} "e2" ]
|});
]

let env = Day11_opam.Opam_env.std_env
  ~arch:"x86_64" ~os:"linux" ~os_distribution:"debian"
  ~os_family:"debian" ~os_version:"12" ()

let () =
  List.iter (fun t ->
    let target = OpamPackage.of_string t in
    Printf.printf "── solve %s (doc=true) ──\n" t;
    (match Day11_solver.Solve.solve
       ~packages:Day11_opam.Git_packages.empty
       ~env ~pins ~doc:true target with
     | Ok r ->
       let sel = OpamPackage.Map.keys r.build_deps
                 |> List.map OpamPackage.to_string in
       Printf.printf "  OK: %s\n" (String.concat " " sel);
       let docsel = OpamPackage.Map.keys r.doc_deps
                    |> List.map OpamPackage.to_string in
       Printf.printf "  doc universe: %s\n" (String.concat " " docsel)
     | Error (msg, _) ->
       Printf.printf "  FAIL:\n%s\n"
         (String.concat "\n"
            (List.map (fun l -> "    " ^ l)
               (String.split_on_char '\n' msg))));
    print_newline ()
  ) [ "tgta.1"; "tgtb.1"; "tgtc.1"; "tgtd.1" ]
