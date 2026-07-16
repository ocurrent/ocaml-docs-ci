type rejection =
  | UserConstraint of OpamFormula.atom
  | Unavailable

type t = {
  env : string -> OpamVariable.variable_contents option;
  packages : Day11_opam.Git_packages.t;
  pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
  constraints : OpamFormula.version_constraint OpamTypes.name_map;
  test : OpamPackage.Name.Set.t;
  prefer_oldest : bool;
  doc : bool;
  post : bool;
  examined_packages : OpamPackage.Name.Set.t ref;
}

let create ?(prefer_oldest = false) ?(test = OpamPackage.Name.Set.empty)
    ?(pins = OpamPackage.Name.Map.empty) ?(doc = false) ?(post = true)
    ~constraints ~env ~packages () =
  { env; packages; pins; constraints; test; prefer_oldest; doc; post;
    examined_packages = ref OpamPackage.Name.Set.empty }

let user_restrictions t name =
  OpamPackage.Name.Map.find_opt name t.constraints

let dev = OpamPackage.Version.of_string "dev"

let env t pkg v =
  if List.mem v OpamPackageVar.predefined_depends_variables then None
  else
    match OpamVariable.Full.to_string v with
    | "version" ->
        Some (OpamTypes.S
          (OpamPackage.Version.to_string (OpamPackage.version pkg)))
    | x -> t.env x

(** Read the [x-extra-doc-deps] extension field of a package's opam
    file. These are pkg names listed under

    {[ x-extra-doc-deps: [ "odoc-driver" "sherlodoc" "odig" ] ]}

    The intent of the field is "pull these in when generating my
    full doc-driver-rendered docs" — semantically equivalent to
    declaring each one in [depends:] guarded by [{with-doc & post}].
    {!augment_depends} below realises that equivalence by rewriting
    the opam file's [depends:] before it is handed to opam-0install
    (see {!candidates}), so the standard opam pipeline (solver +
    post-solve dep computation) handles them transitively without
    any other code path needing to consult the extension field
    directly. *)
let get_extra_doc_deps opamfile =
  let open OpamParserTypes.FullPos in
  let extensions = OpamFile.OPAM.extensions opamfile in
  match OpamStd.String.Map.find_opt "x-extra-doc-deps" extensions with
  | None -> OpamPackage.Name.Set.empty
  | Some value ->
    let extract_name item =
      match item.pelem with
      | String name -> Some name
      | Option (inner, _) ->
        (match inner.pelem with
         | String name -> Some name
         | _ -> None)
      | _ -> None
    in
    let extract_names acc v =
      match v.pelem with
      | List { pelem = items; _ } ->
        List.fold_left (fun acc item ->
          match extract_name item with
          | Some name ->
            OpamPackage.Name.Set.add
              (OpamPackage.Name.of_string name) acc
          | None -> acc
        ) acc items
      | _ -> acc
    in
    extract_names OpamPackage.Name.Set.empty value

(** Rewrite [opam]'s [depends:] with synthetic [{with-doc & post}]
    atoms for every entry in [x-extra-doc-deps]. The result is what
    the package would have looked like if those entries had been
    declared in [depends:] under that filter — making them
    standards-compliant opam, evaluated transitively by the solver
    and by post-solve dep computation, instead of only being
    consulted as roots for the target package.

    This must rewrite the opam file itself (in {!candidates}) rather
    than augment formulas passing through {!filter_deps}: opam-0install
    routes both [depends:] and [conflicts:] through [filter_deps], so
    augmenting there also appends the extras to the conflicts formula —
    an unconstrained conflict atom excludes every version of the extra,
    making any package with [x-extra-doc-deps] (and no pre-existing
    [conflicts:] escape branch) unsolvable. *)
let augment_depends opam =
  let extras = get_extra_doc_deps opam in
  if OpamPackage.Name.Set.is_empty extras then opam
  else
    let with_doc =
      OpamTypes.FIdent ([], OpamVariable.of_string "with-doc", None) in
    let post =
      OpamTypes.FIdent ([], OpamVariable.of_string "post", None) in
    let with_doc_and_post = OpamTypes.FAnd (with_doc, post) in
    let mk_atom n : OpamTypes.filtered_formula =
      OpamFormula.Atom
        (n, OpamFormula.Atom (OpamTypes.Filter with_doc_and_post))
    in
    let extras_formula =
      OpamPackage.Name.Set.elements extras
      |> List.map mk_atom
      |> OpamFormula.ands
    in
    OpamFile.OPAM.with_depends
      (OpamFormula.And (OpamFile.OPAM.depends opam, extras_formula))
      opam

(** Rewrite OxCaml patch-or-guard disjunctions to put the patches
    side first. opam-0install processes a disjunction's left
    branch first and rarely backtracks across multiple
    disjunctions when a later constraint forces the other branch.
    With [.guard] lex-max and the formula's natural ordering
    putting it on the left ([oxcaml-X | oxcaml-X-patches]), the
    solver commits to [.guard] — which then conflicts with the
    actual upstream package the target wanted. Swapping to
    [oxcaml-X-patches | oxcaml-X] makes the satisfiable branch
    the first try, the solver commits there, and the chain
    propagates [-patches.enabled] throughout. No-op on package
    names not matching the [oxcaml-X / oxcaml-X-patches] shape. *)
let is_oxcaml_guard_pair a b =
  let a_s = OpamPackage.Name.to_string a in
  let b_s = OpamPackage.Name.to_string b in
  String.length a_s > 7 && String.sub a_s 0 7 = "oxcaml-"
  && not (Astring.String.is_suffix ~affix:"-patches" a_s)
  && b_s = a_s ^ "-patches"

let rec swap_guard_pairs : OpamTypes.formula -> OpamTypes.formula = function
  | OpamFormula.Empty -> OpamFormula.Empty
  | OpamFormula.Atom _ as a -> a
  | OpamFormula.Block f -> OpamFormula.Block (swap_guard_pairs f)
  | OpamFormula.And (l, r) ->
    OpamFormula.And (swap_guard_pairs l, swap_guard_pairs r)
  | OpamFormula.Or (l, r) ->
    let l' = swap_guard_pairs l and r' = swap_guard_pairs r in
    (match l', r' with
     | OpamFormula.Atom (a, _), OpamFormula.Atom (b, _)
       when is_oxcaml_guard_pair a b ->
       OpamFormula.Or (r', l')
     | _ -> OpamFormula.Or (l', r'))

(** Drop dependency atoms that are [odoc] guarded by [{with-doc}] (the
    near-universal opam convention "odoc is needed to build my docs").
    odoc is the doc TOOL — docs-ci mounts it from the prebuilt
    per-compiler tool layer, never from a package's own solution — so it
    should not be a solved dependency at all. Removing it at the formula
    level (before filtering) means odoc is never pulled into a package's
    universe, which in turn means odoc's own [x-extra-doc-deps]
    (odoc-driver / sherlodoc / odig → eio, js_of_ocaml, tyxml, …) never
    fire: the whole doc-toolchain closure stops leaking into every
    package's doc-deps. odoc kept when it's a *real* (unguarded) dep —
    e.g. odoc-driver genuinely depends on odoc — so only the with-doc
    convention is stripped. *)
let drop_odoc_with_doc (f : OpamTypes.filtered_formula) : OpamTypes.filtered_formula =
  let odoc = OpamPackage.Name.of_string "odoc" in
  let mentions_with_doc cond =
    OpamFormula.fold_left (fun acc foc ->
      acc || (match foc with
        | OpamTypes.Filter filt ->
          List.exists (fun v ->
            OpamVariable.Full.to_string v = "with-doc")
            (OpamFilter.variables filt)
        | OpamTypes.Constraint _ -> false))
      false cond
  in
  OpamFormula.map (fun (name, cond) ->
    if OpamPackage.Name.equal name odoc && mentions_with_doc cond
    then OpamFormula.Empty
    else OpamFormula.Atom (name, cond))
    f

let filter_deps t pkg f =
  let dev =
    OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
  let test =
    OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
  f
  |> drop_odoc_with_doc
  |> OpamFilter.partial_filter_formula (env t pkg)
  |> OpamFilter.filter_deps ~build:true ~post:t.post ~test
       ~doc:t.doc ~dev ~dev_setup:false ~default:false
  |> swap_guard_pairs

let version_compare t (v1, v1_avoid, _) (v2, v2_avoid, _) =
  match (v1_avoid, v2_avoid) with
  | true, true | false, false ->
      if t.prefer_oldest then
        OpamPackage.Version.compare v1 v2
      else
        OpamPackage.Version.compare v2 v1
  | true, false -> 1
  | false, true -> -1

let candidates t name =
  t.examined_packages :=
    OpamPackage.Name.Set.add name !(t.examined_packages);
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (version, opam) ->
      let pkg = OpamPackage.create name version in
      let available = OpamFile.OPAM.available opam in
      (match OpamFilter.eval ~default:(B false) (env t pkg) available with
       | B true -> [ (version, Ok (augment_depends opam)) ]
       | _ -> [ (version, Error Unavailable) ])
  | None ->
      let versions = Day11_opam.Git_packages.get_versions t.packages name in
      let user_constraints = user_restrictions t name in
      OpamPackage.Version.Map.bindings versions
      |> List.filter_map (fun (v, opam) ->
             let pkg = OpamPackage.create name v in
             let avoid =
               OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam in
             let available = OpamFile.OPAM.available opam in
             match OpamFilter.eval_to_bool ~default:false
                     (env t pkg) available with
             | true -> Some (v, avoid, opam)
             | false -> None)
      |> (fun l ->
           if List.for_all (fun (_, avoid, _) -> avoid) l then [] else l)
      |> List.sort (version_compare t)
      |> List.map (fun (v, _, opam) ->
             match user_constraints with
             | Some test when
                 not (OpamFormula.check_version_formula
                        (OpamFormula.Atom test) v) ->
                 (v, Error (UserConstraint (name, Some test)))
             | _ -> (v, Ok (augment_depends opam)))

let pp_rejection f = function
  | UserConstraint x ->
      Fmt.pf f "Rejected by user-specified constraint %s"
        (OpamFormula.string_of_atom x)
  | Unavailable ->
      Fmt.string f "Availability condition not satisfied"

let examined_packages t = !(t.examined_packages)

let with_doc_post ~doc ~post t = { t with doc; post }
