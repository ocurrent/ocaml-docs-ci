type rejection = UserConstraint of OpamFormula.atom | Unavailable

let ( / ) = Filename.concat

let with_dir path fn =
  let ch = Unix.opendir path in
  Fun.protect ~finally:(fun () -> Unix.closedir ch) (fun () -> fn ch)

let list_dir path =
  let rec aux acc ch =
    match Unix.readdir ch with name -> aux (name :: acc) ch | exception End_of_file -> Some acc
  in
  match with_dir path (aux []) with v -> v | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None

type t = {
  env : string -> OpamVariable.variable_contents option;
  packages_dirs : string list;
  pins : (OpamPackage.Version.t * OpamFile.OPAM.t) OpamPackage.Name.Map.t;
  constraints : OpamFormula.version_constraint OpamTypes.name_map;
  (* User-provided constraints *)
  test : OpamPackage.Name.Set.t;
  prefer_oldest : bool;
}

let load t pkg =
  let { OpamPackage.name; version = _ } = pkg in
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (_, opam) -> opam
  | None -> (
      let path =
        List.find_map
          (fun package_dir ->
            let path =
              package_dir / OpamPackage.Name.to_string name / OpamPackage.to_string pkg / "opam"
            in
            if Bos.OS.Path.exists (Fpath.of_string path |> Result.get_ok) |> Result.get_ok then
              Some path
            else None)
          t.packages_dirs
      in
      match path with
      | Some opam_path -> OpamFile.OPAM.read (OpamFile.make (OpamFilename.raw opam_path))
      | None -> failwith "opam file not found." )

let get_opamfile = load

let user_restrictions t name = OpamPackage.Name.Map.find_opt name t.constraints

let dev = OpamPackage.Version.of_string "dev"

let env t pkg v =
  if List.mem v OpamPackageVar.predefined_depends_variables then None
  else
    match OpamVariable.Full.to_string v with
    | "version" -> Some (OpamTypes.S (OpamPackage.Version.to_string (OpamPackage.version pkg)))
    | x -> t.env x

let filter_deps t pkg f =
  let dev = OpamPackage.Version.compare (OpamPackage.version pkg) dev = 0 in
  let test = OpamPackage.Name.Set.mem (OpamPackage.name pkg) t.test in
  f
  |> OpamFilter.partial_filter_formula (env t pkg)
  |> OpamFilter.filter_deps ~build:true ~post:true ~test ~doc:false ~dev ~default:false

let version_compare t v1 v2 =
  if t.prefer_oldest then OpamPackage.Version.compare v1 v2 else OpamPackage.Version.compare v2 v1

let rec remove_duplicates aux = function
  | [] -> List.rev aux
  | [ a ] -> List.rev (a :: aux)
  | v1 :: v2 :: q when v1 = v2 -> remove_duplicates aux (v1 :: q)
  | h :: q -> remove_duplicates (h :: aux) q

let candidates t name =
  match OpamPackage.Name.Map.find_opt name t.pins with
  | Some (version, opam) -> [ (version, Ok opam) ]
  | None -> (
      let versions_dirs =
        List.map (fun pkgdir -> pkgdir / OpamPackage.Name.to_string name) t.packages_dirs
      in
      let versions_lists =
        List.filter_map
          (fun d -> list_dir d |> Option.map (List.map (fun x -> (d, x))))
          versions_dirs
        |> List.flatten
      in
      match versions_lists with
      | [] ->
          OpamConsole.log "opam-0install" "Package %S not found!" (OpamPackage.Name.to_string name);
          []
      | versions ->
          let user_constraints = user_restrictions t name in
          versions
          |> List.filter_map (fun (versions_dir, dir) ->
                 match OpamPackage.of_string_opt dir with
                 | Some pkg when Sys.file_exists (versions_dir / dir / "opam") ->
                     Some (OpamPackage.version pkg)
                 | _ -> None)
          |> List.stable_sort (version_compare t)
          |> remove_duplicates []
          |> List.map (fun v ->
                 match user_constraints with
                 | Some test when not (OpamFormula.check_version_formula (OpamFormula.Atom test) v)
                   ->
                     (v, Error (UserConstraint (name, Some test)))
                 | _ -> (
                     let pkg = OpamPackage.create name v in
                     let opam = load t pkg in
                     let available = OpamFile.OPAM.available opam in
                     match OpamFilter.eval ~default:(B false) (env t pkg) available with
                     | B true -> (v, Ok opam)
                     | B false -> (v, Error Unavailable)
                     | _ ->
                         OpamConsole.error "Available expression not a boolean: %s"
                           (OpamFilter.to_string available);
                         (v, Error Unavailable) )) )

let pp_rejection f = function
  | UserConstraint x ->
      Fmt.pf f "Rejected by user-specified constraint %s" (OpamFormula.string_of_atom x)
  | Unavailable -> Fmt.string f "Availability condition not satisfied"

let create ?(prefer_oldest = false) ?(test = OpamPackage.Name.Set.empty)
    ?(pins = OpamPackage.Name.Map.empty) ~constraints ~env packages_dirs =
  { env; packages_dirs; pins; constraints; test; prefer_oldest }
