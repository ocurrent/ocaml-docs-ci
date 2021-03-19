module StringMap = Map.Make (String)

type t = {
  packages : OpamPackage.Version.Set.t OpamPackage.Name.Map.t;
  universes : Package.Set.t StringMap.t;
}

let pp_name f str =
  Fmt.pf f "%s"
    (Astring.String.map (function x when Astring.Char.Ascii.is_alphanum x -> x | _ -> '_') str)

let pp_link f = Fmt.pf f "{!childpage:%a}" pp_name

let v (compilations : (Package.t * bool) list) =
  let universes : Package.Set.t StringMap.t ref = ref StringMap.empty in
  let packages : OpamPackage.Version.Set.t OpamPackage.Name.Map.t ref =
    ref OpamPackage.Name.Map.empty
  in

  List.iter
    (fun (package, is_blessed) ->
      if is_blessed then
        let opam = Package.opam package in
        let name, version = (OpamPackage.name opam, OpamPackage.version opam) in
        packages :=
          OpamPackage.Name.Map.update name
            (OpamPackage.Version.Set.add version)
            OpamPackage.Version.Set.empty !packages
      else
        let digest = package |> Package.universe |> Package.Universe.hash in
        universes :=
          StringMap.(
            add digest
              (Package.Set.add package
                 (try find digest !universes with Not_found -> Package.Set.empty))
              !universes))
    compilations;
  { universes = !universes; packages = !packages }

let universes f t =
  let open Fmt in
  pf f
    {|{0 Universes}
  These universes are for those packages that are compiled against an alternative set of dependencies 
  than those in the 'packages' hierarchy.
  
  %a
  |}
    (list ~sep:(any "\n") pp_link)
    (t.universes |> StringMap.bindings |> List.map fst)

let universe ~t f uid =
  let open Fmt in
  let packages = StringMap.find uid t.universes in
  let universe = packages |> Package.Set.choose |> Package.universe in
  let pp_universe_deps f universe =
    let packages = Package.Universe.deps universe in
    pf f "%a" (list ~sep:(any "\n") Package.pp) packages
  in
  let pp_universe_packages f packages =
    let pp_package_link f package =
      pf f "%a" pp_link (package |> Package.opam |> OpamPackage.to_string)
    in
    pf f "%a" (list ~sep:(any "\n") pp_package_link) packages
  in
  pf f
    {|{0 Universe %s}
  {1 Contents}
  The following packages form this dependency universe:
  %a

  {1 Packages}
  This dependency universe has been used to compile the following packages:
  %a
  |}
    uid pp_universe_deps universe pp_universe_packages
    (packages |> Package.Set.elements)

let packages f t =
  let open Fmt in
  let interpose_alphabet f packages =
    let packages = List.map OpamPackage.Name.to_string packages in
    let alpha_heading f name = pf f "{2 %c}" (Astring.Char.Ascii.uppercase name.[0]) in
    let rec inner f ps =
      match ps with
      | a :: b :: rest ->
          if Astring.Char.Ascii.uppercase a.[0] <> Astring.Char.Ascii.uppercase b.[0] then
            pf f "%a\n%a\n%a" pp_link a alpha_heading b inner (b :: rest)
          else pf f "%a\n%a" pp_link a inner (b :: rest)
      | [ a ] -> pf f "%a" pp_link a
      | [] -> ()
    in
    let first = List.hd packages in
    pf f "%a\n%a" alpha_heading first inner packages
  in
  pf f {|{0 Packages}
  
  %a
  |} interpose_alphabet (t.packages |> OpamPackage.Name.Map.keys)

let package ~t f name =
  let open Fmt in
  let package_versions = OpamPackage.Name.Map.find name t.packages in
  let pp_version_link f version = pf f "%a" pp_link (OpamPackage.Version.to_string version) in
  pf f {|{0 Package '%s'}
  {1 Versions}
  %a
  |} (OpamPackage.Name.to_string name)
    (list ~sep:(any "\n") pp_version_link)
    (package_versions |> OpamPackage.Version.Set.elements)
