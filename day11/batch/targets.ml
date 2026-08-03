let small_universe =
  [
    "astring";
    "fmt";
    "fpath";
    "rresult";
    "bos";
    "logs";
    "cmdliner";
    "ptime";
    "uutf";
    "mtime";
    "yojson";
    "eio";
    "lwt";
    "ppxlib";
    "odoc";
    "odoc-parser";
    "re";
    "cstruct";
    "bigstringaf";
  ]

let find_latest_versions git_packages =
  let all_names = Day11_opam.Git_packages.all_names git_packages in
  let compiler_names = Day11_opam_layer.Opamh.compiler_packages in
  let all_names =
    List.filter (fun name -> not (List.mem name compiler_names)) all_names
  in
  List.filter_map
    (fun name ->
      let versions = Day11_opam.Git_packages.get_versions git_packages name in
      let non_avoided =
        OpamPackage.Version.Map.filter
          (fun _v opam ->
            not (OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam))
          versions
      in
      let versions =
        if OpamPackage.Version.Map.is_empty non_avoided then versions
        else non_avoided
      in
      match OpamPackage.Version.Map.max_binding_opt versions with
      | Some (v, _) -> Some (OpamPackage.create name v)
      | None -> None)
    all_names

let find_all_versions git_packages =
  let all_names = Day11_opam.Git_packages.all_names git_packages in
  let compiler_names = Day11_opam_layer.Opamh.compiler_packages in
  let all_names =
    List.filter (fun name -> not (List.mem name compiler_names)) all_names
  in
  List.concat_map
    (fun name ->
      let versions = Day11_opam.Git_packages.get_versions git_packages name in
      OpamPackage.Version.Map.fold
        (fun v _opam acc -> OpamPackage.create name v :: acc)
        versions []
      |> List.rev)
    all_names

let load_package_list filename =
  let json = Yojson.Safe.from_file filename in
  let open Yojson.Safe.Util in
  let parse_list l =
    List.filter_map
      (fun j -> try Some (OpamPackage.of_string (to_string j)) with _ -> None)
      l
  in
  match json with
  | `List l -> parse_list l
  | `Assoc _ -> (
      match json |> member "packages" with `List l -> parse_list l | _ -> [])
  | _ -> []

let pick_latest_version git_packages name =
  let n = OpamPackage.Name.of_string name in
  let versions = Day11_opam.Git_packages.get_versions git_packages n in
  let non_avoided =
    OpamPackage.Version.Map.filter
      (fun _v opam -> not (OpamFile.OPAM.has_flag Pkgflag_AvoidVersion opam))
      versions
  in
  let versions =
    if OpamPackage.Version.Map.is_empty non_avoided then versions
    else non_avoided
  in
  let candidates = OpamPackage.Version.Map.bindings versions |> List.rev in
  List.map (fun (v, _) -> OpamPackage.create n v) candidates

let resolve ?(small = false) ?(all_versions = false) git_packages target =
  match target with
  | None when small && all_versions ->
      Printf.printf "Finding all versions of small universe...\n%!";
      let compiler_names = Day11_opam_layer.Opamh.compiler_packages in
      let names =
        List.filter_map
          (fun name ->
            let n = OpamPackage.Name.of_string name in
            if List.mem n compiler_names then None else Some n)
          small_universe
      in
      List.concat_map
        (fun name ->
          let versions =
            Day11_opam.Git_packages.get_versions git_packages name
          in
          OpamPackage.Version.Map.fold
            (fun v _opam acc -> OpamPackage.create name v :: acc)
            versions []
          |> List.rev)
        names
  | None when small ->
      Printf.printf "Using small universe (%d packages)...\n%!"
        (List.length small_universe);
      List.filter_map
        (fun name ->
          match pick_latest_version git_packages name with
          | v :: _ -> Some v
          | [] -> None)
        small_universe
  | None when all_versions ->
      Printf.printf "Finding all package versions...\n%!";
      find_all_versions git_packages
  | None ->
      Printf.printf "Finding all latest package versions...\n%!";
      find_latest_versions git_packages
  | Some t when String.length t > 0 && t.[0] = '@' ->
      let filename = String.sub t 1 (String.length t - 1) in
      Printf.printf "Loading package list from %s...\n%!" filename;
      load_package_list filename
  | Some t ->
      Printf.printf "Target: %s\n%!" t;
      [ OpamPackage.of_string t ]
