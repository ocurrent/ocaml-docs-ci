type name = string

let name_of_string x = x

type mld = Mld
type cu = CU
type 'a kind = Mld : mld kind | CU : cu kind

type 'a t = {
  file : Fpath.t;
  (* What file is compiled: compile/packages.mld | prep/.../.../uri/uri.cmti *)
  target : Fpath.t option;
  (* Target compilation folder: compile/ *)
  name : name;
  (* Compilation unit name *)
  kind : 'a kind;
}

let odoc_reference : type a. a t -> string =
 fun t ->
  match t.kind with
  | Mld -> "page-\"" ^ t.name ^ "\""
  | CU -> "module-" ^ t.name

let odoc_filename : type a. a t -> string =
 fun t -> match t.kind with Mld -> "page-" ^ t.name | CU -> t.name

let include_path t =
  match t.target with
  | None -> Fpath.split_base t.file |> fst
  | Some target -> target

let odoc_file t =
  match t.target with
  | None -> Fpath.((split_base t.file |> fst) / (odoc_filename t ^ ".odoc"))
  | Some target -> Fpath.(target / (odoc_filename t ^ ".odoc"))

let odocl_file t =
  match t.target with
  | None -> Fpath.((split_base t.file |> fst) / (odoc_filename t ^ ".odocl"))
  | Some target -> Fpath.(target / (odoc_filename t ^ ".odocl"))

let child_pp f dep =
  let Mld = dep.kind in
  Fmt.pf f "--child %s" (odoc_reference dep)

type ('a, 'b) command = {
  children : mld t list;
  parent : 'a t option;
  target : 'b t;
  skip : bool;
}

let v ?(children = []) ?parent target skip = { children; parent; target; skip }

let compile_command ?(odoc = "odoc") { children; parent; target; _ } =
  let parent_pp f parent =
    Fmt.pf f "--parent %s -I %a" (odoc_reference parent) Fpath.pp
      (include_path parent)
  in
  Fmt.str "%s compile --warn-error %a %a %a %a" odoc Fpath.pp target.file
    Fmt.(option (any "-o " ++ Fpath.pp))
    target.target
    Fmt.(option parent_pp)
    parent
    Fmt.(list ~sep:(any " ") child_pp)
    children

let pp_compile_command ?odoc () f ({ skip; _ } as t) =
  let command = compile_command ?odoc t |> String.escaped in
  if skip then Fmt.pf f "echo skipping"
  else Fmt.pf f "(echo \"%s\" && %s) || exit 1" command command

let pp_link_command ?(odoc = "odoc") () f { children; target; skip; _ } =
  let include_paths =
    List.rev_map include_path children |> List.sort_uniq Fpath.compare
  in
  let include_pp f path = Fmt.pf f "-I %a" Fpath.pp path in
  let command =
    Fmt.str "%s link %a %a" odoc Fpath.pp (odoc_file target)
      Fmt.(list ~sep:(any " ") include_pp)
      include_paths
    |> String.escaped
  in
  if skip then Fmt.pf f "echo skipping"
  else Fmt.pf f "(echo \"%s\" && %s) || exit 1" command command

let pp_html_command ?(odoc = "odoc") ?output () f t =
  let command =
    Fmt.str "%s html %a %a" odoc Fpath.pp (odocl_file t)
      Fmt.(option (any "-o " ++ Fpath.pp))
      output
    |> String.escaped
  in
  Fmt.pf f "(echo \"%s\" && %s) || exit 1" command command

module Gen = struct
  module StringMap = Map.Make (String)

  type 'a odoc = 'a t
  type odoc_dyn = Mld of mld t | CU of cu t

  let digest = function
    | Mld { name; target; _ } | CU { name; target; _ } ->
        name
        ^ "-"
        ^ (target |> Option.map Fpath.to_string |> Option.value ~default:"")

  type t = {
    packages : odoc_dyn OpamPackage.Version.Map.t OpamPackage.Name.Map.t;
    universes : odoc_dyn Package.Map.t StringMap.t;
  }

  let pp_link : type a. Format.formatter -> a odoc -> unit =
   fun f v ->
    match v.kind with
    | Mld -> Fmt.pf f "{!childpage:\"%s\"}" v.name
    | CU -> Fmt.pf f "{!childmodule:%s}" v.name

  let pp_link_dyn f = function
    | Mld odoc -> pp_link f odoc
    | CU odoc -> pp_link f odoc

  let v (compilations : (Package.t * bool * odoc_dyn) list) =
    let universes : odoc_dyn Package.Map.t StringMap.t ref =
      ref StringMap.empty
    in
    let packages : odoc_dyn OpamPackage.Version.Map.t OpamPackage.Name.Map.t ref
        =
      ref OpamPackage.Name.Map.empty
    in

    List.iter
      (fun (package, is_blessed, root_odoc) ->
        if is_blessed then
          let opam = Package.opam package in
          let name, version =
            (OpamPackage.name opam, OpamPackage.version opam)
          in
          packages :=
            OpamPackage.Name.Map.update name
              (OpamPackage.Version.Map.add version root_odoc)
              OpamPackage.Version.Map.empty !packages
        else
          let digest = package |> Package.universes_hash in
          universes :=
            StringMap.(
              add digest
                (Package.Map.add package root_odoc
                   (try find digest !universes
                    with Not_found -> Package.Map.empty))
                !universes))
      compilations;

    { universes = !universes; packages = !packages }

  let all_packages t = t.packages |> OpamPackage.Name.Map.keys
  let all_universes t = t.universes |> StringMap.bindings |> List.map fst

  (* Compilation unit definitions *)
  let packages_odoc =
    {
      file = Fpath.(v "compile" / "packages.mld");
      target = None;
      name = "packages";
      kind = Mld;
    }

  let package_odoc name =
    let name = OpamPackage.Name.to_string name in
    {
      file = Fpath.(v "compile" / "packages" / (name_of_string name ^ ".mld"));
      target = None;
      name = name_of_string name;
      kind = Mld;
    }

  let universes_odoc =
    {
      file = Fpath.(v "compile" / "universes.mld");
      target = None;
      name = "universes";
      kind = Mld;
    }

  let universe_odoc hash =
    {
      file = Fpath.(v "compile" / "universes" / (name_of_string hash ^ ".mld"));
      target = None;
      name = name_of_string hash;
      kind = Mld;
    }

  (* Compilation descriptions *)

  type gen_page = {
    content : string;
    odoc : mld odoc;
    compilation : (mld, mld) command;
  }

  let universes t =
    let open Fmt in
    let children =
      t.universes
      |> StringMap.bindings
      |> List.map fst
      |> List.map universe_odoc
    in
    let content =
      str
        {|{0 Universes}
    These universes are for those packages that are compiled against an alternative set of dependencies 
    than those in the 'packages' hierarchy.
    
    %a
    |}
        (list ~sep:(any "\n\n") pp_link)
        children
    in
    let odoc = universes_odoc in
    let compilation =
      {
        children =
          t.universes
          |> StringMap.bindings
          |> List.map fst
          |> List.map universe_odoc;
        parent = None;
        target = universes_odoc;
        skip = false;
      }
    in
    { content; odoc; compilation }

  let universe ~t hash =
    let open Fmt in
    let packages = StringMap.find hash t.universes in
    let universe = packages |> Package.Map.choose |> fst |> Package.universe in
    let pp_universe_deps f universe =
      let packages = Package.Universe.deps universe in
      packages
      |> List.map Package.opam
      |> List.map OpamPackage.to_string
      |> pf f "%a" (list ~sep:(any "\n\n") string)
    in
    let pp_universe_packages f packages =
      let pp_package_link f (_, odoc) = pf f "%a" pp_link_dyn odoc in
      pf f "%a" (list ~sep:(any "\n\n") pp_package_link) packages
    in
    let content =
      str
        {|{0 Universe %s}
    {1 Contents}
    The following packages form this dependency universe:
    %a

    {1 Packages}
    This dependency universe has been used to compile the following packages:
    %a
    |}
        hash pp_universe_deps universe pp_universe_packages
        (packages |> Package.Map.bindings)
    in
    let odoc = universe_odoc hash in
    let children =
      packages
      |> Package.Map.bindings
      |> List.map (function
           | _, Mld t -> t
           | _ -> failwith "Package entry page should be an mld.")
    in
    let compilation =
      { children; parent = Some universes_odoc; target = odoc; skip = false }
    in
    { content; odoc; compilation }

  let packages t =
    let open Fmt in
    let interpose_alphabet f (packages : OpamPackage.Name.t list) =
      let alpha_heading f name =
        let name = OpamPackage.Name.to_string name in
        pf f "{2 %c}" (Astring.Char.Ascii.uppercase name.[0])
      in
      let rec inner f ps =
        match ps with
        | a :: b :: rest ->
            let a_str = OpamPackage.Name.to_string a in
            let b_str = OpamPackage.Name.to_string b in
            if
              Astring.Char.Ascii.uppercase a_str.[0]
              <> Astring.Char.Ascii.uppercase b_str.[0]
            then
              pf f "%a\n\n%a\n\n%a" pp_link (package_odoc a) alpha_heading b
                inner (b :: rest)
            else pf f "%a\n\n%a" pp_link (package_odoc a) inner (b :: rest)
        | [ a ] -> pf f "%a" pp_link (package_odoc a)
        | [] -> ()
      in
      let first = List.hd packages in
      pf f "%a\n\n%a" alpha_heading first inner packages
    in
    let content =
      str {|{0 Packages}
  
  %a
  |} interpose_alphabet
        (t.packages |> OpamPackage.Name.Map.keys)
    in
    let odoc = packages_odoc in
    let compilation =
      {
        children =
          t.packages |> OpamPackage.Name.Map.keys |> List.map package_odoc;
        parent = None;
        target = odoc;
        skip = false;
      }
    in
    { content; odoc; compilation }

  let package ~t name =
    let open Fmt in
    let package_versions = OpamPackage.Name.Map.find name t.packages in
    let children =
      package_versions
      |> OpamPackage.Version.Map.values
      |> List.map (function
           | Mld t -> t
           | _ -> failwith "Package entry page should be an mld.")
    in
    let content =
      str
        {|{0 Package '%s'}
    {1 Versions}
    %a
    |}
        (OpamPackage.Name.to_string name)
        (list ~sep:(any "\n\n") pp_link)
        children
    in
    let odoc = package_odoc name in
    let compilation =
      { children; parent = Some packages_odoc; target = odoc; skip = false }
    in
    ({ content; odoc; compilation }, [])

  let pp_rule ?(odoc = "odoc") ~target ~output f (_, t) =
    let Mld = t.target.kind in
    let file_mld = t.target.file in
    let file_odoc = odoc_file t.target in
    let file_odocl = odocl_file t.target in
    Fmt.pf f
      "\n\
       %s-link:: %a\n\n\
       %s-compile:: %a\n\n\
       %a: %a\n\
       \t@@%a\n\n\
       %a: %a %a\n\
       \t@@%a\n\
       \t@@%a\n"
      target Fpath.pp file_odocl target Fpath.pp file_odoc Fpath.pp file_odoc
      Fpath.pp file_mld
      (pp_compile_command ~odoc ())
      t Fpath.pp file_odocl Fpath.pp file_odoc
      Fmt.(list ~sep:(any " ") (using odoc_file Fpath.pp))
      t.children (pp_link_command ~odoc ()) t
      (pp_html_command ~odoc ~output ())
      t.target

  let pp_makefile ?(odoc = "odoc") ~output f t =
    let extract_gen_page { compilation; content; _ } = (content, compilation) in
    let packages = packages t in
    let universes = universes t in
    let packages_indexes =
      t.packages
      |> OpamPackage.Name.Map.keys
      |> List.map (fun x -> package ~t x |> fst)
    in
    let universes_indexes =
      t.universes
      |> StringMap.bindings
      |> List.map (fun (k, _) -> universe ~t k)
    in
    let pages = packages_indexes @ universes_indexes in
    let compilation_units = List.map extract_gen_page pages in
    Fmt.pf f
      ".PHONY: roots-compile roots-link pages-compile pages-link\n\n\
       %%.mld: %%.mld.new\n\
       \t@cmp --silent $< $@@ || (echo \"$@@ changed!\" && cp $< $@@)\n\n\
       %a\n\
       %a\n\
       %a"
      (pp_rule ~odoc ~target:"roots" ~output)
      (extract_gen_page packages)
      (pp_rule ~odoc ~target:"roots" ~output)
      (extract_gen_page universes)
      (Fmt.list ~sep:(Fmt.any "\n") (pp_rule ~odoc ~target:"pages" ~output))
      compilation_units

  let pp_gen_files_commands f t =
    let all_packages =
      t
      |> all_packages
      |> List.map (fun name ->
             let v, deps = package ~t name in
             v :: deps)
      |> List.flatten
    in
    let all_universes = t |> all_universes |> List.map (universe ~t) in
    let all_files =
      (packages t :: universes t :: all_packages) @ all_universes
    in
    let open Fmt in
    let pp_gen f { content; odoc; _ } =
      pf f "echo '%s' > %a.new" content Fpath.pp odoc.file
    in
    (list ~sep:(any "\n") pp_gen) f all_files

  let pp_commands ~pp_cmd f t =
    let { compilation = packages_cmd; _ } = packages t in
    let { compilation = universes_cmd; _ } = universes t in
    let packages_indexes =
      t.packages |> OpamPackage.Name.Map.keys |> List.map (package ~t)
    in
    let universes_indexes =
      t.universes
      |> StringMap.bindings
      |> List.map (fun (k, _) -> universe ~t k)
    in

    Fmt.pf f
      {|
        %a
        %a
        %a
        %a
        |}
      pp_cmd packages_cmd pp_cmd universes_cmd
      Fmt.(
        list ~sep:(any "\n") (fun f ({ compilation; _ }, versions_pages) ->
            pf f "%a\n%a" pp_cmd compilation
              (list ~sep:(any "\n") (fun f { compilation; _ } ->
                   pp_cmd f compilation))
              versions_pages))
      packages_indexes
      Fmt.(
        list ~sep:(any "\n") (fun f { compilation; _ } -> pp_cmd f compilation))
      universes_indexes

  let pp_compile_commands = pp_commands ~pp_cmd:(pp_compile_command ())
  let pp_link_commands = pp_commands ~pp_cmd:(pp_link_command ())
end
