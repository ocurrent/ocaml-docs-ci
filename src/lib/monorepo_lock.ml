type t = { lockfile : Opamfile.t; dev_repos_output : string list } [@@deriving yojson]

let make ~opam_file ~dev_repo_output = { lockfile = opam_file; dev_repos_output = dev_repo_output }

let marshal t = to_yojson t |> Yojson.Safe.to_string

let unmarshal s =
  match Yojson.Safe.from_string s |> of_yojson with Ok x -> x | Error e -> failwith e

type project = { name : string; dev_repo : string; repo : string; packages : string list }

let lockfile t = t.lockfile

let clean = Astring.String.trim ~drop:(function ' ' | '\t' | '"' -> true | _ -> false)

let build_project_list (packages : Opamfile.pkg list) dev_repos_output =
  let module StringMap = Map.Make (String) in
  let repo_map = ref StringMap.empty in
  List.iter
    (fun (pkg : Opamfile.pkg) ->
      match StringMap.find_opt pkg.repo !repo_map with
      | Some pkgs -> repo_map := StringMap.add pkg.repo (pkg :: pkgs) !repo_map
      | None -> repo_map := StringMap.add pkg.repo [ pkg ] !repo_map)
    packages;
  let dev_repo_map = ref StringMap.empty in
  let _ =
    List.fold_left
      (fun name (line : string) ->
        match String.split_on_char ':' line with
        | "name" :: rest -> String.concat ":" rest
        | "dev-repo" :: rest ->
            let dev_repo = String.concat ":" rest in
            dev_repo_map := StringMap.add (clean name) (clean dev_repo) !dev_repo_map;
            ""
        | _ -> "")
      "" dev_repos_output
  in
  StringMap.fold
    (fun repo (pkgs : Opamfile.pkg list) aux ->
      let packages = List.map (fun (pkg : Opamfile.pkg) -> clean pkg.name) pkgs in
      let name =
        List.fold_left
          (fun cur_name name ->
            match cur_name with
            | Some cur_name
              when String.(length cur_name < length name) || StringMap.mem name !dev_repo_map ->
                Some cur_name
            | _ -> Some name)
          None packages
        |> Option.get
      in
      { name; dev_repo = StringMap.find name !dev_repo_map; repo = clean repo; packages } :: aux)
    !repo_map []

let projects t =
  let packages = Opamfile.get_packages t.lockfile in
  build_project_list packages t.dev_repos_output

let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) ()

let parse_opam_dev_repo dev_repo =
  let module String = Astring.String in
  let repo, branch =
    match String.cuts ~sep:"#" dev_repo with
    | [ repo ] -> (repo, None)
    | [ repo; branch ] -> (repo, Some branch)
    | _ -> failwith "String.cuts dev_repo"
  in
  let repo = if String.is_prefix ~affix:"git+" repo then String.drop ~max:4 repo else repo in
  Printf.printf "repo: %s\n" repo;
  (repo, branch)

let commits ?(filter = fun _ -> true) lock =
  let open Current.Syntax in
  Current.component "track projects from lockfile"
  |> let** lockv = lock in
     (* Bind: the list of tracked projects is dynamic *)
     let projects = projects lockv in
     Printf.printf "got %d projects to track.\n" (List.length projects);
     projects |> List.filter filter
     |> List.map (fun (x : project) ->
            let repo_url, repo_branch = parse_opam_dev_repo x.dev_repo in
            Current_git.clone ~schedule:daily ?gref:repo_branch repo_url)
     |> Current.list_seq
     |> Current.collapse ~key:"lock" ~value:"" ~input:lock
