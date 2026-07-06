(* Target selection is two orthogonal axes: which versions of a package
   to take, and which packages to include. *)
type version_mode =
  | All_versions
  | Latest_n of int
      (** Keep the [n] most-recent (non-avoided) versions of each
          package. [Latest_n 1] = newest only. *)

type name_filter =
  | All_names
  | Names of string list
      (** Track only these exact package names. *)

type target_mode = { versions : version_mode; names : name_filter }

type t = {
  name : string;
  opam_repositories : string list;
  odoc_repo : string option;
  opam_build_repo : string option;
  compiler : string option;
  target_mode : target_mode;
  with_doc : bool;
  with_jtw : bool;
  jtw_repo : string option;
  arch : string;
  os_distribution : string;
  os_version : string;
  driver_compiler : string;
  extra_pins : string list;
  pinned_versions : string list;
  patches_dir : string option;
  base_image_digest : string option;
  base_image_updated : string option;
  (* ISO-8601 timestamp of when base_image_digest was resolved *)
  html_dir : string option;
  (* Destination for generated HTML when the profile is driven by
     a doc pipeline ([ocaml-docs-ci] etc.). Each profile gets its
     own [html_dir] so a single pipeline run can output to several
     separate sites in parallel. [None] means build-only, no docs. *)
}

let version_mode_to_json = function
  | All_versions -> `String "all"
  | Latest_n n -> `Assoc [ ("latest", `Int n) ]

let name_filter_to_json = function
  | All_names -> `String "all"
  | Names l -> `Assoc [ ("only", `List (List.map (fun s -> `String s) l)) ]

let target_mode_to_json { versions; names } =
  `Assoc [ ("versions", version_mode_to_json versions);
           ("names", name_filter_to_json names) ]

let version_mode_of_json = function
  | `String "all" -> Ok All_versions
  | `Assoc [ ("latest", `Int n) ] -> Ok (Latest_n n)
  | _ -> Error (`Msg "invalid version_mode")

let names_of_json l =
  Names (List.filter_map (function `String s -> Some s | _ -> None) l)

let name_filter_of_json = function
  | `String "all" -> Ok All_names
  | `Assoc [ ("only", `List l) ] -> Ok (names_of_json l)
  | _ -> Error (`Msg "invalid name_filter")

let target_mode_of_json = function
  (* New two-axis form: {"versions": …, "names": …}. *)
  | `Assoc fields when List.mem_assoc "versions" fields
                    || List.mem_assoc "names" fields ->
    let v = match List.assoc_opt "versions" fields with
      | None -> Ok All_versions | Some j -> version_mode_of_json j in
    let n = match List.assoc_opt "names" fields with
      | None -> Ok All_names | Some j -> name_filter_of_json j in
    (match v, n with
     | Ok versions, Ok names -> Ok { versions; names }
     | (Error _ as e), _ | _, (Error _ as e) -> e)
  (* Back-compat with the old single-enum [target_mode]. *)
  | `String "all_versions" -> Ok { versions = All_versions; names = All_names }
  | `String "latest_versions" -> Ok { versions = Latest_n 1; names = All_names }
  | `Assoc [ ("packages", `List l) ] ->
    Ok { versions = All_versions; names = names_of_json l }
  | _ -> Error (`Msg "invalid target_mode")

let opt_to_json = function
  | None -> `Null
  | Some s -> `String s

let opt_of_json = function
  | `Null -> None
  | `String s -> Some s
  | _ -> None

let to_json t =
  `Assoc [
    ("name", `String t.name);
    ("opam_repositories", `List (List.map (fun s -> `String s) t.opam_repositories));
    ("odoc_repo", opt_to_json t.odoc_repo);
    ("opam_build_repo", opt_to_json t.opam_build_repo);
    ("compiler", opt_to_json t.compiler);
    ("target_mode", target_mode_to_json t.target_mode);
    ("with_doc", `Bool t.with_doc);
    ("with_jtw", `Bool t.with_jtw);
    ("jtw_repo", opt_to_json t.jtw_repo);
    ("arch", `String t.arch);
    ("os_distribution", `String t.os_distribution);
    ("os_version", `String t.os_version);
    ("driver_compiler", `String t.driver_compiler);
    ("extra_pins", `List (List.map (fun s -> `String s) t.extra_pins));
    ("pinned_versions",
      `List (List.map (fun s -> `String s) t.pinned_versions));
    ("patches_dir", opt_to_json t.patches_dir);
    ("base_image_digest", opt_to_json t.base_image_digest);
    ("base_image_updated", opt_to_json t.base_image_updated);
    ("html_dir", opt_to_json t.html_dir);
  ]

let of_json json =
  try
    let open Yojson.Safe.Util in
    let str key = json |> member key |> to_string in
    let str_opt key = json |> member key |> opt_of_json in
    let str_list key =
      json |> member key |> to_list |> List.map to_string in
    let tm = target_mode_of_json (json |> member "target_mode") in
    match tm with
    | Error e -> Error e
    | Ok target_mode ->
      Ok {
        name = str "name";
        opam_repositories = str_list "opam_repositories";
        odoc_repo = str_opt "odoc_repo";
        opam_build_repo = str_opt "opam_build_repo";
        compiler = str_opt "compiler";
        target_mode;
        with_doc = json |> member "with_doc" |> to_bool_option
                   |> Option.value ~default:false;
        with_jtw = json |> member "with_jtw" |> to_bool_option
                   |> Option.value ~default:false;
        jtw_repo = str_opt "jtw_repo";
        arch = (try str "arch" with _ -> "x86_64");
        os_distribution = (try str "os_distribution" with _ -> "debian");
        os_version = (try str "os_version" with _ -> "bookworm");
        driver_compiler = (try str "driver_compiler"
                           with _ -> "ocaml-base-compiler.5.4.1");
        extra_pins = (try str_list "extra_pins" with _ -> []);
        pinned_versions = (try str_list "pinned_versions" with _ -> []);
        patches_dir = str_opt "patches_dir";
        base_image_digest = str_opt "base_image_digest";
        base_image_updated = str_opt "base_image_updated";
        html_dir = str_opt "html_dir";
      }
  with exn ->
    Rresult.R.error_msgf "Profile.of_json: %s" (Printexc.to_string exn)

let save ~dir t =
  let path = Fpath.(dir / (t.name ^ ".json")) in
  try
    ignore (Bos.OS.Dir.create ~path:true dir);
    let data = Yojson.Safe.pretty_to_string (to_json t) in
    Bos.OS.File.write path data
  with exn ->
    Rresult.R.error_msgf "Profile.save: %s" (Printexc.to_string exn)

(* Resolve a profile path against the .day11 root. Absolute entries are
   returned unchanged; a relative entry such as ["overlays/odoc-master/repo"]
   or ["cache/.../html-quick"] is taken relative to [day11_dir], so
   profiles needn't bake in an absolute [/home/app] prefix and stay
   portable across hosts / containers (the daemon's HOME differs between
   the host and the container). Applied to every path-valued field. *)
let resolve_repo ~day11_dir s =
  if Filename.is_relative s then
    Fpath.(day11_dir // v s |> normalize |> to_string)
  else s

let load ~dir ~name =
  let path = Fpath.(dir / (name ^ ".json")) in
  match Bos.OS.File.read path with
  | Error _ as e -> e
  | Ok data ->
    try
      match of_json (Yojson.Safe.from_string data) with
      | Error _ as e -> e
      | Ok profile ->
        (* [dir] is the [profiles/] dir, so its parent is the .day11
           root that relative paths resolve against. Resolve every
           path-valued field, not just [opam_repositories] — e.g.
           [html_dir] was left as an absolute container path and failed
           to bind-mount when the daemon ran on the host. *)
        let day11_dir = Fpath.parent dir in
        let r = resolve_repo ~day11_dir in
        let ro = Option.map r in
        Ok { profile with
             opam_repositories = List.map r profile.opam_repositories;
             odoc_repo = ro profile.odoc_repo;
             opam_build_repo = ro profile.opam_build_repo;
             jtw_repo = ro profile.jtw_repo;
             patches_dir = ro profile.patches_dir;
             html_dir = ro profile.html_dir }
    with exn ->
      Rresult.R.error_msgf "Profile.load %s: %s" name (Printexc.to_string exn)

let list ~dir =
  match Bos.OS.Dir.contents dir with
  | Error _ -> []
  | Ok entries ->
    List.filter_map (fun p ->
      let name = Fpath.basename p in
      if Fpath.has_ext ".json" p then
        Some (Fpath.rem_ext (Fpath.v name) |> Fpath.to_string)
      else None
    ) entries

let delete ~dir ~name =
  let path = Fpath.(dir / (name ^ ".json")) in
  Bos.OS.File.delete path

let os_dir_name t =
  Day11_opam_build.Base.os_dir_name
    ~os_distribution:t.os_distribution ~os_version:t.os_version ~arch:t.arch

let base_image_tag t =
  Printf.sprintf "%s:%s" t.os_distribution t.os_version

let default_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  Fpath.v (Filename.concat home ".day11")

let now_iso8601 () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.tm_year + 1900) (tm.tm_mon + 1) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec

(** Resolve the base image digest from the Docker registry.
    This calls `docker manifest inspect` which queries the registry
    without pulling. Can be slow (~10-15s). *)
let resolve_base_digest t =
  let tag = base_image_tag t in
  let cmd = Printf.sprintf
    "docker manifest inspect %s 2>/dev/null" (Filename.quote tag) in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try while true do Buffer.add_char buf (input_char ic) done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  let json_str = Buffer.contents buf in
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    let manifests = json |> member "manifests" |> to_list in
    let arch = t.arch in
    let docker_arch = match arch with
      | "x86_64" | "amd64" -> "amd64"
      | "aarch64" -> "arm64"
      | a -> a
    in
    List.find_map (fun m ->
      let plat = m |> member "platform" in
      let m_arch = plat |> member "architecture" |> to_string in
      let m_os = plat |> member "os" |> to_string in
      if m_arch = docker_arch && m_os = "linux" then
        Some (m |> member "digest" |> to_string)
      else None
    ) manifests
  with _ -> None

(** Check if the base image digest is older than [max_age_days]. *)
let base_image_stale ?(max_age_days = 30) t =
  match t.base_image_updated with
  | None -> true  (* no timestamp = stale *)
  | Some ts ->
    (* Parse ISO-8601 timestamp *)
    try
      Scanf.sscanf ts "%4d-%2d-%2dT%2d:%2d:%2dZ"
        (fun year mon day hour min sec ->
          let tm = { Unix.tm_sec = sec; tm_min = min; tm_hour = hour;
                     tm_mday = day; tm_mon = mon - 1; tm_year = year - 1900;
                     tm_wday = 0; tm_yday = 0; tm_isdst = false } in
          let then_t, _ = Unix.mktime tm in
          let age_days = (Unix.gettimeofday () -. then_t) /. 86400. in
          age_days > float max_age_days)
    with _ -> true

(** Update the profile's base image digest by querying the registry.
    Returns the updated profile (caller must save it). *)
let refresh_base_digest t =
  match resolve_base_digest t with
  | Some digest ->
    Ok { t with
      base_image_digest = Some digest;
      base_image_updated = Some (now_iso8601 ()) }
  | None ->
    Error (`Msg (Printf.sprintf
      "Failed to resolve digest for %s" (base_image_tag t)))

let track_limit t =
  match t.target_mode.versions with
  | All_versions -> None
  | Latest_n n -> Some n

let track_filter t =
  match t.target_mode.names with
  | All_names -> []
  | Names pkgs -> pkgs

let target_mode_summary t =
  let v = match t.target_mode.versions with
    | All_versions -> "all versions"
    | Latest_n 1 -> "latest version"
    | Latest_n n -> Printf.sprintf "latest %d versions" n in
  let n = match t.target_mode.names with
    | All_names -> "all packages"
    | Names l -> Printf.sprintf "%d packages" (List.length l) in
  Printf.sprintf "%s of %s" v n

let pp fmt t =
  Fmt.pf fmt "@[<v>\
    Profile: %s@,\
    Opam repos: %s@,\
    Odoc repo: %s@,\
    Opam-build repo: %s@,\
    Compiler: %s@,\
    Targets: %s@,\
    Docs: %b@,\
    Platform: %s-%s-%s@,\
    Base image: %s%s@,\
    Driver compiler: %s@,\
    HTML dir: %s\
    @]"
    t.name
    (String.concat ", " t.opam_repositories)
    (Option.value ~default:"(none)" t.odoc_repo)
    (Option.value ~default:"(none)" t.opam_build_repo)
    (Option.value ~default:"(auto)" t.compiler)
    (target_mode_summary t)
    t.with_doc
    t.os_distribution t.os_version t.arch
    (match t.base_image_digest with
     | Some d -> String.sub d 0 (min 20 (String.length d)) ^ "..."
     | None -> "(not pinned)")
    (match t.base_image_updated with
     | Some ts -> Printf.sprintf " (%s)" ts
     | None -> "")
    (if t.driver_compiler = "" then "(auto)" else t.driver_compiler)
    (Option.value ~default:"(none)" t.html_dir)
