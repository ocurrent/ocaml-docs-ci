let prep_version = "v3"
let network = Misc.network
let cache = []

module OpamPackage = struct
  include OpamPackage

  let to_yojson t = `String (OpamPackage.to_string t)

  let of_yojson = function
    | `String str -> (
        match OpamPackage.of_string_opt str with
        | Some x -> Ok x
        | None -> Error "failed to parse version")
    | _ -> Error "failed to run version"
end

let opam_cache_version = "v1"

module Cache = struct
  let fname id pkg =
    let name = OpamPackage.to_string pkg in
    let fname = name ^ "-opam.tar.bz2" in
    Fpath.(Current.state_dir id / name / fname)

  let id = "opam-files-cache-" ^ opam_cache_version

  type cache_value = bool * string

  let fname = fname id

  let mem job pkg =
    let fname = fname pkg in
    match Bos.OS.Path.exists fname with
    | Ok true -> true
    | Ok false -> false
    | Error (`Msg m) ->
        Current.Job.log job
          "Error checking for opam files cache for package %s: %s"
          (OpamPackage.to_string pkg)
          m;
        false

  let write ((pkg, value) : OpamPackage.t * cache_value) =
    let fname = fname pkg in
    let _ = Bos.OS.Dir.create (fst (Fpath.split_base fname)) |> Result.get_ok in
    let file = open_out (Fpath.to_string fname) in
    Marshal.to_channel file value [];
    close_out file

  let read job pkg : cache_value option =
    let fname = fname pkg in
    try
      let file = open_in (Fpath.to_string fname) in
      let result = Marshal.from_channel file in
      close_in file;
      Some result
    with (Failure _ | Sys_error _) as e ->
      Current.Job.log job "Error reading opam files cache for package %s: %s"
        (OpamPackage.to_string pkg)
        (Printexc.to_string e);
      None
end

module OpamFiles = struct
  type t = No_context

  let id = "opam-files"
  let auto_cancel = true

  module Key = struct
    type t = { repo : Current_git.Commit.t; packages : OpamPackage.t list }

    let digest { repo; packages } =
      Current_git.Commit.hash repo
      ^ String.concat ";" (List.map OpamPackage.to_string packages)
  end

  module Value = struct
    type ('a, 'b) r = ('a, 'b) result = Ok of 'a | Error of 'b
    [@@deriving yojson]

    type serialisable =
      (OpamPackage.t * (bool * string, [ `Msg of string ]) r) list
    [@@deriving yojson]

    type t = (bool * string, [ `Msg of string ]) r OpamPackage.Map.t

    let to_yojson t = t |> OpamPackage.Map.bindings |> serialisable_to_yojson

    let of_yojson t =
      t |> serialisable_of_yojson |> Result.map OpamPackage.Map.of_list

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string
    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let pool = Current.Pool.create ~label:"git_pool" 5

  let build _ job { Key.repo; packages } =
    let package_path pkg =
      Fpath.(
        v "packages"
        / (OpamPackage.name pkg |> OpamPackage.Name.to_string)
        / OpamPackage.to_string pkg)
    in
    let open Lwt.Infix in
    Current.Job.start ~pool ~level:Harmless job >>= fun () ->
    let to_do = List.filter (fun x -> not (Cache.mem job x)) packages in
    Current_git.with_checkout ~job repo (fun path ->
        Current.Process.with_tmpdir ~prefix:"opam-files" (fun fp ->
            let open Lwt.Infix in
            let r =
              Lwt_list.map_s
                (fun pkg ->
                  let open Lwt.Syntax in
                  let tarfile =
                    Fpath.(
                      fp / Fmt.str "%s-opam.tar.bz2" (OpamPackage.to_string pkg))
                  in
                  let has_depext =
                    match
                      Bos.OS.File.read Fpath.(path // package_path pkg / "opam")
                    with
                    | Ok content ->
                        let f = OpamFile.OPAM.read_from_string content in
                        let _x = OpamFile.OPAM.depends f in
                        OpamFile.OPAM.depexts f <> []
                    | _ -> false
                  in
                  let command =
                    Bos.Cmd.(
                      v "tar"
                      % "-C"
                      % Fpath.to_string path
                      % "-jcf"
                      % Fpath.to_string tarfile
                      % Fpath.to_string (package_path pkg))
                  in
                  Current.Job.log job "About to exec";
                  let* res =
                    Current.Process.exec ~cancellable:true ~job
                      ("", Bos.Cmd.to_list command |> Array.of_list)
                  in
                  Current.Job.log job "Finished";
                  match res with
                  | Ok () -> (
                      let res = Bos.OS.File.read tarfile in
                      match res with
                      | Ok contents ->
                          Cache.write
                            (pkg, (has_depext, Base64.encode_string contents));
                          Lwt.return (pkg, Ok ())
                      | Error (`Msg m) -> Lwt.return (pkg, Error (`Msg m)))
                  | Error (`Msg m) -> Lwt.return (pkg, Error (`Msg m)))
                to_do
            in
            r >>= fun results ->
            let results_map = OpamPackage.Map.of_list results in
            List.map
              (fun x ->
                match Cache.read job x with
                | Some v -> (x, Ok v)
                | None -> (
                    match OpamPackage.Map.find x results_map with
                    | Error (`Msg m) -> (x, Error (`Msg m))
                    | Ok () ->
                        (x, Error (`Msg "Failed to read cache after writing"))
                    | exception Not_found ->
                        (x, Error (`Msg "Didn't even try to solve for package"))
                    ))
              packages
            |> OpamPackage.Map.of_list
            |> Lwt.return_ok))

  let pp f { Key.repo; packages } =
    Fmt.pf f "opamfiles\n%a\n%a" Current_git.Commit.pp_short repo
      (Fmt.list (Fmt.of_to_string OpamPackage.to_string))
      packages
end

module type F = Current_cache.S.BUILDER

module OpamFilesCache = Current_cache.Make (OpamFiles)

let not_base x =
  not
    (List.mem
       (OpamPackage.name_to_string x)
       [
         "base-unix";
         "base-bigarray";
         "base-threads";
         "ocaml-config";
         "ocaml";
         "ocaml-base-compiler";
         "ocaml-variants";
       ])

let add_base ocaml_version init =
  let add_one x pkgs = if List.mem x pkgs then pkgs else x :: pkgs in
  let mk n v =
    OpamPackage.create
      (OpamPackage.Name.of_string n)
      (OpamPackage.Version.of_string v)
  in
  let v = ocaml_version in
  let std =
    List.fold_right add_one
      [
        mk "conf-graphviz" "0.1";
        mk "conf-which" "1";
        mk "base-unix" "base";
        mk "base-bigarray" "base";
        mk "base-threads" "base";
        mk "ocaml-base-compiler" (Ocaml_version.to_string ocaml_version);
        mk "ocaml" (Ocaml_version.to_string ocaml_version);
      ]
      init
  in
  let extra =
    List.assoc (Ocaml_version.major v)
      [
        ( 5,
          [
            mk "base-domains" "base";
            mk "base-nnp" "base";
            mk "base-effects" "base";
            mk "host-arch-x86_64" "1";
            mk "host-system-other" "1";
            mk "ocaml-options-vanilla" "1";
          ] );
        ( 4,
          [
            mk "ocaml-options-vanilla" "1";
            mk "host-system-other" "1";
            mk "host-arch-x86_64" "1";
          ] );
      ]
  in
  let ocaml_config =
    if Ocaml_version.major v = 5 then [ mk "ocaml-config" "3" ]
    else if Ocaml_version.minor v >= 12 then [ mk "ocaml-config" "2" ]
    else [ mk "ocaml-config" "1" ]
  in
  ocaml_config @ std @ extra

let spec ~ssh ~tools_base ~base ~opamfiles (prep : Package.t) =
  let open Obuilder_spec in
  (* the list of packages to install (which is a superset of the packages to prep) *)
  let all_deps = Package.universe prep |> Package.Universe.deps in

  let ocaml_version = Package.ocaml_version prep in
  let prep_caches =
    [ Obuilder_spec.Cache.v "prep-cache" ~target:"/home/opam/.cache/prep" ]
  in

  let cache = cache @ prep_caches in

  let packages_topo_list =
    all_deps
    |> Package.topo_sort
    |> List.filter (fun x -> Package.opam x |> not_base)
  in

  (* Only enable dune cache for dune >= 2.1 - to remove errors like:
     #=== ERROR while compiling base64.2.3.0 =======================================#
     # context              2.0.8 | linux/x86_64 | ocaml-base-compiler.4.06.1 | file:///src
     # path                 ~/.opam/4.06/.opam-switch/build/base64.2.3.0
     # command              ~/.opam/4.06/bin/dune build -p base64 -j 127
     # exit-code            1
     # env-file             ~/.opam/log/base64-1-9efc19.env
     # output-file          ~/.opam/log/base64-1-9efc19.out
     ### output ###
     # Error: link: /home/opam/.cache/dune/db/v2/temp/promoting: Invalid cross-device link
  *)
  let prep_storage_folder = (Storage.Prep0, prep) in
  let prep_folder = Storage.folder Prep0 prep in

  let create_dir_and_copy_logs_if_not_exist =
    let command =
      Fmt.str
        "([ -d $1 ] || (echo FAILED:$2 && mkdir -p $1 && cp ~/opam.err.log $1 \
         && opam show $3 --raw > $1/opam)) && (%s)"
        (Misc.tar_cmd (Fpath.v "$1"))
    in
    Storage.for_all [ prep_storage_folder ] command
  in

  let install_opamfiles pkg tar_b64 =
    let name = OpamPackage.to_string pkg in
    Fmt.str "echo %s ; echo %s | base64 -d | sudo tar -jxC /src" name tar_b64
  in

  let install_all_opamfiles =
    List.filter_map
      (fun pkg ->
        match OpamPackage.Map.find (Package.opam pkg) opamfiles with
        | Ok (_, opamfiles) ->
            Some (install_opamfiles (Package.opam pkg) opamfiles)
        | Error (`Msg _m) -> None
        | exception Not_found -> None)
      packages_topo_list
  in

  let install_cmds =
    List.map (run ~network "%s") (Misc.Cmd.list_list install_all_opamfiles)
  in

  let install_packages =
    "echo \"START OF INSTALL PACKAGES\" && date"
    :: (List.flatten
       @@ List.map
            (fun pkg ->
              match OpamPackage.Map.find (Package.opam pkg) opamfiles with
              | Ok (_has_depext, _opamfiles) ->
                  [
                    Fmt.str
                      "time ~/docs/docs-ci-scripts/download_prep.sh %s %s %s %s"
                      (Config.Ssh.host ssh)
                      (Config.Ssh.storage_folder ssh)
                      (Fpath.to_string (Storage.folder Storage.Prep0 pkg))
                      (if Package.should_cache pkg then "true" else "false");
                  ]
              | Error _ | (exception _) ->
                  [
                    Fmt.str "echo Failed to find opamfiles for %s"
                      (Package.opam pkg |> OpamPackage.to_string);
                  ])
            packages_topo_list)
  in

  let any_depexts =
    List.exists
      (fun pkg ->
        match OpamPackage.Map.find (Package.opam pkg) opamfiles with
        | Ok (has_depext, _) -> has_depext
        | Error _ | (exception _) -> false)
      packages_topo_list
  in

  let extra_opamfiles =
    List.flatten
      (List.map
         (fun pkg ->
           match OpamPackage.Map.find pkg opamfiles with
           | Ok (_has_depext, opamfiles) -> [ install_opamfiles pkg opamfiles ]
           | _ | (exception _) ->
               [ Fmt.str "echo Missing %s" (OpamPackage.to_string pkg) ])
         (add_base ocaml_version []))
  in

  let pkg_opamfile =
    match OpamPackage.Map.find (Package.opam prep) opamfiles with
    | Ok (_has_depext, opamfiles) ->
        [ install_opamfiles (Package.opam prep) opamfiles ]
    | Error _ | (exception _) -> []
  in

  let post_steps =
    [
      "echo \"START OF BUILD PACKAGE\" && date";
      "opam update && /home/opam/opamh make-state --output $(opam var \
       prefix)/.opam-switch/switch-state";
      (if any_depexts then
         Fmt.str "opam depext %s" (OpamPackage.to_string (Package.opam prep))
       else "echo no depexts");
      Fmt.str
        "(opam update && opam install -vv --debug-level=2 \
         --confirm-level=unsafe-yes --solver=builtin-0install %s 2>&1 && opam \
         clean -s | tee ~/opam.err.log) || echo 'Failed to install all \
         packages'"
        (Package.opam prep |> OpamPackage.to_string);
      "echo \"END OF BUILD PACKAGE\" && date";
      Fmt.str "mkdir -p %s" (Fpath.to_string prep_folder);
      Fmt.str "/home/opam/opamh save --output=%s/content.tar %s"
        (Fpath.to_string prep_folder)
        (Package.opam prep |> OpamPackage.name |> OpamPackage.Name.to_string);
      Fmt.str "time rsync -aR ./%s %s:%s/."
        (Fpath.to_string prep_folder)
        (Config.Ssh.host ssh)
        (Config.Ssh.storage_folder ssh);
      "rm -f /tmp/*.tar";
      "rm -rf $(opam var prefix)";
    ]
  in

  let persistent_ssh =
    [
      Fmt.str "/usr/bin/time -f \"SSH time %%E\" ssh -MNf %s"
        (Config.Ssh.host ssh);
    ]
  in

  let copy_results =
    create_dir_and_copy_logs_if_not_exist
    @
    (* Compute hashes *)
    Storage.for_all [ prep_storage_folder ]
      (Storage.Tar.hash_command ~prefix:"HASHES" ())
  in

  (* let install_packages = persistent_ssh @ pkg_opamfile @ extra_opamfiles @ install_packages @ post_steps @ copy_results in *)
  let tools = Voodoo.Prep.spec ~base:tools_base |> Spec.finish in
  base
  |> Spec.children ~name:"tools" tools
  |> Spec.add
       ([
          (* Install required packages *)
          run "sudo mkdir /src";
          run
            "echo \
             b3BhbS12ZXJzaW9uOiAiMi4wIgpicm93c2U6ICJodHRwczovL29wYW0ub2NhbWwub3JnL3BrZy8iCnVwc3RyZWFtOiAiaHR0cHM6Ly9naXRodWIuY29tL29jYW1sL29wYW0tcmVwb3NpdG9yeS90cmVlL21hc3Rlci8iCmFubm91bmNlOiBbCiIiIgpbV0FSTklOR10gb3BhbSBpcyBvdXQtb2YtZGF0ZS4gUGxlYXNlIGNvbnNpZGVyIHVwZGF0aW5nIGl0IChodHRwczovL29wYW0ub2NhbWwub3JnL2RvYy9JbnN0YWxsLmh0bWwpCiIiIiB7KG9wYW0tdmVyc2lvbiA+PSAiMi4xLjB+fiIgJiBvcGFtLXZlcnNpb24gPCAiMi4xLjUiKSB8IG9wYW0tdmVyc2lvbiA8ICIyLjAuMTAifQoiIiIKW0lORk9dIG9wYW0gMi4xIGluY2x1ZGVzIG1hbnkgcGVyZm9ybWFuY2UgaW1wcm92ZW1lbnRzIG92ZXIgMi4wOyBwbGVhc2UgY29uc2lkZXIgdXBncmFkaW5nIChodHRwczovL29wYW0ub2NhbWwub3JnL2RvYy9JbnN0YWxsLmh0bWwpCiIiIiB7b3BhbS12ZXJzaW9uID49ICIyLjAuMTAiICYgb3BhbS12ZXJzaW9uIDwgIjIuMS4wfn4ifQpdCg== \
             | base64 -d | sudo tee /src/repo";
          run "ls -lR /src";
          (* Re-initialise opam after switching from opam.2.0 to 2.3. *)
          run ~network
            "sudo ln -f /usr/bin/opam-2.3 /usr/bin/opam && opam repo remove \
             default --all && opam init --reinit -ni";
          run "sudo mkdir /src/packages";
          run "opam repo add opam /src";
          copy ~from:(`Build "tools") [ "/home/opam/opamh" ] ~dst:"/home/opam/";
          (* Enable build cache conditionally on dune version *)
          env "DUNE_CACHE" "disabled";
          run "mkdir /home/opam/docs";
          (* Pre-install some of the most popular packages *)
          run ~network:Misc.network
            "sudo apt-get update && sudo apt-get install -qq -yy pkg-config \
             libgmp-dev libev-dev libssl-dev zlib1g-dev libpcre3-dev \
             libffi-dev m4 xdot autoconf libsqlite3-dev cmake \
             libcurl4-gnutls-dev libpcre2-dev libsdl2-dev time python3 \
             libexpat1-dev libcairo2-dev";
          run ~network:Misc.network
            "git clone https://github.com/jonludlam/docs-ci-scripts.git \
             /home/opam/docs/docs-ci-scripts && echo %s"
            Config.random;
          (* run ~network ~cache ~secrets:Config.Ssh.secrets "%s" @@ Misc.Cmd.list install_packages *)
        ]
       @ install_cmds
       @ [
           run ~network ~cache ~secrets:Config.Ssh.secrets "%s"
           @@ Misc.Cmd.list
                (persistent_ssh
                @ pkg_opamfile
                @ extra_opamfiles
                @ install_packages);
         ]
       @ [
           run ~network ~cache ~secrets:Config.Ssh.secrets "%s"
           @@ Misc.Cmd.list (post_steps @ copy_results);
         ])

type prep_result = Success | Failed

type prep_output = {
  base : Spec.t;
  hash : string;
  prep_hash : string;
  package : Package.t;
  result : prep_result;
}

module Prep = struct
  type t = No_context

  let id = "voodoo-prep2"
  let auto_cancel = true

  module Key = struct
    type t = {
      prep : Package.t;
      base : Spec.t;
      tools_base : Spec.t;
      config : Config.t;
      opamfiles : (bool * string) Current.or_error OpamPackage.Map.t;
    }

    let digest { prep; base = _; tools_base = _; config = _; opamfiles = _ } =
      (* base is derived from 'prep' so we don't need to include it in the hash *)
      Fmt.str "%s\n%s\n%s\n" prep_version (Package.digest prep)
        (Package.digest prep)
  end

  let pp f Key.{ prep; _ } = Fmt.pf f "Voodoo prep %a" Package.pp prep

  module Value = struct
    type item = Storage.id_hash [@@deriving yojson]
    type t = item list * string list [@@deriving yojson]

    let marshal t = t |> to_yojson |> Yojson.Safe.to_string
    let unmarshal t = t |> Yojson.Safe.from_string |> of_yojson |> Result.get_ok
  end

  let build No_context job Key.{ prep; base; tools_base; config; opamfiles } =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    (* Problem: no rebuild if the opam definition changes without affecting the universe hash.
       Should be fixed by adding the oldest opam-repository commit in the universe hash, but that
       requires changes in the solver.
       For now we rebuild only if voodoo-prep changes.
    *)
    let spec =
      spec ~ssh:(Config.ssh config) ~base ~tools_base ~opamfiles prep
    in
    let action = Misc.to_ocluster_submission spec in
    let version = Misc.cache_hint prep in
    Current.Job.log job "Prep job: prep %a" Package.pp prep;
    let cache_hint = "docs-universe-prep-" ^ version in
    let build_pool =
      Current_ocluster.Connection.pool ~job ~pool:(Config.pool config) ~action
        ~cache_hint
        ~secrets:(Config.Ssh.secrets_values (Config.ssh config))
        (Config.ocluster_connection_prep config)
    in
    let* build_job =
      Current.Job.start_with ~pool:build_pool ~level:Mostly_harmless job
    in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.write job
      (Fmt.str
         "@.To reproduce locally:@.@.cat > prep.spec \
          <<'END-OF-SPEC'@.\o033[34m%s\o033[0m@.END-OF-SPEC@.@.ocluster-client \
          submit-obuilder --local-file prep.spec \\@.--pool linux-x86_64 \
          --connect ocluster-submission.cap --cache-hint %s \\@.--secret \
          ssh_privkey:id_rsa --secret ssh_pubkey:id_rsa.pub--secret \
          ssh_config:ssh_config@.@."
         (Spec.to_spec spec) cache_hint);

    Capnp_rpc_lwt.Capability.with_ref build_job @@ fun build_job ->
    (* extract result from logs *)
    let extract_hashes ((git_hashes, failed), retriable_errors) line =
      let retry_conditions log_line =
        let retry_on =
          [
            "Temporary failure";
            "Could not resolve host";
            "rsync: connection unexpectedly closed";
            "Disconnected: Switch turned off";
          ]
        in
        List.fold_left
          (fun acc str -> acc || Astring.String.is_infix ~affix:str log_line)
          false retry_on
      in
      let escape_on_success log_line =
        let escape_on = [ "Job succeeded" ] in
        List.fold_left
          (fun acc str -> acc || Astring.String.is_infix ~affix:str log_line)
          false escape_on
      in
      match Storage.parse_hash ~prefix:"HASHES" line with
      | Some value -> ((value :: git_hashes, failed), retriable_errors)
      | None -> (
          if escape_on_success line then ((git_hashes, failed), [])
            (* ignore retriable errors if the job has succeeded *)
          else if retry_conditions line then
            ((git_hashes, failed), line :: retriable_errors)
          else
            match String.split_on_char ':' line with
            | [ prev; branch ]
              when Astring.String.is_suffix ~affix:"FAILED" prev ->
                Current.Job.log job "Failed: %s" branch;
                ((git_hashes, branch :: failed), retriable_errors)
            | _ -> ((git_hashes, failed), retriable_errors))
    in

    let fn () =
      let** _ = Current_ocluster.Connection.run_job ~job build_job in
      Misc.fold_logs build_job extract_hashes (([], []), [])
    in

    let** git_hashes, failed =
      Retry.retry_loop ~job ~log_string:(Current.Job.id job) fn
    in
    Lwt.return_ok
      ( List.map
          (fun (r : Storage.id_hash) ->
            Current.Job.log job "%s -> %s" r.id r.hash;
            r)
          git_hashes,
        failed )
end

module PrepCache = Current_cache.Make (Prep)

type t = prep_output

let hash t = t.hash
let prep_hash t = t.prep_hash
let package t = t.package
let result t = t.result
let base t = t.base

type prep = t Package.Map.t

let pp f t = Fmt.pf f "%s:%s" (Package.id t.package) t.hash

let compare a b =
  match String.compare a.hash b.hash with
  | 0 -> Package.compare a.package b.package
  | v -> v

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let v ~config ~spec ~deps ~opamfiles ~prep =
  let open Current.Syntax in
  let opamfiles : OpamFiles.Value.t Current.t = opamfiles in
  Current.component "voodoo-prep %s" (prep |> Package.digest)
  |> let> spec
     and> opamfiles
     and> deps
     and> tools_base = Misc.default_base_image in
     ignore deps;
     PrepCache.get No_context
       { prep; config; base = spec; tools_base; opamfiles }
     |> Current.Primitive.map_result
          (Result.map (fun (artifacts_branches_output, _failed) ->
               let _artifacts_branches_output =
                 artifacts_branches_output
                 |> List.to_seq
                 |> Seq.map (fun Storage.{ id; hash } -> (id, hash))
                 |> StringMap.of_seq
               in

               {
                 base = spec;
                 prep_hash = "";
                 result = Success;
                 package = prep;
                 hash = "";
               }))
