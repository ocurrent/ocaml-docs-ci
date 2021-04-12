let network = [ "host" ]

let download_cache = Obuilder_spec.Cache.v "opam-archives" ~target:"/home/opam/.opam/download-cache"

let dune_cache = Obuilder_spec.Cache.v "opam-dune-cache" ~target:"/home/opam/.cache/dune"

let cache = [ download_cache; dune_cache ]

module Git = Current_git

type t = { voodoo_do : Git.Commit_id.t; voodoo_prep : Git.Commit_id.t }

module Op = struct
  type voodoo = t

  type t = No_context

  let id = "voodoo-repository"

  let pp f _ = Fmt.pf f "voodoo-repository"

  let auto_cancel = false

  module Key = struct
    type t = Git.Commit.t

    let digest = Git.Commit.hash
  end

  module Value = struct
    type t = voodoo

    let to_yojson commit =
      let hash = Git.Commit_id.hash commit in
      let repo = Git.Commit_id.repo commit in
      let gref = Git.Commit_id.gref commit in
      `Assoc [ ("hash", `String hash); ("repo", `String repo); ("gref", `String gref) ]

    let of_yojson_exn json =
      let open Yojson.Safe.Util in
      let hash = json |> member "hash" |> to_string in
      let gref = json |> member "gref" |> to_string in
      let repo = json |> member "repo" |> to_string in
      Git.Commit_id.v ~repo ~gref ~hash

    let marshal { voodoo_do; voodoo_prep } =
      `Assoc [ ("do", to_yojson voodoo_do); ("prep", to_yojson voodoo_prep) ]
      |> Yojson.Safe.to_string

    let unmarshal t =
      let json = Yojson.Safe.from_string t in
      let open Yojson.Safe.Util in
      let voodoo_do = json |> member "do" |> of_yojson_exn in
      let voodoo_prep = json |> member "prep" |> of_yojson_exn in
      { voodoo_do; voodoo_prep }
  end

  let voodoo_prep_paths = Fpath.[ v "voodoo-prep.opam"; v "bin/prep/" ]

  let voodoo_do_paths = Fpath.[ v "voodoo-do.opam"; v "voodoo-lib.opam"; v "bin/do/"; v "lib/" ]

  let get_oldest_commit_for ~job ~dir ~from paths =
    let paths = List.map Fpath.to_string paths in
    let cmd = "git" :: "log" :: "-n" :: "1" :: "--format=format:%H" :: from :: "--" :: paths in
    let cmd = ("", Array.of_list cmd) in
    Current.Process.check_output ~cwd:dir ~job ~cancellable:false cmd |> Lwt_result.map String.trim

  let with_hash ~id hash =
    Git.Commit_id.v ~repo:(Git.Commit_id.repo id) ~gref:(Git.Commit_id.gref id) ~hash

  let build No_context job commit =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let* () = Current.Job.start ~level:Harmless job in
    Git.with_checkout ~job commit @@ fun dir ->
    let id = Git.Commit.id commit in
    let from = Git.Commit_id.hash id in
    let** voodoo_prep = get_oldest_commit_for ~job ~dir ~from voodoo_prep_paths in
    let** voodoo_do = get_oldest_commit_for ~job ~dir ~from voodoo_do_paths in
    Current.Job.log job "Prep commit: %s" voodoo_prep;
    Current.Job.log job "Do commit: %s" voodoo_do;
    let voodoo_prep = with_hash ~id voodoo_prep in
    let voodoo_do = with_hash ~id voodoo_do in
    Lwt.return_ok { voodoo_prep; voodoo_do }
end

module VoodooCache = Current_cache.Make (Op)

let v () =
  let daily = Current_cache.Schedule.v ~valid_for:(Duration.of_day 1) () in
  let git = Git.clone ~schedule:daily ~gref:"main" "git://github.com/ocaml-doc/voodoo" in
  let open Current.Syntax in
  Current.component "voodoo"
  |> let> git = git in
     VoodooCache.get No_context git

let remote_uri commit =
  let repo = Git.Commit_id.repo commit in
  let commit = Git.Commit_id.hash commit in
  repo ^ "#" ^ commit

module Prep = struct
  type voodoo = t

  type t = Git.Commit_id.t

  let v { voodoo_prep; _ } = voodoo_prep

  let spec ~base t =
    let open Obuilder_spec in
    base
    |> Spec.add
         [
           run ~network "sudo apt-get update && sudo apt-get install -yy m4 pkg-config";
           run ~network ~cache "opam pin -ny %s  && opam depext -iy voodoo-prep" (remote_uri t);
           run "cp $(opam config var bin)/voodoo-prep /home/opam";
         ]

  let digest = Git.Commit_id.digest
end

module Do = struct
  type voodoo = t

  type t = Git.Commit_id.t

  let v { voodoo_do; _ } = voodoo_do

  let spec ~base t =
    let open Obuilder_spec in
    base
    |> Spec.add
         [
           run ~network "sudo apt-get update && sudo apt-get install -yy m4";
           run ~network
             "opam pin -ny odoc %s && opam depext -iy odoc &&  opam exec -- odoc --version"
             Config.odoc;
           run ~network ~cache "opam pin -ny %s  && opam depext -iy voodoo-do" (remote_uri t);
           run "cp $(opam config var bin)/odoc $(opam config var bin)/voodoo-do /home/opam";
         ]

  let digest = Git.Commit_id.digest
end
