open Lwt.Infix
module Log = Solver_api.Solver.Log

(** Find the oldest commit that touches all the paths. Should find the most
    recent commit backwards `from` that have touched the paths. Process all the
    paths and check using `OpamFile.OPAM.effectively_equal` to see whether
    Resolve for a packages revdeps.

    Don't want to scope on opam_repository *)
let oldest_commit_with ~job ~from pkgs =
  let paths =
    pkgs
    |> List.map (fun pkg ->
           let name = OpamPackage.name_to_string pkg in
           let version = OpamPackage.version_to_string pkg in
           Printf.sprintf "packages/%s/%s.%s" name name version)
  in
  let clone_path = Current_git.Commit.repo from in
  Current.Job.log job "clone_path %a" Current_git.Commit.pp from;
  (* Equivalent to: git -C path log -n 1 --format=format:%H from -- paths *)
  let cmd =
    "git"
    :: "-C"
    :: Fpath.to_string clone_path
    :: "log"
    :: "-n"
    :: "1"
    :: "--format=format:%H"
    :: Current_git.Commit.hash from
    :: "--"
    :: paths
  in
  Current.Job.log job "oldest_commit_with %a"
    (Fmt.list ~sep:Fmt.sp Fmt.string)
    cmd;
  let cmd = ("", Array.of_list cmd) in
  Process.pread cmd >|= String.trim
