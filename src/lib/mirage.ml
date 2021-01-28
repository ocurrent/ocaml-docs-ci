open Lwt.Infix

let or_raise = function Ok () -> () | Error (`Msg m) -> raise (Failure m)

let ( let>> ) = Lwt_result.bind

let pool = Current.Pool.create ~label:"mirage-pool" 4

(* Run mirage configure  *)
module Configure = struct
  type t = No_context

  module Key = struct
    type t = { base : Spec.t; commit : Current_git.Commit.t; unikernel : string; target : string }

    let digest { base; commit; target; unikernel } =
      let json =
        `Assoc
          [
            ("spec", Spec.to_json base);
            ("commit", `String (Current_git.Commit.hash commit));
            ("unikernel", `String unikernel);
            ("target", `String target);
          ]
      in
      Yojson.to_string json
  end

  module Value = Opamfile

  let unikernel_file_command =
    "find . -maxdepth 1 -type f -not -name '*install.opam' -name '*.opam' -exec cat {} +"

  let spec ~base ~unikernel ~target =
    let open Obuilder_spec in
    base
    |> Spec.add
         [
           run "opam install mirage";
           user ~uid:1000 ~gid:1000;
           copy [ "." ] ~dst:"/src/";
           workdir ("/src/" ^ unikernel);
           run "opam exec -- mirage configure -t %s" target;
           run "%s" unikernel_file_command;
         ]
    |> Spec.finish

  let build No_context job { Key.base; commit; target; unikernel } =
    let spec = spec ~base ~unikernel ~target in
    let dockerfile = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false spec in
    let switch = Current.Switch.create ~label:"mirage-docker-switch" () in
    Lwt.finalize
      (fun () ->
        Current.Job.use_pool ~switch job pool >>= fun () ->
        Current.Job.start ~level:Harmless job >>= fun _ ->
        Current.Job.log job "Starting mirage unikernel analysis (%s/%s)." unikernel target;
        let>> id =
          Current_git.with_checkout ~job commit (fun dir ->
              Bos.OS.File.write Fpath.(dir / "Dockerfile") (dockerfile ^ "\n") |> or_raise;
              let iidfile = Fpath.(dir / "image.id") in
              let cmd =
                Current_docker.Raw.Cmd.docker ~docker_context:None
                  [ "build"; "--iidfile"; Fpath.to_string iidfile; "--"; Fpath.to_string dir ]
              in
              let>> () = Current.Process.exec ~cancellable:true ~job cmd in
              Lwt.return (Bos.OS.File.read iidfile))
        in
        let>> unikernel_opam =
          let cmd =
            Current_docker.Raw.Cmd.docker ~docker_context:None
              [
                "run";
                "-i";
                id;
                "find";
                ".";
                "-maxdepth";
                "1";
                "-type";
                "f";
                "-not";
                "-name";
                "*install.opam";
                "-name";
                "*.opam";
                "-exec";
                "cat";
                "{}";
                "+";
              ]
          in
          Current.Process.check_output ~cancellable:true ~job cmd
        in
        Current.Job.log job "----\nObtained opam file:\n%s\n----" unikernel_opam;
        Lwt.return_ok (OpamParser.string unikernel_opam "monorepo.opam"))
      (fun () -> Current.Switch.turn_off switch)

  let auto_cancel = true

  let pp f _ = Fmt.string f "Mirage configure"

  let id = "mirage-ci-mirage-configure"
end

module CC = Current_cache.Make (Configure)
open Current.Syntax

let configure ~base ~project ~unikernel ~target =
  Current.component "Mirage configure %s@%s" unikernel target
  |> let> base = base and> project = project in
     CC.get No_context { base; commit = project; unikernel; target }

module Build = struct
  type t = Spec.t

  module Key = struct
    type t = { commit : Current_git.Commit.t; unikernel : string; target : string }

    let digest { commit; target; unikernel } =
      let json =
        `Assoc
          [
            ("commit", `String (Current_git.Commit.hash commit));
            ("unikernel", `String unikernel);
            ("target", `String target);
          ]
      in
      Yojson.to_string json
  end

  module Value = Current.Unit

  let spec ~base ~unikernel ~target =
    let open Obuilder_spec in
    base
    |> Spec.add
         [
           run "opam install dune mirage opam-monorepo";
           user ~uid:1000 ~gid:1000;
           copy [ "." ] ~dst:"/src/";
           workdir ("/src/" ^ unikernel);
           run "opam exec -- mirage configure -t %s" target;
           run "opam exec -- make depends";
           run "opam exec -- dune build";
         ]
    |> Spec.finish

  let build base job { Key.commit; target; unikernel } =
    let spec = spec ~base ~unikernel ~target in
    let dockerfile = Obuilder_spec.Docker.dockerfile_of_spec ~buildkit:false spec in
    let switch = Current.Switch.create ~label:"mirage-build-switch" () in
    Lwt.finalize
      (fun () ->
        Current.Job.start ~level:Harmless job >>= fun _ ->
        Current.Job.log job "Starting mirage unikernel build (%s/%s)." unikernel target;
        Current.Job.use_pool ~switch job pool >>= fun () ->
        Current_git.with_checkout ~job commit (fun dir ->
            Bos.OS.File.write Fpath.(dir / "Dockerfile") (dockerfile ^ "\n") |> or_raise;
            let cmd =
              Current_docker.Raw.Cmd.docker ~docker_context:None
                [ "build"; "--"; Fpath.to_string dir ]
            in
            Current.Process.exec ~cancellable:true ~job cmd))
      (fun () -> Current.Switch.turn_off switch)

  let auto_cancel = true

  let pp f _ = Fmt.string f "Mirage build"

  let id = "mirage-ci-mirage-build"
end

module BC = Current_cache.Make (Build)

let build ~base ~project ~unikernel ~target =
  Current.component "Mirage build %s@%s" unikernel target
  |> let> base = base and> project = project in
     BC.get base { commit = project; unikernel; target }
