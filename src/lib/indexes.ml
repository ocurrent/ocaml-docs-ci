module Indexes = struct
  type t = No_context

  let id = "indexes-build"

  let pp f _ = Fmt.pf f "indexes build"

  let auto_cancel = false

  module Key = struct
    type t = Compile.t list

    let digest t =
      List.rev_map Compile.digest t |> String.concat "\n" |> Digest.string |> Digest.to_hex
  end

  module Value = struct
    type t = unit

    let marshal () = ""

    let unmarshal _ = ()
  end

  let build No_context job packages =
    let open Lwt.Syntax in
    let ( let** ) = Lwt_result.bind in
    let state_dir = Current.state_dir "indexes-compile" in
    let output_dir = Current.state_dir "indexes-html" in
    let* () = Current.Job.start ~level:Harmless job in
    let mld =
      Mld.Gen.v
        (List.map
           (fun comp -> (Compile.package comp, Compile.is_blessed comp, Compile.odoc comp))
           packages)
    in
    let remote_folder =
      Fmt.str "%s@@%s:%s/" Config.ssh_user Config.ssh_host Config.storage_folder
    in
    let** () =
      Current.Process.exec ~cancellable:true ~job
        ( "",
          [|
            "rsync";
            "-avzR";
            "--exclude=/*/*/*/*/*/";
            "-e";
            Fmt.str "ssh -p %d -i %a" Config.ssh_port Fpath.pp Config.ssh_priv_key_file;
            remote_folder ^ "./compile";
            Fpath.to_string state_dir;
          |] )
    in
    (* Create files *)
    Bos.OS.File.delete Fpath.(state_dir / "files.sh") |> Result.get_ok;
    Bos.OS.File.write
      Fpath.(state_dir / "files.sh")
      (Fmt.to_to_string Mld.Gen.pp_gen_files_commands mld)
    |> Result.get_ok;
    let** () =
      Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [| "bash"; "./files.sh" |])
    in
    (* Create makefile *)
    Bos.OS.File.delete Fpath.(state_dir / "Makefile") |> Result.get_ok;
    Bos.OS.File.write
      Fpath.(state_dir / "Makefile")
      (Fmt.to_to_string (Mld.Gen.pp_makefile ~odoc:Config.odoc_bin ~output:output_dir) mld)
    |> Result.get_ok;
    let** () =
      Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [| "make"; "roots-compile" |])
    in
    let** () =
      Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [| "make"; Fmt.str "-j%d" Config.jobs; "pages-link"; "roots-link" |])
    in
    let** () =
      Current.Process.exec ~cwd:state_dir ~cancellable:true ~job
        ("", [| Config.odoc_bin; "support-files"; "-o"; Fpath.to_string output_dir |])
    in
    Current.Process.exec ~cancellable:false ~job
      ( "",
        [|
          "rsync";
          "-avzR";
          "-e";
          Fmt.str "ssh -p %d  -i %a" Config.ssh_port Fpath.pp Config.ssh_priv_key_file;
          Fpath.to_string output_dir ^ "/./";
          remote_folder ^ "/html/";
        |] )
end

module IndexesCache = Current_cache.Make (Indexes)

let v packages =
  let open Current.Syntax in
  Current.component "index2"
  |> let> packages = packages in
     IndexesCache.get No_context packages
