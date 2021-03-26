type t = unit

module Indexes = struct
  type t = No_context

  let id = "indexes-build"

  let pp f _ = Fmt.pf f "indexes build"

  let auto_cancel = false

  module Key = struct
    type t = Compile.t list

    let digest t = List.rev_map Compile.digest t |> String.concat "\n" |> Digest.string |> Digest.to_hex
  end

  module Value = struct 
    type t = unit let marshal () = "" let unmarshal _ = () end

  let build No_context job packages = 
    let open Lwt.Syntax in
    let (let**) = Lwt_result.bind in
    let state_dir = Current.state_dir "indexes-compile" in
    let output_dir = Current.state_dir "indexes-html" in
    let* () = Current.Job.start ~level:Harmless job  in
    let mld =
      Mld.Gen.v
        (List.map
           (fun comp -> (Compile.package comp, Compile.is_blessed comp, Compile.odoc comp))
           packages)
    in
    let** () = Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [|"rsync"; "-avzR"; "--exclude=/*/*/*/*/*/"; "mirage-ci:/home/docs/docs-ci/./compile"; "./"|]) in
    Bos.OS.File.write Fpath.(state_dir / "Makefile") (Fmt.to_to_string Mld.Gen.pp_makefile mld) |> Result.get_ok;
    let** () = Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [|"make"; "mlds"; |]) in
    let** () = Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [|"opam"; "exec"; "--switch=412"; "--"; "make"; "odoc"|]) in
    let** () = Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [|"opam"; "exec"; "--switch=412"; "--"; "make"; "odocl"|]) in
    let** () = Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [|"opam"; "exec"; "--switch=412"; "--"; "find"; "-maxdepth"; "4"; "-type"; "f"; "-name"; "'*.odocl'"; "-exec"; "odoc"; "html"; "-o"; Fpath.to_string output_dir; "{}"; ";"|]) in
    let** () = Current.Process.exec ~cwd:state_dir ~cancellable:true ~job ("", [|"opam"; "exec"; "--switch=412"; "--"; "odoc"; "support-files"; "-o"; Fpath.to_string output_dir|]) in
    Lwt.return_ok ()

end

module IndexesCache = Current_cache.Make(Indexes)

(*          run "find . -type f -name '*.cmt' -exec sh -c 'mv \"$1\" \"${1%%.cmt}.odoc\"' _ {} \\;"; *)
let v2 packages =
  let open Current.Syntax in
  Current.component "index2" |>
  let> packages = packages in
  IndexesCache.get No_context packages
