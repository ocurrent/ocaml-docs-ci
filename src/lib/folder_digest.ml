
let id = "digest-cache"

let state_dir = Current.state_dir id

let sync ~job () = 
  let remote_folder =
    Fmt .str "%s@@%s:%s/" Config.ssh_user Config.ssh_host Config.storage_folder
  in
  Current.Process.exec ~cancellable:true ~job ("", [|
"rsync"; "-avzR"; "-e"; Fmt.str "ssh -p %d -i %a" Config.ssh_port Fpath.pp Config.ssh_priv_key_file; remote_folder ^ "/digests/./"; Fpath.to_string state_dir|]) 

let get path = 
  Printf.printf "Path: %s\n" (Fpath.(to_string (state_dir // path)));
  Bos.OS.File.read Fpath.(state_dir // add_ext ".sha256" path ) |> Result.to_option |> Option.map String.trim

let compute paths =
  let pp_compute_digest f folder =
    Fmt.pf f
      "(mkdir -p digests/%a && (find %a/ -type f -exec sha256sum {} \\;) | sort -k 2 | \
       sha256sum > digests/%a.sha256)"
      Fpath.pp (Fpath.parent folder)
      Fpath.pp folder Fpath.pp folder
  in
  Fmt.(str "%a" (list ~sep:(any " && ") pp_compute_digest) paths)

