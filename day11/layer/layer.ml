type t = { hash : string; dir : Fpath.t }

let of_hash ~os_dir hash =
  let len = min 12 (String.length hash) in
  let name = String.sub hash 0 len in
  { hash; dir = Fpath.(os_dir / name) }

let hash t = t.hash
let dir t = t.dir
let fs t = Fpath.(t.dir / "fs")
let meta_path t = Fpath.(t.dir / "layer.json")
let log_path t = Fpath.(t.dir / "layer.log")
let pp f t = Fmt.pf f "%s" (String.sub t.hash 0 (min 12 (String.length t.hash)))

let exists env t =
  Eio.Path.is_file Eio.Path.(env#fs / Fpath.to_string (meta_path t))

let is_ok env t =
  match Meta.load env (meta_path t) with
  | Ok meta -> meta.exit_status = 0
  | Error _ -> false
