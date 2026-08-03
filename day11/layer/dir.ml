let name hash =
  let len = min 12 (String.length hash) in
  String.sub hash 0 len

let path ~os_dir hash = Fpath.(os_dir / name hash)
