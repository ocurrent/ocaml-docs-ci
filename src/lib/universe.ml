type t = { hash : string; deps : OpamPackage.t list }

let hash t = t.hash

let deps t = t.deps

let v deps =
  let str =
    deps |> List.sort OpamPackage.compare
    |> List.fold_left (fun acc p -> Format.asprintf "%s\n%s" acc (OpamPackage.to_string p)) ""
  in
  let hash = Digest.to_hex (Digest.string str) in
  { hash; deps }

let pp f { hash; _ } = Fmt.pf f "%s" hash

let compare { hash; _ } { hash = hash2; _ } = String.compare hash hash2
