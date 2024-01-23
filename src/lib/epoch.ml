type t = { voodoo : Voodoo.t }

let version = "v1"
let v voodoo = { voodoo }

type stage = [ `Linked | `Html ]

let digest stage t =
  let key =
    match stage with
    | `Html ->
        Fmt.str "%s:%s:%s:%s" version
          Voodoo.Do.(v t.voodoo |> digest)
          Voodoo.Prep.(v t.voodoo |> digest)
          Voodoo.Gen.(v t.voodoo |> digest)
    | `Linked ->
        Fmt.str "%s:%s:%s" version
          Voodoo.Do.(v t.voodoo |> digest)
          Voodoo.Prep.(v t.voodoo |> digest)
  in
  key |> Digest.string |> Digest.to_hex

let pp f t =
  Fmt.pf f
    "docs-ci: %s\nvoodoo do: %a\nvoodoo prep: %a\nvoodoo gen: %a"
    version Current_git.Commit_id.pp
    Voodoo.Do.(v t.voodoo |> commit)
    Current_git.Commit_id.pp
    Voodoo.Prep.(v t.voodoo |> commit)
    Current_git.Commit_id.pp
    Voodoo.Gen.(v t.voodoo |> commit)
