type t = { config : Config.t }

let version = "v2"
let v config = { config }

let digest t =
  let key =
    Fmt.str "%s:%s:%s" version (Config.odoc t.config)
      (Config.sherlodoc t.config)
  in
  key |> Digest.string |> Digest.to_hex

let pp f t =
  Fmt.pf f "docs-ci: %s\nodoc: %s\nsherlodoc: %s" version (Config.odoc t.config)
    (Config.sherlodoc t.config)
