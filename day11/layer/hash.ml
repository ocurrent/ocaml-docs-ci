let of_strings parts =
  let s = String.concat "\000" parts in
  Digest.string s |> Digest.to_hex

let base_hash ~image = of_strings [ "base"; image ]

let layer_hash ~base_hash ~dep_hashes ~pkg =
  of_strings ([ "layer"; base_hash; pkg ] @ dep_hashes)
