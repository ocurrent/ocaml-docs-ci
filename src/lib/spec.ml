type t = { base : string; ops : Obuilder_spec.op list }

let to_json { base; ops } =
  `Tuple
    [ `String base; `List (List.map (fun op -> `String (Fmt.str "%a" Obuilder_spec.pp_op op)) ops) ]

let add next_ops { base; ops } = { base; ops = ops @ next_ops }

let finish { base; ops } = Obuilder_spec.stage ~from:base ops

let make base = { base; ops = [] }
