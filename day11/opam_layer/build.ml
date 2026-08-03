type t = {
  hash : string;
  pkg : OpamPackage.t;
  deps : t list;
  universe : Day11_solution.Universe.t;
}

let dir_name b = Day11_layer.Dir.name b.hash
let dir ~os_dir b = Day11_layer.Dir.path ~os_dir b.hash
let layer ~os_dir b = Day11_layer.Layer.of_hash ~os_dir b.hash
