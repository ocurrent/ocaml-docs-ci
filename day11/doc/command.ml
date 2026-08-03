let compute_universe_hash dep_hashes =
  let sorted = List.sort String.compare dep_hashes in
  Day11_layer.Hash.of_strings sorted

let odoc_driver_voodoo ~pkg ~universe:_ ~blessed ~actions ~odoc_bin ~odoc_md_bin
    =
  let pkg_name = OpamPackage.name_to_string pkg in
  Printf.sprintf
    "odoc_driver_voodoo %s --odoc-dir /home/opam/odoc-out --html-dir \
     /home/opam/html --actions %s -j $(nproc) -v %s --odoc %s --odoc-md %s"
    pkg_name actions
    (if blessed then "--blessed" else "")
    odoc_bin odoc_md_bin
