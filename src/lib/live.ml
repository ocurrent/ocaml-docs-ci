module Ssh = Config.Ssh

let set_current ~ssh name kind generation =
  let open Current.Syntax in
  Current.component "Set current folder"
  |> let> generation = generation in
     let new_generation_folder = Storage.Base.generation_folder kind generation in
     let storage_folder = Fpath.(of_string (Ssh.storage_folder ssh) |> Result.get_ok) in
     let target = Fpath.(storage_folder // new_generation_folder) in
     let name = Fpath.(storage_folder / (name ^ "-current")) in
     Symlink.remote_symbolic_link ~level:Harmless ~ssh ~target ~name ()

let set_live ~ssh name kind generation =
  let open Current.Syntax in
  Current.component "Set live folder"
  |> let> generation = generation in
     let new_generation_folder = Storage.Base.generation_folder kind generation in
     let storage_folder = Fpath.(of_string (Ssh.storage_folder ssh) |> Result.get_ok) in
     let target = Fpath.(storage_folder // new_generation_folder) in
     let name = Fpath.(storage_folder / (name ^ "-live")) in
     Symlink.remote_symbolic_link ~level:Dangerous ~ssh ~target ~name ()

let set_to ~ssh name kind generation =
  Current.all [ set_current ~ssh name kind generation; set_live ~ssh name kind generation ]
