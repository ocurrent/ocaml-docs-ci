let extras universes ~path =
  let epochs = Sys.readdir path
               |> Array.to_list
               |> List.filter_map (fun file ->
                      if (String.starts_with ~prefix:"epoch-" file)
                      then Some (Filename.concat path file)
                      else None) in
  let all_files = List.fold_left (fun acc epoch ->
                      acc @ ( ["html-raw/u"; "linked/u"]
                              |> List.filter_map (fun sf ->
                                     let sf = Filename.concat epoch sf in
                                     if Sys.file_exists sf then
                                       Some (Sys.readdir sf |> Array.to_list)
                                     else None)
                              |> List.concat)) [] epochs in
  let all_files = List.sort_uniq compare all_files in
  let files = Sys.readdir universes in
  Array.fold_left (fun acc file ->
      match List.exists (fun f -> String.equal f file) all_files with
      | false -> file :: acc
      | true -> acc) [] files
  |> List.sort_uniq compare

let main base_dir =
  let path = base_dir in

  (* Prep universes *)
  let universes =  path ^ "/prep/universes" in
  let debris = (extras universes ~path) in
  Printf.printf "Files to be deleted in prep: ";
  List.iter (fun file -> Printf.printf "%s " file) debris;
  Printf.printf "\n";
  debris |> List.iter (fun del -> ignore @@ Sys.command ("rm -rf " ^ (Filename.concat universes del)));

  (* Compile universes. *)
  let universes = path ^ "/compile/u" in
  let debris = (extras universes ~path) in
  Printf.printf "Files to be deleted in compile: ";
  List.iter (fun file -> Printf.printf "%s " file) debris;
  Printf.printf "\n";
  debris |> List.iter (fun del -> ignore @@ Sys.command ("rm -rf " ^ (Filename.concat universes del)))

(* Command-line parsing *)

open Cmdliner

let base_dir =
  Arg.(required
       @@ opt (some dir) None
       @@ info ~docv:"BASE_DIR"
            ~doc: "Base directory containing epochs. eg /var/lib/docker/volumes/infra_docs-data/_data" ["base-dir"])

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "Epoch pruning" in
  let info = Cmd.info "epoch" ~doc ~version in
  Cmd.v info
    Term.(const main $ base_dir)

let () = exit @@ Cmd.eval cmd