let ( / ) = Filename.concat

module SS = Set.Make (String)

let main base_dir =
  let path = base_dir in

  let epochs =
    Sys.readdir path
    |> Array.to_list
    |> List.filter (fun file -> String.starts_with ~prefix:"epoch-" file)
    |> List.fold_left
         (fun acc epoch ->
           let full_path = path / epoch in
           List.map (fun sf -> full_path / sf) [ "html-raw/u"; "linked/u" ]
           @ acc)
         []
    |> List.filter Sys.file_exists in

  let epoch_files = List.fold_left (fun s epoch -> Array.fold_right SS.add (Sys.readdir epoch) s) SS.empty epochs in

  (* Prep universes *)
  let universes = path / "/prep/universes" in
  let prep_files = Array.fold_right SS.add (Sys.readdir universes) SS.empty in
  let debris = SS.diff prep_files epoch_files in
  Printf.printf "Files to be deleted in prep: ";
  SS.iter (fun file -> Printf.printf "%s " file) debris;
  Printf.printf "\n";
  debris |> SS.iter (fun del -> ignore @@ Sys.command ("rm -rf " ^ (universes / del)));

  (* Compile universes. *)
  let universes = path / "/compile/u" in
  let comp_files = Array.fold_right SS.add (Sys.readdir universes) SS.empty in
  let debris = SS.diff comp_files epoch_files in
  Printf.printf "Files to be deleted in compile: ";
  SS.iter (fun file -> Printf.printf "%s " file) debris;
  Printf.printf "\n";
  debris |> SS.iter (fun del -> ignore @@ Sys.command ("rm -rf " ^ (universes / del)))

(* Command-line parsing *)

open Cmdliner

let base_dir =
  Arg.(
    required
    @@ opt (some dir) None
    @@ info ~docv:"BASE_DIR"
         ~doc:
           "Base directory containing epochs. eg \
            /var/lib/docker/volumes/infra_docs-data/_data"
         [ "base-dir" ])

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let cmd =
  let doc = "Epoch pruning" in
  let info = Cmd.info "epoch" ~doc ~version in
  Cmd.v info Term.(const main $ base_dir)

let () = exit @@ Cmd.eval cmd
