let ( / ) = Filename.concat

module SS = Set.Make (String)

let remove ~root f files num =
  let _ =
    SS.fold
      (fun del (i, l) ->
        f 1;
        let pcent = Int.div (100 * i) num in
        let nl = if pcent > l then pcent else l in
        Unix.sleepf 0.025;
        let _ = Sys.command ("rm -rf " ^ (root / del)) in
        (i + 1, nl))
      files (0, 0)
  in
  ()

let print files =
  let total =
    SS.fold
      (fun del i ->
        let () = if i < 10 then print_endline @@ Fmt.str "%s" del in
        i + 1)
      files 0
  in
  if total >= 10 then print_endline @@ Fmt.str "... plus %i more\n" (total - 10)

let bar ~total =
  let open Progress in
  let open Progress.Line in
  let frames = [ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ] in
  list
    [
      spinner ~frames ~color:(Color.ansi `green) ();
      bar ~color:(Color.ansi `blue) ~style:`UTF8 total;
      count_to total;
    ]

let main base_dir dry_run silent =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;

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
    |> List.filter Sys.file_exists
  in

  let epoch_files =
    List.fold_left
      (fun s epoch -> Array.fold_right SS.add (Sys.readdir epoch) s)
      SS.empty epochs
  in

  List.iter
    (fun universe ->
      let universes = path / universe in
      let univ_files =
        Array.fold_right SS.add (Sys.readdir universes) SS.empty
      in
      let debris = SS.diff univ_files epoch_files in
      let total = SS.elements debris |> List.length in
      let () = print_endline @@ Fmt.str "Files to be deleted in %s" universe in
      let () = print debris in
      let num = SS.cardinal debris in
      if not dry_run then (
        print_endline (Fmt.str "Deleting %i files in %s" num universe);

        match silent with
        | false ->
            Progress.with_reporter (bar ~total) (fun f ->
                remove ~root:universes f debris num)
        | true -> remove ~root:universes (fun _ -> ()) debris num))
    [ "prep/universes"; "compile/u" ]

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

let dry_run =
  Arg.(
    value
    @@ flag
    @@ info ~docv:"DRY_RUN"
         ~doc:
           "If set, only list the files to be deleted but do not deleted them"
         [ "dry-run" ])

let version =
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v

let silent =
  Arg.(
    value
    @@ flag
    @@ info ~docv:"SILENT"
         ~doc:"Run epoch tool silently emitting no progress bars." [ "s" ])

let cmd =
  let doc = "Epoch pruning" in
  let info = Cmd.info "epoch" ~doc ~version in
  Cmd.v info Term.(const main $ base_dir $ dry_run $ silent)

let () = exit @@ Cmd.eval cmd
