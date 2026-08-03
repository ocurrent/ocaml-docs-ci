(* -- Substring search ---------------------------------------------------- *)

let find_all ~needle hay =
  let nlen = String.length needle in
  let hlen = String.length hay in
  if nlen = 0 || nlen > hlen then []
  else
    let rec aux i acc =
      if i > hlen - nlen then List.rev acc
      else if String.sub hay i nlen = needle then aux (i + nlen) (needle :: acc)
      else aux (i + 1) acc
    in
    aux 0 []

let eio_path env p = Eio.Path.(env#fs / Fpath.to_string p)

let scan_file env path needles =
  match Eio.Path.load (eio_path env path) with
  | exception _ -> []
  | content -> List.concat_map (fun needle -> find_all ~needle content) needles

(* -- Tree walk ----------------------------------------------------------- *)

let scan env ~layer_fs ~forbidden =
  if forbidden = [] then []
  else
    let results = ref [] in
    let root = Fpath.to_string layer_fs in
    let root_len = String.length root in
    let rec walk dir =
      let dir_ep = eio_path env (Fpath.v dir) in
      match Eio.Path.read_dir dir_ep with
      | exception _ -> ()
      | entries ->
          List.iter
            (fun name ->
              let abs = Filename.concat dir name in
              let abs_ep = eio_path env (Fpath.v abs) in
              match Eio.Path.stat ~follow:false abs_ep with
              | exception _ -> ()
              | (st : Eio.File.Stat.t) -> (
                  match st.kind with
                  | `Directory -> walk abs
                  | `Regular_file ->
                      let hits = scan_file env (Fpath.v abs) forbidden in
                      if hits <> [] then
                        let rel =
                          if
                            String.length abs > root_len
                            && String.sub abs 0 root_len = root
                            && abs.[root_len] = '/'
                          then
                            String.sub abs (root_len + 1)
                              (String.length abs - root_len - 1)
                          else abs
                        in
                        results := (rel, hits) :: !results
                  | _ -> ()))
            entries
    in
    if Eio.Path.is_directory (eio_path env layer_fs) then walk root;
    List.rev !results

(* -- Persistence --------------------------------------------------------- *)

let to_json t : Yojson.Safe.t =
  `Assoc
    (List.map
       (fun (path, hits) -> (path, `List (List.map (fun s -> `String s) hits)))
       t)

let of_json (j : Yojson.Safe.t) : (string * string list) list option =
  match j with
  | `Assoc items -> (
      try
        Some
          (List.map
             (fun (k, v) ->
               match v with
               | `List strs ->
                   let hits =
                     List.map (function `String s -> s | _ -> raise Exit) strs
                   in
                   (k, hits)
               | _ -> raise Exit)
             items)
      with Exit -> None)
  | _ -> None

let save env path t =
  let json = to_json t in
  try
    Eio.Path.save ~create:(`Or_truncate 0o644) (eio_path env path)
      (Yojson.Safe.pretty_to_string json);
    Ok ()
  with exn ->
    Rresult.R.error_msgf "save %a: %s" Fpath.pp path (Printexc.to_string exn)

let load env path =
  match Eio.Path.load (eio_path env path) with
  | exception _ -> None
  | content -> ( try of_json (Yojson.Safe.from_string content) with _ -> None)
