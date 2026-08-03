type entry = { size : int64; mtime : float; inode : int64 }
type t = (string, entry) Hashtbl.t

let is_empty t = Hashtbl.length t = 0
let size t = Hashtbl.length t

let entry_of_stat (st : Eio.File.Stat.t) : entry =
  { size = Optint.Int63.to_int64 st.size; mtime = st.mtime; inode = st.ino }

let eio_path env p = Eio.Path.(env#fs / Fpath.to_string p)

let walk env root =
  let tbl : t = Hashtbl.create 4096 in
  let root_s = Fpath.to_string root in
  let root_len = String.length root_s in
  let rec scan dir =
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
                | `Directory -> scan abs
                | `Regular_file | `Symbolic_link -> (
                    let rel =
                      (* strip [root/] prefix; if the abs path doesn't start
                   with root_s (unlikely), fall back to using abs *)
                      if
                        String.length abs > root_len
                        && String.sub abs 0 root_len = root_s
                        && abs.[root_len] = '/'
                      then
                        String.sub abs (root_len + 1)
                          (String.length abs - root_len - 1)
                      else abs
                    in
                    (* Use stat (following symlinks) so the recorded entry
                 reflects what the file resolves to. An in-place
                 replacement of a file changes inode, even if mtime
                 is carried over. *)
                    match Eio.Path.stat ~follow:true abs_ep with
                    | st -> Hashtbl.replace tbl rel (entry_of_stat st)
                    | exception _ ->
                        (* Broken symlink: record the lstat entry. *)
                        Hashtbl.replace tbl rel (entry_of_stat st))
                | _ -> ()))
          entries
  in
  if Eio.Path.is_directory (eio_path env root) then scan root_s;
  tbl

let take env root = walk env root

let diff env ~before root =
  let after = walk env root in
  Hashtbl.fold
    (fun rel (e : entry) acc ->
      match Hashtbl.find_opt before rel with
      | None -> rel :: acc
      | Some (e0 : entry) ->
          if e.size = e0.size && e.mtime = e0.mtime && e.inode = e0.inode then
            acc
          else rel :: acc)
    after []
