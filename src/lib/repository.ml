type t = string * Current_git.Commit_id.t
(** An opam repository and its name *)

type fetched = string * Current_git.Commit.t

let pp f (v1, v2) = Fmt.pf f "%s:%a%a" v1 Fmt.cut () Current_git.Commit_id.pp v2

let compare (a1, b1) (a2, b2) =
  match String.compare a1 a2 with 0 -> Current_git.Commit_id.compare b1 b2 | v -> v

let fetch c =
  let open Current.Syntax in
  let name =
    let+ name, _ = c in
    name
  in
  let id =
    let+ _, id = c in
    id
  in
  let commit = Current_git.fetch id in
  Current.pair name commit

let unfetch (name, v) = (name, Current_git.Commit.id v)

let current_list_unfetch lst =
  let open Current.Syntax in
  let+ lst = lst in
  List.map unfetch lst
