let rec take n lst =
  match (n, lst) with
  | 0, _ -> []
  | _, [] -> []
  | n, a :: q -> a :: take (n - 1) q

let take = function Some n -> take n | None -> Fun.id

let get_file path =
  Lwt_io.with_file ~mode:Input (Fpath.to_string path) Lwt_io.read

let get_versions ~limit path =
  let open Lwt.Syntax in
  let open Rresult in
  Bos.OS.Dir.contents path
  >>| (fun versions ->
        versions
        |> Lwt_list.map_p (fun path ->
               let+ content = get_file Fpath.(path / "opam") in
               ( path |> Fpath.basename |> OpamPackage.of_string,
                 Digest.(string content |> to_hex) )))
  |> Result.get_ok
  |> Lwt.map (fun v ->
         v
         |> List.sort (fun a b -> -OpamPackage.compare (fst a) (fst b))
         |> take limit)

let dir = Fpath.v "/home/alpha/hack/draft-clone/opam-repository"
let limit = Some 2000

let result =
  let open Lwt.Syntax in
  let open Rresult in
  Bos.OS.Dir.contents Fpath.(dir / "packages") >>| fun packages ->
  packages
  |> Lwt_list.map_s (get_versions ~limit)
  |> Lwt.map (fun v -> List.flatten v)
  |> Lwt.map (fun v -> Fmt.pr "packages: %d" (List.length v))

let () =
  match result with
  | Ok rr -> Lwt_main.run rr
  | _ -> Fmt.failwith "Error happend"
