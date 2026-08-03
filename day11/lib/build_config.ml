type t = {
  opam_repositories : Fpath.t list;
  local_repos : (Fpath.t * string list) list;
  with_doc : bool;
  with_jtw : bool;
  html_output : Fpath.t option;
  jtw_output : Fpath.t option;
}

let save path t =
  let json =
    `Assoc
      [
        ( "opam_repositories",
          `List
            (List.map
               (fun p -> `String (Fpath.to_string p))
               t.opam_repositories) );
        ( "local_repos",
          `List
            (List.map
               (fun (p, pkgs) ->
                 `Assoc
                   [
                     ("path", `String (Fpath.to_string p));
                     ("packages", `List (List.map (fun s -> `String s) pkgs));
                   ])
               t.local_repos) );
        ("with_doc", `Bool t.with_doc);
        ("with_jtw", `Bool t.with_jtw);
        ( "html_output",
          match t.html_output with
          | Some p -> `String (Fpath.to_string p)
          | None -> `Null );
        ( "jtw_output",
          match t.jtw_output with
          | Some p -> `String (Fpath.to_string p)
          | None -> `Null );
      ]
  in
  Bos.OS.File.write path (Yojson.Safe.to_string json)

let load path =
  match Bos.OS.File.read path with
  | Error _ as e -> e
  | Ok data -> (
      try
        let json = Yojson.Safe.from_string data in
        let open Yojson.Safe.Util in
        let opam_repositories =
          json
          |> member "opam_repositories"
          |> to_list
          |> List.map (fun j -> Fpath.v (to_string j))
        in
        let local_repos =
          json
          |> member "local_repos"
          |> to_list
          |> List.map (fun j ->
                 ( Fpath.v (j |> member "path" |> to_string),
                   j |> member "packages" |> to_list |> List.map to_string ))
        in
        let with_doc = json |> member "with_doc" |> to_bool in
        let with_jtw = json |> member "with_jtw" |> to_bool in
        let html_output =
          match json |> member "html_output" with
          | `Null -> None
          | j -> Some (Fpath.v (to_string j))
        in
        let jtw_output =
          match json |> member "jtw_output" with
          | `Null -> None
          | j -> Some (Fpath.v (to_string j))
        in
        Ok
          {
            opam_repositories;
            local_repos;
            with_doc;
            with_jtw;
            html_output;
            jtw_output;
          }
      with exn ->
        Rresult.R.error_msgf "Build_config.load: %s" (Printexc.to_string exn))
