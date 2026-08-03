type t = { ty : string; src : string; dst : string; options : string list }

let to_json { ty; src; dst; options } =
  `Assoc
    [
      ("destination", `String dst);
      ("type", `String ty);
      ("source", `String src);
      ("options", `List (List.map (fun x -> `String x) options));
    ]

let bind_ro ~src dst =
  { ty = "bind"; src; dst; options = [ "ro"; "rbind"; "rprivate" ] }

let bind_rw ~src dst =
  { ty = "bind"; src; dst; options = [ "rw"; "rbind"; "rprivate" ] }
