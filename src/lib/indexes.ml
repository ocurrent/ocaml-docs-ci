let id = "indexes"

let state_dir = Current.state_dir id

module Index = struct

  type t = Config.Ssh.t

  let id = "update-status-metadata"

  let auto_cancel = true

  module Key = struct
    type t = string
    

  let digest package_name =
    Format.asprintf "status-%s" package_name 

  end

  module Value = struct
   type t = (string * Web.Status.t) list

   let digest v =
    Format.asprintf "%a" (Fmt.list (Fmt.pair Format.pp_print_string Web.Status.pp)) v
  end

  let pp f (package_name, _) = Fmt.pf f "Status update package %s" package_name

  module Outcome = Current.Unit

  type versions = { 
    version : string;
    link : string;
    status : string;
  } [@@deriving yojson]

  type v_list = versions list [@@deriving yojson]


  let mkdir_p d =
    let segs = Fpath.segs (Fpath.normalize d) |> List.filter (fun s -> String.length s > 0) in
    let _ = List.fold_left (fun path seg ->
    let d = Fpath.(path // v seg) in
      try Unix.mkdir (Fpath.to_string d) 0o755; d with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> d
      | exn -> raise exn) (Fpath.v ".") segs in
    ()
  
  let publish ssh job package_name v =
    let open Lwt.Syntax in
    let dir = Fpath.(state_dir / package_name) in
    mkdir_p dir;
    let file = Fpath.(dir / "state.json") in
    let ts = List.map (fun (version, status) -> 
      { version;
        link = Format.asprintf "/tailwind/packages/%s/%s/index.html" package_name version; 
        status = Fmt.to_to_string Web.Status.pp status}) v in
    let j = v_list_to_yojson ts in
    let f = open_out (Fpath.to_string file) in
    output_string f (Yojson.Safe.to_string j);
    close_out f;
    let remote_folder =
      Fmt.str "%s@@%s:%s/" (Config.Ssh.user ssh) (Config.Ssh.host ssh) (Config.Ssh.storage_folder ssh)
    in
    let switch = Current.Switch.create ~label:"ssh" () in
  let* () = Current.Job.use_pool ~switch job Remote_cache.ssh_pool in
  let* _ =
    Current.Process.exec ~cancellable:true ~job
      ( "",
        [|
          "rsync";
          "-avzR";
          "-e";
          Fmt.str "ssh -o StrictHostKeyChecking=no -p %d -i %a" (Config.Ssh.port ssh) Fpath.pp (Config.Ssh.priv_key_file ssh);
          remote_folder ^ "/html/tailwind/packages/./";
          Fpath.to_string state_dir;
        |] )
  in
  let* () = Current.Switch.turn_off switch in
  Lwt.return (Ok ())
end

module StatCache = Current_cache.Output (Index)

let v ~ssh ~package_name ~statuses : unit Current.t =
  let open Current.Syntax in
  Current.component "set-status" |>
  let> statuses = statuses in
  StatCache.set ssh package_name statuses