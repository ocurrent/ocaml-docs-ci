type t = {
  terminal : bool;
  cwd : string;
  hostname : string;
  env : (string * string) list;
  mounts : Mount.t list;
  network : bool;
  argv : string list;
  uid : int;
  gid : int;
  cpuset : string option;
  numa_mems : string option;
}

let make ?(terminal = false) ?(cwd = "/") ?(hostname = "container") ?(env = [])
    ?(mounts = []) ?(network = false) ?cpuset ?numa_mems ~argv ~uid ~gid () : t
    =
  {
    terminal;
    cwd;
    hostname;
    env;
    mounts;
    network;
    argv;
    uid;
    gid;
    cpuset;
    numa_mems;
  }

let default_linux_caps =
  [
    "CAP_CHOWN";
    "CAP_DAC_OVERRIDE";
    "CAP_FSETID";
    "CAP_FOWNER";
    "CAP_MKNOD";
    "CAP_SETGID";
    "CAP_SETUID";
    "CAP_SETFCAP";
    "CAP_SETPCAP";
    "CAP_SYS_CHROOT";
    "CAP_KILL";
    "CAP_AUDIT_WRITE";
  ]

let strings xs = `List (List.map (fun x -> `String x) xs)

let to_yojson ~root (t : t) : Yojson.Safe.t =
  `Assoc
    [
      ("ociVersion", `String "1.0.1-dev");
      ( "process",
        `Assoc
          [
            ("terminal", `Bool t.terminal);
            ("user", `Assoc [ ("uid", `Int t.uid); ("gid", `Int t.gid) ]);
            ("args", strings t.argv);
            ( "env",
              strings
                (List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) t.env) );
            ("cwd", `String t.cwd);
            ( "capabilities",
              `Assoc
                [
                  ("bounding", strings default_linux_caps);
                  ("effective", strings default_linux_caps);
                  ("inheritable", strings default_linux_caps);
                  ("permitted", strings default_linux_caps);
                ] );
            (* 1024 is too low for doc containers: odoc_driver_voodoo
          holds open files across a whole universe's worth of .odoc
          units. *)
            ( "rlimits",
              `List
                [
                  `Assoc
                    [
                      ("type", `String "RLIMIT_NOFILE");
                      ("hard", `Int 65536);
                      ("soft", `Int 65536);
                    ];
                ] );
            ("noNewPrivileges", `Bool false);
          ] );
      ("root", `Assoc [ ("path", `String root); ("readonly", `Bool false) ]);
      ("hostname", `String t.hostname);
      ( "mounts",
        `List
          (List.map Mount.to_json t.mounts
          @ [
              Mount.(
                to_json
                  {
                    ty = "proc";
                    src = "proc";
                    dst = "/proc";
                    options = [ "nosuid"; "noexec"; "nodev" ];
                  });
              Mount.(
                to_json
                  {
                    ty = "tmpfs";
                    src = "tmpfs";
                    dst = "/tmp";
                    options =
                      [ "nosuid"; "noatime"; "nodev"; "noexec"; "mode=1777" ];
                  });
              Mount.(
                to_json
                  {
                    ty = "tmpfs";
                    src = "tmpfs";
                    dst = "/dev";
                    options =
                      [ "nosuid"; "strictatime"; "mode=755"; "size=65536k" ];
                  });
              Mount.(
                to_json
                  {
                    ty = "devpts";
                    src = "devpts";
                    dst = "/dev/pts";
                    options =
                      [
                        "nosuid";
                        "noexec";
                        "newinstance";
                        "ptmxmode=0666";
                        "mode=0620";
                        "gid=5";
                      ];
                  });
              Mount.(
                to_json
                  {
                    ty = "sysfs";
                    src = "sysfs";
                    dst = "/sys";
                    options = [ "nosuid"; "noexec"; "nodev"; "ro" ];
                  });
              Mount.(
                to_json
                  {
                    ty = "cgroup";
                    src = "cgroup";
                    dst = "/sys/fs/cgroup";
                    options = [ "ro"; "nosuid"; "noexec"; "nodev" ];
                  });
              Mount.(
                to_json
                  {
                    ty = "tmpfs";
                    src = "shm";
                    dst = "/dev/shm";
                    options =
                      [
                        "nosuid"; "noexec"; "nodev"; "mode=1777"; "size=65536k";
                      ];
                  });
              Mount.(
                to_json
                  {
                    ty = "mqueue";
                    src = "mqueue";
                    dst = "/dev/mqueue";
                    options = [ "nosuid"; "noexec"; "nodev" ];
                  });
            ]
          @
          if t.network then
            [
              Mount.(
                to_json
                  {
                    ty = "bind";
                    src = "/etc/resolv.conf";
                    dst = "/etc/resolv.conf";
                    options = [ "ro"; "rbind"; "rprivate" ];
                  });
            ]
          else []) );
      ( "linux",
        `Assoc
          ((* cgroup cpuset/mems — emitted only when set, so unconfigured
          containers stay identical to the pre-NUMA spec. *)
           (match (t.cpuset, t.numa_mems) with
           | None, None -> []
           | _ ->
               let cpu_kv =
                 List.filter_map
                   (fun (k, v) -> Option.map (fun s -> (k, `String s)) v)
                   [ ("cpus", t.cpuset); ("mems", t.numa_mems) ]
               in
               [ ("resources", `Assoc [ ("cpu", `Assoc cpu_kv) ]) ])
          @ [
              ( "namespaces",
                `List
                  (List.map
                     (fun ns -> `Assoc [ ("type", `String ns) ])
                     ((if t.network then [] else [ "network" ])
                     @ [ "pid"; "ipc"; "uts"; "mount" ])) );
              ( "maskedPaths",
                strings
                  [
                    "/proc/acpi";
                    "/proc/asound";
                    "/proc/kcore";
                    "/proc/keys";
                    "/proc/latency_stats";
                    "/proc/timer_list";
                    "/proc/timer_stats";
                    "/proc/sched_debug";
                    "/sys/firmware";
                    "/proc/scsi";
                  ] );
              ( "readonlyPaths",
                strings
                  [
                    "/proc/bus";
                    "/proc/fs";
                    "/proc/irq";
                    "/proc/sys";
                    "/proc/sysrq-trigger";
                  ] );
              ( "seccomp",
                `Assoc
                  [
                    ("defaultAction", `String "SCMP_ACT_ALLOW");
                    ( "syscalls",
                      `List
                        [
                          `Assoc
                            [
                              ( "names",
                                strings
                                  [
                                    "fsync";
                                    "fdatasync";
                                    "msync";
                                    "sync";
                                    "syncfs";
                                    "sync_file_range";
                                  ] );
                              ("action", `String "SCMP_ACT_ERRNO");
                              ("errnoRet", `Int 0);
                            ];
                        ] );
                    ( "architectures",
                      strings
                        [ "SCMP_ARCH_X86_64"; "SCMP_ARCH_X86"; "SCMP_ARCH_X32" ]
                    );
                  ] );
            ]) );
    ]

let write ~root bundle_dir t =
  let path = Fpath.(bundle_dir / "config.json") in
  try
    Yojson.Safe.to_file (Fpath.to_string path) (to_yojson ~root t);
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Oci_spec.write %a: %s" Fpath.pp path
      (Printexc.to_string exn)

let placeholder_root = "<rootfs>"

let write_template path t =
  try
    Yojson.Safe.to_file (Fpath.to_string path)
      (to_yojson ~root:placeholder_root t);
    Ok ()
  with exn ->
    Rresult.R.error_msgf "Oci_spec.write_template %a: %s" Fpath.pp path
      (Printexc.to_string exn)

(* A template config.json is concrete JSON in which only [root.path]
   is parameterized (= [placeholder_root]). Instantiating it is just a
   targeted substitution of that one field — no need to parse the whole
   spec back into a [t]. We validate the shape so we never silently
   accept a non-template (or already-instantiated) config.json. *)
let instantiate_template ~root (json : Yojson.Safe.t) =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "root" fields with
      | Some (`Assoc root_fields) -> (
          match List.assoc_opt "path" root_fields with
          | Some (`String p) when p = placeholder_root ->
              let root_fields' =
                List.map
                  (fun (k, v) ->
                    if k = "path" then (k, `String root) else (k, v))
                  root_fields
              in
              let fields' =
                List.map
                  (fun (k, v) ->
                    if k = "root" then (k, `Assoc root_fields') else (k, v))
                  fields
              in
              Ok (`Assoc fields')
          | Some (`String p) ->
              Rresult.R.error_msgf
                "Oci_spec.instantiate_template: root.path is %S, not the \
                 template placeholder %S"
                p placeholder_root
          | _ ->
              Rresult.R.error_msg
                "Oci_spec.instantiate_template: root.path missing or not a \
                 string")
      | _ -> Rresult.R.error_msg "Oci_spec.instantiate_template: no root object"
      )
  | _ ->
      Rresult.R.error_msg
        "Oci_spec.instantiate_template: config.json is not a JSON object"

let instantiate_template_file ~template ~root ~bundle_dir =
  match
    try Ok (Yojson.Safe.from_file (Fpath.to_string template))
    with exn ->
      Rresult.R.error_msgf "Oci_spec.instantiate_template_file: read %a: %s"
        Fpath.pp template (Printexc.to_string exn)
  with
  | Error _ as e -> e
  | Ok json -> (
      match instantiate_template ~root json with
      | Error _ as e -> e
      | Ok json -> (
          let path = Fpath.(bundle_dir / "config.json") in
          try
            Yojson.Safe.to_file (Fpath.to_string path) json;
            Ok ()
          with exn ->
            Rresult.R.error_msgf
              "Oci_spec.instantiate_template_file: write %a: %s" Fpath.pp path
              (Printexc.to_string exn)))
