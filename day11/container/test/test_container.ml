(* Tests for the day11_container library.

   Mount and Oci_spec are pure JSON generation — fully unit-testable.
   Overlay and Runc need sudo/runc for real execution, so we only
   test write_spec and error paths here. *)

open Day11_container
open Day11_test_util.Test_util

(* ── Helpers ─────────────────────────────────────────────────────── *)

let is_ok msg r = ok_or_fail msg r |> ignore
let json_member key json = Yojson.Safe.Util.member key json
let json_to_string json = Yojson.Safe.Util.to_string json
let json_to_bool json = Yojson.Safe.Util.to_bool json
let json_to_int json = Yojson.Safe.Util.to_int json
let json_to_list json = Yojson.Safe.Util.to_list json

(* ── Mount tests ─────────────────────────────────────────────────── *)

let test_mount_to_json () =
  let m : Mount.t =
    {
      ty = "bind";
      src = "/host/path";
      dst = "/container/path";
      options = [ "ro"; "rbind" ];
    }
  in
  let json = Mount.to_json m in
  Alcotest.(check string)
    "destination" "/container/path"
    (json |> json_member "destination" |> json_to_string);
  Alcotest.(check string)
    "type" "bind"
    (json |> json_member "type" |> json_to_string);
  Alcotest.(check string)
    "source" "/host/path"
    (json |> json_member "source" |> json_to_string);
  let opts =
    json |> json_member "options" |> json_to_list |> List.map json_to_string
  in
  Alcotest.(check (list string)) "options" [ "ro"; "rbind" ] opts

let test_mount_bind_ro () =
  let m = Mount.bind_ro ~src:"/host" "/container" in
  Alcotest.(check string) "ty" "bind" m.ty;
  Alcotest.(check bool) "has ro" true (List.mem "ro" m.options)

let test_mount_bind_rw () =
  let m = Mount.bind_rw ~src:"/host" "/container" in
  Alcotest.(check string) "ty" "bind" m.ty;
  Alcotest.(check bool) "no ro" false (List.mem "ro" m.options);
  Alcotest.(check bool) "has rw" true (List.mem "rw" m.options)

(* ── Oci_spec tests ──────────────────────────────────────────────── *)

let make_basic_spec ?(network = false) () =
  Oci_spec.make ~cwd:"/home/opam" ~hostname:"builder"
    ~env:[ ("PATH", "/usr/bin"); ("HOME", "/home/opam") ]
    ~network
    ~argv:[ "sh"; "-c"; "echo hello" ]
    ~uid:1000 ~gid:1000 ()

(* Helper to convert a spec to the JSON form used by the older tests
   that inspected the JSON directly. *)
let spec_to_json spec = Oci_spec.to_yojson ~root:"/rootfs" spec

let test_oci_spec_basic () =
  let spec = spec_to_json (make_basic_spec ()) in
  (* Check ociVersion *)
  Alcotest.(check bool)
    "has ociVersion" true
    (json_member "ociVersion" spec <> `Null);
  (* Check process *)
  let process = json_member "process" spec in
  Alcotest.(check bool)
    "terminal false" false
    (process |> json_member "terminal" |> json_to_bool);
  let user = json_member "user" process in
  Alcotest.(check int) "uid" 1000 (user |> json_member "uid" |> json_to_int);
  Alcotest.(check int) "gid" 1000 (user |> json_member "gid" |> json_to_int);
  (* Check argv *)
  let args =
    process |> json_member "args" |> json_to_list |> List.map json_to_string
  in
  Alcotest.(check (list string)) "argv" [ "sh"; "-c"; "echo hello" ] args;
  (* Check root *)
  let root = json_member "root" spec in
  Alcotest.(check string)
    "root path" "/rootfs"
    (root |> json_member "path" |> json_to_string);
  (* Check hostname *)
  Alcotest.(check string)
    "hostname" "builder"
    (spec |> json_member "hostname" |> json_to_string)

let test_oci_spec_env () =
  let spec = spec_to_json (make_basic_spec ()) in
  let process = json_member "process" spec in
  let env =
    process |> json_member "env" |> json_to_list |> List.map json_to_string
  in
  Alcotest.(check bool) "has PATH" true (List.mem "PATH=/usr/bin" env);
  Alcotest.(check bool) "has HOME" true (List.mem "HOME=/home/opam" env)

let test_oci_spec_seccomp () =
  let spec = spec_to_json (make_basic_spec ()) in
  let linux = json_member "linux" spec in
  let seccomp = json_member "seccomp" linux in
  Alcotest.(check string)
    "default action" "SCMP_ACT_ALLOW"
    (seccomp |> json_member "defaultAction" |> json_to_string);
  (* Check that fsync is intercepted *)
  let syscalls = seccomp |> json_member "syscalls" |> json_to_list in
  Alcotest.(check bool) "has syscall rules" true (List.length syscalls > 0);
  let rule = List.hd syscalls in
  let names =
    rule |> json_member "names" |> json_to_list |> List.map json_to_string
  in
  Alcotest.(check bool) "has fsync" true (List.mem "fsync" names);
  Alcotest.(check string)
    "action" "SCMP_ACT_ERRNO"
    (rule |> json_member "action" |> json_to_string)

let test_oci_spec_network_disabled () =
  let spec = spec_to_json (make_basic_spec ~network:false ()) in
  let linux = json_member "linux" spec in
  let namespaces = linux |> json_member "namespaces" |> json_to_list in
  let ns_types =
    List.map (fun ns -> ns |> json_member "type" |> json_to_string) namespaces
  in
  Alcotest.(check bool) "has network ns" true (List.mem "network" ns_types);
  (* No resolv.conf mount *)
  let mounts = spec |> json_member "mounts" |> json_to_list in
  let mount_dsts =
    List.map (fun m -> m |> json_member "destination" |> json_to_string) mounts
  in
  Alcotest.(check bool)
    "no resolv.conf" false
    (List.mem "/etc/resolv.conf" mount_dsts)

let test_oci_spec_network_enabled () =
  let spec = spec_to_json (make_basic_spec ~network:true ()) in
  let linux = json_member "linux" spec in
  let namespaces = linux |> json_member "namespaces" |> json_to_list in
  let ns_types =
    List.map (fun ns -> ns |> json_member "type" |> json_to_string) namespaces
  in
  (* No network namespace when networking enabled *)
  Alcotest.(check bool) "no network ns" false (List.mem "network" ns_types);
  (* Has resolv.conf bind mount *)
  let mounts = spec |> json_member "mounts" |> json_to_list in
  let mount_dsts =
    List.map (fun m -> m |> json_member "destination" |> json_to_string) mounts
  in
  Alcotest.(check bool)
    "has resolv.conf" true
    (List.mem "/etc/resolv.conf" mount_dsts)

let test_oci_spec_with_mounts () =
  let user_mount = Mount.bind_ro ~src:"/host/repo" "/opam-repo" in
  let spec =
    spec_to_json
      (Oci_spec.make ~hostname:"test" ~mounts:[ user_mount ] ~argv:[ "true" ]
         ~uid:0 ~gid:0 ())
  in
  let mounts = spec |> json_member "mounts" |> json_to_list in
  let mount_dsts =
    List.map (fun m -> m |> json_member "destination" |> json_to_string) mounts
  in
  (* User mount should be first *)
  Alcotest.(check bool) "has user mount" true (List.mem "/opam-repo" mount_dsts);
  (* System mounts should follow *)
  Alcotest.(check bool) "has /proc" true (List.mem "/proc" mount_dsts)

let test_oci_spec_capabilities () =
  let spec = spec_to_json (make_basic_spec ()) in
  let process = json_member "process" spec in
  let caps = json_member "capabilities" process in
  let bounding =
    caps |> json_member "bounding" |> json_to_list |> List.map json_to_string
  in
  Alcotest.(check bool) "has CAP_CHOWN" true (List.mem "CAP_CHOWN" bounding);
  Alcotest.(check bool)
    "has CAP_SYS_CHROOT" true
    (List.mem "CAP_SYS_CHROOT" bounding)

let test_oci_spec_terminal () =
  let spec =
    spec_to_json
      (Oci_spec.make ~terminal:true ~hostname:"debug" ~network:true
         ~argv:[ "/bin/bash" ] ~uid:1000 ~gid:1000 ())
  in
  let process = json_member "process" spec in
  Alcotest.(check bool)
    "terminal true" true
    (process |> json_member "terminal" |> json_to_bool)

(* ── Oci_spec.write tests ────────────────────────────────────────── *)

let test_write_spec () =
  with_tmp_dir @@ fun dir ->
  let spec = make_basic_spec () in
  Oci_spec.write ~root:"/rootfs" dir spec |> is_ok "write";
  let config_path = Fpath.(dir / "config.json") in
  Alcotest.(check bool)
    "config.json exists" true
    (Bos.OS.File.exists config_path |> Result.get_ok);
  (* Verify it's valid JSON *)
  let content = Bos.OS.File.read config_path |> Result.get_ok in
  let parsed = Yojson.Safe.from_string content in
  Alcotest.(check bool)
    "valid JSON" true
    (json_member "ociVersion" parsed <> `Null)

let test_write_template () =
  with_tmp_dir @@ fun dir ->
  let spec = make_basic_spec () in
  let path = Fpath.(dir / "config.json") in
  Oci_spec.write_template path spec |> is_ok "write_template";
  let content = Bos.OS.File.read path |> Result.get_ok in
  let parsed = Yojson.Safe.from_string content in
  let root = json_member "root" parsed in
  let path_field = json_member "path" root in
  Alcotest.(check string)
    "rootfs is placeholder" Oci_spec.placeholder_root
    (match path_field with `String s -> s | _ -> "")

(* Instantiating a template must reproduce exactly what a direct
   [to_yojson ~root] would have produced — i.e. write_template +
   instantiate_template round-trips back to the concrete spec. This is
   the property a layer-replay tool relies on. *)
let test_instantiate_template () =
  let spec = make_basic_spec () in
  let templated = Oci_spec.to_yojson ~root:Oci_spec.placeholder_root spec in
  let instantiated =
    Oci_spec.instantiate_template ~root:"/run/rootfs" templated |> Result.get_ok
  in
  let direct = Oci_spec.to_yojson ~root:"/run/rootfs" spec in
  Alcotest.(check bool)
    "instantiated template equals direct to_yojson" true (instantiated = direct);
  let root_path = json_member "root" instantiated |> json_member "path" in
  Alcotest.(check string)
    "root substituted" "/run/rootfs"
    (match root_path with `String s -> s | _ -> "")

let test_instantiate_template_rejects_non_template () =
  let spec = make_basic_spec () in
  (* An already-instantiated spec (real root, not the placeholder)
     must be rejected rather than silently mangled. *)
  let concrete = Oci_spec.to_yojson ~root:"/real/rootfs" spec in
  Alcotest.(check bool)
    "non-template rejected" true
    (Result.is_error (Oci_spec.instantiate_template ~root:"/x" concrete))

let test_instantiate_template_file () =
  with_tmp_dir @@ fun dir ->
  let spec = make_basic_spec () in
  let template = Fpath.(dir / "template.json") in
  Oci_spec.write_template template spec |> is_ok "write_template";
  let bundle = Fpath.(dir / "bundle") in
  Bos.OS.Dir.create ~path:true bundle |> Result.get_ok |> ignore;
  Oci_spec.instantiate_template_file ~template ~root:"/run/rootfs"
    ~bundle_dir:bundle
  |> is_ok "instantiate_template_file";
  let content =
    Bos.OS.File.read Fpath.(bundle / "config.json") |> Result.get_ok
  in
  let parsed = Yojson.Safe.from_string content in
  let root_path = json_member "root" parsed |> json_member "path" in
  Alcotest.(check string)
    "bundle root substituted" "/run/rootfs"
    (match root_path with `String s -> s | _ -> "")

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_container"
    [
      ( "Mount",
        [
          Alcotest.test_case "to_json" `Quick test_mount_to_json;
          Alcotest.test_case "bind_ro" `Quick test_mount_bind_ro;
          Alcotest.test_case "bind_rw" `Quick test_mount_bind_rw;
        ] );
      ( "Oci_spec",
        [
          Alcotest.test_case "basic" `Quick test_oci_spec_basic;
          Alcotest.test_case "env" `Quick test_oci_spec_env;
          Alcotest.test_case "seccomp" `Quick test_oci_spec_seccomp;
          Alcotest.test_case "network disabled" `Quick
            test_oci_spec_network_disabled;
          Alcotest.test_case "network enabled" `Quick
            test_oci_spec_network_enabled;
          Alcotest.test_case "with mounts" `Quick test_oci_spec_with_mounts;
          Alcotest.test_case "capabilities" `Quick test_oci_spec_capabilities;
          Alcotest.test_case "terminal" `Quick test_oci_spec_terminal;
          Alcotest.test_case "write" `Quick test_write_spec;
          Alcotest.test_case "write_template" `Quick test_write_template;
          Alcotest.test_case "instantiate_template" `Quick
            test_instantiate_template;
          Alcotest.test_case "instantiate_template rejects non-template" `Quick
            test_instantiate_template_rejects_non_template;
          Alcotest.test_case "instantiate_template_file" `Quick
            test_instantiate_template_file;
        ] );
    ]
