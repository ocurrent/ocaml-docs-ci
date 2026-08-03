(* Integration test: build a minimal container and run a command.

   Requires: Linux, runc, sudo, busybox (statically linked).
   Run with: DAY11_INTEGRATION=true dune exec day11/container/test/test_integration.exe *)

open Day11_container
open Day11_test_util.Test_util

let with_tmp_dir f =
  let dir = Bos.OS.Dir.tmp "day11_integ_%s" |> Result.get_ok in
  Fun.protect
    ~finally:(fun () ->
      (* sudo rm because the container may create root-owned files *)
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      ignore (Day11_sys.Sudo.rm_rf ~sw (env :> Eio_unix.Stdenv.base) dir))
    (fun () -> f dir)

(** Create a minimal rootfs with busybox *)
let create_rootfs rootfs =
  let busybox = "/usr/bin/busybox" in
  if not (Sys.file_exists busybox) then Alcotest.fail "busybox not found";
  (* Minimal directory structure *)
  List.iter
    (fun d -> mkdir Fpath.(rootfs / d))
    [ "bin"; "proc"; "tmp"; "dev"; "sys"; "etc" ];
  (* Copy busybox and create sh symlink *)
  let bb_dst = Fpath.(rootfs / "bin" / "busybox") in
  Bos.OS.File.read (Fpath.v busybox)
  |> Result.get_ok
  |> Bos.OS.File.write bb_dst
  |> Result.get_ok;
  Unix.chmod (Fpath.to_string bb_dst) 0o755;
  Unix.symlink "busybox" (Fpath.to_string Fpath.(rootfs / "bin" / "sh"));
  Unix.symlink "busybox" (Fpath.to_string Fpath.(rootfs / "bin" / "echo"));
  Unix.symlink "busybox" (Fpath.to_string Fpath.(rootfs / "bin" / "cat"));
  Unix.symlink "busybox" (Fpath.to_string Fpath.(rootfs / "bin" / "ls"))

(* ── Tests ───────────────────────────────────────────────────────── *)

let test_runc_echo () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let rootfs = Fpath.(dir / "rootfs") in
  create_rootfs rootfs;
  (* Generate OCI spec *)
  let spec =
    Oci_spec.make ~hostname:"test"
      ~env:[ ("PATH", "/bin") ]
      ~argv:[ "/bin/echo"; "hello from container" ]
      ~uid:0 ~gid:0 ()
  in
  Oci_spec.write ~root:(Fpath.to_string rootfs) dir spec
  |> ok_or_fail "write_spec";
  (* Run it *)
  let container_id = "day11-test-" ^ string_of_int (Unix.getpid ()) in
  (* Clean up any stale container *)
  ignore (Runc.delete ~sw env container_id);
  let run =
    Runc.run ~sw env ~bundle:dir ~container_id |> ok_or_fail "runc run"
  in
  ignore (Runc.delete ~sw env container_id);
  Alcotest.(check bool) "exit 0" true (run.Day11_sys.Run.status = `Exited 0);
  Alcotest.(check bool)
    "output contains hello" true
    (String.trim run.output = "hello from container"
    || String.trim run.errors = "hello from container")

let test_overlay_and_runc () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  (* Create a base rootfs as "layer 0" *)
  let base_layer = Fpath.(dir / "base") in
  let base_fs = Fpath.(base_layer / "fs") in
  create_rootfs base_fs;
  (* Create a second layer that adds a file *)
  let layer1 = Fpath.(dir / "layer1") in
  mkdir Fpath.(layer1 / "fs" / "etc");
  write_file Fpath.(layer1 / "fs" / "etc" / "greeting") "hello from layer1";
  (* Stack layers into lower dir *)
  let lower = Fpath.(dir / "lower") in
  mkdir lower;
  Day11_layer.Stack.merge ~sw env ~layer_dirs:[ base_layer; layer1 ]
    ~target:lower
  |> ok_or_fail "stack";
  (* Verify stacking worked *)
  Alcotest.(check bool)
    "busybox in lower" true
    (Bos.OS.File.exists Fpath.(lower / "bin" / "busybox") |> Result.get_ok);
  Alcotest.(check bool)
    "greeting in lower" true
    (Bos.OS.File.exists Fpath.(lower / "etc" / "greeting") |> Result.get_ok);
  (* Set up overlay *)
  let upper = Fpath.(dir / "upper") in
  let work = Fpath.(dir / "work") in
  let merged = Fpath.(dir / "merged") in
  List.iter mkdir [ upper; work; merged ];
  Overlay.mount ~sw env ~lower:[ lower ] ~upper ~work ~target:merged
  |> ok_or_fail "overlay mount";
  (* Generate OCI spec using the overlay as rootfs *)
  let spec =
    Oci_spec.make ~hostname:"test"
      ~env:[ ("PATH", "/bin") ]
      ~argv:[ "/bin/cat"; "/etc/greeting" ]
      ~uid:0 ~gid:0 ()
  in
  Oci_spec.write ~root:(Fpath.to_string merged) dir spec
  |> ok_or_fail "write_spec";
  let container_id = "day11-overlay-" ^ string_of_int (Unix.getpid ()) in
  ignore (Runc.delete ~sw env container_id);
  let run =
    Runc.run ~sw env ~bundle:dir ~container_id |> ok_or_fail "runc run"
  in
  (* Cleanup *)
  ignore (Runc.delete ~sw env container_id);
  ignore (Overlay.umount ~sw env merged);
  Alcotest.(check bool) "exit 0" true (run.Day11_sys.Run.status = `Exited 0);
  Alcotest.(check bool)
    "reads layer1 content" true
    (String.trim run.output = "hello from layer1"
    || String.trim run.errors = "hello from layer1")

(* ── Hybrid lowerdir plan ─────────────────────────────────────────── *)

(** Create [n] fake "dep" layer dirs. Each one has [fs/data/<i>.txt] with
    content ["payload-<i>"], so we can verify the merged view contains data from
    every layer. The base layer provides /bin/cat and a /data directory we can
    append into. *)
let make_dep_layers parent_dir n =
  List.init n (fun i ->
      let layer_dir = Fpath.(parent_dir / Printf.sprintf "dep-%03d" i) in
      let fs = Fpath.(layer_dir / "fs") in
      let data = Fpath.(fs / "data") in
      mkdir data;
      write_file
        Fpath.(data / Printf.sprintf "%03d.txt" i)
        (Printf.sprintf "payload-%d" i);
      layer_dir)

(** End-to-end test for [Stack.plan_lowerdir]: create [n_layers] fake dep
    layers, plan, optionally merge, mount overlay, and verify every dep's file
    is visible inside the container. *)
let run_hybrid_plan_test ~n_layers ~budget () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  (* Base rootfs (busybox) *)
  let base_layer = Fpath.(dir / "base") in
  let base_fs = Fpath.(base_layer / "fs") in
  create_rootfs base_fs;
  mkdir Fpath.(base_fs / "data");
  (* placeholder so /data exists *)
  (* N dep layers, each adding /data/<i>.txt *)
  let deps_parent = Fpath.(dir / "deps") in
  mkdir deps_parent;
  let dep_layers = make_dep_layers deps_parent n_layers in
  (* Plan the layout *)
  let upper = Fpath.(dir / "upper") in
  let work = Fpath.(dir / "work") in
  let merged = Fpath.(dir / "merged") in
  let merged_lower = Fpath.(dir / "lower") in
  List.iter mkdir [ upper; work; merged ];
  let entry_cost d = String.length (Fpath.to_string Fpath.(d / "fs")) + 1 in
  let fixed_overhead =
    String.length "lowerdir="
    + String.length (Fpath.to_string base_fs)
    + String.length ",upperdir="
    + String.length (Fpath.to_string upper)
    + String.length ",workdir="
    + String.length (Fpath.to_string work)
  in
  let merged_overhead = String.length (Fpath.to_string merged_lower) + 1 in
  let available = budget - fixed_overhead in
  let separate, to_merge =
    Day11_layer.Stack.plan_lowerdir ~available ~merged_overhead ~entry_cost
      dep_layers
  in
  Alcotest.(check int)
    "all layers accounted for" n_layers
    (List.length separate + List.length to_merge);
  (* If anything to merge, do the cp-merge *)
  if to_merge <> [] then (
    mkdir merged_lower;
    Day11_layer.Stack.merge ~sw env ~layer_dirs:to_merge ~target:merged_lower
    |> ok_or_fail "stack.merge");
  (* Build the overlay lower list: separate dep fs/ dirs + (merged
     lower if any) + base. This must be the same construction
     run_in_layers.ml uses. *)
  let lower_dirs =
    List.map (fun d -> Fpath.(d / "fs")) separate
    @ (if to_merge = [] then [] else [ merged_lower ])
    @ [ base_fs ]
  in
  (* Verify the actual mount-options string fits the budget. This is
     the key invariant plan_lowerdir is meant to enforce. *)
  let options =
    Printf.sprintf "lowerdir=%s,upperdir=%s,workdir=%s"
      (String.concat ":" (List.map Fpath.to_string lower_dirs))
      (Fpath.to_string upper) (Fpath.to_string work)
  in
  Alcotest.(check bool)
    (Printf.sprintf "options string %d bytes ≤ budget %d"
       (String.length options) budget)
    true
    (String.length options <= budget);
  (* Mount the overlay *)
  Overlay.mount ~sw env ~lower:lower_dirs ~upper ~work ~target:merged
  |> ok_or_fail "overlay mount";
  Fun.protect
    ~finally:(fun () -> ignore (Overlay.umount ~sw env merged))
    (fun () ->
      (* Run a container that lists /data and verifies every file is
         present. We use shell to count files and check their content. *)
      let script =
        Printf.sprintf
          "n=$(ls /data | wc -l); echo \"count=$n\"; for i in $(ls /data); do \
           cat /data/$i; echo; done"
      in
      let spec =
        Oci_spec.make ~hostname:"test"
          ~env:[ ("PATH", "/bin") ]
          ~argv:[ "/bin/sh"; "-c"; script ]
          ~uid:0 ~gid:0 ()
      in
      Oci_spec.write ~root:(Fpath.to_string merged) dir spec
      |> ok_or_fail "write_spec";
      let container_id =
        Printf.sprintf "day11-hybrid-%d-%d" n_layers (Unix.getpid ())
      in
      ignore (Runc.delete ~sw env container_id);
      let run =
        Runc.run ~sw env ~bundle:dir ~container_id |> ok_or_fail "runc run"
      in
      ignore (Runc.delete ~sw env container_id);
      Alcotest.(check bool)
        "container exit 0" true
        (run.Day11_sys.Run.status = `Exited 0);
      let out = run.output ^ run.errors in
      (* Check the file count *)
      let expected_count_line = Printf.sprintf "count=%d" n_layers in
      Alcotest.(check bool)
        (Printf.sprintf "container saw all %d files" n_layers)
        true
        (let lines = String.split_on_char '\n' out in
         List.exists (fun l -> String.trim l = expected_count_line) lines);
      (* Check every payload is present *)
      for i = 0 to n_layers - 1 do
        let expected = Printf.sprintf "payload-%d" i in
        Alcotest.(check bool)
          (Printf.sprintf "payload-%d visible" i)
          true
          (let lines = String.split_on_char '\n' out in
           List.exists (fun l -> String.trim l = expected) lines)
      done)

(** Multi-lower path: small layer count, generous budget. Should keep every
    layer separate and never invoke Stack.merge. *)
let test_hybrid_pure_multi_lower () =
  run_hybrid_plan_test ~n_layers:30 ~budget:4000 ()

(** Forced split: many layers, tight budget. Plan must split into separate +
    merged buckets, and the assembled mount must still show every dep's content.
*)
let test_hybrid_forced_split () =
  (* 60 layers with realistic-ish (long) path components inside the
     temp dir. With a deliberately small budget, the plan is forced
     to merge most of them. *)
  run_hybrid_plan_test ~n_layers:60 ~budget:1500 ()

(** Realistic 4K test: enough layers that pure multi-lower would overflow
    PAGE_SIZE. Without the split, the mount would fail. *)
let test_hybrid_real_4k_overflow () =
  (* The temp dirs in this test have long paths under /tmp, and
     each "dep-NNN" entry costs around 60 bytes. With ~70+ deps,
     the lowerdir string would exceed 4096. The plan must split. *)
  run_hybrid_plan_test ~n_layers:80 ~budget:4000 ()

let () =
  if not (is_integration ()) then
    Printf.printf
      "Skipping integration tests (set DAY11_INTEGRATION=true to run)\n"
  else
    Alcotest.run "day11_container_integration"
      [
        ( "Container",
          [
            Alcotest.test_case "runc echo" `Slow test_runc_echo;
            Alcotest.test_case "overlay + runc" `Slow test_overlay_and_runc;
          ] );
        ( "Hybrid lowerdir plan",
          [
            Alcotest.test_case "pure multi-lower" `Slow
              test_hybrid_pure_multi_lower;
            Alcotest.test_case "forced split (small budget)" `Slow
              test_hybrid_forced_split;
            Alcotest.test_case "4K boundary" `Slow test_hybrid_real_4k_overflow;
          ] );
      ]
