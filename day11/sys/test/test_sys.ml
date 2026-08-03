(* Tests for the day11_sys library.

   All Eio-dependent tests are wrapped in [with_eio]. Filesystem tests
   use [with_tmp_dir] for isolation. *)

open Day11_sys
open Day11_test_util.Test_util

(* ── Helpers ─────────────────────────────────────────────────────── *)

let is_ok msg r = ok_or_fail msg r |> ignore

let is_error msg = function
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (msg ^ ": expected Error, got Ok")

let read_file path = Bos.OS.File.read path |> Result.get_ok

(* ── Run tests ───────────────────────────────────────────────────── *)

let test_run_echo () =
  with_eio @@ fun ~sw env ->
  let cmd = Bos.Cmd.(v "echo" % "hello") in
  let r = Run.run ~sw env cmd None in
  Alcotest.(check string) "stdout" "hello\n" r.output;
  Alcotest.(check string) "stderr" "" r.errors;
  Alcotest.(check bool) "exit 0" true (r.status = `Exited 0);
  Alcotest.(check bool) "time > 0" true (r.time >= 0.0)

let test_run_failure () =
  with_eio @@ fun ~sw env ->
  let cmd = Bos.Cmd.(v "false") in
  let r = Run.run ~sw env cmd None in
  Alcotest.(check bool) "exit 1" true (r.status = `Exited 1)

let test_run_stderr () =
  with_eio @@ fun ~sw env ->
  let cmd = Bos.Cmd.(v "sh" % "-c" % "echo err >&2") in
  let r = Run.run ~sw env cmd None in
  Alcotest.(check string) "stderr captured" "err\n" r.errors;
  Alcotest.(check string) "stdout empty" "" r.output

let test_run_output_file_passthrough () =
  with_eio @@ fun ~sw env ->
  let path = Fpath.v "/tmp/test_marker" in
  let cmd = Bos.Cmd.(v "echo" % "x") in
  let r = Run.run ~sw env cmd (Some path) in
  Alcotest.(check bool) "output_file stored" true (r.output_file = Some path)

let test_run_signaled () =
  with_eio @@ fun ~sw env ->
  (* SIGKILL can't be caught — guaranteed to produce Signaled status *)
  let cmd = Bos.Cmd.(v "sh" % "-c" % "kill -KILL $$") in
  let r = Run.run ~sw env cmd None in
  Alcotest.(check bool)
    "signaled" true
    (match r.status with `Signaled _ -> true | _ -> false)

let test_run_concurrent () =
  with_eio @@ fun ~sw env ->
  (* Multiple fibers spawning concurrently must each get their own
     correct stdout — no crossed wires on the shared helper socket. *)
  let n = 8 in
  let inputs = List.init n (fun i -> i) in
  let results =
    Eio.Fiber.List.map
      (fun i ->
        let cmd = Bos.Cmd.(v "echo" % string_of_int i) in
        (i, Run.run ~sw env cmd None))
      inputs
  in
  List.iter
    (fun (i, r) ->
      Alcotest.(check string)
        (Printf.sprintf "stdout %d" i)
        (string_of_int i ^ "\n")
        r.Run.output;
      Alcotest.(check bool)
        (Printf.sprintf "exit %d" i)
        true
        (r.Run.status = `Exited 0))
    results

let test_run_nonexistent_binary () =
  with_eio @@ fun ~sw env ->
  (* Run.run returns a t, not a result — a nonexistent binary should
     either produce a Signaled/Exited status or raise Eio.Exn.Io.
     It must NOT raise Failure. *)
  try
    let r = Run.run ~sw env Bos.Cmd.(v "nonexistent_binary_xyz") None in
    (* If it returns, it should have a non-zero exit *)
    Alcotest.(check bool) "non-zero exit" true (r.status <> `Exited 0)
  with
  | Eio.Exn.Io _ ->
      (* Eio wraps the spawn failure — this is acceptable *)
      ()
  | Failure msg when msg = "not implemented" -> Alcotest.fail "not implemented"

(* ── Dir_lock tests ──────────────────────────────────────────────── *)

let test_dir_lock_basic () =
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "layer") in
  mkdir target;
  let executed = ref false in
  let r =
    Dir_lock.with_lock target (fun ~set_temp_log_path:_ _path ->
        executed := true;
        Ok ())
  in
  is_ok "lock" r;
  Alcotest.(check bool) "body executed" true !executed

let test_dir_lock_skips_when_marker_exists () =
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "layer") in
  mkdir target;
  let marker = Fpath.v "layer.json" in
  write_file Fpath.(target / "layer.json") "{}";
  let executed = ref false in
  let r =
    Dir_lock.with_lock ~marker_file:marker target
      (fun ~set_temp_log_path:_ _path ->
        executed := true;
        Ok ())
  in
  is_ok "lock" r;
  Alcotest.(check bool) "body skipped" false !executed

let test_dir_lock_mutual_exclusion () =
  with_eio @@ fun ~sw:_ _env ->
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "layer") in
  mkdir target;
  (* Two fibers try to lock the same dir. Track ordering. *)
  let log = ref [] in
  Eio.Fiber.both
    (fun () ->
      Dir_lock.with_lock target (fun ~set_temp_log_path:_ _path ->
          log := "a-enter" :: !log;
          Eio.Fiber.yield ();
          log := "a-exit" :: !log;
          Ok ())
      |> ignore)
    (fun () ->
      Dir_lock.with_lock target (fun ~set_temp_log_path:_ _path ->
          log := "b-enter" :: !log;
          Eio.Fiber.yield ();
          log := "b-exit" :: !log;
          Ok ())
      |> ignore);
  let log = List.rev !log in
  (* One must fully complete before the other enters *)
  let a_first = log = [ "a-enter"; "a-exit"; "b-enter"; "b-exit" ] in
  let b_first = log = [ "b-enter"; "b-exit"; "a-enter"; "a-exit" ] in
  Alcotest.(check bool) "serialized" true (a_first || b_first)

let test_dir_lock_cleanup_on_error () =
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "layer") in
  mkdir target;
  let r =
    Dir_lock.with_lock target (fun ~set_temp_log_path:_ _path ->
        Error (`Msg "intentional"))
  in
  is_error "body error propagated" r;
  (* Lock file should be cleaned up — a second lock should succeed *)
  let r2 =
    Dir_lock.with_lock target (fun ~set_temp_log_path:_ _path -> Ok ())
  in
  is_ok "second lock after error" r2

let test_dir_lock_body_raises () =
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "layer") in
  mkdir target;
  (* Body raises rather than returning Error — Fun.protect must still
     release the mutex and the file lock. *)
  let raised =
    try
      let _ : (unit, [ `Msg of string ]) result =
        Dir_lock.with_lock target (fun ~set_temp_log_path:_ _ -> raise Exit)
      in
      false
    with Exit -> true
  in
  Alcotest.(check bool) "exception escaped" true raised;
  (* A second acquisition must succeed — would deadlock if mutex leaked *)
  let r = Dir_lock.with_lock target (fun ~set_temp_log_path:_ _ -> Ok ()) in
  is_ok "second lock after raise" r

let test_dir_lock_cross_process () =
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "layer") in
  mkdir target;
  let lock_path = Fpath.to_string Fpath.(target + ".lock") in
  match Unix.fork () with
  | 0 ->
      (* Child: take POSIX lockf and hold for 500ms, then exit *)
      (try
         let fd = Unix.openfile lock_path [ Unix.O_CREAT; Unix.O_RDWR ] 0o644 in
         Unix.lockf fd Unix.F_LOCK 0;
         Unix.sleepf 0.5;
         Unix.close fd
       with _ -> ());
      Unix._exit 0
  | child_pid ->
      (* Parent: give child a moment to grab the lock, then attempt our
       own. The cross-process lockf path must wait until the child
       releases. *)
      Unix.sleepf 0.1;
      let t_start = Unix.gettimeofday () in
      let r = Dir_lock.with_lock target (fun ~set_temp_log_path:_ _ -> Ok ()) in
      let elapsed = Unix.gettimeofday () -. t_start in
      is_ok "cross-process lock" r;
      Alcotest.(check bool) "waited for child" true (elapsed >= 0.3);
      ignore (Unix.waitpid [] child_pid)

let test_dir_lock_with_lock_file () =
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "layer") in
  mkdir target;
  let locks_dir = Fpath.(dir / "locks") in
  mkdir locks_dir;
  let lock_file = Fpath.(locks_dir / "build-yojson.2.2.2.lock") in
  let r =
    Dir_lock.with_lock ~lock_file target (fun ~set_temp_log_path:_ _path ->
        Ok ())
  in
  is_ok "lock with explicit lock_file" r;
  (* Lock file should have been created *)
  Alcotest.(check bool)
    "lock file created" true
    (Bos.OS.File.exists lock_file |> Result.get_ok)

(* ── Tree tests ──────────────────────────────────────────────────── *)

let populate_tree dir =
  (* Create a small tree for testing:
     dir/a.txt
     dir/sub/b.txt
     dir/sub/c.txt *)
  mkdir Fpath.(dir / "sub");
  write_file Fpath.(dir / "a.txt") "aaa";
  write_file Fpath.(dir / "sub" / "b.txt") "bbb";
  write_file Fpath.(dir / "sub" / "c.txt") "ccc"

let test_tree_copy () =
  with_tmp_dir @@ fun dir ->
  let src = Fpath.(dir / "src") in
  let tgt = Fpath.(dir / "tgt") in
  mkdir src;
  populate_tree src;
  let r = Tree.copy ~source:src ~target:tgt in
  is_ok "copy" r;
  Alcotest.(check string) "a.txt" "aaa" (read_file Fpath.(tgt / "a.txt"));
  Alcotest.(check string)
    "sub/b.txt" "bbb"
    (read_file Fpath.(tgt / "sub" / "b.txt"));
  Alcotest.(check string)
    "sub/c.txt" "ccc"
    (read_file Fpath.(tgt / "sub" / "c.txt"))

let test_tree_hardlink () =
  with_tmp_dir @@ fun dir ->
  let src = Fpath.(dir / "src") in
  let tgt = Fpath.(dir / "tgt") in
  mkdir src;
  populate_tree src;
  let r = Tree.hardlink ~source:src ~target:tgt in
  is_ok "hardlink" r;
  (* Contents should match *)
  Alcotest.(check string) "a.txt" "aaa" (read_file Fpath.(tgt / "a.txt"));
  (* Verify they're actually hardlinks (same inode) *)
  let src_stat = Unix.stat (Fpath.to_string Fpath.(src / "a.txt")) in
  let tgt_stat = Unix.stat (Fpath.to_string Fpath.(tgt / "a.txt")) in
  Alcotest.(check int) "same inode" src_stat.Unix.st_ino tgt_stat.Unix.st_ino

let test_tree_hardlink_symlink () =
  with_tmp_dir @@ fun dir ->
  let src = Fpath.(dir / "src") in
  let tgt = Fpath.(dir / "tgt") in
  mkdir src;
  write_file Fpath.(src / "real.txt") "data";
  Unix.symlink "real.txt" (Fpath.to_string Fpath.(src / "link.txt"));
  let r = Tree.hardlink ~source:src ~target:tgt in
  is_ok "hardlink with symlink" r;
  let link_path = Fpath.to_string Fpath.(tgt / "link.txt") in
  let stat = Unix.lstat link_path in
  Alcotest.(check bool)
    "preserved as symlink" true
    (stat.Unix.st_kind = Unix.S_LNK);
  Alcotest.(check string)
    "link target preserved" "real.txt" (Unix.readlink link_path)

let test_tree_clense () =
  with_tmp_dir @@ fun dir ->
  (* clense removes files from target whose mtime matches source.
     In production, matching files are hardlinks (same inode) from the
     overlay lower dir, so mtime equality is exact. We simulate this
     by hardlinking, then breaking the link for the file that should
     survive. *)
  let src = Fpath.(dir / "src") in
  let tgt = Fpath.(dir / "tgt") in
  mkdir src;
  populate_tree src;
  (* Hardlink gives identical inodes and thus identical mtimes *)
  Tree.hardlink ~source:src ~target:tgt |> ok_or_fail "setup";
  (* Break the hardlink for a.txt by writing a new file *)
  Unix.unlink (Fpath.to_string Fpath.(tgt / "a.txt"));
  Unix.sleepf 0.05;
  write_file Fpath.(tgt / "a.txt") "modified";
  let r = Tree.clense ~source:src ~target:tgt in
  is_ok "clense" r;
  (* a.txt should survive (different mtime — link was broken) *)
  Alcotest.(check bool)
    "a.txt survives" true
    (Bos.OS.File.exists Fpath.(tgt / "a.txt") |> Result.get_ok);
  (* b.txt and c.txt should be removed (same inode/mtime as source) *)
  Alcotest.(check bool)
    "sub/b.txt removed" false
    (Bos.OS.File.exists Fpath.(tgt / "sub" / "b.txt") |> Result.get_ok);
  Alcotest.(check bool)
    "sub/c.txt removed" false
    (Bos.OS.File.exists Fpath.(tgt / "sub" / "c.txt") |> Result.get_ok)

let test_tree_copy_permission_error () =
  with_tmp_dir @@ fun dir ->
  let src = Fpath.(dir / "src") in
  mkdir src;
  populate_tree src;
  (* Make a subdir unreadable *)
  Unix.chmod (Fpath.to_string Fpath.(src / "sub")) 0o000;
  let tgt = Fpath.(dir / "tgt") in
  let r = Tree.copy ~source:src ~target:tgt in
  (* Restore permissions for cleanup *)
  Unix.chmod (Fpath.to_string Fpath.(src / "sub")) 0o755;
  is_error "permission denied" r

(* ── Atomic_swap tests ───────────────────────────────────────────── *)

let test_atomic_swap_full_cycle () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "docs" / "yojson" / "2.2.2") in
  mkdir Fpath.(dir / "docs" / "yojson");
  let staging = Atomic_swap.prepare target |> ok_or_fail "prepare" in
  write_file Fpath.(staging / "index.html") "<html/>";
  let committed = Atomic_swap.commit ~sw env target |> ok_or_fail "commit" in
  Alcotest.(check bool) "committed" true committed;
  (* Final dir should have our content *)
  Alcotest.(check string)
    "content" "<html/>"
    (read_file Fpath.(target / "index.html"))

let test_atomic_swap_rollback () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "docs" / "pkg" / "1.0") in
  mkdir Fpath.(dir / "docs" / "pkg");
  let staging = Atomic_swap.prepare target |> ok_or_fail "prepare" in
  write_file Fpath.(staging / "index.html") "<html/>";
  Atomic_swap.rollback ~sw env target |> is_ok "rollback";
  (* Staging dir should be gone *)
  Alcotest.(check bool)
    "staging removed" false
    (Bos.OS.Dir.exists staging |> Result.get_ok);
  (* Target should not exist *)
  Alcotest.(check bool)
    "target absent" false
    (Bos.OS.Dir.exists target |> Result.get_ok)

let test_atomic_swap_cleanup_stale () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  (* Create stale .new and .old dirs that a crash would leave *)
  let stale_new = Fpath.(dir / "p" / "yojson" / "2.2.2.new") in
  let stale_old = Fpath.(dir / "p" / "yojson" / "2.2.2.old") in
  mkdir stale_new;
  mkdir stale_old;
  write_file Fpath.(stale_new / "junk") "x";
  write_file Fpath.(stale_old / "junk") "x";
  Atomic_swap.cleanup_stale ~sw env dir |> is_ok "cleanup";
  Alcotest.(check bool)
    ".new removed" false
    (Bos.OS.Dir.exists stale_new |> Result.get_ok);
  Alcotest.(check bool)
    ".old removed" false
    (Bos.OS.Dir.exists stale_old |> Result.get_ok)

let test_atomic_swap_commit_no_staging () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "docs" / "pkg" / "1.0") in
  mkdir Fpath.(dir / "docs" / "pkg");
  (* No prepare — commit must report Ok false rather than failing *)
  let committed = Atomic_swap.commit ~sw env target |> ok_or_fail "commit" in
  Alcotest.(check bool) "no staging => no commit" false committed

let test_atomic_swap_commit_replaces_existing () =
  with_eio @@ fun ~sw env ->
  with_tmp_dir @@ fun dir ->
  let target = Fpath.(dir / "docs" / "pkg" / "1.0") in
  mkdir Fpath.(dir / "docs" / "pkg");
  (* First commit *)
  let s1 = Atomic_swap.prepare target |> ok_or_fail "prepare1" in
  write_file Fpath.(s1 / "index.html") "v1";
  Atomic_swap.commit ~sw env target |> ok_or_fail "commit1" |> ignore;
  (* Second commit should replace *)
  let s2 = Atomic_swap.prepare target |> ok_or_fail "prepare2" in
  write_file Fpath.(s2 / "index.html") "v2";
  let committed = Atomic_swap.commit ~sw env target |> ok_or_fail "commit2" in
  Alcotest.(check bool) "committed" true committed;
  Alcotest.(check string)
    "content replaced" "v2"
    (read_file Fpath.(target / "index.html"))

(* ── Util tests ──────────────────────────────────────────────────── *)

let test_nproc () =
  let n = Util.nproc () in
  Alcotest.(check bool) "nproc > 0" true (n > 0)

let test_dir_size () =
  with_tmp_dir @@ fun dir ->
  write_file Fpath.(dir / "a") (String.make 100 'x');
  write_file Fpath.(dir / "b") (String.make 200 'y');
  let size = Util.dir_size dir |> ok_or_fail "dir_size" in
  (* Should be at least 300 bytes *)
  Alcotest.(check bool) "size >= 300" true (size >= 300)

let test_dir_size_nonexistent () =
  let r = Util.dir_size (Fpath.v "/nonexistent/path/xyz") in
  is_error "nonexistent dir" r

(* ── Sudo tests ──────────────────────────────────────────────────── *)

(* These tests skip when [sudo -n true] fails — i.e. sudo would prompt
   for a password or is not installed. *)

let test_sudo_run_echo () =
  if not (Day11_test_util.Test_util.sudo_available ()) then Alcotest.skip ()
  else
    with_eio @@ fun ~sw env ->
    let cmd = Bos.Cmd.(v "echo" % "ok") in
    let r = Sudo.run ~sw env cmd |> ok_or_fail "sudo run" in
    Alcotest.(check string) "stdout" "ok\n" r.Run.output;
    Alcotest.(check bool) "exit 0" true (r.Run.status = `Exited 0)

let test_sudo_rm_rf () =
  if not (Day11_test_util.Test_util.sudo_available ()) then Alcotest.skip ()
  else
    with_eio @@ fun ~sw env ->
    with_tmp_dir @@ fun dir ->
    let target = Fpath.(dir / "to-remove") in
    mkdir target;
    write_file Fpath.(target / "f") "x";
    Sudo.rm_rf ~sw env target |> ok_or_fail "rm_rf";
    Alcotest.(check bool)
      "removed" false
      (Bos.OS.Dir.exists target |> Result.get_ok)

(* ── Test registration ───────────────────────────────────────────── *)

let () =
  Alcotest.run "day11_sys"
    [
      ( "Run",
        [
          Alcotest.test_case "echo" `Quick test_run_echo;
          Alcotest.test_case "failure" `Quick test_run_failure;
          Alcotest.test_case "stderr" `Quick test_run_stderr;
          Alcotest.test_case "signaled" `Quick test_run_signaled;
          Alcotest.test_case "concurrent fibers" `Quick test_run_concurrent;
          Alcotest.test_case "output_file passthrough" `Quick
            test_run_output_file_passthrough;
          Alcotest.test_case "nonexistent binary" `Quick
            test_run_nonexistent_binary;
        ] );
      ( "Dir_lock",
        [
          Alcotest.test_case "basic" `Quick test_dir_lock_basic;
          Alcotest.test_case "skips when marker exists" `Quick
            test_dir_lock_skips_when_marker_exists;
          Alcotest.test_case "mutual exclusion" `Quick
            test_dir_lock_mutual_exclusion;
          Alcotest.test_case "cleanup on error" `Quick
            test_dir_lock_cleanup_on_error;
          Alcotest.test_case "body raises" `Quick test_dir_lock_body_raises;
          Alcotest.test_case "cross-process" `Quick test_dir_lock_cross_process;
          Alcotest.test_case "with explicit lock_file" `Quick
            test_dir_lock_with_lock_file;
        ] );
      ( "Tree",
        [
          Alcotest.test_case "copy" `Quick test_tree_copy;
          Alcotest.test_case "hardlink" `Quick test_tree_hardlink;
          Alcotest.test_case "hardlink preserves symlink" `Quick
            test_tree_hardlink_symlink;
          Alcotest.test_case "clense" `Quick test_tree_clense;
          Alcotest.test_case "copy permission error" `Quick
            test_tree_copy_permission_error;
        ] );
      ( "Atomic_swap",
        [
          Alcotest.test_case "full cycle" `Quick test_atomic_swap_full_cycle;
          Alcotest.test_case "rollback" `Quick test_atomic_swap_rollback;
          Alcotest.test_case "cleanup stale" `Quick
            test_atomic_swap_cleanup_stale;
          Alcotest.test_case "commit replaces existing" `Quick
            test_atomic_swap_commit_replaces_existing;
          Alcotest.test_case "commit with no staging" `Quick
            test_atomic_swap_commit_no_staging;
        ] );
      ( "Sudo",
        [
          Alcotest.test_case "run echo" `Quick test_sudo_run_echo;
          Alcotest.test_case "rm_rf" `Quick test_sudo_rm_rf;
        ] );
      ( "Util",
        [
          Alcotest.test_case "nproc" `Quick test_nproc;
          Alcotest.test_case "dir_size" `Quick test_dir_size;
          Alcotest.test_case "dir_size nonexistent" `Quick
            test_dir_size_nonexistent;
        ] );
    ]
