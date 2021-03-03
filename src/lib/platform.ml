type ocaml_version = V4_10 | V4_11

let pp_ocaml f = function V4_10 -> Fmt.pf f "4.10" | V4_11 -> Fmt.pf f "4.11"

let pp_exact_ocaml f = function V4_10 -> Fmt.pf f "4.10.2" | V4_11 -> Fmt.pf f "4.11.2"

type os = Debian | Ubuntu | Fedora

let os_version = function Ubuntu -> "20.04" | Fedora -> "33" | Debian -> "10"

let os_family = function Ubuntu -> "ubuntu" | Fedora -> "fedora" | Debian -> "debian"

let pp_os f t = Fmt.pf f "%s-%s" (os_family t) (os_version t)

type arch = Arm64 | Amd64

let arch_to_string = function Arm64 -> "arm64" | Amd64 -> "x86_64"

type system = { ocaml : ocaml_version; os : os }

let pp_system f { ocaml; os } = Fmt.pf f "%a-ocaml-%a" pp_os os pp_ocaml ocaml

let spec t = Spec.make @@ Fmt.str "ocaml/opam:%a" pp_system t

type t = { system : system; arch : arch }

let platform_id t =
  match t.arch with
  | Arm64 -> "arm64-" ^ Fmt.str "%a" pp_system t.system
  | Amd64 -> "x86_64-" ^ Fmt.str "%a" pp_system t.system

let pp_platform f t =
  Fmt.pf f "%s / %a / %a" (arch_to_string t.arch) pp_os t.system.os pp_ocaml t.system.ocaml

let ocluster_pool { arch; _ } = match arch with Arm64 -> "linux-arm64" | Amd64 -> "linux-x86_64"

(* Base configuration.. *)

let system = { ocaml = V4_11; os = Debian }

let platform_amd64 = { system; arch = Amd64 }

let platform_arm64 = { system; arch = Arm64 }
