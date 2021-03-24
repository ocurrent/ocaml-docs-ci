(* MLD/CU compilation rules *)

type name = string

val name_of_string : string -> name

type mld = Mld

type cu = CU

type 'a kind = Mld : mld kind | CU : cu kind

type 'a t = { file : Fpath.t; target : Fpath.t option; name : name; kind : 'a kind }


type ('a, 'b) command


val v : ?children: mld t list -> ?parent: 'a t -> 'b t -> bool -> ('a, 'b) command

val pp_compile_command : _ command Fmt.t

val pp_link_command : _ command Fmt.t

val pp_html_command : ?output:Fpath.t -> unit -> _ t Fmt.t

(* Index pages generation *)

module Gen : sig
  type 'a odoc = 'a t

  type odoc_dyn = Mld of mld t | CU of cu t

  type t

  val v : (Package.t * bool * odoc_dyn) list -> t

  val pp_gen_files_commands : t Fmt.t

  val pp_compile_commands : t Fmt.t

  val pp_link_commands : t Fmt.t
end
