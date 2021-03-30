(* MLD/CU compilation rules *)

type name = string

val name_of_string : string -> name

type mld = Mld

type cu = CU

type 'a kind = Mld : mld kind | CU : cu kind

type 'a t = { file : Fpath.t; target : Fpath.t option; name : name; kind : 'a kind }

type ('a, 'b) command

val v : ?children:mld t list -> ?parent:'a t -> 'b t -> bool -> ('a, 'b) command

val compile_command : ?odoc:string -> _ command -> string

val pp_compile_command : ?odoc:string -> unit -> _ command Fmt.t

val pp_link_command : ?odoc:string -> unit -> _ command Fmt.t

val pp_html_command : ?odoc:string -> ?output:Fpath.t -> unit -> _ t Fmt.t

(* Index pages generation *)

module Gen : sig
  type 'a odoc = 'a t

  type odoc_dyn = Mld of mld t | CU of cu t

  val digest : odoc_dyn -> string

  type t

  val v : (Package.t * bool * odoc_dyn) list -> t

  type gen_page = { content : string; odoc : mld odoc; compilation : (mld, mld) command }

  val universes : t -> gen_page

  val packages : t -> gen_page

  val pp_makefile : ?odoc:string -> output:Fpath.t -> t Fmt.t

  val pp_gen_files_commands : t Fmt.t

  val pp_compile_commands : t Fmt.t

  val pp_link_commands : t Fmt.t
end
