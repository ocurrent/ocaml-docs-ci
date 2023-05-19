(* MLD/CU compilation rules *)

type name = string

val name_of_string : string -> name

type mld = Mld  (** An mld file *)
type cu = CU  (** An odoc compilation unit *)
type 'a kind = Mld : mld kind | CU : cu kind

type 'a t = {
  file : Fpath.t;
  target : Fpath.t option;
  name : name;
  kind : 'a kind;
}
(** The type for an odoc compilation. *)

type ('a, 'b) command

val v : ?children:mld t list -> ?parent:'a t -> 'b t -> bool -> ('a, 'b) command
(** [v ~children ~parent t skip] is the command to compile t, potentially having
    a [parent] and multiple [children] pages. *)

val compile_command : ?odoc:string -> _ command -> string
(** The odoc compile command *)

val pp_compile_command : ?odoc:string -> unit -> _ command Fmt.t
(** The odoc compile command formatter *)

val pp_link_command : ?odoc:string -> unit -> _ command Fmt.t
(** The odoc link command formatter *)

val pp_html_command : ?odoc:string -> ?output:Fpath.t -> unit -> _ t Fmt.t
(** The odoc html command formatter *)

(* Index pages generation *)

module Gen : sig
  type 'a odoc = 'a t
  type odoc_dyn = Mld of mld t | CU of cu t

  val digest : odoc_dyn -> string

  type t
  (** The index pages generator *)

  val v : (Package.t * bool * odoc_dyn) list -> t

  type gen_page = {
    content : string;
    odoc : mld odoc;
    compilation : (mld, mld) command;
  }
  (** A page to generate is described by its content, its mld compilation unit
      and its associated compilation command. *)

  val universes : t -> gen_page
  val packages : t -> gen_page
  val pp_makefile : ?odoc:string -> output:Fpath.t -> t Fmt.t
  val pp_gen_files_commands : t Fmt.t
  val pp_compile_commands : t Fmt.t
  val pp_link_commands : t Fmt.t
end
