(** Compilation step

The documentation compilation is done as an ocluster. It takes for input one prep/ folder and its
compiled dependencies. It uses `voodoo-do` to perform the compilation, link and html generation 
steps, outputting the results in the compile/ and html/ folders.  
*)

type hashes = {
  html_tailwind_commit_hash : string;
  html_tailwind_tree_hash : string;
  html_classic_commit_hash : string;
  html_classic_tree_hash : string;
}

type t
(** A compiled package *)

val hashes : t -> hashes
(** Hash of the compiled artifacts  *)

val is_blessed : t -> bool
(** A blessed package is compiled in the compile/packages/... hierarchy, whereas a non-blessed 
 package is compiled in the compile/universes/... hierarchy *)

val package : t -> Package.t
(** The compiled package *)

val v :
  config:Config.t ->
  name:string ->
  voodoo:Voodoo.Gen.t Current.t ->
  Compile.t Current.t ->
  t Current.t
