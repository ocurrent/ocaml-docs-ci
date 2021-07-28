(** Compilation step

The documentation compilation is done as an ocluster. It takes for input one prep/ folder and its
compiled dependencies. It uses `voodoo-do` to perform the compilation, link and html generation 
steps, outputting the results in the compile/ and html/ folders.  
*)

type hashes = { html_raw_hash : string }

type t
(** A compiled package *)

val hashes : t -> hashes
(** Hash of the compiled artifacts  *)

val blessing : t -> Package.Blessing.t
(** A blessed package is compiled in the compile/p/... hierarchy, whereas a non-blessed 
 package is compiled in the compile/u/... hierarchy *)

val package : t -> Package.t
(** The compiled package *)

val v :
  generation:Epoch.t Current.t ->
  config:Config.t ->
  name:string ->
  voodoo:Voodoo.Gen.t Current.t ->
  Compile.t Current.t ->
  t Current.t
