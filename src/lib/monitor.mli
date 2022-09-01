type t
(** The type for the ci monitor*)

val make : unit -> t
(** Create a monitor *)

type pipeline_tree =
  | Item : 'a Current.t -> pipeline_tree
  | Seq of (string * pipeline_tree) list
  | And of (string * pipeline_tree) list 
  | Or of (string * pipeline_tree) list 
(** The pipeline dependency tree to produces artifacts for
    a given package. *)

val register :
  t ->
  (OpamPackage.t * string) list ->
  (Package.t * _ Current.t) list OpamPackage.Map.t ->
  Package.Blessing.Set.t Current.t OpamPackage.Map.t ->
  pipeline_tree Package.Map.t -> unit
(** Register Current.t values for each package in the CI system. *)

val routes :
  t -> Current.Engine.t -> Current_web.Resource.t Routes.route list
(** Routes for the renderer *)
