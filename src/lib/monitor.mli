type t
(** The type for the ci monitor. *)

type state =
  | Done
  | Running
  | Failed  (** The state of a package in the pipeline *)

(* type step_type =
   | Prep
   | DepCompilePrep of OpamPackage.t
   | DepCompileCompile of OpamPackage.t
   | Compile
   | BuildHtml *)

type step_status = Err of string | Active | Blocked | OK

type step = { typ : string; job_id : string option; status : step_status }
[@@deriving show, eq]

val make : unit -> t
(** Create a monitor. *)

type pipeline_tree =
  | Item : 'a Current.t -> pipeline_tree
  | Seq of (string * pipeline_tree) list
  | And of (string * pipeline_tree) list
  | Or of (string * pipeline_tree) list
      (** The pipeline dependency tree to produces artifacts for a given
          package. *)

val get_blessing : t -> Package.Blessing.Set.t Current.t OpamPackage.Map.t
(** Temporarily access the blessing set for fetching package information to
    return over capnp. *)

val register :
  t ->
  (OpamPackage.t * string) list ->
  (Package.t * _ Current.t) list OpamPackage.Map.t ->
  Package.Blessing.Set.t Current.t OpamPackage.Map.t ->
  pipeline_tree Package.Map.t ->
  unit
(** Register Current.t values for each package in the CI system. *)

val routes : t -> Current.Engine.t -> Current_web.Resource.t Routes.route list
(** Routes for the renderer *)

val map_versions :
  t -> (OpamPackage.Version.t * state) list OpamPackage.Name.Map.t
(** Map of package name to versions *)

val lookup_known_packages : t -> string list
(** Get a list of the names of known projects *)

val lookup_status :
  t -> name:string -> (OpamPackage.Name.t * OpamPackage.Version.t * state) list
(** Get a list of version and status tuples for a project *)

type package_steps = {
  package : OpamPackage.t;
  status : state;
  steps : step list;
}
[@@deriving eq]

val pp_package_steps : Format.formatter -> package_steps -> unit
val lookup_steps : t -> name:string -> (package_steps list, string) result
