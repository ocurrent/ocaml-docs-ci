module Status = struct
  type bless_status = Blessed | Universe

  type pending_status = Prep | Compile of bless_status

  type t = Pending of pending_status | Failed | Success of bless_status

  let to_int = function
    | Failed -> 0
    | Pending Prep -> 1
    | Pending (Compile Universe) -> 2
    | Pending (Compile Blessed) -> 3
    | Success Universe -> 4
    | Success Blessed -> 5

  let compare a b = to_int a - to_int b

  let pp f v =
    Fmt.pf f
      ( match v with
      | Failed -> "failed"
      | Pending Prep -> "pending prep"
      | Pending (Compile Universe) -> "compiling (universe)"
      | Pending (Compile Blessed) -> "compiling (blessed)"
      | Success Universe -> "success (universe)"
      | Success Blessed -> "success (blessed)" )

  let to_web = function Failed -> `Failed | Pending _ -> `Pending | Success _ -> `Success
end

type t = {
  updates : (Package.t * Status.t) Lwt_stream.t;
  push_update : (Package.t * Status.t) option -> unit;
}

let make () =
  let updates, push_update = Lwt_stream.create () in
  { updates; push_update }

let set_package_status ~package ~status t =
  let open Current.Syntax in
  let+ package = package and+ status = status in
  t.push_update (Some (package, status))

module SMap = Map.Make (String)

type version = {
  name : OpamPackage.Name.t;
  version : OpamPackage.Version.t;
  universes : Status.t SMap.t;
}

let get_blessed_status t =
  t.universes |> SMap.bindings
  |> List.fold_left
       (fun acc -> function
         | _, Status.Pending (Compile Blessed) -> Status.Pending (Compile Blessed)
         | _, Success Blessed -> Success Blessed | _, Pending Prep when acc = Failed -> Pending Prep
         | _, (Pending (Compile Universe | Prep) | Success Universe | Failed) -> acc)
       Status.Failed

let get_blessed_universe t =
  t.universes |> SMap.bindings
  |> List.find_map (function
       | id, (Status.(Success Blessed) | Pending (Compile Blessed)) -> Some id
       | _ -> None)

type package = { name : OpamPackage.Name.t; versions : version OpamPackage.Version.Map.t }

type state = { mutable data : package OpamPackage.Name.Map.t }

module Graphql = struct
  open Graphql_lwt

  let status =
    Schema.(
      enum "Status" ~doc:"Node status"
        ~values:
          [
            enum_value "FAILED" ~value:`Failed;
            enum_value "PENDING" ~value:`Pending;
            enum_value "SUCCESS" ~value:`Success;
          ])

  let universe =
    Schema.(
      obj "PackageVersionUniverse" ~doc:"An opam package version universe" ~fields:(fun _ ->
          [
            field "name" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ (name, _, _, _) -> OpamPackage.Name.to_string name);
            field "version" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ (_, version, _, _) -> OpamPackage.Version.to_string version);
            field "universe" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ (_, _, universe, _) -> universe);
            field "status" ~typ:(non_null status)
              ~args:Arg.[]
              ~resolve:(fun _ (_, _, _, status) -> status |> Status.to_web);
          ]))

  let version =
    Schema.(
      obj "PackageVersion" ~doc:"An opam package version" ~fields:(fun _ ->
          [
            field "name" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ ({ name; _ } : version) -> OpamPackage.Name.to_string name);
            field "version" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ { version; _ } -> OpamPackage.Version.to_string version);
            field "blessed_universe" ~typ:string
              ~args:Arg.[]
              ~resolve:(fun _ -> get_blessed_universe);
            field "status" ~typ:(non_null status)
              ~args:Arg.[]
              ~resolve:(fun _ t -> get_blessed_status t |> Status.to_web);
            field "universes"
              ~typ:(non_null (list (non_null universe)))
              ~args:Arg.[]
              ~resolve:(fun _ { universes; name; version } ->
                SMap.bindings universes |> List.map (fun (u, s) -> (name, version, u, s)));
          ]))

  let package =
    Schema.(
      obj "Package" ~doc:"An opam package" ~fields:(fun _ ->
          [
            field "name" ~typ:(non_null string)
              ~args:Arg.[]
              ~resolve:(fun _ { name; _ } -> OpamPackage.Name.to_string name);
            field "versions"
              ~typ:(non_null (list (non_null version)))
              ~args:Arg.[]
              ~resolve:(fun _ { versions; _ } -> OpamPackage.Version.Map.values versions);
          ]))

  let schema =
    Schema.(
      schema
        [
          field "packages"
            ~typ:(non_null (list (non_null package)))
            ~args:Arg.[]
            ~resolve:(fun { ctx; _ } () -> OpamPackage.Name.Map.values ctx.data);
          field "static_files_endpoint"
            ~typ:(non_null string)
            ~args:Arg.[]
            ~resolve:(fun _ () -> Config.docs_public_endpoint)
        ])

  module Graphql_cohttp_lwt = Graphql_cohttp.Make (Schema) (Cohttp_lwt_unix.IO) (Cohttp_lwt.Body)

  let run ~port (ctx : state) =
    let callback = Graphql_cohttp_lwt.make_callback (fun _req -> ctx) schema in
    let server = Cohttp_lwt_unix.Server.make_response_action ~callback () in
    let mode = `TCP (`Port port) in
    Cohttp_lwt_unix.Server.create ~mode server
end

let update package status t =
  let opam = Package.opam package in
  let name = OpamPackage.name opam in
  let version = OpamPackage.version opam in
  let universe = Package.universe package |> Package.Universe.hash in

  let package_version_update { name; version; universes } =
    let universes = SMap.add universe status universes in
    { name; version; universes }
  in
  let package_name_update { name; versions } =
    let versions =
      OpamPackage.Version.Map.update version package_version_update
        { name; version; universes = SMap.empty }
        versions
    in
    { name; versions }
  in
  t.data <-
    OpamPackage.Name.Map.update name package_name_update
      { name; versions = OpamPackage.Version.Map.empty }
      t.data

let serve ~port t =
  let state = { data = OpamPackage.Name.Map.empty } in
  Lwt.choose
    [
      Lwt_stream.iter (fun (package, status) -> update package status state) t.updates;
      Graphql.run ~port state;
    ]
