open Graphql_lwt
open Lwt.Syntax
open Lwt.Infix

module Info = struct
  type t = {
    synopsis : string;
    description : string;
    authors : string list;
    license : string;
    publication_date : string;
    homepage : string list;
    tags : string list;
    maintainers : string list;
    dependencies : (OpamPackage.Name.t * string option) list;
    depopts : (OpamPackage.Name.t * string option) list;
    conflicts : (OpamPackage.Name.t * string option) list;
  }
end

type dependencies = {
  dependencyName: string;
  constraints: string;
}

(* type dependencies_with_constraints = {
  dependencyName: string;
  constraints: string;
}

type dependencies_without_constraints = {
  dependencyName: string;
} *)

(* type dependants = {
  dependantName: string;
  version: string
} *)

type package = {
  name : string;
  version : string;
  synopsis : string;
  description : string;
  license : string;
  publication_date : string;
  authors : string;
  homepage : string;
  tags : string;
  maintainers : string;
  dependencies: dependencies list;
}

type response = {
  totalPackages: int;
  limit: int;
  packages: package list;
}

let starts_with s1 s2 =
  let len1 = String.length s1 in
  if len1 > String.length s2 then
    false
  else
    let s1 = String.lowercase_ascii s1 in
    let s2 = String.lowercase_ascii s2 in
    String.(equal (sub s2 0 (length s1)) s1)

let is_package s1 s2 =
  let len1 = String.length s1 in
  if len1 > String.length s2 then
    false
  else
    let s1 = String.lowercase_ascii s1 in
    let s2 = String.lowercase_ascii s2 in
    String.(equal s2 s1)

let get_dependencies deps = 
  let dependencies = List.map (function | (name, None) -> { dependencyName = OpamPackage.Name.to_string name; constraints = "" } | (name, Some constraints) -> { dependencyName = OpamPackage.Name.to_string name; constraints = constraints }) deps in
  dependencies

let all_packages ~state =
  let packagesFromSource = State.all_packages_gql(state) in
  let packagesFromSource = OpamPackage.Name.Map.bindings(packagesFromSource) in
  let packages = List.map (fun (name, (version, info)) -> { name = OpamPackage.Name.to_string name; version = OpamPackage.Version.to_string version; synopsis = info.Package.Info.synopsis; description = info.Package.Info.description; license = info.Package.Info.license; publication_date = info.Package.Info.publication_date; authors = String.concat ";" info.Package.Info.authors;  homepage = String.concat ";" info.Package.Info.homepage; tags = String.concat ";" info.Package.Info.tags; maintainers = String.concat ";" info.Package.Info.maintainers; dependencies = (get_dependencies info.Package.Info.dependencies) }) packagesFromSource in
  packages

let single_package ~state name version =
  let version = OpamPackage.Version.of_string version in
  let versions = State.get_package_gql state name in
  let info = OpamPackage.Version.Map.find version versions in
  let package = { name = OpamPackage.Name.to_string name; version = OpamPackage.Version.to_string version; synopsis = info.Package.Info.synopsis; description = info.Package.Info.description; license = info.Package.Info.license; publication_date = info.Package.Info.publication_date; authors = String.concat ";" info.Package.Info.authors;  homepage = String.concat ";" info.Package.Info.homepage; tags = String.concat ";" info.Package.Info.tags; maintainers = String.concat ";" info.Package.Info.maintainers; dependencies = (get_dependencies info.Package.Info.dependencies) } in
  package

let get_package_dependendants dependencies packageName = 
  let res = List.find_opt (fun deps -> is_package packageName deps.dependencyName) dependencies in
  match res with
  | None -> false
  | Some _ -> true

let package_dependendants ~state packageName = 
  let all_packages = all_packages ~state in
  let dependendants = List.filter (fun pkg -> get_package_dependendants pkg.dependencies packageName ) all_packages in
  dependendants

let package_dependencies packages packageName = 
  let dependencies = List.find (fun pkg -> is_package packageName pkg.name) packages in
  dependencies

let dependencies =
  Graphql_lwt.Schema.(obj "package" ~fields:(fun _ ->
    [ field
      "dependencyName"
      ~doc:"Unique dependency name"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.dependencyName)
    ; field
      "constraints"
      ~doc:"Dependency constraints"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.constraints)
    ]
  ))

let package =
  Graphql_lwt.Schema.(obj "package" ~fields:(fun _ ->
    [ field
      "name"
      ~doc:"Unique package name"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.name)
    ; field
      "version"
      ~doc:"Package latest release version"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.version)
    ; field
      "synopsis"
      ~doc:"The synopsis of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.synopsis)
    ; field
      "description"
      ~doc:"The description of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.description)
    ; field
      "license"
      ~doc:"The license of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.license)
    ; field
      "publication_date"
      ~doc:"The publication date of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.publication_date)
    ; field
      "authors"
      ~doc:"The authors of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.authors)
      ; field
      "homepage"
      ~doc:"The homepage of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.homepage)
    ; field
      "tags"
      ~doc:"The tags of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.tags)
    ; field
      "maintainers"
      ~doc:"The maintainers of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.maintainers)
      ; field
      "dependencies"
      ~doc:"The dependencies of the package"
      ~args:Arg.[]
      ~typ:(non_null (list (non_null dependencies)))
      ~resolve:(fun _ p -> p.dependencies)
    ]
  ))

let response = 
  Graphql_lwt.Schema.(obj "response" ~fields:(fun _ ->
    [ field
      "totalPackages"
      ~doc:"total number of packages"
      ~args:Arg.[]
      ~typ:(non_null int)
      ~resolve:(fun _ p -> p.totalPackages)
      ; field
      "limit"
      ~doc:"packages limit to send at a time"
      ~args:Arg.[]
      ~typ:(non_null int)
      ~resolve:(fun _ p -> p.limit)
    ; field
      "packages"
      ~doc:"packages"
      ~args:Arg.[]
      ~typ:(non_null (list (non_null package)))
      ~resolve:(fun _ p -> p.packages)
    ]
  ))
        
let schema ~(state:State.t) : Dream.request Graphql_lwt.Schema.schema =
  Graphql_lwt.Schema.(schema [
    field
      "allPackages"
      ~typ:(non_null response)
      ~args:
        Arg.
          [ 
            arg'
              ~doc:
                "Filter packages in asc by name or date or based on search query"
                "filter"
              ~typ:string
              ~default:"sortByName";
            arg'
              ~doc:
                "Paginate packages"
                "offset"
              ~typ:int
              ~default: 0;
            arg'
              ~doc:
                "Paginate packages"
                "limit"
              ~typ:int
              ~default: 500
          ]
          ~resolve:(fun _ () filter offset limit ->
            let packages = all_packages ~state in 
            let totalPackages = List.length packages in
            let limit = limit in
            if filter = "sortByName" then 
              let packagesList = Array.of_list packages in 
              let packages = Array.to_list (Array.sub packagesList offset limit) in
              let response = { totalPackages; limit; packages } in 
              response
            else if filter = "" then 
              let packagesList = Array.of_list packages in 
              let packages = Array.to_list (Array.sub packagesList offset limit) in
              let response = { totalPackages; limit; packages } in 
              response
            else
              let packages = List.filter
              (fun package -> starts_with filter package.name) (packages) in
              let response = { totalPackages; limit; packages } in
              response
          );
    field
      "package"
      ~typ:(non_null package)
      ~args:
        Arg.
          [ 
            arg'
              ~doc:
                "Get a single package by name"
                "name"
              ~typ:string
              ~default:"";
            arg'
              ~doc:
                "Get a single package by version"
                "version"
              ~typ:string
              ~default:"none";
          ]
          ~resolve:(fun _ () name version ->
            (* match name with
            | None -> "Not Found Error"
            | Some name ->  *)
              if version = "none" then 
                let all_packages = all_packages ~state in 
              (* let single_package = List.find_opt (fun package -> is_package name package.name) all_packages in  *)
                let single_package = List.find (fun package -> is_package name package.name) all_packages in 
                single_package
              else 
                let name = OpamPackage.Name.of_string name in
                let package = single_package ~state name version in
                package
          );
    field
      "dependants"
      ~typ:(non_null (list (non_null package)))
      ~args:
        Arg.
          [ 
            arg'
              ~doc:
                "All package dependants"
                "name"
              ~typ:string
              ~default:""
          ]
          ~resolve:(fun _ () name ->
            let package_dependendants = package_dependendants ~state name in
            package_dependendants);
    field
      "dependencies"
      ~typ:(non_null (list (non_null package)))
      ~args:
        Arg.
          [ 
            arg'
              ~doc:
                "All package dependencies"
                "name"
              ~typ:string
              ~default:""
          ]
          ~resolve:(fun _ () name ->
            let all_packages = all_packages ~state in 
            let single_package = List.find (fun package -> is_package name package.name) all_packages in
            let package_dependencies = List.map (fun deps -> package_dependencies all_packages deps.dependencyName ) single_package.dependencies in
            package_dependencies)
    ])