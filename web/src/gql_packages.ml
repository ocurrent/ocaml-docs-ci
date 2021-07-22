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

type package = {
  name : string;
  version : string;
  synopsis : string;
}

type res = {
  totalPackages: int;
  packages: package list
}

let starts_with s1 s2 =
  let len1 = String.length s1 in
  if len1 > String.length s2 then
    false
  else
    let s1 = String.lowercase_ascii s1 in
    let s2 = String.lowercase_ascii s2 in
    String.(equal (sub s2 0 (length s1)) s1)

let allPackages ~state =
  let packagesFromSource = State.all_packages_lst(state) in
  let packagesFromSource = OpamPackage.Name.Map.bindings(packagesFromSource) in
  let packages = List.map (fun (name, (version, info)) -> { name = OpamPackage.Name.to_string name; version = OpamPackage.Version.to_string version; synopsis = info.Package.Info.synopsis }) packagesFromSource in
  packages

let package =
  Graphql_lwt.Schema.(obj "package" ~fields:(fun _ ->
    {
      field
      "totalPackages"
      ~doc:"Unique package name"
      ~args:Arg.[]
      ~typ:(non_null int)
      ~resolve:(fun _ p -> p.totalPackages);
    [ field
      "name"
      ~doc:"Unique package name"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.packages.name)
    ; field
      "version"
      ~doc:"Package latest release version"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.packages.version)
    ; field
      "synopsis"
      ~doc:"The synopsis of the package"
      ~args:Arg.[]
      ~typ:(non_null string)
      ~resolve:(fun _ p -> p.packages.synopsis)
    ]
    }
  ))
        
let schema ~(state:State.t) : Dream.request Graphql_lwt.Schema.schema =
  Graphql_lwt.Schema.(schema [
    field
          "allPackages"
          ~typ:(non_null (list (non_null package)))
          ~args:
            Arg.
              [ arg'
                  ~doc:
                    "Filter packages in asc by name or date or based on search \
                     query"
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
                  ~default: 200
              ]
          ~resolve:(fun _ () filter offset limit ->
            let packages = allPackages ~state in 
            let totalPackages = List.length packages in
            if filter = "sortByName" then 
              let packagesList = Array.of_list packages in 
              let packages = Array.to_list (Array.sub packagesList offset limit) in
              let res = { totalPackages; packages } in 
              res
            else if filter = "" then 
              let packagesList = Array.of_list packages in 
              let packages = Array.to_list (Array.sub packagesList offset limit) in
              let res = { totalPackages; packages } in 
              res
            else
              let packages = List.filter
              (fun package -> starts_with filter package.name) (packages) in
              let res = { totalPackages; packages } in
              res)
    ])