open Js_of_ocaml
open Brr
open Brr_io
open Fut.Syntax
module Html = Dom_html

type package = {
  name : string;
  version : string;
  synopsis : string;
}

let packages_holder = ref None

let remove_old_tbody () =
  let tf = Html.(createTfoot document) in
  tf##.id := Js.string "tfoot";
  let table =
    Js.Opt.get
      (Html.document##getElementById (Js.string "clear_tbody"))
      (fun () -> assert false)
  in
  let tbody =
    Js.Opt.get
      (Html.document##getElementById (Js.string "opam_packages"))
      (fun () -> assert false)
  in
  Dom.replaceChild table tf tbody

let create_new_tbody () =
  let table =
    Js.Opt.get
      (Html.document##getElementById (Js.string "clear_tbody"))
      (fun () -> assert false)
  in
  let tfoot =
    Js.Opt.get
      (Html.document##getElementById (Js.string "tfoot"))
      (fun () -> assert false)
  in
  let tbody = Html.(createTbody document) in
  tbody##.id := Js.string "opam_packages";
  Dom.replaceChild table tbody tfoot

let package_query ~search =
  let query = 
    {|
      {
        allPackages (filter: "|} ^ search ^ {|") {
          totalPackages
          limit
          packages {
            name
            version
            synopsis
          }
        }
      }
    |}
  in
  Ezjsonm.value_to_string (`O [ "query", `String query ])

let page_query ~offset ~limit =
  let query = 
    {|
      {
        allPackages (offset: "|} ^ offset ^ {|", limit: "|} ^ limit ^ {|") {
          totalPackages
          limit
          packages {
            name
            version
            synopsis
          }
        }
      }
    |}
  in
  Ezjsonm.value_to_string (`O [ "query", `String query ])

(* 
let create_pagination totalPackages limit = 
  let pages = totalPackages / limit in
  let rem = totalPackages mod limit in
  if rem > 0 then
    let pages = pages + 1
  else 
    let pages = pages
  for i = 0 to pages () do
    let btn_name = 
    let btn ^ page = Html.(createButton document) in
    let query = page_query i limit
    let req =  fetch_packages query in
    in
  legendBtns##.onclick :=
    Html.handler (fun v ->
      Js.Opt.iter v##.target (fun t ->
        search_handler (Js.to_string t##.innerHTML));
      Js._true);
    link##.innerHTML := Js.string page + 1;
  done
  let pg1 = fetch_packages page_query 0 300 in
  Fut.await pg1 format_packages;
  let pg2 = fetch_packages page_query 301 300 in
  Fut.await pg1 format_packages; 
  
*)

let display_pkgs { name; synopsis; version } =
  let loader =
    Js.Opt.get
      (Html.document##getElementById (Js.string "overlay"))
      (fun () -> assert false)
  in
  let content =
    Js.Opt.get
      (Html.document##getElementById (Js.string "content"))
      (fun () -> assert false)
  in
  let tbody =
    Js.Opt.get
      (Html.document##getElementById (Js.string "opam_packages"))
      (fun () -> assert false)
  in
  let link = Html.(createA document) in
  link##.href := Js.string ("packages/" ^ name ^ "/" ^ version ^ "/");
  link##.innerHTML := Js.string name;
  let tr = Html.(createTr document) in
  let td1 = Html.(createTd document) in
  Dom.appendChild td1 link ;
  Dom.appendChild tr td1;
  let td2 = Html.(createTd document) in
  td2##.innerHTML := Js.string synopsis;
  Dom.appendChild tr td2;
  let td3 = Html.(createTd document) in
  td3##.innerHTML := Js.string version;
  Dom.appendChild tr td3;
  Dom.appendChild tbody tr;
  loader##.style##.display := Js.string "none";
  content##.style##.display := Js.string "block"

let get_string key l =
  match List.assoc key l with `String s -> s | _ -> raise Not_found

let format_packages packages =
  match packages with
  | Some packages ->
    (try
      let packages = Jstr.to_string (Json.encode packages) in
      Console.log [ packages ];
      let json = Ezjsonm.from_string packages in
      Console.log [ json ];
      let json = Ezjsonm.find json [ "data"; "allPackages"; "packages" ] in
      Console.log [ json ];
      match json with
      | `A pkgs ->
         let add_pkg l = function
           | `O pkg ->
             let name = get_string "name" pkg in
             let synopsis = get_string "synopsis" pkg in
             let version = get_string "version" pkg in
             { name; synopsis; version } :: l
           | _ ->
             l
         in
           List.iter display_pkgs (List.rev (List.fold_left add_pkg [] pkgs))
      | _ ->
        Console.log [ Js.string packages ]
     with
    | e ->
      Console.error [ Js.string ("Packages Error - " ^ Printexc.to_string e) ])
  | None ->
    Console.error [ Js.string "There was an error" ]

let get_packages_response_data response =
  let* data = Fetch.Body.json (Fetch.Response.as_body response) in
  match data with
  | Ok response ->
    packages_holder := Some response;
    Fut.return (Some response)
  | Error _ ->
    Console.error [ Jstr.of_string "ERROR" ];
    Fut.return None

let get_packages url query =
  let init =
    Fetch.Request.init
      ~method':(Jstr.of_string "POST")
      ~body:(Fetch.Body.of_jstr (Jstr.of_string query))
      ~headers:
        (Fetch.Headers.of_assoc
           [ Jstr.of_string "Content-Type", Jstr.of_string "application/json" ])
      ()
  in
  let* result = Fetch.url ~init (Jstr.of_string url) in
  match result with
  | Ok response ->
    get_packages_response_data response
  | Error _ ->
    Console.error [ Jstr.of_string "ERROR" ];
    Fut.return None

let fetch_packages query =
  let url = "./graphql" in
  let result = get_packages url query in
  result

let search_handler data =
  let loader =
    Js.Opt.get
      (Html.document##getElementById (Js.string "overlay"))
      (fun () -> assert false)
  in
  loader##.style##.display := Js.string "block";
  let search_data = String.lowercase_ascii data in
  let query = package_query ~search:search_data in
  let pkgs = fetch_packages query in
  remove_old_tbody ();
  create_new_tbody ();
  Fut.await pkgs format_packages

let start _ =
  let query = package_query ~search:"sortByName" in
  let result = fetch_packages query in
  Fut.await result format_packages;
  let searchInput=
    Js.Opt.get
      (Html.document##getElementById (Js.string "filter"))
      (fun () -> assert false)
  in
  let searchBtn =
    Js.Opt.get
      (Html.document##getElementById (Js.string "search"))
      (fun () -> assert false)
  in
  searchBtn##.onclick :=
    Html.handler (fun v ->
      Js.Opt.iter v##.target (fun _ ->
        Js.Opt.iter (Html.CoerceTo.input searchInput) (fun t ->
          search_handler (Js.to_string t##.value)));
      Js._true);
  let legendBtns =
    Js.Opt.get
      (Html.document##getElementById (Js.string "legends"))
      (fun () -> assert false)
  in
  legendBtns##.onclick :=
    Html.handler (fun v ->
      Js.Opt.iter v##.target (fun t ->
        search_handler (Js.to_string t##.innerHTML));
      Js._true);
  Js._false

let _ = Html.window##.onload := Html.handler start