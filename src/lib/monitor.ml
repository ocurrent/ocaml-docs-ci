
type pipeline_tree =
  | Item : 'a Current.t -> pipeline_tree
  | Seq of (string * pipeline_tree) list
  | And of (string * pipeline_tree) list 
  | Or of (string * pipeline_tree) list 

type preps = U : (Package.t * _ Current.t) list OpamPackage.Map.t -> preps

type t = {
  mutable preps: preps;
  mutable blessing: Package.Blessing.Set.t Current.t OpamPackage.Map.t;
  mutable trees:  pipeline_tree Package.Map.t 
}

let make () = {
  preps = U OpamPackage.Map.empty;
  blessing = OpamPackage.Map.empty;
  trees = Package.Map.empty;
}

let register t preps blessing trees =
  t.preps <- U preps;
  t.blessing <- blessing;
  t.trees <- trees

let (let*) = Result.bind
let (let+) a f = Result.map f a

let rec simplify = 
  function
  | And [(_, a)] -> simplify a
  | v -> v 

let render_level = 
  let open Tyxml_html in
  function
  | 0 -> h1
  | 1 -> h2
  | 2 -> h3
  | 3 -> h4
  | 4 -> h5
  | _ -> h6

let render_list ~bullet ~level ~items ~render =
  let open Tyxml_html in
  ul(
    List.map (fun (name, item) -> 
      li ~a:[a_style ("list-style-type: " ^ bullet)]  [
      (render_level level) [ txt name ];
      render ~level:(level+1) item
    ]) items)

let rec render ~level = 
  let open Tyxml_html in
  function
  | Item current ->
    let result = Current.observe current in
    let container =
      try 
        Current.Analysis.metadata current
        |> Current.observe
        |> Result.to_option
        |> Option.join
        |> function
        | Some {job_id = Some job_id; _} -> 
          fun v -> a ~a:[a_href ("/job/" ^ job_id)] [txt v]
        | _ -> txt
      with (* if current is not a primitive term *)
      Failure _ -> txt
    in
    begin
    match result with
    | Error (`Msg  msg) -> container ("error: " ^ msg)
    | Error (`Active _) -> container "active"
    | Error `Blocked -> container "blocked"
    | Ok _ -> container "OK"
    end
  | Seq items -> 
    render_list ~bullet:"decimal" ~level ~items ~render
  | And items -> 
    render_list ~bullet:"circle" ~level ~items ~render
  | Or items -> 
    render_list ~bullet:"|" ~level ~items ~render

let get_package_info t name =
  let* opam_package = 
    OpamPackage.of_string_opt name 
    |> Option.to_result ~none:"invalid package name"
  in
  let* blessing_current =
    OpamPackage.Map.find_opt opam_package t.blessing
    |> Option.to_result ~none:"couldn't find package"
  in
  let+ blessing_set = 
    Current.observe blessing_current
    |> function
    | Ok v -> Ok v
    | Error _ -> Error "couldn't find blessing set"
  in
  match
    Package.Blessing.Set.blessed blessing_set
  with
  | None -> 
    let U preps = t.preps in
    Or
    (OpamPackage.Map.find opam_package preps
    |> List.map (fun (package, current) ->
      "prep " ^ Package.id package, Item current))
  | Some blessed_package ->
    let blessed_pipeline =
      Package.Map.find blessed_package t.trees
    in
    blessed_pipeline


let render_package_state t name =
  let* blessed_pipeline = get_package_info t name
  in
  let open Tyxml_html in
  Ok
  [
    h1 [txt ("Package " ^ name)];
    (render ~level:1 (simplify blessed_pipeline))
  ]

let handle t ~engine:_ str =
  object
    inherit Current_web.Resource.t

    method! private get context =
      let response = 
        match render_package_state t str with
        | Ok page -> page
        | Error msg -> Tyxml_html.[
            txt ("An error occured:");
            br ();  
            i [
              txt msg
            ]
          ]
      in
      Current_web.Context.respond_ok context response

  end

type state = Done | Running | Failed

let max a b = match (a,b) with
  | Done, v -> v
  | v, Done -> v
  | _, Failed -> Failed
  | Failed, _ -> Failed
  | Running, Running -> Running

let rec pipeline_state = function
  | Item v ->
    let result = Current.observe v
    in
    (match result with
    | Ok _ -> Done
    | Error (`Active _) -> Running
    | Error `Blocked -> Running
    | Error (`Msg _) -> Failed)
  | Seq lst | And lst | Or lst -> 
    List.fold_left 
      (fun acc (_, v) -> max (pipeline_state v) acc) 
      Done 
      lst

let opam_package_state t name =
  match 
    let+ blessed_pipeline = get_package_info t name
    in pipeline_state blessed_pipeline
  with
  | Ok v -> v
  | _ -> Failed

let render_link (pkg, _) =
  let open Tyxml_html in
  let name = OpamPackage.to_string pkg in
  li [
    a ~a:[a_href ("/package/"^name)] [
      txt name
    ]
  ]

let render_package_root t =
  let failed, pending =
    OpamPackage.Map.keys t.blessing
    |> List.map (fun k -> k, opam_package_state t (OpamPackage.to_string k))
    |> List.filter (fun (_, st) -> st != Done)
    |> List.partition (fun (_, st) -> st == Failed)
  in
  let open Tyxml_html in
  [
    h1 [txt "Failed packages"];
    ul (List.map render_link failed);
    h1 [txt "Running packages"];
    ul (List.map render_link pending);
  ]

let handle_root t ~engine:_ =
  object
    inherit Current_web.Resource.t
    method! nav_link = Some "Packages"

    val! can_get = `Viewer

    method! private get context =
      let response = render_package_root t
      in
      Current_web.Context.respond_ok context response

  end
      
let routes t engine =
  Routes.
    [
      (s "package" / str /? nil) @--> handle t ~engine;
      (s "package" /? nil) @--> handle_root t ~engine;
    ]
