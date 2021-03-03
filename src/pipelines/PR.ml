module Github = Current_github
module Git = Current_git

type pr_info = { id : string; label : string; pipeline : unit Current.t }

type t = {
  mirage_skeleton : pr_info list ref;
  mirage_dev : pr_info list ref;
  mirage : pr_info list ref;
  pipeline : unit Current.t;
}

type gh_repo = {
  ci : Github.Api.Commit.t list Current.t;
  main : Github.Api.Commit.t Current.t;
  refs : Github.Api.refs Current.t;
}

let repo_refs ~github repo =
  let refs = Github.Api.refs github repo in
  Current.primitive ~info:(Current.component "repository refs") (fun () -> refs) (Current.return ())

let github_setup ~github owner name =
  let gh = { Github.Repo_id.owner; name } in
  let ci_refs = Github.Api.ci_refs ~staleness:(Duration.of_day 30) github gh in
  let repo_refs = repo_refs ~github gh in
  let default_branch = Github.Api.head_commit github gh in
  { ci = ci_refs; refs = repo_refs; main = default_branch }

let url kind id = Uri.of_string (Fmt.str "https://ci.mirage.io/github/%s/prs/%s" kind id)

let github_status_of_state kind id status =
  let url = url kind id in
  match status with
  | Ok _ -> Github.Api.Status.v ~url `Success ~description:"Passed"
  | Error (`Active _) -> Github.Api.Status.v ~url `Pending
  | Error (`Msg m) -> Github.Api.Status.v ~url `Failure ~description:m

let perform_test ~platform ~mirage_dev ~mirage_skeleton ~mirage ~repos kind gh_commit =
  let open Current.Syntax in
  let repos =
    let+ repos = repos and+ mirage_dev = mirage_dev in
    ("mirage-dev", mirage_dev) :: repos
  in
  let* gh_commit' = gh_commit in
  let id =
    Fmt.str "%s-%s"
      (Github.Api.Commit.id gh_commit' |> Git.Commit_id.hash)
      (Mirage_ci_lib.Platform.platform_id platform)
  in
  let pipeline = Skeleton.v_main ~platform ~mirage ~repos mirage_skeleton in
  let result =
    Current.return { pipeline; label = Fmt.str "%a" Github.Api.Commit.pp gh_commit'; id }
  in
  let+ _ =
    match Mirage_ci_lib.Config.v.enable_commit_status with
    | false -> pipeline
    | true ->
        pipeline |> Current.state ~hidden:true
        |> Current.map (github_status_of_state kind id)
        |> Github.Api.Commit.set_status gh_commit
             (Fmt.str "Mirage CI - %a" Mirage_ci_lib.Platform.pp_platform platform)
  and+ result = result in
  result

let update lst value =
  let open Current.Syntax in
  let+ value = value in
  lst := List.flatten value

module CommitUrl = struct
  type t = Github.Api.Commit.t * (string * string)

  let pp f (_, (text, _)) = Fmt.pf f "%s" text

  let url f (_, (_, url)) = Fmt.pf f "%s" url

  let compare (a, _) (b, _) = Github.Api.Commit.compare a b
end

let pp_url ~(repo : Github.Repo_id.t) f ref =
  match ref with
  | `Ref ref -> Fmt.pf f "https://github.com/%s/%s/tree/%s" repo.owner repo.name ref
  | `PR pr -> Fmt.pf f "https://github.com/%s/%s/pull/%d" repo.owner repo.name pr

let url_of_commit (commit : Github.Api.Commit.t) (refs : Github.Api.refs) =
  let open Github in
  let map = Api.all_refs refs in
  let repo = Api.Commit.repo_id commit in
  let commit_refs =
    Api.Ref_map.filter (fun _ commit' -> Api.Commit.(hash commit' = hash commit)) map
  in
  Api.Ref_map.bindings commit_refs |> function
  | [] -> ("no refs point to this commit", "")
  | (ref, _) :: _ -> (Fmt.str "Github: %a" Api.Ref.pp ref, Fmt.to_to_string (pp_url ~repo) ref)

let make github repos =
  let id_of gh_commit = Current.map Github.Api.Commit.id gh_commit in
  let gh_mirage_skeleton = github_setup ~github "mirage" "mirage-skeleton" in
  let gh_mirage = github_setup ~github "mirage" "mirage" in
  let gh_mirage_dev = github_setup ~github "mirage" "mirage-dev" in
  let mirage_skeleton = id_of gh_mirage_skeleton.main in
  let mirage = id_of gh_mirage.main in
  let mirage_dev = id_of gh_mirage_dev.main in
  let mirage_skeleton_prs = ref [] in
  let mirage_dev_prs = ref [] in
  let mirage_prs = ref [] in
  let pipeline =
    Current.with_context mirage_skeleton @@ fun () ->
    Current.with_context mirage @@ fun () ->
    Current.with_context mirage_dev @@ fun () ->
    let mirage_skeleton =
      gh_mirage_skeleton.ci
      |> Current.pair gh_mirage_skeleton.refs
      |> Current.map (fun (refs, commits) -> List.map (fun c -> (c, url_of_commit c refs)) commits)
      |> Current.list_map_url
           (module CommitUrl)
           (fun commit ->
             let commit = Current.map fst commit in
             let mirage_skeleton = id_of commit in
             Mirage_ci_lib.Platform.[ platform_amd64; platform_arm64 ]
             |> List.map (fun platform ->
                    perform_test ~platform ~mirage_dev ~mirage_skeleton ~mirage ~repos
                      "mirage-skeleton" commit
                    |> Current.collapse
                         ~key:(Fmt.str "%a" Mirage_ci_lib.Platform.pp_platform platform)
                         ~value:"" ~input:commit)
             |> Current.list_seq)
      |> update mirage_skeleton_prs
      (*and mirage_dev = Current.list_map (module Github.Api.Commit) (fun gh_mirage_dev ->
          let mirage_dev = id_of gh_mirage_dev in
          perform_test ~mirage_dev ~mirage_skeleton ~mirage ~repos "mirage-dev" gh_mirage_dev) gh_mirage_dev.ci
          |> update mirage_dev_prs
        and mirage = Current.list_map (module Github.Api.Commit) (fun gh_mirage ->
          let mirage = id_of gh_mirage in
          perform_test ~mirage_dev ~mirage_skeleton ~mirage ~repos "mirage" gh_mirage) gh_mirage.ci
          |> update mirage_prs*)
    in
    Current.all_labelled
      [
        ("mirage-skeleton", mirage_skeleton);
        (*("mirage-dev", mirage_dev);
          ("mirage", mirage);*)
      ]
  in
  {
    pipeline;
    mirage_skeleton = mirage_skeleton_prs;
    mirage_dev = mirage_dev_prs;
    mirage = mirage_prs;
  }

let to_current t = t.pipeline

open Current_web
open Tyxml.Html

let render_pipeline ~job_info a = Fmt.str "%a" (Current.Analysis.pp_html ~job_info) a

let r (pr : pr_info) =
  object
    inherit Current_web.Resource.t

    val! can_get = `Viewer

    method! private get ctx =
      let pipeline = pr.pipeline in
      let job_info { Current.Metadata.job_id; update } =
        let url = job_id |> Option.map (fun id -> Fmt.str "/job/%s" id) in
        (update, url)
      in
      Context.respond_ok ctx
        [
          style
            [
              Unsafe.data
                {|
      #pipeline_container {
        display: flex;
        flex-direction: row;
      }

      #logs_iframe {
        height: calc(100vh - 40px);
        flex: 1;
        border: none;
        border-left: solid gray 1px;
        padding-left: 10px;
        margin-left: 10px;
      }

      |};
            ];
          div ~a:[ a_id "pipeline_container" ]
            [
              div ~a:[ a_id "pipeline" ] [ Unsafe.data (render_pipeline ~job_info pipeline) ];
              Unsafe.data "<iframe id='logs_iframe' ></iframe>";
            ];
          script
            (Unsafe.data
               {|
        let logs = document.getElementById("logs_iframe");

        function setLogsUrl(url) {
          logs.src = url;
        }
        |});
        ]

    method! nav_link = Some "New pipeline rendering"
  end

let route skeleton_prs pr_id =
  let pr = List.find (fun pr -> pr.id = pr_id) !skeleton_prs in
  (* TODO: what if not found*)
  r pr

let routes t =
  Routes.
    [
      (s "github" / s "mirage-skeleton" / s "prs" / str /? nil) @--> route t.mirage_skeleton;
      (s "github" / s "mirage-dev" / s "prs" / str /? nil) @--> route t.mirage_dev;
      (s "github" / s "mirage" / s "prs" / str /? nil) @--> route t.mirage;
    ]
