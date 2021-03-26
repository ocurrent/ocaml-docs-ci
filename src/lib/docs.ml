module Git = Current_git

let track = Track.track_packages

let solve = Solver.v

let select_jobs ~targets jobs =
  let open Current.Syntax in
  let+ targets = targets and+ jobs = jobs in
  Jobs.schedule ~targets jobs

module Prep = Prep

