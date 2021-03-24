module Git = Current_git

let track = Track.track_packages

let solve = Solver.v

let select_jobs ~targets =
  let open Current.Syntax in
  let+ targets = targets in
  Jobs.schedule ~targets

module Prep = Prep
