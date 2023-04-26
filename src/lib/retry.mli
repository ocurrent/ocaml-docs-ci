val retry_loop :
  ?sleep_duration:(int -> float) ->
  ?log_string:string ->
  ?number_of_attempts:int ->
  ?max_number_of_attempts:int ->
  (unit -> ('a * 'c list, 'e) Lwt_result.t) ->
  ('a, 'e) Lwt_result.t
(** Retry the given function - the function is expected to return 'e to indicate
    retriable errors.

    Retries occur max_number_of_attempts times defaulting to 2. *)
