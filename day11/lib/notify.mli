(** Pluggable notifications.

    Send messages to external services (Slack, Zulip, Telegram, Email) or to
    stdout. Each channel uses environment variables for credentials and
    endpoints; see the implementation for required variables. *)

(** Supported notification channels. *)
type channel = Slack | Zulip | Telegram | Email | Stdout

val send : channel:channel -> message:string -> int
(** Send [message] to the given [channel]. Returns [0] on success, [1] on
    failure. External channels dispatch via [curl]; {!Stdout} prints directly.
*)

val channel_of_string : string -> channel option
(** Parse a lowercase channel name (e.g. ["slack"], ["telegram"]). *)

val channel_to_string : channel -> string
(** Lowercase string representation of a {!type:channel}. *)
