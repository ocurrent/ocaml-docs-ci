type channel = Slack | Zulip | Telegram | Email | Stdout

let channel_of_string = function
  | "slack" -> Some Slack
  | "zulip" -> Some Zulip
  | "telegram" -> Some Telegram
  | "email" -> Some Email
  | "stdout" -> Some Stdout
  | _ -> None

let channel_to_string = function
  | Slack -> "slack"
  | Zulip -> "zulip"
  | Telegram -> "telegram"
  | Email -> "email"
  | Stdout -> "stdout"

let env key =
  try Sys.getenv key
  with Not_found ->
    failwith (Printf.sprintf "Environment variable %s not set" key)

let run_curl args =
  let cmd =
    String.concat " "
      ("curl" :: "-s" :: "-o" :: "/dev/null" :: "-w" :: "'%{http_code}'" :: args)
  in
  let ic = Unix.open_process_in cmd in
  let output = try input_line ic with End_of_file -> "" in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 ->
      let code =
        try
          int_of_string
            ( String.trim output |> fun s ->
              if String.length s >= 2 && s.[0] = '\'' then
                String.sub s 1 (String.length s - 2)
              else s )
        with _ -> 0
      in
      if code >= 200 && code < 300 then 0 else 1
  | _ -> 1

let send ~channel ~message =
  match channel with
  | Stdout ->
      print_endline message;
      0
  | Slack ->
      let url = env "SLACK_WEBHOOK_URL" in
      let escaped = String.concat "\\\"" (String.split_on_char '"' message) in
      run_curl
        [
          "-X";
          "POST";
          "-H";
          "'Content-type: application/json'";
          "-d";
          Printf.sprintf "'{\"text\":\"%s\"}'" escaped;
          url;
        ]
  | Zulip ->
      let email = env "ZULIP_BOT_EMAIL" in
      let api_key = env "ZULIP_BOT_API_KEY" in
      let server = env "ZULIP_SERVER" in
      let stream = env "ZULIP_STREAM" in
      run_curl
        [
          "-u";
          Printf.sprintf "%s:%s" email api_key;
          "-X";
          "POST";
          Printf.sprintf "%s/api/v1/messages" server;
          "-d";
          Printf.sprintf "'type=stream&to=%s&topic=day10&content=%s'" stream
            message;
        ]
  | Telegram ->
      let token = env "TELEGRAM_BOT_TOKEN" in
      let chat_id = env "TELEGRAM_CHAT_ID" in
      let escaped = String.concat "\\\"" (String.split_on_char '"' message) in
      let escaped = String.concat "\\n" (String.split_on_char '\n' escaped) in
      run_curl
        [
          "-X";
          "POST";
          "-H";
          "'Content-type: application/json'";
          Printf.sprintf "'https://api.telegram.org/bot%s/sendMessage'" token;
          "-d";
          Printf.sprintf "'{\"chat_id\":\"%s\",\"text\":\"%s\"}'" chat_id
            escaped;
        ]
  | Email -> (
      let to_addr = env "EMAIL_TO" in
      let from_addr = env "EMAIL_FROM" in
      let cmd =
        Printf.sprintf "echo %s | mail -s 'Day10 Notification' -r %s %s"
          (Filename.quote message) from_addr to_addr
      in
      match Unix.system cmd with Unix.WEXITED 0 -> 0 | _ -> 1)
