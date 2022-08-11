(** Message requests *)
module Messages = Messages

(** [init domain email key] initialises connection config with the Zulip server 
    with [domain] as domain name, [email] as user's mail and [key] as user's API key *)
let init domain email key : Config.config =
  let url = Format.sprintf "https://%s/api/v1" domain in
  { domain; email; key; url }
