(** Connection configuration to Zulip API.
    [domain] is a domain name of Zulip server
    [email] is an existing user's email connected to the domain
    [key] is a user's API key
    [url] is a base url of Zulip server that contains domain name. *)
type config = { domain : string; email : string; key : string; url : string }
