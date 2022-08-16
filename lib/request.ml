open Config

let get_url config endpoint = Format.sprintf "%s/%s" config.url endpoint

let get_auth_token config =
  Base64.encode_string @@ Format.sprintf "%s:%s" config.email config.key

let request_api config meth endpoint content_type content =
  let url = get_url config endpoint in
  let auth = get_auth_token config in
  let headers = [ ("Authorization", "Basic " ^ auth) ] in
  EzCohttp_lwt.post ~meth ~content_type ~headers ~content (URL url)

let request_api_no_body config meth endpoint =
  let url = get_url config endpoint in
  let auth = get_auth_token config in
  let headers = [ ("Authorization", "Basic " ^ auth) ] in
  EzCohttp_lwt.post ~meth ~headers (URL url)
