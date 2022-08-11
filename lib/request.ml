open Config

let get_url config endpoint = Format.sprintf "%s/%s" config.url endpoint

let get_auth_token config =
  Base64.encode_string @@ Format.sprintf "%s:%s" config.email config.key

let request_api config meth endpoint content_type content =
  let url = get_url config endpoint in
  let auth = get_auth_token config in
  let headers = [ ("Authorization", "Basic " ^ auth) ] in
  EzCurl_lwt.make ~meth ~content_type ~headers ~content url

let request_api_no_body config meth endpoint =
  let url = get_url config endpoint in
  let auth = get_auth_token config in
  let headers = [ ("Authorization", "Basic " ^ auth) ] in
  EzCurl_lwt.make ~meth ~headers url
