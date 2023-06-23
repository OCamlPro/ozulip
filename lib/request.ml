open Config

let get_url config endpoint = Format.sprintf "%s/%s" config.url endpoint

let get_auth_token config =
  Base64.encode_string @@ Format.sprintf "%s:%s" config.email config.key

let get_auth_headers config =
  let auth = get_auth_token config in
  [ ("Authorization", "Basic " ^ auth) ]

let api_get config endpoint content =
  let url = get_url config endpoint in
  let headers = get_auth_headers config in
  let content = EzAPI.Url.encode_args ~url:true content in
  EzCohttp_lwt.get ~meth:`GET ~headers (URL (url ^ "?" ^ content))

let request_api config meth endpoint content_type content =
  let url = get_url config endpoint in
  let headers = get_auth_headers config in
  EzCohttp_lwt.post ~meth ~content_type ~headers ~content (URL url)

let request_api_no_body config meth endpoint =
  let url = get_url config endpoint in
  let headers = get_auth_headers config in
  EzCohttp_lwt.post ~meth ~headers (URL url)
