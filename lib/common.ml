open Json_encoding

let pp_string fmt = Format.fprintf fmt "\"%s\""
let pp_int fmt = Format.fprintf fmt "%d"
let pp_sep fmt () = Format.fprintf fmt ", "

let pp_list pp_elt fmt l =
  Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep pp_elt) l

let pp_int_list = pp_list pp_int
let pp_string_list = pp_list pp_string

let response_field response field =
  match Yojson.Basic.from_string response with
  | `Assoc fields -> (*Yojson.Basic.to_string @@ List.assoc field fields*) ""
  | _ -> assert false

let response_uri_enc =
  conv
    (fun uri -> ("", "", uri))
    (fun (_, _, uri) -> uri)
    (obj3 (req "result" string) (req "msg" string) (req "uri" string))

let response_id_enc =
  conv
    (fun id -> (id, "", ""))
    (fun (id, _, _) -> id)
    (obj3 (req "id" int) (req "msg" string) (req "result" string))

let construct = EzEncoding.construct
let destruct = EzEncoding.destruct

let print_error code msg =
  Printf.eprintf "Zulip API error %d : %s\n" code
    (match msg with
    | None -> "Internal error"
    | Some s -> response_field "msg" s)

let unmatch_args name arg = match arg with Some x -> [ (name, x) ] | _ -> []
let mime_form_url = "application/x-www-form-urlencoded"
