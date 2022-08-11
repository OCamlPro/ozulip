open Common
open Config
open EzAPI
open Lwt.Infix

type user_ids = User_ids of int list | User_emails of string list
type stream_id = Stream_id of int | Stream_name of string

type destination =
  | Private of user_ids
  | Stream of { stream_id : stream_id; stream_topic : string }

let private_emails emails = Private (User_emails emails)
let private_ids ids = Private (User_ids ids)

let stream_name name topic =
  let stream_id = Stream_name name in
  Stream { stream_id; stream_topic = topic }

let stream_id id topic =
  let stream_id = Stream_id id in
  Stream { stream_id; stream_topic = topic }

type propagate_mode = One | Later | All

let wrap_url = List.map (fun (a, b) -> (a, [ b ]))

let encode_body_send_message queue_id local_id dest content =
  let args =
    match dest with
    | Private users ->
        let users =
          match users with
          | User_ids ids -> Format.asprintf "%a" pp_int_list ids
          | User_emails emails -> Format.asprintf "%a" pp_string_list emails
        in
        [ ("type", "private"); ("to", users) ]
    | Stream { stream_id; stream_topic } ->
        let id =
          match stream_id with
          | Stream_id id -> string_of_int id
          | Stream_name name -> name
        in
        [ ("type", "stream"); ("to", id); ("topic", stream_topic) ]
  in
  let q_id = unmatch_args "queue_id" queue_id in
  let l_id = unmatch_args "local_id" local_id in
  let args = args @ q_id @ l_id in
  let body = wrap_url (args @ [ ("content", content) ]) in
  Url.encode_args ~url:true body

let send_message ?queue_id ?local_id config dest content =
  let data = encode_body_send_message queue_id local_id dest content in
  Request.request_api config "POST" "messages" mime_form_url data >>= function
  | Ok response -> destruct response_id_enc response |> Lwt.return_some
  | Error (code, msg) ->
      print_error code msg;
      Lwt.return_none

let upload_file config filename =
  let file_content =
    try
      let ic = open_in_bin filename in
      let file_content = In_channel.input_all ic in
      close_in ic;
      Some file_content
    with _ -> None
  in
  match file_content with
  | Some file_content -> (
      let boundary = Format.sprintf "%x" (Hashtbl.hash filename) in
      let content =
        let buff = Buffer.create 1333 in
        Buffer.add_string buff ("--" ^ boundary);
        Buffer.add_string buff
          (Format.sprintf
             "\r\n\
              Content-Disposition: form-data; name=\"filename\"; \
              filename=\"%s\""
             (Filename.basename filename));
        Buffer.add_string buff "\r\nContent-Type: application/octet-stream";
        Buffer.add_string buff "\r\n\r\n";
        Buffer.add_string buff file_content;
        Buffer.add_string buff ("--" ^ boundary ^ "--\r\n");
        Buffer.contents buff
      in
      Request.request_api config "POST" "user_uploads"
        ("multipart/form-data; boundary=" ^ boundary)
        content
      >>= function
      | Ok response -> destruct response_uri_enc response |> Lwt.return_some
      | Error (code, msg) ->
          print_error code msg;
          Lwt.return_none)
  | None ->
      Printf.eprintf "Input Error : file %s does not exist\n" filename;
      Lwt.return_none

let encode_body_edit_message topic stream_id content prop_mode notif_old
    notif_new =
  let topic = unmatch_args "topic" topic in
  let stream_id = unmatch_args "stream_id" stream_id in
  let content = unmatch_args "content" content in
  let prop_mode =
    [
      ( "propagate_mode",
        match prop_mode with
        | One -> "change_one"
        | All -> "change_all"
        | Later -> "change_later" );
    ]
  in
  let notif_old =
    [ ("send_notification_to_old_thread", string_of_bool notif_old) ]
  in
  let notif_new =
    [ ("send_notification_to_new_thread", string_of_bool notif_new) ]
  in
  let args = topic @ stream_id @ content @ prop_mode @ notif_old @ notif_new in
  Url.encode_args ~url:true (wrap_url args)

let edit_message ?topic ?stream_id ?content ?(prop_mode = One)
    ?(notif_old = true) ?(notif_new = true) config id =
  let data =
    encode_body_edit_message topic stream_id content prop_mode notif_old
      notif_new
  in
  let endpoint = Format.sprintf "messages/%d" id in
  Request.request_api config "PATCH" endpoint mime_form_url data >>= function
  | Ok _ -> Lwt.return_true
  | Error (code, msg) ->
      print_error code msg;
      Lwt.return_false

let delete_message config id =
  let endpoint = Format.sprintf "messages/%d" id in
  Request.request_api_no_body config "DELETE" endpoint >>= function
  | Ok _ -> Lwt.return_true
  | Error (code, msg) ->
      print_error code msg;
      Lwt.return_false