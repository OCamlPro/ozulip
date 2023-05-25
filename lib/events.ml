open Common
open Lwt.Infix

module Smap = Map.Make(String)

module Message = struct
  type recipients =
    | Stream of int * string
    | Private of int list

  type flag =
    | Read 
    | Starred
    | Collapsed
    | Mentioned
    | Wildcard_mentioned
    | Has_alert_word
    | Historical

  let flag =
    Json_encoding.(
      string_enum [
        "read", Read;
        "starred", Starred;
        "collapsed", Collapsed;
        "mentioned", Mentioned;
        "wildcard_mentioned", Wildcard_mentioned;
        "has_alert_word", Has_alert_word;
        "historical", Historical;
      ]
    )

  type t = { 
    id: int ;
    sender_id: int ;
    sender_email: string ;
    sender_full_name: string;
    recipients: recipients ;
    content : string ;
    flags : flag list ;
  }

  let content { content; _ } = content

  let sender_mention ?(silent = false) { sender_full_name; sender_id; _ } =
    Format.asprintf "@%s**%s|%d**" (if silent then "_" else "") sender_full_name sender_id

  let sender_destination { sender_id; _ } = Messages.private_ids [sender_id]

  let destination { recipients; _ } =
    match recipients with
    | Stream (stream_id, topic) -> Messages.stream_id stream_id topic
    | Private ids -> Messages.private_ids ids

  (* Message filters *)

  let is_trusted ?(trusted_ids = []) ?(trusted_emails = []) { sender_id; sender_email; _ } =
    List.mem sender_id trusted_ids || List.mem sender_email trusted_emails

  let has_flag flag m =
    if List.mem flag m.flags then true else false

  let is_own_message config m = m.sender_email = config.Config.email

  let is_privmsg { recipients; _ } =
    (* A message is a privmsg if it has exactly two recipients: us and the
       sender. *)
    match recipients with
    | Private [ _; _ ] -> true
    | Private _ | Stream _ -> false

  let reply ?(privmsg = false) ?(mention = true) config m =
    (* Note: the slightly convoluted implementation ensures that we don't keep a
       reference to [m] in the closure. *)
    let dest = if privmsg then sender_destination m else destination m in
    let mention =
      if privmsg || is_privmsg m || not mention then None 
      else Some (sender_mention m)
    in
    fun reply ->
      let reply =
        match mention with
        | Some mention -> mention ^ " " ^ reply
        | None -> reply
      in
      Messages.send_message config dest reply
end

type event = ..

type event +=
  | Message of Message.t
  | Heartbeat
  | Other of Json_repr.ezjsonm

type event_type =
  [ `Message
  ]

type queue_id = string

type events_queue = {
  queue_id : queue_id;
  last_event_id : int;
  zulip_feature_level : int;
  event_queue_longpoll_timeout_seconds : int;
}

let pp_event_type ppf = function
  | `Message -> Format.fprintf ppf "\"message\""
  | `EventType et -> Format.fprintf ppf "\"%s\"" (String.escaped et)

let add pp key x args = (key, [ Format.asprintf "%a" pp x ]) :: args

let add_opt pp key opt args =
  match opt with Some x -> add pp key x args | None -> args

let pp_string ppf s = Format.fprintf ppf "%s" s

let bool_opt = add_opt pp_bool
let int_opt = add_opt pp_int
let list_opt pp = add_opt (pp_list pp)
let list pp = add (pp_list pp)
let bool = add pp_bool
let string = add pp_string

let register_result_enc =
  let open EzEncoding in
  let open Json_encoding in
  conv
    (fun {
           queue_id;
           last_event_id;
           zulip_feature_level;
           event_queue_longpoll_timeout_seconds;
         } ->
      ( queue_id,
        last_event_id,
        zulip_feature_level,
        event_queue_longpoll_timeout_seconds ))
    (fun ( queue_id,
           last_event_id,
           zulip_feature_level,
           event_queue_longpoll_timeout_seconds ) ->
      {
        queue_id;
        last_event_id;
        zulip_feature_level;
        event_queue_longpoll_timeout_seconds;
      })
  @@ obj4
       (req ~description:"The ID of the queue allocated for the client."
          "queue_id" string)
       (req
          ~description:
            "The initial value of `last_event_id` to pass to `events`."
          "last_event_id" int)
       (req ~description:"The server's current Zulip feature level."
          "zulip_feature_level" int)
       (dft "event_queue_longpoll_timeout_seconds" int 90)

let request meth config endpoint args =
  let data = EzAPI.Url.encode_args ~url:true args in
  Request.request_api config meth endpoint mime_form_url data

let register ?apply_markdown ?client_gravatar ?include_subscribers
    ?slim_presence ?(event_types = []) ?all_public_streams ?fetch_event_types ?narrow
    config =
  let args =
    bool_opt "apply_markdown" apply_markdown
    @@ bool_opt "client_gravatar" client_gravatar
    @@ bool_opt "include_subscribers" include_subscribers
    @@ bool_opt "slim_presence" slim_presence
    @@ list pp_event_type "event_types" event_types
    @@ bool_opt "all_public_streams" all_public_streams
    @@ list_opt pp_event_type "fetch_event_types" fetch_event_types
    @@ list_opt (pp_pair pp_string pp_string) "narrow" narrow
    @@ []
  in

  let open Lwt_result.Infix in
  request `POST config "register" args >|= fun s ->
  Json_encoding.destruct ~ignore_extra_fields:true register_result_enc
  (EzEncoding.Ezjsonm.from_string s)

let deregister ~queue_id config =
  let data = EzAPI.Url.encode_args ~url:true (string "queue_id" queue_id []) in
  Request.request_api config `DELETE "deregister" mime_form_url data >>= function
  | Ok _ -> Lwt_result.return ()
  | Error e -> Lwt_result.fail e

module Encodings = struct
  open Json_encoding

  let message =
    let open Message in
    let unexpected what expected =
      raise (Cannot_destruct ([], Unexpected (what, expected)))
    in
    let missing_field key =
      raise (Cannot_destruct ([], Missing_field key))
    in
    let assoc fs key =
      try Smap.find key fs with Not_found -> missing_field key
    in
    let ( |>> ) x f = Json_encoding.destruct ~ignore_extra_fields:true f x in
    custom
      ~is_object:true
      ~schema:Json_schema.any
      (fun _ -> 
        failwith "construct: not supported")
      (function 
        | `O fs -> 
          let fs = List.to_seq fs |> Smap.of_seq in
          let field = assoc fs in
          let id = field "id" |>> int in
          let sender_id = field "sender_id" |>> int in
          let sender_email = field "sender_email" |>> string in
          let sender_full_name = field "sender_full_name" |>> string in
          let type_ = field "type" |>> string_enum ["stream", `Stream; "private", `Private] in
          let recipients =
            match type_ with
            | `Stream ->
              let stream_id = field "stream_id" |>> int in
              let topic = field "subject" |>> string in
              Stream (stream_id, topic)
            | `Private ->
              let display_recipient = field "display_recipient" |>>
                list (obj1 (req "id" int)) in
              Private display_recipient
          in
          let content = field "content" |>> string in
          { id; sender_id; sender_email; sender_full_name; recipients; content; flags = [] }
        | `Bool _ -> unexpected "boolean" "object"
        | `Null -> unexpected "null" "object"
        | `Float _ -> unexpected "float" "object"
        | `String _ -> unexpected "string" "object"
        | `A _ -> unexpected "array" "object")

  let message_event =
    let open Message in
    conv 
      (fun message -> ((), message, message.flags))
      (fun ((), message, flags) -> { message with flags })
    @@
    obj3
      (req "type" (constant "message"))
      (req "message" message)
      (req "flags" (list Message.flag))

  let heartbeat_event =
    obj1 (req "type" (constant "heartbeat"))

  let event =
    merge_objs (obj1 (req "id" int)) @@
      union [
        case message_event (function Message e -> Some e | _ -> None) (fun e -> Message e);
        case heartbeat_event (function Heartbeat -> Some () | _ -> None) (fun () -> Heartbeat);

        (* It looks like [any_ezjson_object] doesn't actually work with [merge_objs]... *)
        case any_ezjson_object (function Other e -> Some e | _ -> None) (fun e -> Other e);
      ]

  let events_response =
    obj2
      (req "queue_id" string)
      (req "events" (list event))
end

let events ?last_event_id ?(blocking = true) ~queue_id config =
  let args =
    int_opt "last_event_id" last_event_id
    @@ bool "dont_block" (not blocking)
    @@ string "queue_id" queue_id @@ []
  in

  let open Lwt_result.Infix in
  Request.api_get config "events" args >|= fun x ->
    Format.printf "%s@." x;
  Json_encoding.destruct ~ignore_extra_fields:true Encodings.events_response
  (EzEncoding.Ezjsonm.from_string x)

let backoff ?(exp = 2.) ?(ceiling = 10) f x =
  let rec aux i t =
    f x >>= function
    | Ok r -> Lwt.return r
    | Error _ ->
      Lwt_unix.sleep t >>= fun () ->
      if i < ceiling then 
        aux (i + 1) (exp *. t)
      else
        aux i t
  in aux 0 1.

let stream ?event_types config =
  let stream, push = Lwt_stream.create () in
  let register = backoff @@ register ?event_types in
  (* Repeatedly request events using the provided timeout.

     The timeout returned by [register] is normally higher than the heartbeat
     frequency of the server, and so we shouldn't hit it unless we get
     disconnected.
  *)
  let rec events' ({ queue_id; last_event_id; _ } as events_config) =
    let timeout = float_of_int events_config.event_queue_longpoll_timeout_seconds in
    Lwt.pick [
      (Lwt_unix.sleep timeout >|= fun () -> None);
      (events ~last_event_id ~queue_id config >|= fun e -> Some e);
    ] >>= function
    | None -> events' events_config
    | Some e -> Lwt.return e
  in
  let rec aux ({ queue_id; last_event_id; _ } as events_config) =
    events' events_config >>= function
    | Ok (_, events) -> 
      let last_event_id =
      List.fold_left
        (fun last_id (id, ev) -> 
          begin match ev with
          | Heartbeat -> ()
          | _ -> push (Some ev)
          end; max id last_id)
        last_event_id events
      in
      aux { events_config with last_event_id }
    | Error (code, status) ->
      (* If we get a 4XX HTTP error (client error), we stop trying -- if we are
         making a bogus query somehow, we probably are going to be stuck in an
         error loop otherwise.
       *)
      if 400 <= code && code < 500 then begin
        push None;
        Lwt.fail_with ("HTTP client error " ^ string_of_int code) 
      end else 
        (* TODO: only register a new queue if we get a BAD_EVENT_QUEUE_ID error
           code. 
         *)
        register config >>= aux
  in
  Lwt.async (fun () -> register config >>= aux);
  stream

let messages config =
  Lwt_stream.map (function
    | Message m -> m
    | _ -> assert false) @@
  stream ~event_types:[`Message] config
let strip_initial_mentions =
  let re = Re.(Perl.re {|^\s*@\*\*[^\*]+\*\*\s*|} |> compile) in
  Re.replace_string ~all:true re ~by:""

let commands ?trusted_ids ?trusted_emails ?(strip_mentions = true) config =
  let is_trusted = 
    match trusted_ids, trusted_emails with
    | None, None -> fun _ -> true
    | _ -> Message.is_trusted ?trusted_ids ?trusted_emails 
  in
  messages config |>
  Lwt_stream.filter_map (fun m -> 
    if
      not (Message.is_own_message config m) &&
      (Message.has_flag Mentioned m || Message.is_privmsg m) &&
      not (Message.has_flag Read m) &&
      is_trusted m
    then
      let m = if strip_mentions then 
        { m with content = strip_initial_mentions m.content }
      else
        m
      in
      Some m
    else
      None)

let interact ?trusted_ids ?trusted_emails ?privmsg ?mention config f =
  commands ?trusted_ids ?trusted_emails config |>
  Lwt_stream.iter_p (fun m -> 
    let send = Message.reply ?privmsg ?mention config m in
    f m.Message.content >>= function
    | Some reply -> send reply >|= fun _ -> ()
    | None -> Lwt.return_unit)