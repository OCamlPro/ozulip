(** {1 Message destination} *)

type destination
(** Destination information *)

val private_emails : string list -> destination
(** [private_emails emails] create destination for private message with users with giving [emails] emails. *)

val private_ids : int list -> destination
(** [private_ids ids] create destination for private message with users with giving [ids] ids. *)

val stream_name : string -> string -> destination
(** [stream_name name topic] create destination for stream message with specified stream name [name] under the topic [topic]. *)

val stream_id : int -> string -> destination
(** [stream_id id topic] create destination for stream message with specified stream id [id] under the topic [topic]. *)

(** {1 Message selection} *)

(** Indicates which message(s) should be edited. *)
type propagate_mode = One | Later | All

(** {1 Requests} *)

val send_message :
  ?queue_id:string ->
  ?local_id:string ->
  Config.config ->
  destination ->
  string ->
  (int, int * string option) result Lwt.t
(** [send_message ~queue_id ~local_id config dest content]
    sends a stream or a private message with content [content]. Returns message's id.
    [config] is connection configuration.
    [dest] is a message destination created with [stream_*]
      functions for stream's message and [private_*] for private message.
    [queue_id] event queue ID for client supporting local echo.
    [local_id] a unique string-format identifier for client supporting local echo. *)

val upload_file : Config.config -> string -> string option Lwt.t
(** [upload_file config path] uploads a single file under the path [path].
    Retruns corresponding URI of the file on the server. *)

val edit_message :
  ?topic:string ->
  ?stream_id:string ->
  ?content:string ->
  ?prop_mode:propagate_mode ->
  ?notif_old:bool ->
  ?notif_new:bool ->
  Config.config ->
  int ->
  bool Lwt.t
(** [edit_message ~topic ~stream_id ~content ~prop_mode ~notif_old ~notif_new config id]
    edits/updates the content, topic, or stream of a message. Returns boolean that tells
    if request was succesfull.
    [topic] the topic to move the message(s) to, to request changing the topic.
    [stream_id] the stream ID to move the message(s) to, to request moving messages to another stream.
    [content] the updated content of the target message. Note that a message's content and stream
    cannot be changed at the same time.
    [prop_mode] tells which message(s) should be edited.
    [notif_old] whether to send breadcrumb message to the old thread to notify users where the messages were moved to.
    [notif_new] whether to send a notification message to the new thread to notify users where the messages came from.
  *)

val delete_message : Config.config -> int -> bool Lwt.t
(** [delete_message config id] permanently delete a message with giving id. Returns boolean that tells
    if request was succesfull.*)
