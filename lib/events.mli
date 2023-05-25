(**

This module provides both {{!section-highlevel}high-level} and
{{!section-lowlevel}low-level} interfaces to the real-time events API.

 *)

(** {1:highlevel High-Level Interface} 

 *)

(** The {!module-Message} module represents the content of ["message"] events
returned by Zulip's real-time events API. *)
module Message : sig
  (** Message flags.

      See {: https://zulip.com/api/update-message-flags#available-flags}.
   *)
  type flag =
    | Read 
    | Starred
    | Collapsed
    | Mentioned
    | Wildcard_mentioned
    | Has_alert_word
    | Historical

  (** The type of messages. *)
  type t

  (** Returns the message's content as a string. *)
  val content : t -> string

  (** Returns a mention (or silent mention) of the message's sender. *)
  val sender_mention : ?silent:bool -> t -> string

  (** Returns a {!Messages.destination} for privately replying to the sender. *)
  val sender_destination : t -> Messages.destination

  (** Returns the message's destination as a {!Messages.destination}. *)
  val destination : t -> Messages.destination

  (** Tests whether a message has the given message flag. *)
  val has_flag : flag -> t -> bool

  (** Determines whether a message should be trusted.

    Messages sent by users that are *either* in the [trusted_ids] or
    [trusted_emails] lists are considered as trusted.

    If none of [trusted_ids] and [trusted_emails] is provided (or if both lists
    are empty), no message will be trusted.
   *)
  val is_trusted :
    ?trusted_ids:int list -> ?trusted_emails:string list -> t -> bool

  (** Returns whether the message was sent by the current user. *)
  val is_own_message : Config.config -> t -> bool

  (** Returns whether the message is a self-message, i.e. a message in a
      conversation between the current user and themselves.

      Note that this is different from [is_own_message]: all "selfmsg" are sent
      by the current user (i.e. [is_own_message] is [true]), but the current
      user can send messages in conversations involving other users (in which
      case [is_own_message] would be [true] but [is_selfmsg] would be [false]).
   *)
  val is_selfmsg : t -> bool

  (** Returns whether the message is in a private conversation with exactly two
      users.

      Note that what OZulip calls a "privmsg" here is not the same as what Zulip
      calls a "private" message, because Zulip's "private" message can also be
      group conversations with many users.
   *)
  val is_privmsg : t -> bool

  (** Returns whether the message is in a private *group* conversation.

      Messages sent to a stream are never "groupmsg", and neither are private
      messages with two members or less.
   *)
  val is_groupmsg : t -> bool

  (** Reply to a message. 

      @param privmsg Determines how to reply to commands. By default, replies are
        sent to the same stream that the command was sent to. If [privmsg] is
        [true], the replies are always sent as private message to the user
        initiating the command instead.
      @param mention By default, a mention of the sender is added to the reply.
        If [mention] is set false, no such mention will be added. Mentions are
        never added in private messages.
  *)
  val reply :
    ?privmsg:bool ->
    ?mention:bool ->
    Config.config ->
    t ->
    string ->
    (int, int * string option) Lwt_result.t
end

(** Interact with other users.

    This is a utility function meant for bot authors.

    [interact config f] will call the function [f] on each command received (see
    {!commands} for details), and replies (see {!Message.reply}) with the result
    of the call to [f], if any.

    Interactions are parallelized internally, so that a long-running interaction
    will not block the processing of other commands.
 *)
val interact :
    ?trusted_ids:int list ->
    ?trusted_emails:string list ->
    ?privmsg:bool ->
    ?mention:bool ->
    Config.config ->
    (string -> string option Lwt.t) ->
    unit Lwt.t

(** Returns a stream of the commands sent to the user. 

    This is an utility function meant for bot authors.

    A command is a message:
     - That was not sent by the current user,
     - That has not been read yet,
     - That either was sent in a one-on-one private conversation, or otherwise
       mentions the current user (through a regular mention, not a wildcard).

    If either [trusted_ids] or [trusted_emails] are provided, this function
    discards messages that are not trusted (as determined by
    {!Message.is_trusted}). Otherwise, all messages are kept.

    By default, initial mentions are stripped from the message content for
    convenience. This behavior can be disabled by setting [strip_mentions] to
    [false].
*)
val commands :
      ?trusted_ids:int list ->
      ?trusted_emails:string list ->
      ?strip_mentions:bool ->
      Config.config ->
      Message.t Lwt_stream.t

(** Returns a stream of the message events.

    This stream contains all the messages returned by Zulip, and includes unread
    messages, and messages sent by the current user.

    Bot authors may find {!commands} or {!interact} more useful.
*)
val messages : Config.config -> Message.t Lwt_stream.t

(** {1:lowlevel Low-Level Interface} *)

(** Events returned by the {{: https://zulip.com/api/real-time-events}real-time
    events API}.

    This is an extensible type to allow supporting additional event types in the
    future without breaking backwards compatibility.
 *)
type event = ..

type event +=
  | Message of Message.t
    (** A message that was sent to the user. *)
  | Heartbeat
    (** Heartbeat events sent by Zulip to keep the connection alive. 

        These should only be relevant to users of the low-level {!events}
        interface. High-level interfaces such as {!stream} automatically deals
        with heartbeat events for you.
     *)

(** The event types that can be filtered in calls to {!register}. *)
type event_type = [ `Message ]

(** A queue identifier is really just a string, but we wrap it in a private type
    for clarity. 
 *)
type queue_id = private string

(** Event queue configuration.

    This is the type returned by {{: https://zulip.com/api/register-queue}the
    ["register"] endpoint}.
 *)
type events_queue = {
  queue_id : queue_id;
  last_event_id : int;
  zulip_feature_level : int;
  event_queue_longpoll_timeout_seconds : int;
}

(** Low-level wrapper around the ["register"] endpoint.

    See the {{: https://zulip.com/api/register-queue}Zulip documentation} for
    more details.
 *)
val register :
    ?apply_markdown:bool ->
    ?client_gravatar:bool ->
    ?include_subscribers:bool ->
    ?slim_presence:bool ->
    ?event_types:[< event_type as 'a ] list ->
    ?all_public_streams:bool ->
    ?fetch_event_types:'a list ->
    ?narrow:(string * string) list ->
    Config.config ->
    (events_queue, int * string option) Lwt_result.t

(** Low-level wrapper around the ["events"] endpoint.

    See the {{: https://zulip.com/api/get-events}Zulip documentation} for more
    details.
 *)
val events :
    ?last_event_id:int ->
    ?blocking:bool ->
    queue_id:queue_id ->
    Config.config ->
    (queue_id * (int * event) list, int * string option) Lwt_result.t

(** Low-level wrapper around the ["deregister"] endpoint.

    See the {{: https://zulip.com/api/delete-queue}Zulip documentation} for more
    details.
 *)
val deregister : 
    queue_id:string ->
    Config.config ->
    (unit, int * string option) Lwt_result.t

(** Returns a stream of events sent by the Zulip API.

    This is a wrapper around the low-level {!register} and {!events} function
    that takes care of dealing with reconnections, queue management, and
    heartbeats automatically (in particular, the returned stream will never
    contain a {!Heartbeat} event).

    Most of the time, you want to use {!messages} or {!commands} instead.
*)
val stream :
    ?event_types:event_type list ->
    Config.config ->
    event Lwt_stream.t