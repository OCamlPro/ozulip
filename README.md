# ozulip
OCaml bindings to [Zulip API](https://zulip.com/api/).

## Messages

### Send message

```ocaml
    let _ =
        let open Ozulip in
        let conf = init "www.domain.com" "email@email.com" "key" in
        let dest = Messages.stream_name "general" "welcom topic" in
        Messages.send_message conf dest "New message from `ozulip`" 
```

### Upload a file

```ocaml
    let _ =
        let open Ozulip in
        let conf = init "www.domain.com" "email@email.com" "key" in
        let%lwt uri = Messages.upload_file conf "/path/to/your/file" in
        let dest = Messages.stream_name "general" "welcom topic" in
        let msg = Format.sprintf "Check out the [this file](%s)!" uri in 
        Messages.send_message conf dest msg
```

### Edit message

```ocaml
    let _ =
        let open Ozulip in
        let conf = init "www.domain.com" "email@email.com" "key" in
        let dest = Messages.stream_name "general" "welcom topic" in
        let%lwt mess_id =  Messages.send_message conf dest "New message from `ozulip`" in
        Messages.edit_message ~content:"New content of my message" mess_id
```

### Delete message

```ocaml
    let _ =
        let open Ozulip in
        let conf = init "www.domain.com" "email@email.com" "key" in
        let dest = Messages.stream_name "general" "welcom topic" in
        let%lwt mess_id =  Messages.send_message conf dest "New message from `ozulip`" in
        Messages.delete_message mess_id
```

### High-level user interaction

```ocaml
    let _ =
        let open Ozulip in
        let conf = init ~domain ~email ~key in
        Events.interact ~trusted_emails:["you@example.com"] conf (fun msg ->
            if String.lowercase_ascii msg = "ping"
            then Lwt.return_some "pong"
            else Lwt.return_none)
```