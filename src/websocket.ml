open Bridge

type app_cb = Types.server_message -> unit [@bs]

let make_sender emit url = 

  let nonce = ref 0 in
  let getNonce _ = 
    let n = !nonce in
    nonce := n+1; n
  in

  let ready = ref false in
  let callbacks : (int, app_cb) Hashtbl.t = Hashtbl.create 350 in 
  let msg_buffer = Queue.create () in

  (* deal with queued up messages *)
  let flush_msg_buffer w = Queue.iter (WebSocket.send w) msg_buffer; Queue.clear msg_buffer in

  let on_open w = Js.log "connected." ; ready := true ; flush_msg_buffer w in

  let on_message (msg : WebSocket.websocket_message) = 

    Js.log (WebSocket.dataGet msg) ;

    match Serialization.parse_server_message (WebSocket.dataGet msg) with
    | Ok(x) -> 
        (* look for a reference *)
        begin match x.ref with
        | None -> x.body |> emit

        | Some(n) -> 

            if Hashtbl.mem callbacks n 
            then 
              (* there is a reference and a registered callback *)
              let cb = Hashtbl.find callbacks n in
              begin 
                cb x.body [@bs] ; 
                Hashtbl.remove callbacks n 
              end
            else ()
        end

    | Error(err) -> "Error: " ^ err |> Js.log
  in

  let on_close _ = ready := false in

  let ws = 
    WebSocket.get_websocket { url; on_open; on_message; on_close } 
  in

  fun outboundMessage f ->
    
    (* compute the payload *)
    let cm = Serialization.encode_client_message outboundMessage in
    let n = getNonce () in

    Js.Dict.set cm "nonce" (Js.Json.number (Js.Int.toFloat n));
    let payload = Js.Json.stringify (Js.Json.object_ cm) in
    
    match f with
      | None -> ()
      | Some cb ->
          Hashtbl.add callbacks n cb ;

    if !ready 
      then begin flush_msg_buffer ws; WebSocket.send ws payload; end
      else Queue.add payload msg_buffer

