let make_sender emit url = 

  let nonce = ref 0 in
  let getNonce _ = 
    let n = !nonce in
    nonce := n+1; n
  in
  let ready = ref false in
  let callbacks = Hashtbl.create 350 in 
  let msg_buffer = Queue.create () in

  let on_open _ = ready := true in

  let on_message (msg : Bridge.WebSocket.websocket_message) = 

    match Serialization.parse_server_message msg.data with
    | Ok(x) -> 
        (* look for a reference *)
        begin match x.ref with
        | None -> Types.ServerMessage(x.body) |> emit

        | Some(n) -> 

            if Hashtbl.mem callbacks n 
            then 
              (* there is a reference and a registered callback *)
              begin Hashtbl.find callbacks n x.body; 
              Hashtbl.remove callbacks n
              end
            else ()
        end

    | Error(err) -> Js.log err
  in

  let on_close _ = ready := false in

  let sendString = 
    Bridge.WebSocket.get_websocket_send { url; on_open; on_message; on_close } 
  in

  fun outboundMessage f ->

    (* deal with queued up messages *)
    let flush_msg_buffer _ = Queue.iter sendString msg_buffer; Queue.clear msg_buffer in

    (* compute the payload *)
    let cm = Serialization.encode_client_message outboundMessage in
    let n = getNonce () in
    begin Js.Dict.set cm "nonce" (Js.Json.number (Js.Int.toFloat n));
    let payload = Js.Json.stringify (Js.Json.object_ cm) in
    
    match f with
      | None -> ()
      | Some cb ->
          Hashtbl.add callbacks n cb ;

    if !ready 
      then begin flush_msg_buffer (); sendString payload; end
      else Queue.add payload msg_buffer
    end

