let makeSender url = 

  let nonce = ref 0 in
  let getNonce _ = 
    let n = !nonce in
    nonce := n+1; n
  in
  let ready = ref false in
  let callbacks = Hashtbl.create 350 in 

  let on_open _ = ready := true in

  let on_message (msg : Bridge.websocket_message) = 
    match Serialization.parse_server_message msg.data with
    | Ok(x) -> 
        begin match x.ref with
        | None -> ()
        | Some(n) -> 
            if Hashtbl.mem callbacks n 
            then 
              begin Hashtbl.find callbacks n x.body; 
              Hashtbl.remove callbacks n
              end
            else ()
        end

    | Error(err) -> Js.log err
  in

  let on_close _ = ready := false in

  let sendString = Bridge.get_websocket_send { url; on_open; on_message; on_close } in

  fun outboundMessage f ->

    let cm = Serialization.encode_client_message outboundMessage in
    let n = getNonce () in

    begin Js.Dict.set cm "nonce" (Js.Json.number (Js.Int.toFloat n));
    Hashtbl.add callbacks n f;
    let payload = Js.Json.stringify (Js.Json.object_ cm) in
    sendString payload 
    end

