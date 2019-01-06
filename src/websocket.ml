let makeSender url = 
  let on_open = fun _ -> () in
  let on_message = fun _ -> () in
  let on_close = fun _ -> () in
  Bridge.getWebSocketSend { url; on_open; on_message; on_close }
