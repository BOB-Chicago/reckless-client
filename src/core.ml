open Types
open Bridge
open Bridge.VDom 

module Option = Belt.Option

let send_sync_message = 
  WithDerivation("/sync", fun x -> SendMsg (Sync x, fun _ -> NoOp))

let (>>) = Util.(>>)

(* ~~~~~~~~~ *)
(* App logic *)
(* ~~~~~~~~~ *)

(* Pure state transformations *)

let clear_donation_inputs state = 
  { state with input_fields = 
    { state.input_fields with
      donation_memo = ""
      ; donation_amount = "" }
  }


let clear_key_inputs state =
  { state with input_fields =
    { state.input_fields with key_entry = "" }
  }


let clear_blob_inputs s = 
  { s with input_fields = { s.input_fields with blob_paste = "" ; blob_note = "" } } 


let set_page page state = { state with active_page = page }


let focus_pr pr state = { state with active_pr = Some pr }


let add_payment_request pr state =
  Js.Array.push pr state.payment_requests ; state  


let update_key k state = { state with key = Js.Nullable.fromOption k }

let increment_blob_index state = { state with blob_index = state.blob_index + 1 }


(* reactions *)

let toEffect stim = match stim with

  | Effect eff -> eff

  | Click (Nav p) -> 
      StateUpdate (set_page p, NoOp)

  | Click (SetKey x) -> 
      let next = match x with
        | Some _ -> send_sync_message
        | None -> NoOp
      in
      StateUpdate (
        set_page LocManageKeys >> update_key x >> clear_key_inputs, 
        next
      )

  | Click Donate ->
      WithState(fun state -> 
        let amount = Js.Float.fromString state.input_fields.donation_amount in
        let memo = "[donation] " ^ state.input_fields.donation_memo in

        let handler = function
          | PaymentRequest (req, r_hash) -> 
            let pr = { req; r_hash; memo; paid = false; date = Js.Date.make () } in
              StateUpdate (
                add_payment_request pr >> focus_pr pr >> set_page LocPaymentRequest, 
                NoOp
              )

          | _ -> NoOp
        in

        let op = 
          if Js.Float.isNaN amount then NoOp
          else SendMsg(DonateMsg(memo, Js.Math.floor amount), handler) 
        in

        StateUpdate (
          clear_donation_inputs, 
          op
        )
      )
  
  | Click GenRandomKey ->
      let keyBuf = Util.random_bytes 32 in
      let hex = Util.hex_encode keyBuf in
      StateUpdate (
        update_key (Some hex), 
        send_sync_message
      )

  | Click UploadBlob ->
      let get_pr blob_key state = 

        let response_handler = function
          | PaymentRequest (req, r_hash) -> 
              let pr = { req; r_hash; memo = "blob upload"; paid = false; date = Js.Date.make () } in
              StateUpdate (
                add_payment_request pr >> focus_pr pr >> set_page LocPaymentRequest,  
                NoOp
              )

          | _ -> NoOp
        in

        let paste = state.input_fields.blob_paste in

        let blob_content = if Util.is_hex paste then paste else paste |> Crypto.encode |> Util.hex_encode in

        StateUpdate (
            clear_blob_inputs, 
            SendMsg(
              NewBlob(blob_content, blob_key, 7), 
              response_handler
            )
          )

      in
      WithState(fun state ->
      StateUpdate(
        increment_blob_index,
        WithDerivation(
          "/blob/" ^ Js.Int.toString state.blob_index, 
          fun blob_key -> get_pr blob_key state
        )
      ))

  | Click (ViewPaymentRequest pr) ->
      StateUpdate (
        focus_pr pr >> set_page LocPaymentRequest, 
        NoOp
      )

  | Input (x, s) ->
      let u state = { state with input_fields =
          match x with
          | KeyEntry -> { state.input_fields with key_entry = s } 
          | DonationMemo -> { state.input_fields with donation_memo = s }
          | DonationAmount -> { state.input_fields with donation_amount = s }
          | BlobNote -> { state.input_fields with blob_note = s }
          | BlobPaste -> { state.input_fields with blob_paste = s }
        } 
      in
      StateUpdate (u, NoOp)
  
  | PushMessage(Confirmation(_, r_hash)) -> 
      Js.log "Confirmation" ;
      let u state =  
        let mark pr = if pr.r_hash == r_hash then { pr with paid = true } else pr in
        let pr1 = Array.map mark (state.payment_requests) in
        { state with payment_requests = pr1 } 
      in
      StateUpdate (u, NoOp)


  | _ -> NoOp ;;


(* ~~~~~~~~~~~~~~~ *)
(* Effect handling *)
(* ~~~~~~~~~~~~~~~ *)

let rec runEffect send state eff = match eff with 
  | SendMsg (msg, handler) -> 
      Js.log "SendMsg" ;
      let next response = runEffect send state (handler response) in
      Js.Promise.then_ next (send msg)

  | StateUpdate (updater, next) -> 
      Js.log "StateUpdate" ;
      let s1 = updater state in
      runEffect send s1 next

  | WithDerivation (path, handler) ->
      Js.log "WithDerivation" ;
      begin match Js.Nullable.toOption state.key with
      | None -> Js.Promise.resolve state 
      | Some key -> 
          let next dk = runEffect send state (handler dk) in
          Js.Promise.then_ next (Util.derive_key key path)
      end

  | WithState handler -> 
      Js.log "WithState" ;
      runEffect send state (handler state)

  | NoOp -> 
      Js.log "NoOp" ;
      Js.Promise.resolve state 

(* ~~~~~~~~~~~~~ *)
(* Start the app *)
(* ~~~~~~~~~~~~~ *)

let run _ =
  
  (* set up resources *)

  let event_bus = Event.make_event_bus () in
  let proj = create_projector () in

  let emit = Event.emit_stimulus event_bus in
  let ws_emit sm =
    Js.log sm ;
    emit (PushMessage sm) ;
    schedule_render proj
  in

  (* senders *)

  let ws_send = Websocket.make_sender ws_emit (Config.config |. Config.ws_urlGet) in
  let ws_send_promise msg = 
    let f ~resolve:(rv : Types.server_message -> unit [@bs]) ~reject:_ = ws_send msg (Some rv) in
    Js.Promise.make f
  in

  (* load state, if possible *)

  let pulled_state = LocalStorage.get "state" in
  let parsed_state = Option.map (Js.Nullable.toOption pulled_state) Serialization.decode_app_state in
  let state = match parsed_state with
    | Some (Ok s) -> ref s
    | _ -> ref empty_state
  in

  (* capture resources in handler *)

  (* FIXME actions compete to be the last state update *)

  let handler stim = 
    let s1_p = runEffect ws_send_promise !state (toEffect stim) in
    let h s = 
      begin state := s ;
      let encoded = Serialization.encode_app_state s in
      LocalStorage.put "state" (Js.Json.stringify encoded) ;
      schedule_render proj ;
      Js.Promise.resolve ()
      end
    in
    Js.Promise.then_ h s1_p
  in 

  (* start the app *)

  begin Event.register_handler event_bus handler;

  Effect send_sync_message |> emit ;

  let app = Dom.get_element_by_id Dom.doc "app" in
  replace proj app (fun _ -> Interface.render emit !state);
  schedule_render proj;

  end
