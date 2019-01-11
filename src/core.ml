open Types
open Bridge
open Bridge.VDom

module Option = Belt.Option

let send_sync_message = 
  WithDerivation("/sync", fun x -> SendMsg (Sync x, fun _ -> NoOp))

(* ~~~~~~~~~ *)
(* App logic *)
(* ~~~~~~~~~ *)

let update stim =
  Js.log stim;
  match stim with
  | Click (Nav x) -> 
      let u state = { state with active_page = x } in
      StateUpdate (u, NoOp)

  | Click (SetKey (Some x)) -> 
      let u state = 
        { state with key = Js.Nullable.return x
          ; input_fields =
            { state.input_fields with key_entry = "" }
          ; active_page = LocManageKeys
        }
      in
      StateUpdate (u, send_sync_message)

  | Click Donate ->
      WithState(fun state -> 
        let amount = 
          let f = Js.Float.fromString state.input_fields.donation_amount in
          Js.Math.floor f
        in
        let memo = state.input_fields.donation_memo in
        (* Clear input fields *)
        let clear_input state = { state with input_fields = 
              { state.input_fields 
                with donation_memo = ""
                   ; donation_amount = "" } }
        in
        let put_req req r_hash s = 
          let pr = { req; r_hash; memo; amount; paid = false; date = Js.Date.make () } in
          Js.Array.push pr s.payment_requests ; s
        in
        let handler = function
          | PaymentRequest (req, r_hash) -> StateUpdate (put_req req r_hash, NoOp)
          | _ -> NoOp
        in
        let op = SendMsg(DonateMsg(memo,amount), handler) in
        StateUpdate (clear_input, op)
      )
  
  | Click GenRandomKey ->
      let keyBuf = Util.random_bytes 32 in
      let hex = Util.hex_encode keyBuf in
      let u state = { state with key = Js.Nullable.return hex } in
      StateUpdate (u, send_sync_message)

  | Input (x, s) ->
      let u state = { state with input_fields =
          match x with
          | KeyEntry -> { state.input_fields with key_entry = s } 
          | DonationMemo -> { state.input_fields with donation_memo = s }
          | DonationAmount -> { state.input_fields with donation_amount = s }
        } 
      in
      StateUpdate (u, NoOp)
  
  | PushMessage(Confirmation(_, r_hash)) -> 
      let u state =  
        let mark pr = if pr.r_hash == r_hash then { pr with paid = true } else pr in
        let pr1 = Array.map mark (state.payment_requests) in
        { state with payment_requests = pr1 } 
      in
      StateUpdate (u, NoOp)

  | Continue eff -> eff

  | _ -> NoOp ;;


(* ~~~~~~~~~~~~~~~ *)
(* Effect handling *)
(* ~~~~~~~~~~~~~~~ *)

let rec runEffect send state eff = match eff with 
  | SendMsg (msg, handler) -> 
      let next _ = Js.Promise.resolve state in
      Js.Promise.then_ next (send msg handler)

  | StateUpdate (updater, next) -> 
      runEffect send (updater state) next

  | WithDerivation (path, handler) ->
      begin match Js.Nullable.toOption state.key with
      | None -> Js.Promise.resolve state 
      | Some key -> 
          let next dk = runEffect send state (handler dk) in
          Js.Promise.then_ next (Util.derive_key key path)
      end

  | WithState handler -> runEffect send state (handler state)

  | NoOp -> Js.Promise.resolve state 


(* ~~~~~~~~~~~~~~~~~ *)
(* Interface helpers *)
(* ~~~~~~~~~~~~~~~~~ *)

let inputName k = match k with
  | KeyEntry -> "key_entry"
  | DonationMemo -> "donation_memo"
  | DonationAmount -> "donation_amount"

let navText p = match p with
  | LocStart -> ">> start page"
  | LocManageKeys -> "manage keys"
  | LocShowKey -> "show key"
  | LocEnterKey -> "enter a new key"
  | LocDonate -> "donate" 
  | LocPaymentRequests -> "payment requests"

let header text =  
  h "h1" (vnode_attributes ()) [| h_text text |]

let row =  
  let key = Util.random_key () in
  let atts = vnode_attributes ~key ~class_: "row" () in 
  h "div" atts 

let column = 
  let key = Util.random_key () in
  let atts = vnode_attributes ~key ~class_: "column" () in 
  h "div" atts

let par text = 
  let key = Util.random_key () in
  let atts = vnode_attributes ~key ~class_: "simple" () in 
  h "p" atts [| h_text text |]

let button emit text t = 
  let key = Util.random_key () in
  let on_click = fun _ -> emit (Click t) in
  let atts = vnode_attributes ~key ~class_:"button" ~onclick:on_click () in
  h "div" atts [| h_text text |]

let input_ emit t value x = 
  let key = inputName x in
  let contents e = 
      let target = Event.targetGet e in
      begin match Event.valueGet target with
        | Some v -> emit (Input (x, v)) 
        | None -> () 
      end ;
  in
  let input_elt = h "input" (vnode_attributes ~key ~value ~oninput: contents ()) [||] in
  let wrapper = h "div" (vnode_attributes ~key ~class_: "input" ()) in
  match t with

    | Some text ->
        wrapper [| h "p" (vnode_attributes ()) [| h_text text |]
         ; input_elt 
         |]
    | None -> wrapper [| input_elt |] 
 


(* ~~~~~~~~~ *)
(* Interface *)
(* ~~~~~~~~~ *)

let render emit state =
  let button' = button emit in
  let input_elt = input_ emit in
  let nav p = button' (navText p) (Nav p) in
  let forgetKey = button' "forget your key" (SetKey None) in

  let content = match state.active_page with 

    | LocStart ->  
        [| header "BOB chicago #reckless" 
         ; par "This is BOB's demo site"
         ; row [| nav LocDonate; nav LocPaymentRequests;  nav LocManageKeys |] 
         |]

    | LocManageKeys ->  
        [| header "Manage your key"
         ; match Js.Nullable.toOption state.key with 
           | None -> 
              row [| nav LocEnterKey; button' "generate a random key" GenRandomKey; nav LocStart |]
           | Some _ -> 
              row [| forgetKey; nav LocShowKey; nav LocStart |]
         |]

    | LocShowKey -> 
        let keyMsg = Belt.Option.getWithDefault (Js.Nullable.toOption state.key) "NO_KEY" in 
        [| header "Your current key"
         ; par keyMsg
         ; row [| forgetKey; nav LocStart |] 
         |] 

    | LocEnterKey -> 
        let enter_key = button' "set key" (SetKey(Some(state.input_fields.key_entry))) in
        [| header "Please enter a new key"
         ; input_elt None state.input_fields.key_entry KeyEntry 
         ; row [| enter_key; nav LocStart |] 
         |]

    | LocPaymentRequests -> 
        let pr_strings = Array.map Format.payment_request state.payment_requests in
        let col_elts = Array.map par pr_strings in
        [| header "Your payment requests"
         ; column col_elts 
         ; row [| nav LocStart |]
        |]

    | LocDonate ->
        let donate = button' "donate" Donate in
        [| header "Donate to BOB"
         ; input_elt (Some "donation message") state.input_fields.donation_memo DonationMemo
         ; input_elt (Some "donation amount") state.input_fields.donation_amount DonationAmount
         ; row [| donate; nav LocStart |]
         |]
  
  in h "div" (vnode_attributes ~class_: "main" ()) content

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

  let app_send msg h =
    let next x = Js.Promise.resolve (emit (Continue(h x))) in
    Js.Promise.then_ next (ws_send_promise msg)
  in

  let handler stim = 
    let action = update stim in
    let s1_p = runEffect app_send !state action in
    let h s = 
      begin state := s ;
      let encoded = Serialization.encode_app_state s in
      LocalStorage.put "state" (Js.Json.stringify encoded) ;
      Js.Promise.resolve ()
      end
    in
    Js.Promise.then_ h s1_p
  in 

  (* start the app *)

  begin Event.register_handler event_bus handler;
  let app = get_element_by_id doc "app" in
  replace proj app (fun _ -> render emit !state);
  schedule_render proj;
  end

