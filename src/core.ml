open Types
open Bridge
open Bridge.VDom

(* ~~~~~~~~~ *)
(* App logic *)
(* ~~~~~~~~~ *)

let update stim state =
  Js.log stim;
  Js.log state;
  match stim with
  | Click (Nav x) -> 
      ({ state with active_page = x }, None)

  | Click (SetKey x) -> 
      let state1 = 
        { state with key = Js.Nullable.fromOption x
          ; input_fields =
            { state.input_fields with key_entry = "" }
          ; active_page = LocManageKeys
        }
      in
      (state1, None)

  | Click Donate ->
      let amount = 
        let f = Js.Float.fromString state.input_fields.donation_amount in
        Js.Math.floor f
      in
      let state1 = { state with input_fields = 
            { state.input_fields 
              with donation_memo = ""
                 ; donation_amount = "" } }
      in
      (state1, Some (DonateMsg (state.input_fields.donation_memo, amount)))
  
  | Click GenRandomKey ->
      let keyBuf = Util.randomBytes 32 in
      let hex = Util.hexEncode keyBuf in
      let state1 = { state with key = Js.Nullable.return hex } in
      (state1, None)

  | Input (x, s) ->
      let state1 = { state with input_fields =
          match x with
          | KeyEntry -> { state.input_fields with key_entry = s } 
          | DonationMemo -> { state.input_fields with donation_memo = s }
          | DonationAmount -> { state.input_fields with donation_amount = s }
        } 
      in
      (state1, None)
  
  | ServerMessage(Confirmation(IdW32(t,v),r_hash)) -> 
      (state, None)

  | _ -> (state, None) ;;


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
        let Some key = Js.Nullable.toOption state.key in 
        [| header "Your current key"
         ; par key
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
  let event_bus = Event.make_event_bus () in
  let proj = create_projector () in

  let emit = Event.emit_stimulus event_bus in
  let ws_emit sm =
    Js.log sm ;
    emit (ServerMessage sm) ;
    schedule_render proj
  in

  let ws_send = Websocket.make_sender ws_emit (Config.config |. Config.ws_urlGet) in

  let app = get_element_by_id doc "app" in
  let state = ref empty_state in

  let handler stim = 
    let (s1, m) = update stim !state in
    begin state := s1;
    match m with
      | None -> ()
      | Some (DonateMsg(_, _) as msg) -> 
          Js.log msg ;
    end
  in 

  begin Event.register_handler event_bus handler;
  replace proj app (fun _ -> render emit !state);
  schedule_render proj;
  end

