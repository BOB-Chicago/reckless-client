open Types
open Bridge

(* ~~~~~~~~~ *)
(* App logic *)
(* ~~~~~~~~~ *)

let update stim state =
  match stim with
  | Click (Nav x) -> 
      ({ state with active_page = x }, None)

  | Click (SetKey x) -> 
      ({ state with key = x }, None)

  | Click Donate ->
      let amount = 
        let f = Js.Float.fromString state.input_fields.donation_amount in
        Js.Math.floor f
      in
      (state, Some (DonateMsg (state.input_fields.donation_memo, amount)))
  
  | Click GenRandomKey ->
      let keyBuf = Util.randomBytes 32 in
      let hex = Util.hexEncode keyBuf in
      let state1 = { state with key = Some hex } in
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

  | _ -> (state, None) ;;


(* ~~~~~~~~~~~~~~~~~ *)
(* Interface helpers *)
(* ~~~~~~~~~~~~~~~~~ *)


let navText p = match p with
  | LocStart -> ">> start page"
  | LocManageKeys -> "manage keys"
  | LocShowKey -> "show key"
  | LocEnterKey -> "enter a new key"
  | LocDonate -> "donate" 
  | LocPaymentRequests -> "payment requests"

let header text =  
  h "h1" (vnode_attributes ()) [| hText text |]

let row =  
  let atts = vnode_attributes ~class_: "row" () in 
  h "div" atts 

let column = 
  let atts = vnode_attributes ~class_: "column" () in 
  h "div" atts

let par text = 
  let atts = vnode_attributes ~class_: "simple" () in 
  h "p" atts [| hText text |]

let button emit text t = 
  let on_click = fun _ -> emit (Click t) in
  let atts = vnode_attributes ~class_:"button" ~onclick:on_click () in
  h "div" atts [| hText text |]

let input_ emit t x = 
  let on_input e = 
        match e.target.value with
        | Some v -> emit (Input (x, v)) 
        | None -> ()
  in
  let input_elt = h "input" (vnode_attributes ~oninput: on_input ()) [||] in
  match t with

  | Some text ->
      h "div" (vnode_attributes ~class_: "input" ())
        [| h "p" (vnode_attributes ()) [| hText text |]
         ; input_elt 
         |]
 
  | None ->
      input_elt


(* ~~~~~~~~~ *)
(* Interface *)
(* ~~~~~~~~~ *)

let render emit state =
  let button' = button emit in
  let input_elt = input_ emit in
  let nav p = button' (navText p) (Nav p) in
  let forgetKey = button' "forget your key" (SetKey None) in

  match state.active_page with 

  | LocStart ->  
      [| header "BOB chicago #reckless" 
       ; row [| nav LocDonate; nav LocPaymentRequests;  nav LocManageKeys |] 
       |]

  | LocManageKeys ->  
      [| header "Manage your key"
       ; match state.key with 
         | None -> 
            row [| nav LocEnterKey; button' "generate a random key" GenRandomKey; nav LocStart |]
         | Some _ -> 
            row [| forgetKey; nav LocShowKey; nav LocStart |]
       |]

  | LocShowKey -> 
      [| header "Your current key"
       ; let Some key = state.key in par key
       ; row [| forgetKey; nav LocStart |] 
       |] 

  | LocEnterKey -> 
      [| header "Please enter a new key"
       ; input_elt None KeyEntry
       ; row [| nav LocStart |] 
       |]

  | LocPaymentRequests -> 
      [| header "Your payment requests"
       ; column [||] 
       ; row [| nav LocStart |]
      |]

  | LocDonate ->
      [| header "Donate to BOB"
       ; input_elt (Some "donation message") DonationMemo
       ; input_elt (Some "donation amount") DonationAmount
       ; row [| nav LocStart |]
       |]

