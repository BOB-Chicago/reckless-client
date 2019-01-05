(* ~~~~~~~~~ *)
(* Locations *)
(* ~~~~~~~~~ *)

type app_location =
  | LocStart
  | LocManageKeys
  | LocEnterKey
  | LocShowKey
  | LocPaymentRequests
  | LocDonate

type input_field =
  | KeyEntry
  | DonationMemo
  | DonationAmount

(* ~~~~~~~~~~~~~ *)
(* Click targets *)
(* ~~~~~~~~~~~~~ *)

type click_target =
  | Nav of app_location

  | GetPayReq of string * int 

  | GenRandomKey
  | SetKey of string option

(* ~~~~~~~~~~~~~~~~~ *)
(* Protocol messages *)
(* ~~~~~~~~~~~~~~~~~ *)

type server_message =
  | Ack
  | Object
  | PaymentRequest of string * string 
  | Confirmation of string 

(* ~~~~~~~ *)
(* Stimuli *)
(* ~~~~~~~ *)

type stimulus =
  | ServerMessage of server_message
  | Click of click_target
  | Input of input_field * string

(* ~~~~~~~~~~ *)
(* Data model *)
(* ~~~~~~~~~~ *)

type payment_request =
  { r_hash: string
  ; memo: string
  ; amount: int
  ; paid: bool
  } [@@bs.deriving jsConverter]


(* ~~~~~~~~ *)
(* Reactive *)
(* ~~~~~~~~ *)

type user_provided_data =
  { key_entry: string
  ; donation_memo: string
  ; donation_amount: string
  }

type app_state =
  { active_page: app_location
  ; key: string option
  ; payment_requests: payment_request array
  ; payment_request_cursor: int
  ; input_fields: user_provided_data 
  } [@@bs.deriving jsConverter]


let empty_state =
  { active_page = LocStart
  ; key = None
  ; payment_requests = [||]
  ; payment_request_cursor = 0
  ; input_fields =
    { key_entry = ""
    ; donation_memo = ""
    ; donation_amount = "" 
    }
  }

let update stim state =
  match stim with
    | Click (Nav x) -> 
        { state with active_page = x }
    | Click (SetKey x) -> 
        { state with key = x } 
    | Input (x, s) ->
        { state with input_fields =
          match x with
          | KeyEntry -> { state.input_fields with key_entry = s } 
          | DonationMemo -> { state.input_fields with donation_memo = s }
          | DonationAmount -> { state.input_fields with donation_amount = s }
        } ;;


(* ~~~~~~~~~~~~~~~~~ *)
(* Interface helpers *)
(* ~~~~~~~~~~~~~~~~~ *)

open Bridge

type interface_kit =
  { header: string -> app_element
  ; row: app_element array -> app_element
  ; column: app_element array -> app_element
  ; par: string -> app_element
  ; button: string -> click_target -> app_element
  }


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
  let on_input e = emit (Input (x, e.target.value)) in
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


(* ~~~~~~~~~~~~~ *)
(* Start the app *)
(* ~~~~~~~~~~~~~ *)

