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
  [@@bs.deriving jsConverter]

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

type app_state =
  { active_page: app_location; 
    key: string option;
    payment_requests: payment_request;
    payment_request_cursor: int
  } [@@bs.deriving jsConverter]


let update stim state =
  match stim with
    | Click (Nav x) -> 
        { state with active_page = x }
    | Click (SetKey x) -> 
        { state with key = x } ;;


(* ~~~~~~~~~ *)
(* Interface *)
(* ~~~~~~~~~ *)

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


let make_ui_kit emit =
  { header = fun text -> h "h1" (vnode_attributes ()) [| hText text |] 

  ; row =  
      let atts = vnode_attributes ~class_: "row" () in 
      h "div" atts 

  ; column = 
      let atts = vnode_attributes ~class_: "column" () in 
      h "div" atts

  ; par = fun text -> 
      let atts = vnode_attributes ~class_:"simple" () in 
      h "p" atts [| hText text |]

  ; button = 
      let atts = vnode_attributes ~class_:"button" () in
      fun text _ -> h "div" atts [| hText text |]

  }


let render state =
  let { page; header; row; column; par; button; } = make_ui_kit emit in
  let nav p = button (navText p) (Click (Nav p)) in
  let elts = match state.active_page with 
    | LocStart ->  
      [| header "BOB chicago #reckless" 
       ; row [| nav LocDonate; nav LocPaymentRequests;  nav LocManageKeys |] 
      |]
    | LocManageKeys ->  
        let forgetKey = button "forget your key" (SetKey None) in
        [| header "Manage your key"
         ; match state.key with 
            | None -> row [| nav LocEnterKey; button "generate a random key" GenRandomKey; nav LocStart |]
            | Some _ -> row [| forgetKey; nav LocShowKey; nav LocStart |]
        |]
    | LocShowKey -> [| header "Your current key"; let Some key = state.key in par key; row [| forgetKey; nav LocStart |] |] 
    | LocEnterKey -> [| header "Please enter a new key"; input KeyEntry; row [| nav LocStart |] |]
    | LocPaymentRequests -> 
        [| header "Your payment requests"
         ; column page_of_prs
         ; row [| nav Start |]
        |]
    | LocDonate ->
        [| header "Donate to BOB"
         ; input "donation message" DonationMessage
         ; input "donation amount" DonationAmount
         ; row [| nav LocStart |]
         |]
  in page elts


