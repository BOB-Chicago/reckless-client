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
    | Click (Nav x) -> { state with active_page = x }
    | Click (SetKey x) -> { state with key = x }
    | _ -> state

(* ~~~~~~~~~ *)
(* Interface *)
(* ~~~~~~~~~ *)

type app_element = AppElement

external page : app_element array -> unit = "page" [@@bs.module "lib"]
external header : string -> app_element = "header" [@@bs.module "lib"]
external row : app_element array -> app_element = "row" [@@bs.module "lib"]
external button : string -> click_target -> app_element = "button" [@@bs.module "lib"]


let nav p = 
  let name = match p with
    | LocStart -> ">> start page"
    | LocManageKeys -> "manage keys"
    | LocShowKey -> "show key"
    | LocEnterKey -> "enter a new key"
    | LocDonate -> "donate" 
    | LocPaymentRequests -> "payment requests"
  in button name (Nav p)


let render state =
  let elts = match state.active_page with 
    | LocStart ->  
      [| header "BOB chicago #reckless" 
       ; row [| nav LocDonate; nav LocPaymentRequests;  nav LocManageKeys |] 
      |]
    | LocManageKeys ->  
        [| header "Manage your key"
         ; match state.key with 
            | None -> row [| nav LocEnterKey; button "generate a random key" GenRandomKey; nav LocStart |]
            | Some _ -> row [| button "forget your key" (SetKey None); nav LocShowKey; nav LocStart |]
        |]
    | LocShowKey -> [| header "Your current key" |] 
  in page elts


