(* ~~~~~~~~~ *)
(* Locations *)
(* ~~~~~~~~~ *)

type app_location =
  | LocStart
  | LocManageKeys
  | LocEnterKey
  | LocShowKey
  | LocPaymentRequests of int 
  | LocDonate

(* ~~~~~~~~~~~~~ *)
(* Click targets *)
(* ~~~~~~~~~~~~~ *)

type click_target =
  | ClickNav of app_location

  | ClickGetPayReq of string * int 

  | ClickRandomKey
  | ClickReplaceKey of string

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

[@bs_deriving {jsConverter: newType}]
type payment_request =
  { r_hash: string
  ; memo: string
  ; amount: int
  ; paid: bool
  }


(* ~~~~~~~~ *)
(* Reactive *)
(* ~~~~~~~~ *)

type app_state =
  { active_page: app_location; 
    key: string;
    payment_requests: payment_request;
  }

let update stim state =
  match stim with
  | Click (ClickNav x) -> { state with active_page = x }
  | Click (ClickReplaceKey x) -> { state with key = x }
  | stim -> state

(* ~~~~~~~~~ *)
(* Interface *)
(* ~~~~~~~~~ *)

