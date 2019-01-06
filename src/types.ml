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

  | Donate 

  | GenRandomKey
  | SetKey of string option

(* ~~~~~~~~~~~~~~~~~ *)
(* Protocol messages *)
(* ~~~~~~~~~~~~~~~~~ *)

type tagged_id = 
  | IdW32 of string * int
  | IdB of string * string

type server_message =
  | Ack
  | Object
  | PaymentRequest of string * string 
  | Confirmation of tagged_id * string 

type 'a session_message =
  { nonce: int
  ; ref: int option
  ; body: 'a 
  } [@@bs.deriving jsConverter]

type client_message =
  | DonateMsg of string * int

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


(* ~~~~~ *)
(* State *)
(* ~~~~~ *)

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


