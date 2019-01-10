(* ~~~~~~ *)
(* Config *)
(* ~~~~~~ *)

type app_config =
  { node_uri : string
  ; ws_url: string
  }

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
  (* pr, r_hash *)
  | PaymentRequest of string * string 
  | Confirmation of tagged_id * string 

type 'a session_message =
  { nonce: int
  ; ref: int option
  ; body: 'a 
  } [@@bs.deriving jsConverter]

type client_message =
  | DonateMsg of string * int
  | Sync of string

(* ~~~~~~~ *)
(* Stimuli *)
(* ~~~~~~~ *)

type stimulus =
  | PushMessage of server_message
  | Click of click_target
  | Input of input_field * string

(* ~~~~~~~~~~~ *)
(* effects kit *)
(* ~~~~~~~~~~~ *)

type 's effects_lang =
  | NoOp
  | SendMsg of client_message * (server_message -> 's effects_lang)
  | StateUpdate of ('s -> 's) * 's effects_lang
  | WithDerivation of string * (string -> 's effects_lang)
  | WithPaymentReq of string * int * (string -> string -> 's effects_lang)
  | WithState of ('s -> 's effects_lang)


(* ~~~~~~~~~~ *)
(* Data model *)
(* ~~~~~~~~~~ *)

type payment_request =
  { r_hash: string
  ; req: string
  ; memo: string
  ; date: Js.Date.t
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
  ; key: string Js.Nullable.t
  ; payment_requests: payment_request array
  ; payment_request_cursor: int
  ; input_fields: user_provided_data 
  } [@@bs.deriving jsConverter]

let empty_state =
  { active_page = LocStart
  ; key = Js.Nullable.null
  ; payment_requests = [||]
  ; payment_request_cursor = 0
  ; input_fields =
    { key_entry = ""
    ; donation_memo = ""
    ; donation_amount = "" 
    }
  }


