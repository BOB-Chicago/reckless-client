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
  | LocPaymentRequestList
  | LocPaymentRequest
  | LocDonate
  | LocBlobUpload

type input_field =
  | KeyEntry

  | DonationMemo
  | DonationAmount

  | BlobPaste

  
(* ~~~~~~~~~~ *)
(* Data model *)
(* ~~~~~~~~~~ *)

type payment_request =
  { r_hash: string
  ; req: string
  ; memo: string
  ; date: Js.Date.t
  ; paid: bool
  } [@@bs.deriving jsConverter]


(* ~~~~~~~~~~~~~ *)
(* Click targets *)
(* ~~~~~~~~~~~~~ *)

type click_target =
  | Nav of app_location

  | Donate 

  | GenRandomKey
  | SetKey of string option

  | UploadBlob

  | ViewPaymentRequest of payment_request


(* ~~~~~ *)
(* State *)
(* ~~~~~ *)

type user_provided_data =
  { key_entry: string

  ; donation_memo: string
  ; donation_amount: string

  ; blob_paste: string
  }

type app_state =
  { active_page: app_location

  ; key: string Js.Nullable.t

  ; payment_requests: payment_request array
  ; payment_request_cursor: int
  ; active_pr: payment_request option

  ; input_fields: user_provided_data 

  } [@@bs.deriving jsConverter]

let empty_state =
  { active_page = LocStart

  ; key = Js.Nullable.null

  ; payment_requests = [||]
  ; payment_request_cursor = 0
  ; active_pr = None

  ; input_fields =
    { key_entry = ""
    ; donation_memo = ""
    ; donation_amount = "" 
    ; blob_paste = ""
    }
  }


(* ~~~~~~~~~~~~~~~~~ *)
(* Protocol messages *)
(* ~~~~~~~~~~~~~~~~~ *)

type object_type_w32 =
  | ContributionT
  | ItemT
  | OrderT
  | SurveyT

type tagged_id = 
  | IdW32 of object_type_w32 * int
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
  | NewBlob of string * string * int
  | Resolve of tagged_id * string option
  | Sync of string


(* ~~~~~~~~~~~ *)
(* effects kit *)
(* ~~~~~~~~~~~ *)

type 's effects_lang =
  | NoOp
  | SendMsg of client_message * (server_message -> 's effects_lang)
  | StateUpdate of ('s -> 's) * 's effects_lang
  | WithDerivation of string * (string -> 's effects_lang)
  | WithState of ('s -> 's effects_lang)


(* ~~~~~~~ *)
(* Stimuli *)
(* ~~~~~~~ *)

type stimulus =
  | PushMessage of server_message
  | Effect of app_state effects_lang
  | Click of click_target
  | Input of input_field * string

