open Types
open Bridge
open Bridge.VDom

(* ~~~~~~~~~~~~~~~~~ *)
(* Interface helpers *)
(* ~~~~~~~~~~~~~~~~~ *)

let input_name k = match k with
  | KeyEntry -> "key_entry"
  | DonationMemo -> "donation_memo"
  | DonationAmount -> "donation_amount"
  | BlobPaste -> "blob_paste"

let nav_text p = match p with
  | LocStart -> ">> start page"
  | LocManageKeys -> "manage keys"
  | LocShowKey -> "show key"
  | LocEnterKey -> "enter a new key"
  | LocDonate -> "donate" 
  | LocPaymentRequestList -> "payment requests"
  | LocPaymentRequest -> "payment request"
  | LocBlobUpload -> "data upload"

let get_copy str = Dom.innerHTMLGet (Dom.get_element_by_id Dom.doc str)

(* ~~~~~~~~~~ *)
(* Components *)
(* ~~~~~~~~~~ *)

let div cl xs = 
  let key = Util.random_key () in
  h "div" (vnode_attributes ~key ~class_: cl ()) xs

let header text =  
  h "h1" (vnode_attributes ()) [| h_text text |]

let link href x =
  let key = Util.random_key () in
  h "a" (vnode_attributes ~key ~href ()) [| x |]

let text_link href text = link href (h_text text)

let internal_link onclick text =
  let key = Util.random_key () in
  let atts = vnode_attributes ~onclick ~key ~class_: "internalLink" () in
  h "span" atts [| h_text text |]

let row =  
  let key = Util.random_key () in
  let atts = vnode_attributes ~key ~class_: "row" () in 
  h "div" atts 

let column = 
  let key = Util.random_key () in
  let atts = vnode_attributes ~key ~class_: "column" () in 
  h "div" atts

let par_node xs =
  let key = Util.random_key () in
  let atts = vnode_attributes ~key () in 
  h "p" atts xs

let par text = par_node [| h_text text |]

let explainer tag = 
  let atts = vnode_attributes ~key: "explainer" ~class_: "explainer" ~innerHTML: (get_copy tag) () in
  h "div" atts [||]

let button emit text t = 
  let key = Util.random_key () in
  let on_click = fun _ -> emit (Click t) in
  let atts = vnode_attributes ~key ~class_:"button" ~onclick:on_click () in
  h "div" atts [| h_text text |]

let input_ emit t value x = 
  let key = input_name x in

  let contents e = 
      let target = Event.targetGet e in
      begin match Event.valueGet target with
        | Some v -> emit (Input (x, v)) 
        | None -> () 
      end ;
  in

  let input_elt = 
    let atts = vnode_attributes ~key ~value ~oninput: contents () in
    h "input" atts [||] 
  in

  let wrapper = 
    let atts = vnode_attributes ~key ~class_: "input" () in
    h "div" atts 
  in

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
  let nav p = button' (nav_text p) (Nav p) in
  let forgetKey = button' "forget your key" (SetKey None) in

  let node_uri = Config.node_uriGet (Config.config) in

  let content = match state.active_page with 

    | LocStart ->  
        let controls = 
          if Js.Nullable.isNullable state.key 
          then [| nav LocManageKeys |]
          else [| nav LocDonate; nav LocPaymentRequestList; nav LocManageKeys |]
        in

        let links =
          [| h "span" (vnode_attributes ()) [| h_text "this code on github: " |]
          ; text_link "https://github.com/bob-chicago/reckless-client" "client" 
          ; text_link "https://github.com/bob-chicago/reckless-server" "server"
          |]
        in

        let node_qr = h "div" (vnode_attributes ~class_: "qrcode" ~innerHTML: (Qr.qrencode node_uri) ()) [||] in

        [| header "BOB chicago #reckless" 
         ; row controls 
         ; explainer "welcome" 
         ; row links
         ; par "Connect to our lightning node:"
         ; link ("lightning://" ^ node_uri) node_qr
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
        let enter_key = button' "set key" (SetKey(Some state.input_fields.key_entry)) in
        [| header "Please enter a new key"
         ; input_elt None state.input_fields.key_entry KeyEntry 
         ; row [| enter_key; nav LocStart |] 
         |]

    | LocPaymentRequest ->
        begin match state.active_pr with
        | None -> 
            [| header "Payment request not found" 
             ; row [| nav LocPaymentRequestList; nav LocStart |] 
            |]

        | Some pr -> 
            let msg = (if pr.paid then "[PAID] " else "") ^ "Payment request: " ^ pr.memo in
            let qr = h "div" (vnode_attributes ~class_: "qrcode" ~innerHTML: (Qr.qrencode pr.req) ()) [||] in
            [| header msg
            ; explainer "payment-request"
            ; link ("lightning:" ^ pr.req) qr
            ; row [| nav LocPaymentRequestList; nav LocStart |]
            |] 

        end

    | LocPaymentRequestList -> 
        let fmt pr = 
          let part1 = 
            let is_paid = if pr.paid then " PAID" else "" in
            "(" ^  Js.Date.toString pr.date ^ is_paid ^ "): "
          in
          let click _ = emit (Click (ViewPaymentRequest pr)) in
          par_node [| h_text part1; internal_link click pr.memo |] in
        let pr_nodes = Array.map fmt state.payment_requests in
        [| header "Your payment requests"
         ; column pr_nodes 
         ; row [| nav LocStart |]
        |]

    | LocDonate ->
        let donate = button' "donate" Donate in
        [| header "Donate to BOB"
         ; explainer "donation"
         ; input_elt (Some "donation message") state.input_fields.donation_memo DonationMemo
         ; input_elt (Some "donation amount") state.input_fields.donation_amount DonationAmount
         ; row [| donate; nav LocStart |]
         |]

    | LocBlobUpload ->
        let upload = button' "upload" UploadBlob in
        [| header "Upload raw binary data"
         ; input_elt (Some "blob note") state.input_fields.blob_note BlobNote
         ; input_elt (Some "enter data") state.input_fields.blob_paste BlobPaste
         ; row [| upload; nav LocStart |]
         |]
  
  in 
 
  div "main" content 

