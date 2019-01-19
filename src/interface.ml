open Types
open Bridge
open Bridge.VDom

(* ~~~~~~~~~~~~~~~~~ *)
(* Interface helpers *)
(* ~~~~~~~~~~~~~~~~~ *)

let inputName k = match k with
  | KeyEntry -> "key_entry"
  | DonationMemo -> "donation_memo"
  | DonationAmount -> "donation_amount"
  | BlobPaste -> "blob_paste"

let navText p = match p with
  | LocStart -> ">> start page"
  | LocManageKeys -> "manage keys"
  | LocShowKey -> "show key"
  | LocEnterKey -> "enter a new key"
  | LocDonate -> "donate" 
  | LocPaymentRequests -> "payment requests"
  | LocBlobUpload -> "data upload"


(* ~~~~~~~~~~ *)
(* Components *)
(* ~~~~~~~~~~ *)

let header text =  
  h "h1" (vnode_attributes ()) [| h_text text |]

let link href text =
  let key = Util.random_key () in
  let atts = vnode_attributes ~href ~key () in
  h "a" atts [| h_text text |]

let row =  
  let key = Util.random_key () in
  let atts = vnode_attributes ~key ~class_: "row" () in 
  h "div" atts 

let column = 
  let key = Util.random_key () in
  let atts = vnode_attributes ~key ~class_: "column" () in 
  h "div" atts

let parNode xs =
  let key = Util.random_key () in
  let atts = vnode_attributes ~key () in 
  h "p" atts xs

let par text = parNode [| h_text text |]

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
  let nav p = button' (navText p) (Nav p) in
  let forgetKey = button' "forget your key" (SetKey None) in

  let content = match state.active_page with 

    | LocStart ->  
        let controls = 
          if Js.Nullable.isNullable state.key 
          then [| nav LocManageKeys |]
          else [| nav LocDonate; nav LocPaymentRequests;  nav LocManageKeys |]
        in
        [| header "BOB chicago #reckless" 
         ; par "This is BOB's demo site"
         ; row controls 
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

    | LocPaymentRequests -> 
        let fmt pr = 
          let part1 = 
            let is_paid = if pr.paid then " PAID" else "" in
            "(" ^  Js.Date.toString pr.date ^ is_paid ^ "): "
          in
          let url = "lightning:" ^ pr.req in
          parNode [| h_text part1; link url pr.memo |] in
        let pr_nodes = Array.map fmt state.payment_requests in
        [| header "Your payment requests"
         ; column pr_nodes 
         ; row [| nav LocStart |]
        |]

    | LocDonate ->
        let donate = button' "donate" Donate in
        [| header "Donate to BOB"
         ; input_elt (Some "donation message") state.input_fields.donation_memo DonationMemo
         ; input_elt (Some "donation amount") state.input_fields.donation_amount DonationAmount
         ; row [| donate; nav LocStart |]
         |]

    | LocBlobUpload ->
        let upload = button' "upload" UploadBlob in
        [| header "Upload raw binary data"
         ; input_elt (Some "enter data") state.input_fields.blob_paste BlobPaste
         ; row [| upload; nav LocStart |]
         |]
  
  in h "div" (vnode_attributes ~class_: "main" ()) content

