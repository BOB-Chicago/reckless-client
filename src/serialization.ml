open Types
open Js.Json
open Js.Result

module Option = Belt.Option

(* tagged id *)

let decodeTaggedId raw =

  match classify raw with
  | JSONObject obj -> 
      let tagWith t = 
        let f v = Types.IdW32(t, v) in
        Js.Dict.get obj "value" |. 
        Option.flatMap decodeNumber |. 
        Option.map Js.Math.floor |.
        Option.map f
      in
      let decodeTag = function
        | "ContributionT" -> Some ContributionT
        | "ItemT" -> Some ItemT
        | "OrderT" -> Some OrderT
        | "SurveyT" -> Some SurveyT
        | _ -> None
      in
      Js.Dict.get obj "type" |. Option.flatMap decodeString |. Option.flatMap decodeTag |. Option.flatMap tagWith

  | _ -> None 

(* server message *)

let parse_server_message msg =
  (* nonce & ref fields *)
  let outer obj f =
    match Js.Dict.get obj "nonce" with
    | None -> Error("server message / nonce")
    | Some nonceRaw ->
        begin match classify nonceRaw with
        | JSONNumber n ->
            let ref =  (Js.Dict.get obj "ref") |. Option.flatMap decodeNumber |. Option.map Js.Math.floor in
            let nonce = Js.Math.floor n in
            f nonce ref
        | _ -> Error("server message / nonce : type")
        end
  in

  let pack body nonce ref = Ok { nonce; ref; body } in

  match classify (parseExn msg) with
  | JSONObject obj -> 
      begin match Js.Dict.get obj "tag" with
      | None -> Error("server message / tag")
      | Some rawTag -> 
          begin match classify rawTag with
          | JSONString tag ->
              begin match tag with
              | "ack" -> outer obj (pack Ack) 

              | "paymentRequest" -> 
                  let p = Js.Dict.get obj "paymentRequest" |. Option.flatMap decodeString in
                  let r = Js.Dict.get obj "rHash" |. Option.flatMap decodeString in
                  begin match (p, r) with
                  | (Some pr, Some r_hash) ->
                      outer obj (pack (PaymentRequest(pr, r_hash)))
                  | _ -> Error("server message (paymentRequest) / (paymentRequest | r_hash)")
                  end


              | "confirmation" -> 
                  let i = Js.Dict.get obj "id" |. Option.flatMap decodeTaggedId in
                  let r = Js.Dict.get obj "rHash" |. Option.flatMap decodeString in
                  begin match (i, r) with
                  | (Some id, Some r_hash) -> 
                      outer obj (pack (Confirmation(id, r_hash)))
                  | _ -> Error("server message (confirmation)")
                  end
                      

              | "object" -> outer obj (pack Types.Object) 
              
              | _ -> Error("server message / tag (unknown)")
              end

          | _ -> Error("server message / tag : type")
          end
      end

  | _ -> Error("server message: type")

(* client message *)

let encode_client_message msg =
  match msg with
  | DonateMsg(memo, amount) -> 
      let spec = 
        [| ("tag", string "donate")
         ; ("message", string memo)
         ; ("amount", number (Js.Int.toFloat amount)) 
        |]
      in
      Js.Dict.fromArray spec
  | Sync key ->
      let spec =
        [| ("tag", string "sync")
         ; ("key", string key)
        |]
      in
      Js.Dict.fromArray spec

  | NewBlob (body, key, ttl) ->
      let spec =
        [| ("tag", string "newBlob")
         ; ("data", string body)
         ; ("key", string key)
         ; ("lifetime", number (Js.Int.toFloat ttl))
        |]
      in
      Js.Dict.fromArray spec
    

(* payment request *)

let encode_payment_request pr =
  let spec = 
    [| ("r_hash", Js.Json.string pr.r_hash)
     ; ("req", Js.Json.string pr.req)
     ; ("memo", Js.Json.string pr.memo)
     ; ("date", Js.Date.toString pr.date |. Js.Json.string)
     ; ("paid", Js.Json.boolean pr.paid )
     |]
  in
  Js.Dict.fromArray spec |. Js.Json.object_  

let decode_payment_request json =
  match classify json with
  | JSONObject obj -> 
      let rh = Js.Dict.get obj "r_hash" |. Option.flatMap decodeString in
      let r = Js.Dict.get obj "req" |. Option.flatMap decodeString in
      let m = Js.Dict.get obj "memo" |. Option.flatMap decodeString  in
      let d = Js.Dict.get obj "date" |. Option.flatMap decodeString in
      let p = Js.Dict.get obj "paid" |. Option.flatMap decodeBoolean in
      begin match (rh, r, m, d, p) with
      | ( Some r_hash, Some req, Some memo, Some dateString, Some paid) ->
          Ok { r_hash; memo; req; date = Js.Date.fromString dateString; paid }
      | _ -> Error "payment_request : field"
      end
  | _ -> Error "payment_request : type"

(* app state *)

let encode_app_state state = 
  let prs = 
    Array.map encode_payment_request state.payment_requests |. Js.Json.array
  in
  let key = 
    match Js.Nullable.toOption state.key with
    | Some s -> Js.Json.string s
    | None -> Js.Json.null
  in
  let spec = 
    [| ("key", key)
     ; ("payment_requests", prs)
    |]
  in
  Js.Dict.fromArray spec |. Js.Json.object_

let decode_app_state str = 
  match classify (parseExn str) with
    | JSONObject obj ->
        let k = Js.Dict.get obj "key" |. Option.flatMap decodeString |. Js.Nullable.fromOption in
        let decode_pr x = match decode_payment_request x with
          | Ok pr -> Some pr
          | _ -> None
        in
        let raw_array = Js.Dict.get obj "payment_requests" |. Option.flatMap decodeArray in
        let pr_result = Option.map raw_array (Array.map decode_pr) in
        let step xs z = match z with
          | Some x -> Belt.List.add xs x
          | None -> xs
        in
        let prs = match pr_result with
          | None -> [||]
          | Some xs -> Array.of_list (Js.Array.reduce step [] xs)
        in
        Ok { empty_state with key = k; payment_requests = prs }

    | _ -> Error "app_state : type"

