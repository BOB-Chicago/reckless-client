open Types
open Js.Json
open Js.Result

module Option = Belt.Option

let decodeTaggedId raw =
  match classify raw with
  | JSONObject obj -> 
      begin match Js.Dict.get obj "type" |. Option.flatMap decodeString with
      | Some "ContributionT" ->
          let f v = Types.IdW32("ContributionT", v) in
          Js.Dict.get obj "value" |. 
          Option.flatMap decodeNumber |. 
          Option.map Js.Math.floor |.
          Option.map f

      | _ -> None 
      end

  | _ -> None 

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
      match Js.Dict.get obj "tag" with
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

  | _ -> Error("server message: type")



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
