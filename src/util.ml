(* crypto *)
open Bridge.Crypto

module Uint8Array = Js.Typed_array.Uint8Array

let randomBytes n =
  let arr = Uint8Array.fromLength n in
  get_random_values arr;
  arr

let sha256 bytes =
  let buf = Uint8Array.buffer bytes in
  digest "SHA-256" buf

let deriveKey bytes path = 
  let pathBytes = encode path in
  let buf = uint8Array_concat [| bytes; pathBytes |] in 
  sha256 buf 

(* Hex *)

let hexDigits = "0123456789abcdef" 

let toHexDigit i =
  Js.String.get hexDigits i

let fromHexDigit x =
  Js.String.indexOf x hexDigits

let hexEncode =
  (* This function must be uncurried *)
  let step = fun [@bs] s v -> 
    let q = v / 16 in
    let r = v - 16 * q in
    Js.String.concatMany [| toHexDigit q; toHexDigit r |] s 
  in
  Uint8Array.reduce step ""

let hexDecode hex = 
  let l = String.length hex in
  let rec f i =
    if i >= l 
    then [] 
    else 
      let hi = Js.String.charAt i hex in
      let lo = Js.String.charAt (i+1) hex in
      let x = 16 * (fromHexDigit hi) + fromHexDigit lo in
      x :: f (i+2)
  in Array.of_list (f 0)

let random_key _ =
  hexEncode (randomBytes 8)

