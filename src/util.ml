(* crypto *)
open Bridge.Crypto

module Uint8Array = Js.Typed_array.Uint8Array


(* Hex *)

let hex_digits = "0123456789abcdef" 

let to_hex_digit i =
  Js.String.get hex_digits i

let from_hex_digit x =
  Js.String.indexOf x hex_digits

let hex_encode =
  (* This function must be uncurried *)
  let step = fun [@bs] s v -> 
    let q = v / 16 in
    let r = v - 16 * q in
    Js.String.concatMany [| to_hex_digit q; to_hex_digit r |] s 
  in
  Uint8Array.reduce step ""

let hex_decode hex = 
  let l = String.length hex in
  let rec f i =
    if i >= l 
    then [] 
    else 
      let hi = Js.String.charAt i hex in
      let lo = Js.String.charAt (i+1) hex in
      let x = 16 * (from_hex_digit hi) + from_hex_digit lo in
      x :: f (i+2)
  in Uint8Array.make (Array.of_list (f 0))


(* hashing & randomness *)

let random_bytes n =
  let arr = Uint8Array.fromLength n in
  get_random_values arr;
  arr

let sha256 bytes =
  let buf = Uint8Array.buffer bytes in
  let d = digest "SHA-256" buf in
  Js.Promise.then_ (fun x -> Js.Promise.resolve (Uint8Array.fromBuffer x)) d

let derive_key hex path = 
  let bytes = hex_decode hex in
  let pathBytes = encode path in
  let buf = uint8Array_concat [| bytes; pathBytes |] in 
  Js.Promise.then_ (fun x -> Js.Promise.resolve (hex_encode x)) (sha256 buf)

let random_key _ =
  hex_encode (random_bytes 8)
