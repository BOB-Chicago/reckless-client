let randomBytes n =
  let arr = Js.Typed_array.Uint8Array.fromLength n in
  Bridge.getRandomValues arr;
  arr

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
  Js.Typed_array.Uint8Array.reduce step ""

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

