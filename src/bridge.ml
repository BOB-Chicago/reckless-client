(* Ian: This stuff is in it's own source file simply because my syntax highlighter
 * cannot handle the module pragmas *)

module Uint8Array = Js.Typed_array.Uint8Array

(** 
 * We assume that application config is held in the global variable
 * `app_config`.  To configure the app, we need the url of the WebSocket server
 * and the node URI of our LND node. 
 *)

module Config = struct

  type app_config = 
    { node_uri : string 
    ; ws_url : string 
    } [@@bs.deriving abstract]

  external config : app_config = "app_config" [@@bs.val] [@@bs.scope "window"]

end

(**
 * The app uses the JavaScript event system to create a simple message queue.  
 *)
module Event = struct

  type event_target = { value: string option } [@@bs.deriving abstract]

  type event = { target: event_target  } [@@bs.deriving abstract]

  type event_bus 

  external emit_stimulus : event_bus -> Types.stimulus -> unit = "emit" [@@bs.send]
  external register_handler : event_bus -> (Types.stimulus -> unit Js.Promise.t) -> unit = "register" [@@bs.send]
  external make_event_bus : unit -> event_bus = "makeEventBus" [@@bs.module "./lib"]

end

(**
 * We use `maquette.js` for building and modifying the DOM tree.  This module
 * provides bindings to the essentials.
 *)
module VDom = struct

  (* Generic type to hide details of working with the dom tree *)
  type app_element 
  type node
  
  external dom_node : app_element -> node = "domNode" [@@bs.send]
  external focus : node -> unit = "focus" [@@bs.send]

  type vnode_attributes =
    { class_ : string [@bs.as "class"] [@bs.optional]
    ; key : string [@bs.optional]
    ; value : string [@bs.optional]
    ; href : string [@bs.optional]
    ; onclick: unit -> unit [@bs.optional]
    ; oninput: Event.event -> unit [@bs.optional]
    ; onchange: Event.event -> unit [@bs.optional]
    } [@@bs.deriving abstract]

  type document
  type projector

  external h_text : string -> app_element = "%identity"
  external h : string -> vnode_attributes -> app_element array -> app_element = "h" [@@bs.module "maquette"]
  external create_projector : unit -> projector = "createProjector" [@@bs.module "maquette"]
  external schedule_render : projector -> unit = "scheduleRender" [@@bs.send]
  external replace : projector -> app_element -> (unit -> app_element) -> unit = "replace" [@@bs.send]

  external doc : document = "document" [@@bs.val]
  external get_element_by_id : document -> string -> app_element = "getElementById" [@@bs.send]

end

(**
 * This module provides a simple API for working with a WebAPIs WebSocket, and
 * the JavaScript glue is implemented in `lib.js`.
 *)
module WebSocket = struct

  type websocket

  type websocket_message = { data: string } [@@bs.deriving abstract]

  type websocket_config =
    { url: string
    ; on_open: websocket -> unit
    ; on_close: unit -> unit
    ; on_message: websocket_message -> unit
    } [@@bs.deriving jsConverter]

  external get_websocket : websocket_config -> websocket = "getWebSocket" [@@bs.module "./lib"]
  external send : websocket -> string -> unit = "send" [@@bs.send]

end


let ws_config_to_js = WebSocket.websocket_configToJs

(**
 * Here is a simple crypto API that can be used idiomatically in the OCaml code. 
 *)
module Crypto = struct

  external uint8Array_concat : Uint8Array.t array -> Uint8Array.t = "uint8ArrayConcat" [@@bs.module "./lib"]
  external encode : string -> Uint8Array.t = "encode" [@@bs.module "./lib"]
  external get_random_values : Uint8Array.t -> unit = "getRandomValues" [@@bs.val][@@bs.scope "window", "crypto"]

  external digest : string -> Js.Typed_array.ArrayBuffer.t -> Js.Typed_array.ArrayBuffer.t Js.Promise.t = "digest" [@@bs.val][@@bs.scope "window", "crypto", "subtle"]

end 


module LocalStorage = struct

  external get : string -> string Js.Nullable.t = "getItem" [@@bs.val] [@@bs.scope "window", "localStorage"]
  external put : string -> string -> unit = "setItem" [@@bs.val] [@@bs.scope "window", "localStorage"]

end
