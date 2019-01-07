(* Ian: This stuff is in it's own source file simply because my syntax highlighter
 * cannot handle the module pragmas *)

module Uint8Array = Js.Typed_array.Uint8Array

module Config = struct

  type app_config = 
    { node_uri : string 
    ; ws_url : string 
    } [@@bs.deriving abstract]

  external config : app_config = "app_config" [@@bs.val] [@@bs.scope "window"]

end

module Event = struct

  type event_target = { value: string option } [@@bs.deriving abstract]
  external focus : event_target -> unit = "focus" [@@bs.send]

  type event = { target: event_target  } [@@bs.deriving abstract]

  type event_bus 

  external emit_stimulus : event_bus -> Types.stimulus -> unit = "emit" [@@bs.send]
  external register_handler : event_bus -> (Types.stimulus -> unit) -> unit = "register" [@@bs.send]
  external make_event_bus : unit -> event_bus = "makeEventBus" [@@bs.module "./lib"]

end


module VDom = struct

  (* Generic type to hide details of working with the dom tree *)
  type app_element 

  type vnode_attributes =
    { class_ : string [@bs.as "class"] [@bs.optional]
    ; key : string [@bs.optional]
    ; value : string [@bs.optional]
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


module WebSocket = struct

  type websocket_message = { data: string }

  type websocket_config =
    { url: string
    ; on_open: unit -> unit
    ; on_close: unit -> unit
    ; on_message: websocket_message -> unit
    } [@@bs.deriving jsConverter]

  external get_websocket_send : websocket_config -> string -> unit = "getWebSocketSend" [@@bs.module "./lib"]

end

module Crypto = struct

  external uint8Array_concat : Uint8Array.t array -> Uint8Array.t = "uint8ArrayConcat" [@@bs.module "./lib"]
  external encode : string -> Uint8Array.t = "encode" [@@bs.module "./lib"]
  external get_random_values : Uint8Array.t -> unit = "getRandomValues" [@@bs.val][@@bs.scope "window", "crypto"]

  external digest : string -> Js.Typed_array.ArrayBuffer.t -> Js.Typed_array.ArrayBuffer.t Js.Promise.t = "digest" [@@bs.val][@@bs.scope "window", "crypto", "subtle"]

end 
