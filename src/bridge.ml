(* Ian: This stuff is in it's own source file simply because my syntax highlighter
 * cannot handle the module pragmas *)

module Uint8Array = Js.Typed_array.Uint8Array


(* Generic type to hide details of working with the dom tree *)
type app_element 

type event_target = { value: string option }
type event = { target: event_target  }

type vnode_attributes =
  { class_ : string [@bs.as "class"] [@bs.optional]
  ; onclick: unit -> unit [@bs.optional]
  ; oninput: event -> unit [@bs.optional]
  } [@@bs.deriving abstract]

type document
type projector

type websocket_message = { data: string }

type websocket_config =
  { url: string
  ; on_open: unit -> unit
  ; on_close: unit -> unit
  ; on_message: websocket_message -> unit
  }

external h_text : string -> app_element = "%identity"
external h : string -> vnode_attributes -> app_element array -> app_element = "h" [@@bs.module "maquette"]
external create_projector : unit -> projector = "createProjector" [@@bs.module "maquette"]
external schedule_render : projector -> unit = "scheduleRender" [@@bs.send]
external replace : projector -> app_element -> (unit -> app_element) -> unit = "replace" [@@bs.send]
external doc : document = "document" [@@bs.val]
external get_element_by_id : document -> string -> app_element = "getElementById" [@@bs.send]

external get_websocket_send : websocket_config -> string -> unit = "getWebSocketSend" [@@bs.module "./lib"]
external uint8Array_concat : Uint8Array.t array -> Uint8Array.t = "uint8ArrayConcat" [@@bs.module "./lib"]
external encode : string -> Uint8Array.t = "encode" [@@bs.module "./lib"]
external get_random_values : Uint8Array.t -> unit = "getRandomValues" [@@bs.val][@@bs.scope "window", "crypto"]

type event_bus = 
  { register: (Types.stimulus -> unit) -> unit
  ; emit: Types.stimulus -> unit
  }

external make_event_bus : unit -> event_bus = "makeEventBus" [@@bs.module "./lib"]

external digest : string -> Js.Typed_array.ArrayBuffer.t -> Js.Typed_array.ArrayBuffer.t Js.Promise.t = "digest" [@@bs.val][@@bs.scope "window", "crypto", "subtle"]
