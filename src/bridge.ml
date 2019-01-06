(* Ian: This stuff is in it's own source file simply because my syntax highlighter
 * cannot handle the module pragmas *)

(* Generic type to hide details of working with the dom tree *)
type app_element = AppElement

type event_target = { value: string }
type event = { target: event_target  }

type vnode_attributes =
  { class_ : string [@bs.as "class"] [@bs.optional]
  ; onclick: unit -> unit [@bs.optional]
  ; oninput: event -> unit [@bs.optional]
  } [@@bs.deriving abstract]

type projector =
  { scheduleRender : unit -> unit 
  }

type websocket_message = { data: string }

type websocket_config =
  { url: string
  ; on_open: unit -> unit
  ; on_close: unit -> unit
  ; on_message: websocket_message -> unit
  }

external hText : string -> app_element = "%identity"
external h : string -> vnode_attributes -> app_element array -> app_element = "h" [@@bs.module "maquette"]
external createProjector : unit -> projector = "createProjector" [@@bs.module "maquette"]

external getWebSocketSend : websocket_config -> string -> unit = "getWebSocketSend" [@@bs.module "./lib"]

external getRandomValues : Js.Typed_array.Uint8Array.t -> unit = "getRandomValues" [@@bs.val][@@bs.scope "window", "crypto"]
