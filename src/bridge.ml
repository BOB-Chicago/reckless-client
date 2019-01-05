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

external hText : string -> app_element = "%identity"
external h : string -> vnode_attributes -> app_element array -> app_element = "h" [@@bs.module "maquette"]
external createProjector : unit -> projector = "createProjector" [@@bs.module "maquette"]
