(* Generic type to hide details of working with the dom tree *)
type app_element = AppElement

type vnode_attributes =
  { class_ : string [@bs.as "class"] [@bs.optional] }
  [@@bs.deriving abstract]

type projector =
  { scheduleRender : unit -> unit 
  }

external h : string -> vnode_attributes -> app_element array -> app_element = "h" [@@bs.module "maquette"]
external hText : string -> app_element = "%identity"
external createProjector : unit -> projector = "createProjector" [@@bs.module "maquette"]
