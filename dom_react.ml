open React

module Event_type = struct
  type event_data

  type event_type =
    [ `Onclick 
    | `Ondblclick 
    | `Onmousedown 
    | `Onmouseup 
    | `Onmouseover 
    | `Onmousemove
    | `Onmouseout
    | `Onkeypress
    | `Onkeydown
    | `Onkeyup
    | `Onselect
    | `Onchange ]

  type ('a, 'b) event_map = (event_data * 'a) React.E.t -> ('a * 'b) React.E.t
  type ('a, 'b) event_attacher = event_type * ('a, 'b) event_map -> ('a * 'b) React.E.t

  let vec2   = function | `Vec2 (x,y) -> Some (x,y) | _ -> None
  let int    = function | `Int i      -> Some i     | _ -> None
  val float  = function | `Float f    -> Some f     | _ -> None
  val string = function | `String s   -> Some s     | _ -> None

end

module Dom_html_react = struct

  let createSelect ?_type ?name doc =
    let w = Dom_html.createSelect ?_type ?name doc in
    w, begin fun (et, map) -> 
      let e, send = React.E.create () in
      begin match et with
        | `Onchange -> w ## onchange <- Dom_html.handler (fun ev -> send (`Int (w ## selectedIndex)); Js._false) end;
      React.E.fmap map e end

  let createInput ?_type ?name doc =
    let w = Dom_html.createInput ?_type ?name doc in
    w, begin fun (et, map) -> 
      let e, send = React.E.create () in
      begin match et with
        | `Onchange -> w ## onchange <- Dom_html.handler (fun ev -> send (`String (Js.to_string w ## value)); Js._false) end;
      React.E.fmap map e end
    
end
