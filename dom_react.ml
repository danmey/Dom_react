open React

module Event_type = struct

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

  (* type ('a, 'b) event_map = (event_data * 'a) React.E.t -> ('a * 'b) React.E.t *)
  (* type ('a, 'b) event_attacher = event_type * ('a, 'b) event_map -> ('a * 'b) React.E.t *)

  let vec2   = function | `Vec2 (x,y) -> Some (x,y) | _ -> None
  let int    = function | `Int i      -> Some i     | _ -> None
  let float  = function | `Float f    -> Some f     | _ -> None
  let string = function | `String s   -> Some s     | _ -> None

end

module Dom_html_react = struct

  module E = struct
  type common_event_source =
    [ `Onclick
    | `Ondblclick
    | `Onkeydown
    | `Onkeypress
    | `Onkeyup
    | `Onmousedown
    | `Onmousemove
    | `Onmouseout
    | `Onmouseover
    | `Onmouseup ]

  (* Monster! could be factored out *)
  let basic_event_prim w send = function
    | `Onclick ->
      w ## onclick <-
        let clicks = ref 0 in
        Dom_html.handler 
        (fun ev -> send (`Int (!clicks)); incr clicks; Js._false)
    | `Ondblclick ->
      w ## ondblclick <-
        let clicks = ref 0 in
        Dom_html.handler 
        (fun ev -> send (`Int (!clicks)); incr clicks; Js._false)
    | `Onmousedown ->
      w ## onmousedown <-
        Dom_html.handler 
        (fun ev ->
          let x, y = ev ## clientX, ev ## clientY in
          send (`Vec2 (x,y)); Js._false)
    | `Onmouseup ->
      w ## onmouseup <-
        Dom_html.handler 
        (fun ev ->
          let x, y = ev ## clientX, ev ## clientY in
          send (`Vec2 (x,y)); Js._false)
    | `Onmouseover ->
      w ## onmouseover <-
        Dom_html.handler 
        (fun ev ->
          let x, y = ev ## clientX, ev ## clientY in
          send (`Vec2 (x,y)); Js._false)
    | `Onmousemove ->
      w ## onmousemove <-
        let lastx, lasty = ref 0, ref 0 in
        Dom_html.handler 
        (fun ev ->
          let x, y = ev ## clientX, ev ## clientY in
          let dx, dy = x - !lastx, y - !lasty in
          lastx := x; lasty := y;
          send (`Vec2 (dx,dy)); Js._false)
    | `Onmouseout ->
      w ## onmouseout <-
        Dom_html.handler 
        (fun ev ->
          let x, y = ev ## clientX, ev ## clientY in
          send (`Vec2 (x,y));
          Js._false)
    | `Onkeypress ->
      w ## onkeypress <-
        Dom_html.handler
        (fun ev ->
          send (`Int (ev ## keyCode));
          Js._false)
    | `Onkeydown ->
      w ## onkeydown <-
        Dom_html.handler
        (fun ev ->
          send (`Int (ev ## keyCode));
          Js._false)
    | `Onkeyup ->
      w ## onkeyup <-
        Dom_html.handler
        (fun ev ->
          send (`Int (ev ## keyCode));
          Js._false)

  let install_react w =
      w, fun (et, map) -> 
        let e, send = React.E.create () in
        basic_event_prim w send et;
        React.E.fmap map e
    
  (* Lot of not needed boiler plate *)
  let createSelect ?_type ?name doc =
    install_react (Dom_html.createSelect ?_type ?name doc)
  let createInput ?_type ?name doc =
    install_react (Dom_html.createInput ?_type ?name doc)
  let createTextarea ?_type ?name doc = 
    install_react (Dom_html.createTextarea ?_type ?name doc)
  let createButton   ?_type ?name doc = 
    install_react (Dom_html.createButton ?_type ?name doc)
  let createDiv       doc = 
    install_react (Dom_html.createDiv doc)
  let createImg     doc =
    install_react (Dom_html.createImg doc)
  end
end
