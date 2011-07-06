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

  let basic_event_prim w send =
    let install_handler set_meth handler =
      set_meth handler in
    (* Not entirelly correct if the curried function would be stored *)
    let clicks = ref 0 in
    let mouse_click_handler =
      Dom_html.handler
        (fun ev ->
          send (`Int (!clicks)); incr clicks; Js._false)
    in
    let mouse_handler =
      Dom_html.handler 
        (fun ev ->
          let x, y = ev ## clientX, ev ## clientY in
          send (`Vec2 (x,y)); Js._false) in
    let key_handler =
      Dom_html.handler
        (fun ev ->
          send (`Int (ev ## keyCode));
          Js._false)
    in
    function
    | `Onclick -> install_handler (fun v -> w ## onclick <- v) mouse_click_handler
    | `Ondblclick -> install_handler (fun v -> w ## ondblclick <- v) mouse_click_handler
    | `Onmousedown -> install_handler (fun v -> w ## onmousedown <- v) mouse_handler
    | `Onmouseup -> install_handler (fun v -> w ## onmouseup <- v) mouse_handler
    | `Onmouseover -> install_handler (fun v -> w ## onmouseover <- v) mouse_handler
    | `Onmousemove -> 
        let lastx, lasty = ref 0, ref 0 in
        install_handler (fun v -> w ## onmouseover <- v) (Dom_html.handler begin fun ev ->
          let x, y = ev ## clientX, ev ## clientY in
          let dx, dy = x - !lastx, y - !lasty in
          lastx := x; lasty := y;
          send (`Vec2 (dx,dy)); Js._false end)
    | `Onmouseout -> install_handler (fun v -> w ## onmouseout <- v) mouse_handler
    | `Onkeypress -> install_handler (fun v -> w ## onkeypress <- v) key_handler
    | `Onkeydown -> install_handler (fun v -> w ## onkeydown <- v) key_handler
    | `Onkeyup -> install_handler (fun v -> w ## onkeyup <- v) key_handler

  let install_react w =
      w, fun (et, map) -> 
        let e, send = React.E.create () in
        basic_event_prim w send et;
        React.E.fmap map e
    
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
