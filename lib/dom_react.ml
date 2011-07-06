(*------------------------------------------------------------------------------
  This file is part of Dom_react.

  Dom_react is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Foobar is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
  ----------------------------------------------------------------------------*)

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

  let vec2   = function | `Vec2 (x,y) -> Some (x,y) | _ -> None
  let int    = function | `Int i      -> Some i     | _ -> None
  let float  = function | `Float f    -> Some f     | _ -> None
  let string = function | `String s   -> Some s     | _ -> None

end

module Dom_html_react = struct

  module Prim = struct
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

    module S = struct
      type common_signal_source =
        [ `Id
        | `Title
        | `Lang
        | `Dir
        | `ClassName
        | `Style
        | `InnerHTML
        | `ClientLeft
        | `ClientTop
        | `ClientWidth
        | `ClientHeight
        | `OffsetLeft
        | `OffsetTop
        | `OffsetParent
        | `OffsetWidth
        | `offsetHeight
        | `ScrollLeft
        | `ScrollTop
        | `ScrollWidth
        | `ScrollHeight ]

      (* val createSelect : *)
      (*   ?_type:Js.js_string Js.t -> *)
      (*   ?name:Js.js_string Js.t -> *)
      (*   Dom_html.document Js.t -> *)
      (*   Dom_html.selectElement Js.t * *)
      (*     (update_func * common_signal_source * ([> `Int of int | `Vec2 of int * int ] -> 'a option) -> 'a React.event) *)
          
    end
  end
end
