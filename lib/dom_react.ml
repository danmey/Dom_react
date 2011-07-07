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

  
(* After long though I came to conclusion I need to completely
   deconstruct nice js_of_ocaml `properties' syntax into bunch of
   functions, instead of using polymorphic variant selectors. This
   solution is more type safe, more efficient (avoding constructing
   dynamic continuations), flexible and make type inference happy
   . Maybe such solution would be feasible as a wrapper for whole
   Dom_html library, to allow more functional constructs.  Polymorphic
   variants selectors still remain experimetaly in Prim module. At the
   end I think we will go for hybrid solution. 
   UPDATE: Even for events I decided to go for this solution. *)

module Fun_prop = struct
    let set_selectedIndex w v = w ## selectedIndex <- v
    let set_value w v = w ## value <- Js.string v
    let set_length w v = w ## length <- v
    let set_options w v = w ## options <- v
    let set_disabled w v = w ## disabled <- v
    let set_multiple w v = w ## multiple <- v
    let set_name w v = w ## name <- v
    let set_size w v = w ## size <- v
    let set_tabIndex w v = w ## tabIndex <- v
    let set_innerHTML w v = w ## innerHTML <- Js.string v

    let set_onclick w v = w ## onclick <- Dom_html.handler v
    let set_ondblclick w v = w ## ondblclick <- Dom_html.handler v
    let set_onmousedown w v = w ## onmousedown <- Dom_html.handler v
    let set_onmouseup w v = w ## onmouseup <- Dom_html.handler v
    let set_onmouseover w v = w ## onmouseover <- Dom_html.handler v
    let set_onmousemove w v = w ## onmousemove <- Dom_html.handler v
    let set_onmouseout w v = w ## onmouseout <- Dom_html.handler v
    let set_onkeypress w v = w ## onkeypress <- Dom_html.handler v
    let set_onkeydown w v = w ## onkeydown <- Dom_html.handler v
    let set_onkeyup w v = w ## onkeyup <- Dom_html.handler v
    let set_onload w v = w ## onload <- Dom_html.handler (fun ev -> v(); Js._false)

    let selectedIndex w v = w ## selectedIndex
    let value w v = w ## value
    let length w v = w ## length
    let options w v = w ## options
    let disabled w v = w ## disabled
    let multiple w v = w ## multiple
    let name w v = w ## name
    let size w v = w ## size
    let tabIndex w v = w ## tabIndex
    let innerHTML w v = Js.to_string w ## innerHTML
end


(* Let's say we want a taste of dynamic type system to ensure we can
   broadcast any type of event easily.  *)
  
module Dyn_conv = struct

  exception Event_type

  let vec2   = function 
    | `Vec2 (x,y) -> x,y 
    | _ -> raise Event_type

  let int = function 
      | `Int i -> i
      | `String str -> int_of_string str
      | `Float f -> int_of_float f
      | `Unit -> 0
      | _ -> raise Event_type

  let float  = function 
    | `Float f -> f
    | `Int i -> float_of_int i
    | `String str -> float_of_string str
    | `Unit -> 0.
    | _ -> raise Event_type

  let string = function 
    | `String s   -> s
    | `Int i -> string_of_int i
    | `Float f -> string_of_float f
    | `Unit -> ""
    | _ -> raise Event_type

  let unit _ = ()
  let fix i _ = i
end

module Dom_react = struct
    
  module Prim = struct
    module E = struct
        
      (* Define event selectors, not sure if I should not go with the
      properties, this with map gives so much flexibility in
      conversion of the types as an ouput primitive*)
      (* type common_event_source = *)
      (*   [ `Onclick *)
      (*   | `Ondblclick *)
      (*   | `Onkeydown *)
      (*   | `Onkeypress *)
      (*   | `Onkeyup *)
      (*   | `Onmousedown *)
      (*   | `Onmousemove *)
      (*   | `Onmouseout *)
      (*   | `Onmouseover *)
      (*   | `Onmouseup ] *)

      (* type event_window_source = *)
      (*   [ `Onload *)
      (*   | `Onbeforeunload *)
      (*   | `Onblur *)
      (*   | `Onfocus *)
      (*   | `Onresize] *)

        let mouse_click_handler send =
          let clicks = ref 0 in
          fun ev -> send (!clicks); incr clicks; Js._false
            
        let mouse_handler send =
            (fun ev -> 
              let x, y = ev ## clientX, ev ## clientY in
              send (x,y); Js._false)
            
        let key_handler send =
            (fun ev ->
              send (ev ## keyCode);
              Js._false)
          
        let mouse_move_handler send =
        let lastx, lasty = ref 0, ref 0 in
        fun ev ->
          let x, y = ev ## clientX, ev ## clientY in
          let dx, dy = x - !lastx, y - !lasty in
          lastx := x; lasty := y;
          send (dx,dy); 
          Js._false

        let onclick = Fun_prop.set_onclick, mouse_click_handler
        let ondblclick = Fun_prop.set_ondblclick, mouse_click_handler
        let onmousedown = Fun_prop.set_onmousedown, mouse_handler
        let onmouseup = Fun_prop.set_onmouseup, mouse_handler
        let onmouseover = Fun_prop.set_onmouseover, mouse_handler
        let onmousemove = Fun_prop.set_onmousemove, mouse_move_handler
        let onkeypress = Fun_prop.set_onkeypress, key_handler
        let onkeydown = Fun_prop.set_onkeydown, key_handler
        let onkeyup = Fun_prop.set_onkeyup, key_handler
        let onload = Fun_prop.set_onload, (fun send ev -> send ())

        let create w event =
          let set_meth, handler = event in
          let event, send = E.create () in
          let _ = set_meth w (handler send) in
          event

        let install_react w =
          w, fun event -> create w event
              
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
module E = struct


    let createButton ?_type ?name doc =
      let w = Dom_html.createButton ?_type ?name doc in
      w, fun (e, setter) -> E.map (setter w) e
  end
end
    
