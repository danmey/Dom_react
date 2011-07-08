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
   UPDATE: Even for events I decided to go for functions. *)

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

    (* Should be really wrapped with variants (mapping css_value -> string) *)
    module Css = struct
      let set_color w v = w ## style ## color <- Js.string v
      let set_backgorund w v = w ## style ## background <- Js.string v
      let set_position w v = w ## style ## position <- Js.string v
      let set_left w v = w ## style ## left <- Js.string (string_of_int v)
      let set_top w v = w ## style ## top <- Js.string (string_of_int v)
      let set_width w v = w ## style ## width <- Js.string (string_of_int v)
      let set_height w v = w ## style ## height <- Js.string (string_of_int v)
    end
end

module E = struct
  include React.E
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
  let onmousemove = Fun_prop.set_onmousemove, mouse_handler
  let onkeypress = Fun_prop.set_onkeypress, key_handler
  let onkeydown = Fun_prop.set_onkeydown, key_handler
  let onkeyup = Fun_prop.set_onkeyup, key_handler
  let onload = Fun_prop.set_onload, (fun send ev -> send ())
    
  let create w event =
    let set_meth, handler = event in
    let event, send = E.create () in
    let _ = set_meth w (handler send) in
    event

end

module S = struct
  include React.S
end
