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

module MakePrim(C : sig type 'a t val create : 'a -> ('a t * ('a -> unit)) end) = struct
    
  let mouse_click_handler send =
    let clicks = ref 0 in
    fun ev -> send (!clicks); incr clicks; Js._true
        
  let mouse_handler send =
    (fun ev -> 
      let x, y = ev ## clientX, ev ## clientY in
      send (x,y); Js._false)
      
  let key_handler send =
    (fun ev ->
      send ev ## charCode;
      Js._false)

  let char_handler send =
    (fun ev ->
      match Js.Optdef.to_option ev ## charCode with
        | None 
        | Some 0 -> 
          begin match ev ## keyCode with
            | 8 | 46 as a -> send a; Js._false
            | _ -> Js._true end
        | Some char_code -> send char_code; Js._false)
      
  let mouse_move_handler send =
    let lastx, lasty = ref 0, ref 0 in
    fun ev ->
      let x, y = ev ## clientX, ev ## clientY in
      let dx, dy = x - !lastx, y - !lasty in
      lastx := x; lasty := y;
      send (dx,dy); 
      Js._false

  let onclick = Properties.Set.onclick, mouse_click_handler
  let ondblclick = Properties.Set.ondblclick, mouse_click_handler
  let onmousedown = Properties.Set.onmousedown, mouse_handler
  let onmouseup = Properties.Set.onmouseup, mouse_handler
  let onmouseover = Properties.Set.onmouseover, mouse_handler
  let onmousemove = Properties.Set.onmousemove, mouse_handler
  let onkeypress = Properties.Set.onkeypress, key_handler
  let onkeydown = Properties.Set.onkeydown, key_handler
  let onkeyup = Properties.Set.onkeyup, key_handler
  let onload = Properties.Set.onload, (fun send ev -> send ())

  let onchar = Properties.Set.onkeypress, char_handler
    

end

module E = struct 
  include React.E
  include MakePrim(struct type 'a t = 'a React.E.t let create _ = React.E.create () end)
  let create w event =
    let set_meth, handler = event in
    let event, send = create () in
    let _ = set_meth w (handler send) in
    event
end

module S = struct
  include React.S
  include MakePrim(struct type 'a t = 'a React.S.t let create v = React.S.create v end)
  let create w event value =
    let set_meth, handler = event in
    let event, send = create value in
    let _ = set_meth w (handler send) in
    event
end











