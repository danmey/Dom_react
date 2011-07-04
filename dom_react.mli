(*------------------------------------------------------------------------------
    This file is part of Dom_react.

    Foobar is free software: you can redistribute it and/or modify
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


(** Reactive DOM HTML

*)

open Js

module EventType : sig
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

  val vec2 : [> `Vec2 of 'a * 'b ] -> ('a * 'b) option
  val int : [> `Int of 'a ] -> 'a option
  (* val float : ('a, float) event_map *)
  (* val string : ('a, string) event_map *)

  type ('a, 'b) event_attacher = event_type * ('a, 'b) event_map -> ('a * 'b) React.E.t
end      

open Dom_html
(** {2 {Subset of DOM elements} *)
module Dom_html2 : sig
  open EventType
  (* val createOption : document t -> optionElement t *)
  val createSelect :
    ?_type:Js.js_string Js.t ->
    ?name:Js.js_string Js.t ->
    Dom_html.document Js.t ->
    Dom_html.selectElement Js.t *
      ([< `Onchange ] * ([> `Int of int ] -> 'a option) -> 'a React.event)
  (* val createInput : *)
  (*   ?_type:js_string t -> ?name:js_string t -> document t -> inputElement t *)
  (* val createTextarea : *)
  (*   ?_type:js_string t -> ?name:js_string t -> document t -> textAreaElement t *)
  (* val createButton : *)
  (*   ?_type:js_string t -> ?name:js_string t -> document t -> buttonElement t *)
  (* val createDiv : document t -> divElement t *)
  (* val createQ : document t -> quoteElement t *)
  (* val createImg : document t -> imageElement t *)
end
