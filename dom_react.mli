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


(** Reactive DOM HTML

*)

open Js

module Event_type : sig
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

  val vec2   : [> `Vec2 of int * int ] -> (int * int) option
  val int    : [> `Int of int ]            -> int option
  val float  : [> `Float of float ]        -> float option
  val string : [> `String of string ]      -> string option

end      

open Dom_html
(** {2 {Subset of DOM elements} *)
module Dom_html_react : sig
  open Event_type
   (* Factor out types to aliases *)
  val createSelect :
    ?_type:Js.js_string Js.t ->
    ?name:Js.js_string Js.t ->
    Dom_html.document Js.t ->
    Dom_html.selectElement Js.t *
      ([<        | `Onclick
       | `Ondblclick
       | `Onkeydown
       | `Onkeypress
       | `Onkeyup
       | `Onmousedown
       | `Onmousemove
       | `Onmouseout
       | `Onmouseover
       | `Onmouseup
       ] *
          ([> `Int of int | `Vec2 of int * int ] -> 'a option) ->
       'a React.event)

  val createInput :
    ?_type:Js.js_string Js.t ->
    ?name:Js.js_string Js.t ->
    Dom_html.document Js.t ->
    Dom_html.inputElement Js.t *
      ([< `Onclick
       | `Ondblclick
       | `Onkeydown
       | `Onkeypress
       | `Onkeyup
       | `Onmousedown
       | `Onmousemove
       | `Onmouseout
       | `Onmouseover
       | `Onmouseup ] *
          ([> `Int of int | `Vec2 of int * int ] -> 'a option) ->
       'a React.event)
  val createTextarea :
    ?_type:Js.js_string Js.t ->
    ?name:Js.js_string Js.t ->
    Dom_html.document Js.t ->
    Dom_html.textAreaElement Js.t *
      ([< `Onclick
       | `Ondblclick
       | `Onkeydown
       | `Onkeypress
       | `Onkeyup
       | `Onmousedown
       | `Onmousemove
       | `Onmouseout
       | `Onmouseover
       | `Onmouseup ] *
          ([> `Int of int | `Vec2 of int * int ] -> 'a option) ->
       'a React.event)
  val createButton :
    ?_type:Js.js_string Js.t ->
    ?name:Js.js_string Js.t ->
    Dom_html.document Js.t ->
    Dom_html.buttonElement Js.t *
      ([< `Onclick
       | `Ondblclick
       | `Onkeydown
       | `Onkeypress
       | `Onkeyup
       | `Onmousedown
       | `Onmousemove
       | `Onmouseout
       | `Onmouseover
       | `Onmouseup ] *
          ([> `Int of int | `Vec2 of int * int ] -> 'a option) ->
       'a React.event)
  val createDiv :
    Dom_html.document Js.t ->
    Dom_html.divElement Js.t *
      ([< `Onclick
       | `Ondblclick
       | `Onkeydown
       | `Onkeypress
       | `Onkeyup
       | `Onmousedown
       | `Onmousemove
       | `Onmouseout
       | `Onmouseover
       | `Onmouseup ] *
          ([> `Int of int | `Vec2 of int * int ] -> 'a option) ->
       'a React.event)
  val createImg :
    Dom_html.document Js.t ->
    Dom_html.imageElement Js.t *
      ([< `Onclick
       | `Ondblclick
       | `Onkeydown
       | `Onkeypress
       | `Onkeyup
       | `Onmousedown
       | `Onmousemove
       | `Onmouseout
       | `Onmouseover
       | `Onmouseup ] *
          ([> `Int of int | `Vec2 of int * int ] -> 'a option) ->
       'a React.event)

end
