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


module Fun_prop : sig
           val set_selectedIndex :
             < selectedIndex : < set : 'a -> unit; .. > Js.gen_prop; .. >
             Js.t -> 'a -> unit
           val set_value :
           < value : < set : Js.js_string Js.t -> unit; .. > Js.gen_prop;
             .. >
           Js.t -> string -> unit
           val set_length :
             < length : < set : 'a -> unit; .. > Js.gen_prop; .. > Js.t ->
             'a -> unit
           val set_options :
             < options : < set : 'a -> unit; .. > Js.gen_prop; .. > Js.t ->
             'a -> unit
           val set_disabled :
             < disabled : < set : 'a -> unit; .. > Js.gen_prop; .. > Js.t ->
             'a -> unit
           val set_multiple :
             < multiple : < set : 'a -> unit; .. > Js.gen_prop; .. > Js.t ->
             'a -> unit
           val set_name :
             < name : < set : 'a -> unit; .. > Js.gen_prop; .. > Js.t ->
             'a -> unit
           val set_size :
             < size : < set : 'a -> unit; .. > Js.gen_prop; .. > Js.t ->
             'a -> unit
           val set_tabIndex :
             < tabIndex : < set : 'a -> unit; .. > Js.gen_prop; .. > Js.t ->
             'a -> unit
           val set_innerHTML :
             < innerHTML : < set : Js.js_string Js.t -> unit; .. > Js.gen_prop;
    .. >
      Js.t -> string -> unit
end

module Dyn_conv : sig
  exception Event_type
  val vec2 : [> `Vec2 of 'a * 'b ] -> 'a * 'b
  val int :
    [> `Float of float | `Int of int | `String of string | `Unit ] -> int
  val float :
    [> `Float of float | `Int of int | `String of string | `Unit ] -> float
  val string :
    [> `Float of float | `Int of int | `String of string | `Unit ] -> string
  val fix : 'a -> 'b -> 'a
end


(** {2 {Subset of DOM elements} *)
module Dom_react : sig
  open Dyn_conv

  module Prim : sig
    module E : sig
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

    val createSelect :
      ?_type:Js.js_string Js.t ->
      ?name:Js.js_string Js.t ->
      Dom_html.document Js.t ->
      Dom_html.selectElement Js.t *
        (common_event_source * ([> `Int of int | `Vec2 of int * int ] -> 'a) -> 'a React.event)

    val createInput :
      ?_type:Js.js_string Js.t ->
      ?name:Js.js_string Js.t ->
      Dom_html.document Js.t ->
      Dom_html.inputElement Js.t *
        (common_event_source * ([> `Int of int | `Vec2 of int * int ] -> 'a) -> 'a React.event)

    val createTextarea :
      ?_type:Js.js_string Js.t ->
      ?name:Js.js_string Js.t ->
      Dom_html.document Js.t ->
      Dom_html.textAreaElement Js.t *
        (common_event_source * ([> `Int of int | `Vec2 of int * int ] -> 'a) -> 'a React.event)

    val createButton :
      ?_type:Js.js_string Js.t ->
      ?name:Js.js_string Js.t ->
      Dom_html.document Js.t ->
      Dom_html.buttonElement Js.t *
        (common_event_source * ([> `Int of int | `Vec2 of int * int  ] -> 'a) -> 'a React.event)

    val createDiv :
      Dom_html.document Js.t ->
      Dom_html.divElement Js.t *
        (common_event_source * ([> `Int of int | `Vec2 of int * int ] -> 'a) -> 'a React.event)

    val createImg :
      Dom_html.document Js.t ->
      Dom_html.imageElement Js.t *
        (common_event_source * ([> `Int of int | `Vec2 of int * int ] -> 'a) -> 'a React.event)
  end
  module S : sig
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

    (* type 'a update_func = ('a -> unit) -> unit *)
(*   val createSelect : *)
  (*     ?_type:Js.js_string Js.t -> *)
  (*     ?name:Js.js_string Js.t -> *)
  (*     Dom_html.document Js.t -> *)
  (*     Dom_html.selectElement Js.t * *)
  (*       (update_func * common_signal_source * ([> `Int of int | `Vec2 of int * int ] -> 'a option) -> 'a React.event) *)

  (*   val createInput : *)
  (*     ?_type:Js.js_string Js.t -> *)
  (*     ?name:Js.js_string Js.t -> *)
  (*     Dom_html.document Js.t -> *)
  (*     Dom_html.inputElement Js.t * *)
  (*       (update_func * common_signal_source * ([> `Int of int | `Vec2 of int * int ] -> 'a option) -> 'a React.event) *)

  (*   val createTextarea : *)
  (*     ?_type:Js.js_string Js.t -> *)
  (*     ?name:Js.js_string Js.t -> *)
  (*     Dom_html.document Js.t -> *)
  (*     Dom_html.textAreaElement Js.t * *)
  (*       (update_func * common_signal_source * ([> `Int of int | `Vec2 of int * int ] -> 'a option) -> 'a React.event) *)

  (*   val createButton : *)
  (*     ?_type:Js.js_string Js.t -> *)
  (*     ?name:Js.js_string Js.t -> *)
  (*     Dom_html.document Js.t -> *)
  (*     Dom_html.buttonElement Js.t * *)
  (*       (update_func * common_signal_source * ([> `Int of int | `Vec2 of int * int ] -> 'a option) -> 'a React.event) *)

  (*   val createDiv : *)
  (*     Dom_html.document Js.t -> *)
  (*     Dom_html.divElement Js.t * *)
  (*       (update_func * common_signal_source * ([> `Int of int | `Vec2 of int * int ] -> 'a option) -> 'a React.event) *)

  (*   val createImg : *)
  (*     Dom_html.document Js.t -> *)
  (*     Dom_html.imageElement Js.t * *)
  (*       (update_func * common_signal_source * ([> `Int of int | `Vec2 of int * int ] -> 'a option) -> 'a React.event) *)
  end
  end
  module E : sig
                 val createButton :
           ?_type:Js.js_string Js.t ->
           ?name:Js.js_string Js.t ->
           Dom_html.document Js.t ->
           Dom_html.buttonElement Js.t *
           ('a React.event * (Dom_html.buttonElement Js.t -> 'a -> 'b) ->
            'b React.event)
  end
end
