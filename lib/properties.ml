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

module Set = struct
  let selectedIndex w v = w ## selectedIndex <- v
  let value w v = w ## value <- Js.string v
  let length w v = w ## length <- v
  let options w v = w ## options <- v
  let disabled w v = w ## disabled <- v
  let multiple w v = w ## multiple <- v
  let name w v = w ## name <- v
  let size w v = w ## size <- v
  let tabIndex w v = w ## tabIndex <- v
  let innerHTML w v = w ## innerHTML <- Js.string v
  let checked w v = w ## checked <- Js.bool v

  let onclick w v = w ## onclick <- Dom_html.handler v
  let ondblclick w v = w ## ondblclick <- Dom_html.handler v
  let onmousedown w v = w ## onmousedown <- Dom_html.handler v
  let onmouseup w v = w ## onmouseup <- Dom_html.handler v
  let onmouseover w v = w ## onmouseover <- Dom_html.handler v
  let onmousemove w v = w ## onmousemove <- Dom_html.handler v
  let onmouseout w v = w ## onmouseout <- Dom_html.handler v
  let onkeypress w v = w ## onkeypress <- Dom_html.handler v
  let onkeydown w v = w ## onkeydown <- Dom_html.handler v
  let onkeyup w v = w ## onkeyup <- Dom_html.handler v
  let onload w v = w ## onload <- Dom_html.handler (fun ev -> v(); Js._false)
end

module Get = struct
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
  let checked w _ = Js.to_bool w ## checked
end
    (* Should be really wrapped with variants (mapping css_value -> string) *)
module Css = struct
  module Set = struct
    let color w v = w ## style ## color <- Js.string v
    let background w v = w ## style ## background <- Js.string v
    let backgroundColor w v = w ## style ## backgroundColor <- Js.string v
    let position w v = w ## style ## position <- Js.string v
    let left w v = w ## style ## left <- Js.string (string_of_int v)
    let top w v = w ## style ## top <- Js.string (string_of_int v)
    let width w v = w ## style ## width <- Js.string (string_of_int v)
    let height w v = w ## style ## height <- Js.string (string_of_int v)
    let left w = int_of_string (Js.to_string w ## style ## left)
  end
end

