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


open Dom_react
open React
module Dom_react = Dom_react.Prim.E

let js = Js.string

let onload () =
  let (>>=) = Js.Opt.bind in
  let return = Js.Opt.return in
  let _ = Dom_html.document##getElementById (js"body") >>=
    (fun body ->
      let mouse = Dom_react.create Dom_html.document Dom_react.onmousemove in
      let mouse = S.hold (0,0) mouse in
      let div parent name =
        let element , create_event = Dom_react.createDiv Dom_html.document in
        Dom.appendChild parent (element :> Dom.node Js.t);
        element ## innerHTML <- js name;
        element ## style ## position <- js "absolute";
        let md, mu = create_event Dom_react.onmousedown, create_event Dom_react.onmouseup in
        let md, mu = E.stamp md true, E.stamp mu false in
        let ev = E.select [md; mu] in
        let s = S.hold false ev in
        let col = function
          | true -> "#ff0000"
          | false -> "#ffff00" in
        ignore (S.map (Fun_prop.Css.set_backgorund element) (S.map col s));
        ignore (S.map (fun (x,y) -> Fun_prop.Css.set_left element x; Fun_prop.Css.set_top element y) mouse)
      in
      return (div body "ala ma kota"))
  in
  ()

let e =
  let e = Dom_react.create Dom_html.window Dom_react.onload in
  E.map onload e
