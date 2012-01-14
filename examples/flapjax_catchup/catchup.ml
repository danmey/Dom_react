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

let js = Js.string

let onload () =
  let (>>=) = Js.Opt.bind in
  let return = Js.Opt.return in
  let _ = Dom_html.document##getElementById (js"body") >>=
    (fun body ->
      let open Input in
      let _, time = Named.int 50 "frameRate" in
      let _, delay = Named.int 5000 "delayRate" in
      let delay = React.S.map (fun i -> float i) delay in
      let mouse = Mouse.mouse () in
      let delay_mouse = Time.delay mouse delay in
      let div_el, div = Element.div Base.E.onmouseover in
      div_el ## setAttribute (js"id", js "themouse");
      div_el ## style ## color <- js "#FFFFFF";
      div_el ## style ## backgroundColor <- js "#ff0000";
      div_el ## style ## position <- js "absolute";
      div_el ## style ## padding <- js "10px";
      div_el ## style ## left <- js "0";
      div_el ## style ## top <- js "0";
      let set_pos (x,y) =
        Properties.Css.Set.left div_el x;
        Properties.Css.Set.top div_el (y+50) in
      React.S.map set_pos delay_mouse;
      Dom.appendChild body (div_el :> Dom.node Js.t);
      return ()) in
  ()
;;

let () =
  let e = Base.E.create Dom_html.window Base.E.onload in
  ignore(Base.E.map onload e)
