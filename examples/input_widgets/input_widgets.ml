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
      let w1,c1 = Input.Create.float 123. in
      let w2,c2 = Input.Create.int 42 in
      let w3,c3 = Input.Create.string "foo bar" in
      let p = Dom_html.createP Dom_html.document in
      let w4,m1 = Input.Map.float c1 in
      let w5,m1 = Input.Map.int c2 in
      let w6,m1 = Input.Map.string c3 in
      Dom.appendChild body w1;
      Dom.appendChild body w2;
      Dom.appendChild body w3;
      Dom.appendChild body p;
      Dom.appendChild body w4;
      Dom.appendChild body w5;
      Dom.appendChild body w6;
      return ()) in
  ()
;;

let () =
  let e = Base.E.create Dom_html.window Base.E.onload in
  ignore(Base.E.map onload e)
