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
      let input1,_ = Input.Create.float 123. in
      let input2,i = Input.Create.int 42 in
      let input3,_ = Input.Create.string "foo bar" in
      let input4,_ = Input.Map.int i in
      Dom.appendChild body input1;
      Dom.appendChild body input2;
      Dom.appendChild body input3;
      Dom.appendChild body input4;
      return ()) in
  ()
;;

let () =
  let e = Base.E.create Dom_html.window Base.E.onload in
  ignore(Base.E.map onload e)
