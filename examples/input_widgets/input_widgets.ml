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


let js = Js.string
let element id = Js.Opt.get 
  (Dom_html.document##getElementById (Js.string id))
  (fun () -> failwith "no element id") 

let onload () =
  let body = element "body" in
  let w1,c1 = Input.Create.float 123. in
  let w2,c2 = Input.Create.int 42 in
  let w3,c3 = Input.Create.string "foo bar" in
  let w4,c4 = Input.Create.bool true in

  let c1 = S.Float.(c1 /. S.const 1.3) in
  let c2 = S.Int.(c2 * S.const 2) in
  let c3 = S.map String.uppercase c3 in
  let c4 = S.map not c4 in
  
  let w5,_ = Input.Value.float c1 in
  let w6,_ = Input.Value.int c2 in
  let w7,_ = Input.Value.string c3 in
  let w8,_ = Input.Value.bool c4 in

  let p = Dom_html.createP Dom_html.document in
  let add lst = List.iter (Dom.appendChild body) lst in

  add [w1;w2;w3;w4];
  add [p];
  add [w5;w6;w7;w8]
;;

let () =
  let e = Base.E.create Dom_html.window Base.E.onload in
  ignore(Base.E.map onload e)
