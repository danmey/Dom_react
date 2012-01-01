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


open Dom_react.Base

let js = Js.string

let onload () =
  let (>>=) = Js.Opt.bind in
  let return = Js.Opt.return in
  let _ = Dom_html.document##getElementById (js"body") >>=
     (fun body ->
       
       (* We need to keep events in the list so we can perform action depending on it
          we've chosen an imperative list as it is the only way we can handle in for loop *)
       let event_lst = ref [] in

       (* Create a button with value, and caption under table cell *)
       let button parent name value =
         let element = Dom_html.createButton Dom_html.document in
         Dom.appendChild parent (element :> Dom.node Js.t);
         element ## innerHTML <- js name;

         let ev = E.create element E.onclick in
         let ev = E.stamp ev value in

         event_lst := ev :: !event_lst;
         ev
       in

       (* Our result widget *)
       let result = Dom_html.createInput ~_type:(Js.string "text") Dom_html.document in
       Dom.appendChild body (result :> Dom.node Js.t);

       (* Matrix of buttons  *)
       let table = Dom_html.createTable Dom_html.document in
       Dom.appendChild body (table :> Dom.node Js.t);

       (* Create all the matrix *)
       for row = 0 to 3-1 do
         let tr = table ## insertRow (-1) in
         for col = 0 to 4-1 do
           let td = tr ## insertCell (-1) in
           (* Last column reserved for operators *)
           if col != 3 then
             let index = row * 3 + col + 1 in
               button td (string_of_int index) (`Value index)
           else
             if row < 2 then
               let op = [|"+", (+);"-", (-);"*", ( * ); "/", (/)|].(row) in
               button td (fst op) (`Op (snd op))
             (* ... last column apart from the last row which is 0 *)
             else button td "0" (`Value 0)
         done
       done;
  
       (* Perform calculation accumulate it in tuple (really a stack
          with access to only two top elements)*)
       let eval (a,b) = function
         | `Op op -> (b, op a b)
         | `Value a -> (b, a) in

       let ev =
         E.fold eval (0,0) (E.select !event_lst)
       in
       return (E.map
         (Fun_prop.set_value result) 
         (E.map (fun (a,b) -> string_of_int b) ev))) in
  ()
;;

let () =
  let e = E.create Dom_html.window E.onload in
  ignore(E.map onload e)
