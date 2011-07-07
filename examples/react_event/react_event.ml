let js = Js.string

open Dom_react
open React
let onload ev =
  let (>>=) = Js.Opt.bind in
  (Dom_html.document##getElementById (js"body") >>=
     (fun body ->
       let button name =
         let b, attacher = Dom_react.Prim.E.createButton Dom_html.document in
         let ev = attacher Dom_react.Prim.E.onclick in
         b##innerHTML <- js name;
         Dom.appendChild body (b :> Dom.node Js.t);
         E.map string_of_int ev in
       let react_button e name =
         let w, a = Dom_react.E.createButton Dom_html.document in
         let ev = a (e, Fun_prop.set_innerHTML) in
         w##innerHTML <- js name;
         Dom.appendChild body (w :> Dom.node Js.t);
         ev in
 
       let left = button "<" in
       let right = button ">" in
       let result = react_button left "0" in
       React.E.map (fun i -> Dom_html.window ## alert (js (Printf.sprintf "right clicked: %s" i))) right;
       Js.Opt.return ()));

  (* (Dom_html.document##getElementById (js"body") >>= *)
  (*    (fun body -> *)
  (*      Dom.appendChild body (select :> Dom.node Js.t); *)
  (*      let opt1 = Dom_html.createOption Dom_html.document in *)
  (*      let opt2 = Dom_html.createOption Dom_html.document in *)
  (*      opt1 ## value <- js"ala ma kota"; *)
  (*      opt2 ## value <- js"a kot ma ale"; *)
  (*      select ##  add (opt1, Js.Opt.empty); *)
  (*      select ##  add (opt2, Js.Opt.empty); *)
  (*      Dom.appendChild select (opt1 :> Dom.node Js.t); *)
  (*      Dom.appendChild select (opt1 :> Dom.node Js.t); *)
  (*      React.E.map (fun i -> Dom_html.window ## alert (js (Printf.sprintf "change: %d" i))) ev; *)
  (*      Js.Opt.return ())); *)
  Js._false
;;
Dom_html.window##onload <- (Dom_html.handler onload)
  
  
