open Dom_react.Input

let js = Js.string

let onload ev =
  let (>>=) = Js.Opt.bind in
  (Dom_html.document##getElementById (js"body") >>=
     (fun body ->
       let button name =
         let element = Dom_html.createButton Dom_html.document in
         let ev = E.create element E.onclick in
         element ## innerHTML <- js name;
         Dom.appendChild body (element :> Dom.node Js.t);
         E.map string_of_int ev in
 
       let left = button "<" in
       let right = button ">" in
       E.map (fun i -> Dom_html.window ## alert (js (Printf.sprintf "right clicked: %s" i))) right;
       Js.Opt.return ()));
  Js._false
;;
Dom_html.window##onload <- (Dom_html.handler onload)
  
  
