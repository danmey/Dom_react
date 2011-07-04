let js = Js.string

open Dom_react
let onload ev =
  let (>>=) = Js.Opt.bind in
  let select, attacher = Dom_html_react.createSelect Dom_html.document in
  let ev = attacher (`Onchange, Event_type.int) in
  (Dom_html.document##getElementById (js"body") >>=
     (fun body ->
       Dom.appendChild body (select :> Dom.node Js.t);
       let opt1 = Dom_html.createOption Dom_html.document in
       let opt2 = Dom_html.createOption Dom_html.document in
       opt1 ## value <- js"ala ma kota";
       opt2 ## value <- js"a kot ma ale";
       select ##  add (opt1, Js.Opt.empty);
       select ##  add (opt2, Js.Opt.empty);
       Dom.appendChild select (opt1 :> Dom.node Js.t);
       Dom.appendChild select (opt1 :> Dom.node Js.t);
       React.E.map (fun i -> Dom_html.window ## alert (js (Printf.sprintf "change: %d" i))) ev;
       Js.Opt.return ()));
  Js._false
;;
Dom_html.window##onload <- (Dom_html.handler onload)
  
  
