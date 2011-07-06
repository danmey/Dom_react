let js = Js.string

open Dom_react
open React

let onload ev =
  let (>>=) = Js.Opt.bind in
  let return = Js.Opt.return in
  (Dom_html.document##getElementById (js"body") >>=
     (fun body ->
       
       (* We need to keep events in the list so we can perform action depending on it
          we've chosen an imperative list as it is the only way we can handle in for loop *)
       let event_lst = ref [] in

       (* Create a button with value, and caption under table cell *)
       let button parent name value =
         let w, a = Dom_react.Prim.E.createButton Dom_html.document in
         let ev = a (`Onclick, value) in
         w##innerHTML <- js name;
         Dom.appendChild parent (w :> Dom.node Js.t);
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
           let td = tr##insertCell (-1) in
           (* Last column reserved for operators *)
           if col != 3 then
             let index = row * 3 + col + 1 in
               button td (string_of_int index) (Dyn_conv.fix (`Value index))
           else
             if row < 2 then
               let op = [|"+";"-";"*";"/"|].(row) in
               button td op (Dyn_conv.fix (`Op op))
             (* ... last column apart from the last row which is 0 *)
             else button td "0" (Dyn_conv.fix (`Value 0))
         done
       done;
       
       (* Perform calculation accumulate it in tuple (really a stack
          with access to only two top elements)*)
       let ev =
         E.fold (fun (a,b) -> function
           | `Op op -> let op = match op with
               | "+" -> (+)
               | "-" -> (-) in
             (b, op a b)
           | `Value a -> (b, a)) (1,0) (E.select !event_lst)
       in
       E.map (Fun_prop.set_value result) (E.map (fun (a,b) -> string_of_int b) ev);
       return ()));
  Js._false
;;
Dom_html.window##onload <- (Dom_html.handler onload)
  
  
