module Html = Dom_html
module Dom = Dom
module Rd = React_dom
module R = Lwt_react

let js = Js.string
open Rd.S
open React_dom.S.Css
open React.S.Float
open React

let onload ev =
  let time = time() in
  let mouse = mousef () in
  let wiggle = S.const 100. *. sin (time *. S.const 5.) +. S.const 100. in
  let waggle = S.const 100. *. cos (time *. S.const 5.) +. S.const 100. in
  let e = element ~left:wiggle ~top:(S.const 0.) () in
  element ~position:mouse ();
  (element ~position:(Rd.delay 100. mouse) ()
  >> get `left) >> set e `top;

  (* element ~left:wiggle ~top:waggle (); *)
  Js._false
;;
Html.window##onload <- (Html.handler onload)
  
  
