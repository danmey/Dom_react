module Html = Dom_html
module Dom = Dom
module Rd = React_dom
module R = Lwt_react

let js = Js.string
open Rd.S
open React_dom.S.Css
open React.S.Float
open React.S.Pair
open React

let onload ev =
  let time = time() in
  let mouse = mousef () in
  let wiggle = S.const 100. *. sin (time *. S.const 5.) +. S.const 100. in
  let waggle = S.const 100. *. cos (time *. S.const 5.) +. S.const 100. in
  let e = element ~position:mouse () in
  let f = element ~width:(S.const 400.) () in
  (e >> get `left >> (fun x -> (set f `left (Rd.delay 300. x))));

  (* element ~left:wiggle ~top:waggle (); *)
  Js._false
;;
Html.window##onload <- (Html.handler onload)
  
  
