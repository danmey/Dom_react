module Html = Dom_html
module Dom = Dom
module Rd = Dom_react
module R = Lwt_react

let js = Js.string
open Rd.S
open Dom_react.S.Css
open React.S.Float
open React.S.Pair
open React

let onload ev =
  let time = time() in
  let mouse = mousef () in
  let e = element ~position:mouse () in
  let f = element ~width:(S.const 400.) () in
  let wiggle = (sin time  +. S.const 1.0) *. S.const 0.5 in
  let ( <* ) = ($) in
  let css e s = e --> s in
  let l = e --> `left >> (`left <-- f <* Rd.delay 250.) in
  let l = css e `left >> (`left <-- f <* Rd.delay 215.) in
  let l = wiggle >> (`opacity <-- f) in
  Js._false
;;
Html.window##onload <- (Html.handler onload)
  
  
