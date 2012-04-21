open Dom_html

let mouse () =
  let signal, set = React.S.create (0,0) in
  let receive ev =
    let mx, my = eventAbsolutePosition ev in
    set (mx, my);
    Js._false
  in
  ignore(addEventListener (document :> eventTarget Js.t) Event.mousemove (handler receive) Js._false);
  signal


















