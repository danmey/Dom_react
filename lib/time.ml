let updater ms f =
  let ms' = float ms in
  let rec timeout_id = ref (lazy (Dom_html.window ## setTimeout (Js.wrap_callback loop, ms')))
  and loop ev =
    Dom_html.window ## clearTimeout (Lazy.force !timeout_id);
    let () = f () in
    timeout_id := (lazy Dom_html.window ## setTimeout (Js.wrap_callback loop, ms'));
    ignore (Lazy.force !timeout_id);
  in
  ignore (Lazy.force !timeout_id)

let create w prop updater =
  let signal, send = React.S.create (prop w) in
  updater (fun () -> send (prop w));
  signal

let time ?(accuracy=0.01) ?(start=0.) () =
  let time = ref start in
  let signal, send = React.S.create !time in
  updater (int_of_float (accuracy *. 1000.)) (fun () ->
    time := !time +. accuracy;
    send !time);
  signal

