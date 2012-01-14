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

let delay s d =
  let accuracy = 10. in
  let time = ref 0. in
  let f ms =
    let pending = Queue.create () in
    let news, send = React.S.create None in
    let send _ =
      let ms = React.S.value ms in
      time := !time +. 10.;
      let rec loop () =
        try
          (match Queue.peek pending with
              (t, s) ->
                if t < !time -. ms then
                  begin
                    let (_,s) = Queue.pop pending in
                    send (Some s);
                    loop ()
                  end)
        with Queue.Empty -> ()
      in
      loop ()
    in
    let rec timeout_id = ref (lazy (Dom_html.window ## setTimeout (Js.wrap_callback (loop send), accuracy)))
    and loop f ev =
      f ();
      timeout_id := (lazy Dom_html.window ## setTimeout (Js.wrap_callback (loop send), accuracy));
      ignore (Lazy.force !timeout_id);
    in
    ignore (Lazy.force !timeout_id);
    React.S.l1 (fun x -> Queue.add (!time, x) pending) s;
    React.S.fmap (fun x -> x) (React.S.value s) news in
  f d
