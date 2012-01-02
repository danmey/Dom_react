(*----------------------------------------------------------------------------
   Copyright (c) 2009-2010, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
  ---------------------------------------------------------------------------*)

(* Adopted to Dom_react by Wojciech Meyer *)

(* Breakout clone. *)

open React;;

module Js_helper = struct
  let context =
    let open Js in
    let open Dom_html in
    let js = string in
    let get_opt el = 
      Opt.case el (fun () -> failwith "context") in
      lazy 
        (let canvas = document ## getElementById(js"canvas") in
         get_opt canvas 
           (fun canvas ->
             let canvas = ((Unsafe.coerce canvas) :> canvasElement Js.t) in
             canvas ## getContext (_2d_)))
      
  let with_context f = f (Lazy.force context)
end

module V2 : sig                                                  (* Vectors. *)
  type t
  val v : float -> float -> t
  val o : t
  val ex : t
  val ey : t 
  val x : t -> float
  val y : t -> float 
  val add : t -> t -> t
  val sub : t -> t -> t
  val neg : t -> t
  val smul : float -> t -> t
  val dot : t -> t -> float 
  val to_ints : t -> int * int
end = struct
  type t = { x : float; y : float }
  let v x y = { x = x; y = y }
  let o = v 0. 0. 
  let ex = v 1. 0. 
  let ey = v 0. 1.
  let x p = p.x 
  let y p = p.y 
  let add p p' = v (p.x +. p'.x) (p.y +. p'.y)
  let sub p p' = v (p.x -. p'.x) (p.y -. p'.y)
  let neg p = v (-. p.x) (-. p.y)
  let smul s p = v (s *. p.x) (s *. p.y)
  let dot p p' = p.x *. p'.x +. p.y *. p'.y
  let to_ints p = (truncate p.x, truncate p.y)
end

module Rect : sig                                             (* Rectangles. *)
  type t
  val create : V2.t -> V2.t -> t           (* lower left corner and extents. *)
  val empty : t
  val o : t -> V2.t
  val size : t -> V2.t
  val xmin : t -> float
  val xmax : t -> float
  val ymin : t -> float
  val ymax : t -> float 
end = struct          
  type t = V2.t * V2.t
  let create o size = o, size 
  let empty = V2.o, V2.o 
  let o (o, s) = o
  let size (_, s) = s
  let xmin (o, _) = V2.x o 
  let xmax (o, s) = V2.x o +. V2.x s
  let ymin (o, _) = V2.y o
  let ymax (o, s) = V2.y o +. V2.y s
end

module Draw : sig                        (* Draw with ANSI escape sequences. *)
  val frame : Rect.t
  val init : unit -> unit
  val clear : unit -> unit
  val flush : unit -> unit
  val text : ?center:bool -> ?color:int -> V2.t -> string -> unit
  val rect : ?color:int -> Rect.t -> unit
  val beep : unit -> unit
end = struct
  let r4 r =
    let o = Rect.o r in
    let s = Rect.size r in
    (V2.x o, V2.y o, V2.x s, V2.y s)

  let frame = Rect.create (V2.v 1. 1.) (V2.v 80. 24.)

  let clear () =
    Js_helper.with_context 
      (fun ctx -> ctx ## clearRect (0.,0., 1280., 800.))

  let flush () = ()
  let init () = ()
  
  let text ?(center = true) ?(color = 30) pos str =
    Js_helper.with_context (fun ctx ->
      let x,y = V2.x pos, V2.y pos in
      let x,y = x *. 7., y *. 15. in
      let str = Js.string str in
      ctx ## font <- Js.string"15px monospace";
      ctx ## fillText (str, x, y))
    
  let rect ?(color = 40) r =
    Js_helper.with_context (fun ctx ->
      let x,y,sx,sy = r4 r in
      let x,y,sx,sy = x *. 10., y *. 15., sx *. 10., sy *. 15. in
      ctx ## fillStyle <- Js.string"rgb(200,0,0)";
      ctx ## fillRect (x, y, sx, sy))

  let beep () = ()
end

module Input : sig                              (* Keyboard and time events. *)
  val init : unit -> unit
  val time : float event                                      (* time event. *)
  val key : char event                                    (* keyboard event. *)
  val gather : unit -> unit
end = struct
  let time = (React.S.changes (Dom_react.Base.OldS.time ()))
  let key, send_key = E.create ()
  let gather () = ()
  let init () =
    Dom_html.document ## onkeypress <- Dom_html.handler 
      (fun ev ->
        Js.Optdef.case (ev ## charCode) 
          (fun () -> ()) 
          (fun code ->
            send_key (char_of_int code));
        Js._false)

end

module Game : sig                              (* Game simulation and logic. *)
  type t
  val create : Rect.t -> float event -> [`Left | `Right ] event -> t
  val walls : t -> Rect.t
  val ball : t -> Rect.t signal
  val paddle : t -> Rect.t signal
  val bricks : t -> Rect.t list signal
  val brick_count : t -> int signal
  val collisions : t -> unit event
  val outcome : t -> [> `Game_over of int ] event
end = struct
  type t = { walls : Rect.t;
	     ball : Rect.t signal;
	     paddle : Rect.t signal;
	     bricks : Rect.t list signal;
	     brick_count : int signal;
	     collisions : unit event }

  (* Collisions *)

  let ctime c r d n = Some (n, (r -. c) /. d) 
  let cmin c r d n = if r <= c && d < 0. then ctime c r d n else None
  let cmax c r d n = if r >= c && d > 0. then ctime c r d n else None
  let cinter cmin cmax rmin rmax d n = match d with 
  | d when d < 0. ->
      if rmax -. d < cmin then None else                    (* moving apart. *)
      if rmin -. d >= cmax then 
	if rmin <= cmax then ctime cmax rmin d n else None
      else Some (V2.o, 0.)                         (* initially overlapping. *)
  | d when d > 0. ->
      if rmin -. d > cmax then None else                    (* moving apart. *)
      if rmax -. d <= cmin then 
	if rmax >= cmin then ctime cmin rmax d (V2.neg n) else None
      else Some (V2.o, 0.)                         (* initially overlapping. *)
  | _ (* d = 0. *) ->
    if cmax < rmin || rmax < cmin then None else Some (V2.o, 0.) 

  let crect c r d =                    (* r last moved by d relatively to c. *)
    let inter min max c r d n = cinter (min c) (max c) (min r) (max r) d n in  
    match inter Rect.xmin Rect.xmax c r (V2.x d) V2.ex with
    | None -> None 
    | Some (_, t as x) -> 
	match inter Rect.ymin Rect.ymax c r (V2.y d) V2.ey with
	| None -> None
	| Some (_, t' as y) -> 
	    let _, t as c = if t > t' then x else y in 
	    if t = 0. then None else Some c

  (* Game objects *)
	      
  let moving_rect pos size = S.map (fun pos -> Rect.create pos size) pos

  let ball walls dt collisions = 
    let size = V2.v 2. 1. in
    let x0 = 0.5 *. (Rect.xmax walls -. V2.x size) in
    let p0 = V2.v x0 (0.5 *. Rect.ymax walls) in
    let v0 =
      let sign = if Random.bool () then -1. else 1. in
      let angle = (sign *. (10. +. Random.float 60.) *. 3.14) /. 180. in
      let speed = 18. +. Random.float 2. in
      V2.v (speed *. sin angle) (speed *. cos angle)
    in
    let v = 
      let bounce (n, _) v = V2.sub v (V2.smul (2. *. V2.dot n v) n) in
      S.accum (E.map bounce collisions) v0 
    in
    let dp = S.sample (fun dt v -> V2.smul dt v) dt v in
    let p = 
      let pos p0 = S.fold V2.add p0 dp in
      let adjust (_, pc) = pos pc in                 (* visually sufficient. *)
      S.switch (pos p0) (E.map adjust collisions)
    in
    moving_rect p size, dp
 
  let walls walls (ball, dp) = 
    let left = Rect.xmin walls in 
    let right = Rect.xmax walls in
    let top = Rect.ymin walls in
    let collisions = 
      let collide dp ball =
	let c = match cmin left (Rect.xmin ball) (V2.x dp) V2.ex with
	| Some _ as c -> c
	| None -> 
	    match cmax right (Rect.xmax ball) (V2.x dp) (V2.neg V2.ex) with
	    | Some _ as c -> c
	    | None -> cmin top (Rect.ymin ball) (V2.y dp) V2.ey
	in
	match c with 
	| None -> None 
	| Some (n, t) -> Some (n, V2.sub (Rect.o ball) (V2.smul t dp))
      in
      E.fmap (fun x -> x) (S.sample collide dp ball) 
    in
    walls, collisions

  let paddle walls moves (ball, dp) =
    let speed = 4. in 
    let size = V2.v 9. 1. in
    let xmin = Rect.xmin walls in 
    let xmax = Rect.xmax walls -. (V2.x size) in 
    let p0 = V2.v (0.5 *. xmax) (Rect.ymax walls -. 2.) in
    let control p = function 
      | `Left -> 
	  let x' = V2.x p -. speed in
	  if x' < xmin then V2.v xmin (V2.y p) else V2.v x' (V2.y p)
      | `Right -> 
	  let x' = V2.x p +. speed in 
	  if x' > xmax then V2.v xmax (V2.y p) else V2.v x' (V2.y p)
    in
    let paddle = moving_rect (S.fold control p0 moves) size in 
    let collisions = 
      let collide dp (ball, paddle) = match crect paddle ball dp with
      | None -> None
      | Some (n, t) -> Some (n, V2.sub (Rect.o ball) (V2.smul t dp))
      in
      E.fmap (fun x -> x) (S.sample collide dp (S.Pair.pair ball paddle))
    in
    paddle, collisions
      
  let bricks walls (ball, dp) =
    let bricks0 = 
      let size = Rect.size walls in
      let w = V2.x size in 
      let h = (V2.y size) /. 4. in                    (* use 1/4 for bricks. *)
      let bw, bh = (w /. 8.), h /. 3. in
      let x_count = truncate (w /. bw) in
      let y_count = truncate (h /. bh) in
      let acc = ref [] in
      for x = 0 to x_count - 1 do 
	for y = 0 to y_count - 1 do
          let x = Rect.xmin walls +. (float x) *. bw in
          let y = Rect.ymin walls +. 2. *. bh +. (float y) *. bh in
          acc := Rect.create (V2.v x y) (V2.v bw bh) :: !acc
	done
      done;
      !acc
    in
    let define bricks =
      let cresult = 
	let collide dp (ball, bricks) = 
	  let rec aux c acc bricks ball dp = match bricks with
	  | [] -> c, List.rev acc 
	  | b :: bricks' -> match crect b ball dp with
	    | None -> aux c (b :: acc) bricks' ball dp
	    | c  -> aux c acc bricks' ball dp
	  in
	  match aux None [] bricks ball dp with
	  | None, bl -> None, bl
	  | Some (n, t), bl -> Some (n, V2.sub (Rect.o ball) (V2.smul t dp)),bl
	in
	S.sample collide dp (S.Pair.pair ball bricks) 
      in
      let collisions = E.fmap (fun (c, _) -> c) cresult in
      let bricks_e = E.map (fun (_, bl) -> fun _ -> bl) cresult in
      let bricks' = S.accum bricks_e bricks0 in
      bricks', (bricks', collisions)
    in
    S.fix bricks0 define
 
  (* Game data structure, links game objects *)     
 
  let create w dt moves = 
    let define collisions = 
      let ball = ball w dt collisions in
      let walls, wcollisions = walls w ball in 
      let paddle, pcollisions = paddle w moves ball in 
      let bricks, bcollisions = bricks w ball in
      let collisions' = E.select [pcollisions; wcollisions; bcollisions] in
      let g = 
	{ walls = walls; 
	  ball = S.dismiss collisions' Rect.empty (fst ball);
	  paddle = paddle; 
	  bricks = bricks; 
	  brick_count = S.map List.length bricks;
	  collisions = E.stamp collisions' () }
      in
      collisions', g
    in
    E.fix define
      
  let walls g = g.walls 
  let ball g = g.ball
  let paddle g = g.paddle
  let bricks g = g.bricks
  let brick_count g = g.brick_count
  let collisions g = g.collisions
  let outcome g =                                     (* game outcome logic. *)
    let no_bricks = S.map (fun l -> l = 0) g.brick_count in 
    let miss = S.map (fun b -> Rect.ymax b >= Rect.ymax g.walls) g.ball in
    let game_over = S.changes (S.Bool.( || ) no_bricks miss) in
    S.sample (fun _ l -> `Game_over l) game_over g.brick_count
end

module Render = struct                          
  let str = Printf.sprintf 
  let str_bricks count = if count = 1 then "1 brick" else str "%d bricks" count

  let intro title_color =                        (* draws the splash screen. *)
    let x = 0.5 *. Rect.xmax Draw.frame in
    let y = 0.5 *. Rect.ymax Draw.frame in 
    Draw.clear ();
    Draw.text ~color:title_color (V2.v x (y -. 2.)) "BREAKOUT";
    Draw.text ~color:30 (V2.v x y) 
      "Hit 'a' and 'd' to move the paddle, 'q' to quit";
    Draw.text ~color:31 (V2.v x (y +. 2.)) "Hit spacebar to start the game";
    Draw.flush ()

  let game_init m =                              (* draws game init message. *)
    let x = 0.5 *. Rect.xmax Draw.frame in 
    let y = 0.5 *. Rect.ymax Draw.frame in 
    Draw.text ~color:31 (V2.v x (y +. 2.)) m; 
    Draw.flush ()

  let game ball paddle bricks bcount =              (* draws the game state. *)
    let bl = V2.v (Rect.xmin Draw.frame) (Rect.ymax Draw.frame -. 1.) in
    Draw.clear ();
    List.iter (Draw.rect ~color:40) bricks;
    Draw.rect ~color:44 paddle;
    Draw.rect ~color:41 ball;
    Draw.text ~center:false ~color:30 bl (str "%s left" (str_bricks bcount));
    Draw.flush ()

  let game_over outcome =                     (* draws the game over screen. *)
    let x = 0.5 *. Rect.xmax Draw.frame in 
    let y = 0.5 *. Rect.ymax Draw.frame in
    let outcome_msg = 
      if outcome = 0 then "Congratulations, no bricks left" else
      str "%s left, you can do better" (str_bricks outcome)
    in
    Draw.text ~color:34 (V2.v x (y +. 2.)) "GAME OVER";
    Draw.text ~color:30 (V2.v x (y +. 4.)) outcome_msg;
    Draw.text ~color:31 (V2.v x (y +. 6.)) "Hit spacebar to start again";
    Draw.flush ()
end

module Ui : sig
  val init : unit -> unit event
end = struct
  let key k = E.fmap (fun c -> if c = k then Some () else None) Input.key
  let quit () = E.once (E.stamp (key 'q') `Quit)
  let new_game () = E.once (E.stamp (key ' ') `Game)

  let wait_until ?stop e = match stop with
  | Some s -> E.map (fun v -> s (); v) (E.once e)
  | None -> E.once e
      
  let intro () =
    let color_swap = E.stamp Input.time (fun c -> if c = 31 then 34 else 31) in
    let output = S.l1 Render.intro (S.accum color_swap 34) in
    let stop () = S.stop output in
    wait_until (E.select [quit (); new_game ()]) ~stop

  let game () =
    let run = S.hold false (E.once (E.stamp (key ' ') true)) in
    let moves =
      let move = function 'a' -> Some `Left | 'd' -> Some `Right | _ -> None in
      E.when_ run (E.fmap move Input.key)
    in
    let dt = E.when_ run (E.diff ( -. ) Input.time) in 
    let g = Game.create Draw.frame dt moves in
    let outcome = Game.outcome g in 
    let sound = E.map Draw.beep (Game.collisions g) in
    let output = S.l4 Render.game (Game.ball g) (Game.paddle g) (Game.bricks g)
	(Game.brick_count g)
    in
    let stop () = E.stop sound; S.stop output in
    Render.game_init "Hit spacebar to start the game";
    wait_until (E.select [quit (); outcome]) ~stop

  let game_over outcome = 
    Render.game_over outcome; 
    wait_until (E.select [quit (); new_game ()])

  let init () = 
    let define ui = 
      let display ui = 
	match ui with
	| `Intro -> intro ()
	| `Game -> game ()
	| `Game_over outcome -> game_over outcome
	| `Quit -> game ()
      in
      let ui' = E.switch (display `Intro) (E.map display ui) in
      ui', ui'
    in
    E.stamp (E.fix define) ()                   
end

let main () = 
  Random.self_init ();
  Draw.init ();
  Input.init ();
  let ui = Ui.init () in                                  
  Input.gather ();
  ui

let onload ev =
  let ui = main () in
  Js._false
;;

Dom_html.window##onload <- (Dom_html.handler onload)

(*----------------------------------------------------------------------------
  Copyright (c) 2009-2010, Daniel C. Bünzli
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
        
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.

  3. Neither the name of the Daniel C. Bünzli nor the names of
     contributors may be used to endorse or promote products derived
     from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
  
  
