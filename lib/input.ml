module S = Base.S
module E = Base.E
module H = Dom_html

(* Due to limitation of type inference we need to wrap conversion
   functions to a first class module *)
module type CONVERSION = sig
  type t
  type widget
  val default : string
  val set_string : widget -> string -> unit
  val set : widget -> t -> unit
  val get : widget -> t
  val string_of : t -> string
  val of_string : string -> t
  val creator : widget -> int React.event
end

exception Wrong_format
  
module ValueControl(T : sig 
  type t
  type widget
  val default : string
  val string_of : t -> string
  val of_string : string -> t
  end) = struct
  include T
  let set_string w str = w ## value <- Js.string str
  let set w v = set_string w (T.string_of v)
  let get w = T.of_string (Js.to_string (w ## value))
  let creator w = E.create w E.onchar
end

module IntConversion = struct
  module C = struct
    type t = int
    let default = "0"
    let string_of = string_of_int
    let of_string = int_of_string
  end
  include ValueControl(struct type widget = H.inputElement Js.t include C end)
end

module FloatConversion = struct
  module C = struct
    type t = float
    let default = "0."

    let string_of f =
      let s = string_of_float f in 
      if s = "nan" then 
        raise Wrong_format 
      else s

    let of_string s = 
      try float_of_string s 
      with _ -> float (int_of_string s)
  end
  include ValueControl(struct type widget = H.inputElement Js.t include C end)
end
  
module StringConversion = struct
  module C = struct
    type t = string
    let default = ""
    let string_of s = s
    let of_string s = s
  end
  include ValueControl(struct type widget = H.inputElement Js.t include C end)
end

module BoolConversion = struct
  type t = bool
  type widget = H.inputElement Js.t
  let default = "off"
  let string_of s = if s then "on" else "off"
  let of_string = function
    | "on" -> true
    | "off" -> false
    | _ -> raise Wrong_format
  let set_string w str = w ## value <- Js.string str
  let set w v = set_string w (string_of v)
  let get w = of_string (Js.to_string (w ## value))
end

let text (type t) conversion value widget =
  let module C = (val conversion : CONVERSION 
                  with 
                    type t = t 
                  and type widget = H.inputElement Js.t)in
  let e = C.creator widget in
  C.set widget value;
  let validate = E.map
    (fun char_code ->
      let input = C.get widget in
      let input = C.string_of input in
      try
        let selectionStart = widget ## selectionStart in
        let handle_del incr =
          let left_i, right_i = incr selectionStart in
          let input, selectionStart =
            let left,right =
              String.sub input 0 left_i,
              String.sub input right_i 
                (String.length input - right_i) 
            in
            let input = left ^ right in
            input, selectionStart
          in
          C.set_string widget input;
          widget ## selectionStart <- left_i;
          widget ## selectionEnd <- left_i;
          input
        in
        match char_code with
          | 46 -> handle_del (fun n -> n,n+1)
          | 8 -> handle_del (fun n -> n - 1, n)
          | char_code ->
            let left,right =
              if input = "" then
                "","" else
                String.sub input 0 selectionStart, 
                String.sub input selectionStart 
                  (String.length input - selectionStart) 
            in
            let input' = Printf.sprintf "%s%c%s" left (Char.chr char_code) right in
            let input' = C.string_of (C.of_string input') in
            let input'' = C.string_of (C.of_string input') in
            if input' = input'' then begin
              C.set_string widget input';
              widget ## selectionStart <- selectionStart+1;
              widget ## selectionEnd <- selectionStart+1;
              input'
            end
            else input
      with _ -> input
    ) e 
  in
  S.hold 
    (C.of_string C.default)
    (E.map
       (fun value -> 
         try 
           C.of_string value 
         with _ -> C.of_string C.default) validate)

module CreateText = struct

  let int f v = f "text"
    (module IntConversion : CONVERSION 
      with type t = int 
      and type widget = H.inputElement Js.t) v

  let float f v = f "text"
    (module FloatConversion : CONVERSION 
      with type t = float 
      and type widget = H.inputElement Js.t) v

  let string f v = f "text"
    (module StringConversion : CONVERSION 
      with type t = string 
      and type widget = H.inputElement Js.t) v

  let button name =
    let w = Dom_html.createButton Dom_html.document in
    let e = S.create w S.onclick in
    w ## innerHTML <- Js.string name;
    w, e

  let bool name =
    let w = H.createInput ~_type:(Js.string "checkbox") Dom_html.document in
    let on_click = S.create w S.onclick 0 in
    w, S.map (Properties.Get.checked w) on_click

end

module Create = struct
  include CreateText
  let text _type conv value  =
    let widget = H.createInput ~_type:(Js.string _type) Dom_html.document in
    widget, text conv value widget 

  let int = int text
  let float = float text
  let string = string text
end

module Attach = struct
  include CreateText
  let text _type conv value widget =
    text conv value widget 

  let int = int text
  let float = float text
  let string = string text
end

module Named = struct
  include CreateText

  let failure = (fun () -> Dom_html.window ## alert (Js.string "text"); failwith "text")

  let text _type conv value name =
    Js.Opt.case (Dom_html.document##getElementById (Js.string name)) failure 
      (fun widget ->
        Js.Opt.case (Dom_html.CoerceTo.input widget) failure 
          (fun widget ->
            widget, text conv value widget))

  let int = int text
  let float = float text
  let string = string text

end

module Value = struct
  let custom (type t) type_ conversion s =
    let module C = (val conversion : CONVERSION with type t = t) in
    let w = H.createInput ~_type:(Js.string type_) Dom_html.document in
    w, S.map (fun n -> (Properties.Set.value w (C.string_of n))) s 

  let int = custom "text" 
    (module IntConversion : CONVERSION 
      with type t = int)

  let float = custom "text" 
    (module FloatConversion : CONVERSION 
      with type t = float)

  let string = custom "text" 
    (module StringConversion : CONVERSION 
      with type t = string)

  let bool s =
    let w = H.createInput ~_type:(Js.string "checkbox") Dom_html.document in
    w, S.map (Properties.Set.checked w) s
end
