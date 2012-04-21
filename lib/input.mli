module Create :
sig
  val button : string -> Dom_html.buttonElement Js.t * int React.signal
  val bool : string -> Dom_html.inputElement Js.t * bool React.signal
  val int : int -> Dom_html.inputElement Js.t * int React.signal
  val float : float -> Dom_html.inputElement Js.t * float React.signal
  val string : string -> Dom_html.inputElement Js.t * string React.signal
  end
module Attach :
sig
  val button : string -> Dom_html.buttonElement Js.t * int React.signal
  val bool : string -> Dom_html.inputElement Js.t * bool React.signal
  val int : int -> Dom_html.inputElement Js.t -> int React.signal
  val float : float -> Dom_html.inputElement Js.t -> float React.signal
  val string : string -> Dom_html.inputElement Js.t -> string React.signal
end
module Named :
sig
  val button : string -> Dom_html.buttonElement Js.t * int React.signal
  val bool : string -> Dom_html.inputElement Js.t * bool React.signal
  val int : int -> string -> Dom_html.inputElement Js.t * int React.signal
  val float : float -> string -> Dom_html.inputElement Js.t * float React.signal
  val string : string -> string -> Dom_html.inputElement Js.t * string React.signal
end
module Value :
sig
  val int : int React.signal -> Dom_html.inputElement Js.t * unit React.signal
  val float : float React.signal -> Dom_html.inputElement Js.t * unit React.signal
  val string : string React.signal -> Dom_html.inputElement Js.t * unit React.signal
  val bool : bool React.signal -> Dom_html.inputElement Js.t * unit React.signal
end









