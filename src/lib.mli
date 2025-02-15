module String : sig
  val is_prefix : prefix:string -> string -> bool
  val is_suffix : suffix:string -> string -> bool
  val cut : char -> string -> (string * string) option
  val cuts : char -> string -> string list
  val slice : ?start:int -> ?stop:int -> string -> string
  val trim : string -> string
  val get : string -> int -> char
  val concat : string -> string list -> string
  val length : string -> int
  val equal : string -> string -> bool
end
