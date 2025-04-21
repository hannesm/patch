module String : sig
  val is_prefix : prefix:string -> string -> bool
  val is_suffix : suffix:string -> string -> bool
  val cut : char -> string -> (string * string) option
  val slice : ?start:int -> ?stop:int -> string -> string
end

module List : sig
  val rev_cut : int -> 'a list -> 'a list * 'a list
end
