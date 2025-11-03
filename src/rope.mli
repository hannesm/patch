type t (** The type for a rope data structure *)

val length : t -> int
(** [length t] returns the amount of strings in [t]. *)

val empty : t
(** [empty] is the empty rope. *)

val to_strings : t -> string list
(** [to_strings t] returns a list of strings that is the contents of [t]. *)

val of_strings : string list -> bool -> t
(** [of_strings xs nl] is a rope [t] which contains the strings of [xs]. If
    [nl] is true, the last string will have a newline, otherwise not. *)

val of_string : string -> t
(** [of_string str] will split the string [str] on newline, and return a rope. *)

val to_string : t -> string
(** [to_string t] is the string where the contents of [t] is present. *)

val chop : t -> ?off:int -> int -> t
(** [chop t ~off len] returns a new rope that contains [len] strings starting
    at [off] of the provided rope [t]. Raises Invalid_argument if [len] and
    [off] are not inside the bounds. *)

val shift : t -> int -> t
(** [shift t len] returns a new rope that does not contain the first [len]
    strings, but only the remaining strings of [t]. *)

val concat : t -> t -> t
(** [concat t t'] returns a new rope which contains [t] followed by [t']. *)

val last_is_nl : t -> bool
(** [last_is_nl t] returns [true] if the last string should have a newline. *)
