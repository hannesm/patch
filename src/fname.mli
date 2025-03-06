val parse : string -> (string option, string) result
(** [parse s] parses [s] and returns a filename or [None] if the filename
    is equivalent to [/dev/null].

    Returns [Error msg] in case of error. *)

val parse_git_header : string -> string option
