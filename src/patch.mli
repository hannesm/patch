(** Patch - parsing and applying unified diffs in pure OCaml *)

type hunk = {
  mine_start : int ;
  mine_len : int ;
  mine : string list ;
  their_start : int ;
  their_len : int ;
  their : string list ;
}
(** A hunk contains some difference between two files: each with a start line
    and length, and then the content as lists of string. *)

type parse_error = {
  msg : string;
  lines : string list;
}

exception Parse_error of parse_error

val pp_hunk : mine_no_nl:bool -> their_no_nl:bool -> Format.formatter -> hunk -> unit
(** [pp_hunk ppf hunk] pretty-prints the [hunk] on [ppf], the printing is in the
    same format as [diff] does. *)

type operation =
  | Edit of string * string
  | Delete of string
  | Create of string
  | Rename_only of string * string
  (** The operation of a diff: in-place [Edit], [Delete], [Create], [Rename_only].
      The parameters to the variants are filenames.
      NOTE: in a typical git diff file, [Rename_only] does not have any prefix. *)

val pp_operation : Format.formatter -> operation -> unit
(** [pp_operation ppf op] pretty-prints the operation [op] on [ppf]. *)

val operation_eq : operation -> operation -> bool
(** [operation_eq a b] is true if [a] and [b] are equal. *)

type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}
(** The type of a diff: an operation, a list of hunks, and information whether
    a trailing newline exists on the left and right. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf t] pretty-prints [t] on [ppf]. *)

val pp_list : Format.formatter -> t list -> unit
(** [pp ppf diffs] pretty-prints [diffs] on [ppf]. *)

val parse : p:int -> string -> t list
(** [parse ~p data] decodes [data] as a list of diffs.

    @param p denotes the expected prefix level of the filenames.
    For more information, see the option [-p] in your POSIX-complient
    patch.

    @raise Parse_error if a filename was unable to be parsed *)

val patch : cleanly:bool -> string option -> t -> string option
(** [patch file_contents diff] applies [diff] on [file_contents], resulting in
    the new file contents (or None if deleted). *)

val diff : operation -> string option -> string option -> t option
(** [diff operation content_a content_b] creates a diff between
    [content_a] and [content_b]. Returns [None] if no changes could be detected.

    @raise Invalid_argument if both [content_a] and [content_b] are [None]. *)
