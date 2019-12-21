type hunk = {
  mine_start : int ;
  mine_len : int ;
  mine : string list ;
  their_start : int ;
  their_len : int ;
  their : string list ;
}

val pp_hunk : Format.formatter -> hunk -> unit

type operation =
  | Edit of string
  | Rename of string * string
  | Delete of string
  | Create of string
  | Rename_only of string * string

val pp_operation : git:bool -> Format.formatter -> operation -> unit

val operation_eq : operation -> operation -> bool

type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

val pp : git:bool -> Format.formatter -> t -> unit

val to_diffs : string -> t list

val patch : string option -> t -> string option
