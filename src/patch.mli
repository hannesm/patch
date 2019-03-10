type hunk = {
  mine_start : int ;
  mine_len : int ;
  mine : string list ;
  their_start : int ;
  their_len : int ;
  their : string list ;
}

val pp_hunk : Format.formatter -> hunk -> unit

type t = {
  mine_name : string ;
  their_name : string ;
  hunks : hunk list ;
  no_nl : bool ;
}

val pp : Format.formatter -> t -> unit

val to_diffs : string -> t list

val patch : string option -> t -> (string, [ `Msg of string ]) result
