(* Originally from https://github.com/robur-coop/utcp, written by
   Calascibetta Romain <romain.calascibetta@gmail.com> *)

(* A rope data structure where each node is a line *)

type t =
  | Str of string array * bool * int * int
  | App of t * t * int * int

let length = function
  | Str (_, _, len, _) -> len
  | App (_, _, len, _) -> len

let height = function
  | Str _ -> 0
  | App (_, _, _, h) -> h

(* keep compatibility with 4.08 *)
let max_int (a : int) (b : int) = max a b
let min_int (a : int) (b : int) = min a b
external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string" [@@noalloc]

let append t1 t2 =
  App (t1, t2, length t1 + length t2, 1 + max_int (height t1) (height t2))

let empty = Str (Array.make 0 "", true, 0, 0)

let rec unsafe_sub t start stop =
  if start == 0 && stop = length t then
    t
  else if start == stop then
    empty
  else match t with
    | Str (data, nl, len, off) ->
      assert (stop <= len);
      Str (data, nl, stop - start, off + start)
    | App (l, r, _, _) ->
        let len = length l in
        if stop <= len then unsafe_sub l start stop
        else if start >= len then unsafe_sub r (start - len) (stop - len)
        else append (unsafe_sub l start len) (unsafe_sub r 0 (stop - len))

let chop t ?(off = 0) len =
  if len < 0 || len > length t - off
  then invalid_arg "Rope.chop";
  if len == 0 then empty else unsafe_sub t off (off + len)

let shift t len =
  if len < 0 then t
  else if len == 0 then t
  else
    let max = length t in
    let len = min_int max len in
    let l = len + (max - len) in
    unsafe_sub t len l

let rec last_is_nl = function
  | Str (a, nl, len, off) -> if Array.length a - off = len then nl else true
  | App (_, r, _, _) -> last_is_nl r

let rec byte_length = function
  | Str (s, _, len, off) as a ->
    let sum = ref 0 in
    for idx = off to len + off - 1 do
      let data = Array.unsafe_get s idx in
      sum := !sum + String.length data + 1
    done;
    !sum - if last_is_nl a then 0 else 1
  | App (l, r, _, _) -> byte_length l + byte_length r

let rec into_bytes buf dst_off = function
  | Str (s, _, len, off) as a ->
    let off' = ref dst_off in
    for idx = off to len + off - 1 do
      let data = Array.unsafe_get s idx in
      unsafe_blit_string data 0 buf !off' (String.length data);
      off' := !off' + String.length data + 1;
      if idx - off < len - 1 || (idx - off = len - 1 && last_is_nl a) then
        Bytes.unsafe_set buf (!off' - 1) '\n'
    done
  | App (l, r, _, _) ->
    into_bytes buf dst_off l;
    into_bytes buf (dst_off + byte_length l) r

let to_strings t =
  let rec go acc = function
    | Str (s, _nl, len, off) ->
      let r = ref [] in
      for idx = off to len + off - 1 do
        let data = Array.unsafe_get s idx in
        r := data :: !r
      done;
      List.rev_append !r acc
    | App (l, r, _, _) -> go (go acc r) l in
  go [] t

let to_string t =
  let len = byte_length t in
  let buf = Bytes.create len in
  into_bytes buf 0 t;
  Bytes.unsafe_to_string buf

let concat a b = append a b
let prepend (str, nl) t = append (Str (Array.make 1 str, nl, 1, 0)) t

let append t (str, nl) = append t (Str (Array.make 1 str, nl, 1, 0))

let of_strings xs last_is_nl =
  let d = Array.of_list xs in
  Str (d, last_is_nl, Array.length d, 0)

let of_string str =
  let splitted = String.split_on_char '\n' str in
  let last_is_nl = String.unsafe_get str (String.length str - 1) = '\n' in
  let splitted = if last_is_nl then List.rev (List.tl (List.rev splitted)) else splitted in
  let d = Array.of_list splitted in
  Str (d, last_is_nl, Array.length d, 0)

let equal a b = String.equal (to_string a) (to_string b)
