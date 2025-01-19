module String = Patch_lib.String

type lexer_output =
  | Quoted of (string * string)
  | Unquoted
  | Error of string

exception Cant_parse_octal

let ascii_zero = 48 (* Char.code '0' *)
let octal_to_char c1 c2 c3 =
  let char_to_digit c = Char.code c - ascii_zero in
  try
    Char.chr (
      (char_to_digit c1 lsl 6) lor
      (char_to_digit c2 lsl 3) lor
      char_to_digit c3
    )
  with Invalid_argument _ -> raise Cant_parse_octal

let lex_quoted_char s len i =
  match s.[i] with
  | 'a' -> Some ('\007', 2)
  | 'b' -> Some ('\b', 2)
  | 'f' -> Some ('\012', 2)
  | 'n' -> Some ('\n', 2)
  | 'r' -> Some ('\r', 2)
  | 't' -> Some ('\t', 2)
  | 'v' -> Some ('\011', 2)
  | '\\' -> Some ('\\', 2)
  | '"' -> Some ('"', 2)
  | '0'..'3' as c1 when len >= 3 ->
      begin match s.[i + 1], s.[i + 2] with
      | ('0'..'7' as c2), ('0'..'7' as c3) ->
          (try Some (octal_to_char c1 c2 c3, 4)
           with Cant_parse_octal -> None)
      | _, _ -> None
      end
  | _ -> None

let rec lex_quoted_filename buf s len i =
  if len > 0 then
    match s.[i] with
    | '"' -> Quoted (Buffer.contents buf, String.slice ~start:(i + 1) s)
    | '\\' when len > 2 ->
        let char_size =
          match lex_quoted_char s (len - 1) (i + 1) with
          | Some (c, char_size) -> Buffer.add_char buf c; char_size
          | None -> Buffer.add_char buf s.[i]; 1
        in
        lex_quoted_filename buf s (len - char_size) (i + char_size)
    | c ->
        Buffer.add_char buf c;
        lex_quoted_filename buf s (len - 1) (i + 1)
  else
    Unquoted

let lex_filename buf s len =
  if len > 0 then
    match s.[0] with
    | '"' -> lex_quoted_filename buf s (len - 1) 1
    | _ -> Unquoted
  else
    Error "empty filename"

let parse_filename ~allow_space s =
  match lex_filename (Buffer.create 128) s (String.length s) with
  | Quoted x -> Ok x
  | Unquoted when not allow_space ->
      begin match String.cut ' ' s with
      | None -> Ok (s, "")
      | Some x -> Ok x
      end
  | Unquoted -> Ok (s, "")
  | Error msg -> Error msg

let parse s =
  let filename_and_date =
    match String.cut '\t' s with
    | None ->
        parse_filename ~allow_space:false s
    | Some (filename, date) ->
        match parse_filename ~allow_space:true filename with
        | Ok (filename, "") -> Ok (filename, date)
        | Ok _ -> Error "Unexpected character after closing double-quote"
        | Error _ as err -> err
  in
  match filename_and_date with
  | Ok (filename, date) ->
      if filename = "/dev/null" ||
         let date = String.trim date in
         String.is_prefix ~prefix:"1970-" date ||
         String.is_prefix ~prefix:"1969-" date ||
         String.is_suffix ~suffix:" 1970" date ||
         String.is_suffix ~suffix:" 1969" date then
        (* See https://github.com/hannesm/patch/issues/8 *)
        Ok None
      else
        Ok (Some filename)
  | Error _ as err -> err

let parse_git_header s =
  let parse s =
    match parse_filename ~allow_space:true s with
    | Ok (s, "") -> Ok (String.cut '/' s)
    | Ok _ -> Error "Unexpected character after closing double-quote in header"
    | Error _ as err -> err
  in
  let rec loop s len i =
    if i < len then
      match s.[i] with
      | ' ' | '\t' ->
          let a = parse (String.slice ~stop:i s) in
          let b = parse (String.slice ~start:(i + 1) s) in
          begin match a, b with
          | Ok (Some ("a", a)), Ok (Some ("b", b))
            when a = (b : string) ->
              Some a
          | _, _ -> loop s len (i + 1)
          end
      | _ -> loop s len (i + 1)
    else
      None
  in
  loop s (String.length s) 0
