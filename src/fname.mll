{
module String = Patch_lib.String

type lexer_output =
  | Quoted of string
  | Unquoted
  | Error of string

exception Cant_parse_octal

let ascii_zero = Char.code '0'
let octal_to_char c1 c2 c3 =
  let char_to_digit c = Char.code c - ascii_zero in
  try
    Char.chr (
      (char_to_digit c1 lsl 6) lor
      (char_to_digit c2 lsl 3) lor
      char_to_digit c3
    )
  with Invalid_argument _ -> raise Cant_parse_octal
}

let octal = ['0'-'7']

rule lex_quoted_filename buf = parse
  | "\\a" { Buffer.add_char buf '\007'; lex_quoted_filename buf lexbuf }
  | "\\b" { Buffer.add_char buf '\b'; lex_quoted_filename buf lexbuf }
  | "\\f" { Buffer.add_char buf '\012'; lex_quoted_filename buf lexbuf }
  | "\\n" { Buffer.add_char buf '\n'; lex_quoted_filename buf lexbuf }
  | "\\r" { Buffer.add_char buf '\r'; lex_quoted_filename buf lexbuf }
  | "\\t" { Buffer.add_char buf '\t'; lex_quoted_filename buf lexbuf }
  | "\\v" { Buffer.add_char buf '\011'; lex_quoted_filename buf lexbuf }
  | "\\\\" { Buffer.add_char buf '\\'; lex_quoted_filename buf lexbuf }
  | "\\\"" { Buffer.add_char buf '"'; lex_quoted_filename buf lexbuf }
  | '\\' (['0'-'3'] as c1) (octal as c2) (octal as c3)
      {
        match octal_to_char c1 c2 c3 with
        | octal ->
            Buffer.add_char buf octal;
            lex_quoted_filename buf lexbuf
        | exception Cant_parse_octal -> Unquoted
      }
  | '\\' _ { Unquoted }
  | '"' eof { Quoted (Buffer.contents buf) }
  | '"' _ { Unquoted }
  | _ as c { Buffer.add_char buf c; lex_quoted_filename buf lexbuf }
  | eof { Unquoted }

and lex_filename buf = parse
  | '"' { lex_quoted_filename buf lexbuf }
  | _ { Unquoted }
  | eof { Error "empty filename" }

{
let parse s =
  let filename, _date =
    match String.cut '\t' s with
    | None -> (s, "")
    | Some x -> x
  in
  if filename = "/dev/null" then
    Ok None
  else
    let lexbuf = Lexing.from_string filename in
    match lex_filename (Buffer.create 128) lexbuf with
    | Quoted x -> Ok (Some x)
    | Unquoted -> Ok (Some filename)
    | Error msg -> Error msg
}
