type hunk = {
  mine_start : int ;
  mine_len : int ;
  mine : string list ;
  their_start : int ;
  their_len : int ;
  their : string list ;
}

type parse_error = {
  msg : string;
  lines : string list;
  (* TODO: add the start position of the error *)
}

exception Parse_error of parse_error

let unified_diff ~mine_no_nl ~their_no_nl ppf hunk =
  let no_nl_str = "\\ No newline at end of file\n" in
  let add_no_nl ppf =
    Format.pp_print_string ppf no_nl_str
  in
  let add_line ppf c line =
    Format.pp_print_char ppf c;
    Format.pp_print_string ppf line;
    Format.pp_print_char ppf '\n';
  in
  List.iter (add_line ppf '-') hunk.mine;
  if mine_no_nl then add_no_nl ppf;
  List.iter (add_line ppf '+') hunk.their;
  if their_no_nl then add_no_nl ppf;
  ()

let pp_hunk ~mine_no_nl ~their_no_nl ppf hunk =
  Format.fprintf ppf "%@%@ -%d,%d +%d,%d %@%@\n"
    hunk.mine_start hunk.mine_len hunk.their_start hunk.their_len;
  unified_diff ~mine_no_nl ~their_no_nl ppf hunk

let rec apply_hunk ~cleanly ~fuzz (last_matched_line, offset, lines) ({mine_start; mine_len; mine; their_start = _; their_len; their} as hunk) =
  let mine_start = mine_start + offset in
  let patch_match ~search_offset =
    let mine_start = mine_start + search_offset in
    let rev_prefix, rest = Lib.List.rev_cut (Stdlib.max 0 (mine_start - 1)) lines in
    let rev_actual_mine, suffix = Lib.List.rev_cut mine_len rest in
    let actual_mine = List.rev rev_actual_mine in
    if actual_mine <> (mine : string list) then
      invalid_arg "unequal mine";
    (* TODO: should we check their_len against List.length their? *)
    (mine_start + mine_len, offset + (their_len - mine_len),
     (* TODO: Replace rev_append (rev ...) by the tail-rec when patch
        requires OCaml >= 4.14 *)
     List.rev_append rev_prefix (List.rev_append (List.rev their) suffix))
  in
  try patch_match ~search_offset:0
  with Invalid_argument _ ->
    if cleanly then
      invalid_arg "apply_hunk"
    else
      let max_pos_offset = Stdlib.max 0 (List.length lines - Stdlib.max 0 (mine_start - 1) - mine_len) in
      let max_neg_offset = mine_start - last_matched_line in
      let rec locate search_offset =
        let aux search_offset max_offset =
          try
            if search_offset <= max_offset then
              Some (patch_match ~search_offset)
            else
              None
          with Invalid_argument _ -> None
        in
        if search_offset > max_pos_offset && search_offset > max_neg_offset then
          if fuzz < 3 && List.length mine >= 2 && List.length their >= 2 then
            let hunk =
              if List.hd hunk.mine = (List.hd hunk.their : string) then
                {
                  mine_start = hunk.mine_start + 1;
                  mine_len = hunk.mine_len - 1;
                  mine = List.tl hunk.mine;
                  their_start = hunk.their_start + 1;
                  their_len = hunk.their_len - 1;
                  their = List.tl hunk.their;
                }
              else
                hunk
            in
            let hunk =
              if Lib.List.last hunk.mine = (Lib.List.last hunk.their : string) then
                {
                  mine_start = hunk.mine_start;
                  mine_len = hunk.mine_len - 1;
                  mine = List.rev (List.tl (List.rev hunk.mine));
                  their_start = hunk.their_start;
                  their_len = hunk.their_len - 1;
                  their = List.rev (List.tl (List.rev hunk.their));
                }
              else
                hunk
            in
            if hunk.mine_len = 0 && hunk.their_len = 0 then
              invalid_arg "apply_hunk: equal hunks... why?!"
            else if mine_len = (hunk.mine_len : int) && their_len = (hunk.their_len : int) then
              invalid_arg "apply_hunk: could not apply fuzz"
            else
              apply_hunk ~cleanly ~fuzz:(fuzz + 1) (last_matched_line, offset, lines) hunk
          else
            invalid_arg "apply_hunk"
        else
          match aux search_offset max_pos_offset with
          | Some x -> x
          | None ->
              match aux (-search_offset) max_neg_offset with
              | Some x -> x
              | None -> locate (search_offset + 1)
      in
      locate 1

let to_start_len data =
  (* input being "?19,23" *)
  match Lib.String.cut ',' (Lib.String.slice ~start:1 data) with
  | None when data = "+1" || data = "-1" -> (1, 1)
  | None -> invalid_arg ("start_len broken in " ^ data)
  | Some (start, len) -> (int_of_string start, int_of_string len)

let count_to_sl_sl data =
  if Lib.String.is_prefix ~prefix:"@@ -" data then
    (* input: "@@ -19,23 +19,12 @@ bla" *)
    (* output: ((19,23), (19, 12)) *)
    match List.filter (function "" -> false | _ -> true) (Lib.String.cuts '@' data) with
    | numbers::_ ->
       let nums = String.trim numbers in
       (match Lib.String.cut ' ' nums with
        | None -> invalid_arg "couldn't find space in count"
        | Some (mine, theirs) -> Some (to_start_len mine, to_start_len theirs))
    | _ -> invalid_arg "broken line!"
  else
    None

let sort_into_bags ~counter:(mine_len, their_len) dir mine their m_nl t_nl str =
  let both data =
    if m_nl || t_nl then
      failwith "\"no newline at the end of file\" is not at the end of the file";
    if mine_len = 0 || their_len = 0 then
      failwith "invalid patch (both size exhausted)";
    let counter = (mine_len - 1, their_len - 1) in
    Some (counter, `Both, (data :: mine), (data :: their), m_nl, t_nl)
  in
  let str_len = String.length str in
  if mine_len = 0 && their_len = 0 && (str_len = 0 || str.[0] <> '\\') then
    None
  else if str_len = 0 then
    both "" (* NOTE: this should technically be a parse error but GNU patch accepts that and some patches in opam-repository do use this behaviour *)
  else match String.get str 0, Lib.String.slice ~start:1 str with
    | ' ', data ->
        both data
    | '\t', data ->
        both ("\t"^data) (* NOTE: not valid but accepted by GNU patch *)
    | '+', data ->
        if t_nl then
          failwith "\"no newline at the end of file\" is not at the end of the file";
        if their_len = 0 then
          failwith "invalid patch (+ size exhausted)";
        let counter = (mine_len, their_len - 1) in
        Some (counter, `Their, mine, (data :: their), m_nl, t_nl)
    | '-', data ->
        if m_nl then
          failwith "\"no newline at the end of file\" is not at the end of the file";
        if mine_len = 0 then
          failwith "invalid patch (- size exhausted)";
        let counter = (mine_len - 1, their_len) in
        Some (counter, `Mine, (data :: mine), their, m_nl, t_nl)
    | '\\', _data ->
      (* NOTE: Any line starting with '\' is taken as if it was
         '\ No newline at end of file' by GNU patch so we do the same *)
      (* diff: 'No newline at end of file' turns out to be context-sensitive *)
      (* so: -xxx\n\\No newline... means mine didn't have a newline *)
      (* but +xxx\n\\No newline... means theirs doesn't have a newline *)
      let my_nl, their_nl = match dir with
        | `Both -> true, true
        | `Mine -> true, t_nl
        | `Their -> m_nl, true
      in
      let counter = (mine_len, their_len) in
      Some (counter, dir, mine, their, my_nl, their_nl)
    | _ -> failwith "invalid patch (unknown character)"

let to_hunk count data mine_no_nl their_no_nl =
  match count_to_sl_sl count with
  | None -> None, mine_no_nl, their_no_nl, count :: data
  | Some ((mine_start, mine_len), (their_start, their_len)) ->
    let counter = (mine_len, their_len) in
    let rec step ~counter dir mine their mine_no_nl their_no_nl = function
      | [] | [""] when counter = (0, 0) -> (List.rev mine, List.rev their, mine_no_nl, their_no_nl, [])
      | [""] when counter = (1, 1) -> (List.rev ("" :: mine), List.rev ("" :: their), mine_no_nl, their_no_nl, []) (* GNU patch behaviour *)
      | [""] when counter = (2, 2) -> (List.rev ("" :: "" :: mine), List.rev ("" :: "" :: their), mine_no_nl, their_no_nl, []) (* GNU patch behaviour *)
      | [""] when counter = (3, 3) -> (List.rev ("" :: "" :: "" :: mine), List.rev ("" :: "" :: "" :: their), mine_no_nl, their_no_nl, []) (* GNU patch behaviour *)
      | [] | [""] -> failwith "bad file"
      | x::xs -> match sort_into_bags ~counter dir mine their mine_no_nl their_no_nl x with
        | Some (counter, dir, mine, their, mine_no_nl', their_no_nl') -> step ~counter dir mine their mine_no_nl' their_no_nl' xs
        | None -> (List.rev mine, List.rev their, mine_no_nl, their_no_nl, x :: xs)
    in
    let mine, their, mine_no_nl, their_no_nl, rest = step ~counter `Both [] [] mine_no_nl their_no_nl data in
    (Some { mine_start ; mine_len ; mine ; their_start ; their_len ; their }, mine_no_nl, their_no_nl, rest)

let rec to_hunks (mine_no_nl, their_no_nl, acc) = function
  | [] -> (List.rev acc, mine_no_nl, their_no_nl, [])
  | count::data -> match to_hunk count data mine_no_nl their_no_nl with
    | None, mine_no_nl, their_no_nl, rest -> List.rev acc, mine_no_nl, their_no_nl, rest
    | Some hunk, mine_no_nl, their_no_nl, rest -> to_hunks (mine_no_nl, their_no_nl, hunk :: acc) rest

type git_ext =
  | Rename_only of string * string
  | Delete_only
  | Create_only

type operation =
  | Edit of string * string
  | Delete of string
  | Create of string
  | Git_ext of (string * string * git_ext)

let git_ext_eq a b = match a, b with
  | Delete_only, Delete_only
  | Create_only, Create_only
    -> true
  | Rename_only (a, b), Rename_only (a', b')
    -> String.equal a a' && String.equal b b'
  | Rename_only _, _ | Delete_only, _ | Create_only, _
    -> false

let operation_eq a b = match a, b with
  | Delete a, Delete b
  | Create a, Create b
    -> String.equal a b
  | Edit (a, b), Edit (a', b')
    -> String.equal a a' && String.equal b b'
  | Git_ext (a, b, ext1), Git_ext (a', b', ext2)
    -> String.equal a a' && String.equal b b' && git_ext_eq ext1 ext2
  | Edit _, _ | Delete _, _ | Create _, _ | Git_ext _, _
    -> false

let no_file = "/dev/null"

let pp_filename ppf fn =
  (* NOTE: filename quote format from GNU diffutils *)
  let to_quote ~len s =
    let rec aux i len s =
      if i < len then
        match s.[i] with
        | '\007' | '\b' | '\t' | '\n' | '\011' | '\012' | '\r' | ' ' | '"' | '\\' -> true
        | c when c < ' ' || c > '~' -> true
        | _ -> aux (i + 1) len s
      else
        false
    in
    aux 0 len s
  in
  let rec aux ~len i ppf fn =
    if i < len then
      let c = fn.[i] in
      if c = '\007' then
        Format.pp_print_string ppf "\\a"
      else if c = '\b' then
        Format.pp_print_string ppf "\\b"
      else if c = '\t' then
        Format.pp_print_string ppf "\\t"
      else if c = '\n' then
        Format.pp_print_string ppf "\\n"
      else if c = '\011' then
        Format.pp_print_string ppf "\\v"
      else if c = '\012' then
        Format.pp_print_string ppf "\\f"
      else if c = '\r' then
        Format.pp_print_string ppf "\\r"
      else if c < ' ' || c > '~' then
        Format.fprintf ppf "\\%03o" (Char.code c)
      else if c = '"' then
        Format.pp_print_string ppf "\\\""
      else if c = '\\' then
        Format.pp_print_string ppf "\\\\"
      else
        Format.pp_print_char ppf c;
      aux ~len (i + 1) ppf fn
  in
  let len = String.length fn in
  if to_quote ~len fn then
    Format.fprintf ppf "\"%a\"" (aux ~len 0) fn
  else
    aux ~len 0 ppf fn

let pp_operation ppf = function
  | Edit (old_name, new_name) ->
    Format.fprintf ppf "--- %a\n" pp_filename old_name ;
    Format.fprintf ppf "+++ %a\n" pp_filename new_name
  | Delete name ->
    Format.fprintf ppf "--- %a\n" pp_filename name ;
    Format.fprintf ppf "+++ %a\n" pp_filename no_file
  | Create name ->
    Format.fprintf ppf "--- %a\n" pp_filename no_file ;
    Format.fprintf ppf "+++ %a\n" pp_filename name
  | Git_ext (a, b, ext) ->
      Format.fprintf ppf "diff --git %a %a\n" pp_filename a pp_filename b;
      match ext with
      | Rename_only (from_, to_) ->
          Format.fprintf ppf "rename from %a\n" pp_filename from_;
          Format.fprintf ppf "rename to %a\n" pp_filename to_
      | Delete_only ->
          Format.pp_print_string ppf "deleted file mode 100644\n";
      | Create_only ->
          Format.pp_print_string ppf "new file mode 100644\n";

type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}

let pp ppf {operation; hunks; mine_no_nl; their_no_nl} =
  pp_operation ppf operation;
  let rec aux = function
    | [] ->
        begin match operation with
        | Edit _ | Delete _ | Create _ ->
            assert false
        | Git_ext _ -> () (* already delt with in pp_operation *)
        end
    | [x] -> pp_hunk ~mine_no_nl ~their_no_nl ppf x
    | x::xs ->
        pp_hunk ~mine_no_nl:false ~their_no_nl:false ppf x;
        aux xs
  in
  aux hunks

let pp_list ppf diffs =
  List.iter (pp ppf) diffs

let strip_prefix ~p filename =
  if p = 0 then
    filename
  else
    match Lib.String.cuts '/' filename with
    | [] -> assert false
    | x::xs ->
        (* Per GNU patch's spec: A sequence of one or more adjacent slashes is counted as a single slash. *)
        let filename' = x :: List.filter (function "" -> false | _ -> true) xs in
        let rec drop_up_to n = function
          | [] -> assert false
          | l when n = 0 -> l
          | [_] -> failwith "wrong prefix"
          | _::xs -> drop_up_to (n - 1) xs
        in
        (* GNU patch just drops the max number of slashes when the filename doesn't have enough slashes to satisfy -p *)
        match drop_up_to p filename' with
        | [] -> assert false
        | l -> String.concat "/" l

let operation_of_strings ~p mine their =
  let mine_fn = Lib.String.slice ~start:4 mine
  and their_fn = Lib.String.slice ~start:4 their in
  match Fname.parse mine_fn, Fname.parse their_fn with
  | Ok None, Ok (Some b) -> Create (strip_prefix ~p b)
  | Ok (Some a), Ok None -> Delete (strip_prefix ~p a)
  | Ok (Some a), Ok (Some b) -> Edit (strip_prefix ~p a, strip_prefix ~p b)
  | Ok None, Ok None -> assert false (* ??!?? *)
  | Error msg, _ -> raise (Parse_error {msg; lines = [mine]})
  | _, Error msg -> raise (Parse_error {msg; lines = [their]})

let parse_one ~p data =
  let open (struct
    type mode = Git of string
  end) in
  let is_git = function
    | Some (Git _) -> true
    | None -> false
  in
  (* first locate --- and +++ lines *)
  let rec find_start ~mode ~git_action = function
    | [] ->
        begin match git_action with
        | Some git_action -> Some (Git_ext git_action), []
        | None -> None, []
        end
    | x::xs when Lib.String.is_prefix ~prefix:"diff --git " x ->
        begin match mode, git_action with
        | (None | Some (Git _)), None -> find_start ~mode:(Some (Git x)) ~git_action:None xs
        | None, Some _ -> assert false (* impossible state *)
        | Some (Git _), Some git_action -> (Some (Git_ext git_action), x :: xs)
        end
    | x::y::xs when is_git mode && Lib.String.is_prefix ~prefix:"rename from " x && Lib.String.is_prefix ~prefix:"rename to " y ->
        let git_action = match mode with
          | None -> assert false
          | Some (Git git_filenames) ->
              let from_ = Lib.String.slice ~start:12 x in
              let to_ = Lib.String.slice ~start:10 y in
              let git_filenames = Lib.String.slice ~start:11 git_filenames in
              match Fname.parse_git_header_rename ~from_ ~to_ git_filenames with
              | None -> git_action
              | Some (a, b) ->
                  let a = strip_prefix ~p a in
                  let b = strip_prefix ~p b in
                  Some (a, b, Rename_only (from_, to_))
        in
        find_start ~mode ~git_action xs
    | x::xs when is_git mode && Lib.String.is_prefix ~prefix:"deleted file mode " x ->
        let git_action = match mode with
          | None -> assert false
          | Some (Git git_filenames) ->
              let git_filenames = Lib.String.slice ~start:11 git_filenames in
              match Fname.parse_git_header_same git_filenames with
              | None -> git_action
              | Some (a, b) ->
                  let a = strip_prefix ~p a in
                  let b = strip_prefix ~p b in
                  Some (a, b, Delete_only)
        in
        find_start ~mode ~git_action xs
    | x::xs when is_git mode && Lib.String.is_prefix ~prefix:"new file mode " x ->
        let git_action = match mode with
          | None -> assert false
          | Some (Git git_filenames) ->
              let git_filenames = Lib.String.slice ~start:11 git_filenames in
              match Fname.parse_git_header_same git_filenames with
              | None -> git_action
              | Some (a, b) ->
                  let a = strip_prefix ~p a in
                  let b = strip_prefix ~p b in
                  Some (a, b, Create_only)
        in
        find_start ~mode ~git_action xs
    | x::y::xs when Lib.String.is_prefix ~prefix:"--- " x && Lib.String.is_prefix ~prefix:"+++ " y ->
        begin match git_action, operation_of_strings ~p x y with
        | None, op -> Some op, xs
        | Some (f, _, Delete_only), (Delete f' as op)
        | Some (_, f, Create_only), (Create f' as op)
          when String.equal f f' -> Some op, xs
        | Some (a, b, Rename_only (_, _)), (Edit (a', b') as op)
          when String.equal a a' && String.equal b b' -> Some op, xs
        | Some (_, _, (Rename_only _ | Delete_only | Create_only) as git_op), _
          -> Some (Git_ext git_op), x :: y :: xs
        end
    | x::y::_xs when Lib.String.is_prefix ~prefix:"*** " x && Lib.String.is_prefix ~prefix:"--- " y ->
      failwith "Context diffs are not supported"
    | _::xs -> find_start ~mode ~git_action xs
  in
  match find_start ~mode:None ~git_action:None data with
  | Some (Git_ext _ as operation), rest ->
    let hunks = [] and mine_no_nl = false and their_no_nl = false in
    Some ({ operation ; hunks ; mine_no_nl ; their_no_nl }, rest)
  | Some operation, rest ->
    let hunks, mine_no_nl, their_no_nl, rest = to_hunks (false, false, []) rest in
    Some ({ operation ; hunks ; mine_no_nl ; their_no_nl }, rest)
  | None, [] -> None
  | None, _ -> assert false

let to_lines = String.split_on_char '\n'

let parse ~p data =
  let lines = to_lines data in
  let rec doit ~p acc = function
    | [] -> List.rev acc
    | xs -> match parse_one ~p xs with
      | None -> List.rev acc
      | Some (diff, rest) -> doit ~p (diff :: acc) rest
  in
  doit ~p [] lines

let patch ~cleanly filedata diff =
  match diff.operation with
  | Git_ext (_, _, ext) ->
      if diff.hunks <> [] then
        assert false;
      begin match ext with
      | Rename_only _ -> filedata
      | Delete_only -> None
      | Create_only -> Some ""
      end
  | Delete _ -> None
  | Create _ ->
    begin match diff.hunks with
      | [ the_hunk ] ->
        let lines = String.concat "\n" the_hunk.their in
        let lines = if diff.their_no_nl then lines else lines ^ "\n" in
        Some lines
      | _ -> assert false
    end
  | Edit _ ->
    let old = match filedata with None -> [] | Some x -> to_lines x in
    let _, _, lines = List.fold_left (apply_hunk ~cleanly ~fuzz:0) (0, 0, old) diff.hunks in
    let lines = String.concat "\n" lines in
    let lines =
      match diff.mine_no_nl, diff.their_no_nl with
      | false, true ->
          let len = String.length lines in
          if len > 0 && String.unsafe_get lines (len - 1) = '\n' then
            Lib.String.slice ~stop:(len - 1) lines
          else
            lines
      | true, false -> lines ^ "\n"
      | false, false -> lines
      | true, true -> lines
    in
    Some lines

let diff_op operation a b =
  let rec aux ~mine_start ~mine_len ~mine ~their_start ~their_len ~their l1 l2 =
    let create_diff ~mine_no_nl ~their_no_nl =
      let hunks =
        if mine = [] && their = [] then
          assert false
        else
          let mine = List.rev mine in
          let their = List.rev their in
          [{mine_start; mine_len; mine; their_start; their_len; their}]
      in
      {operation; hunks; mine_no_nl; their_no_nl}
    in
    match l1, l2 with
    | [], [] | [""], [""] when mine = [] && their = [] -> assert false
    | [], [] -> Some (create_diff ~mine_no_nl:true ~their_no_nl:true)
    | [""], [] -> Some (create_diff ~mine_no_nl:false ~their_no_nl:true)
    | [], [""] -> Some (create_diff ~mine_no_nl:true ~their_no_nl:false)
    | [""], [""] -> Some (create_diff ~mine_no_nl:false ~their_no_nl:false)
    | a::l1, ([] | [""]) ->
        aux
          ~mine_start ~mine_len:(mine_len + 1) ~mine:(a :: mine)
          ~their_start ~their_len ~their
          l1 l2
    | ([] | [""]), b::l2 ->
        aux
          ~mine_start ~mine_len ~mine
          ~their_start ~their_len:(their_len + 1) ~their:(b :: their)
          l1 l2
    | a::(_::_ as l1), [b] ->
        aux
          ~mine_start ~mine_len:(mine_len + 1) ~mine:(a :: mine)
          ~their_start ~their_len:(their_len + 1) ~their:(b :: their)
          l1 []
    | [a], b::(_::_ as l2) ->
        aux
          ~mine_start ~mine_len:(mine_len + 1) ~mine:(a :: mine)
          ~their_start ~their_len:(their_len + 1) ~their:(b :: their)
          [] l2
    | a::l1, b::l2 when mine = [] && their = [] && String.equal a b ->
        aux
          ~mine_start:(mine_start + 1) ~mine_len ~mine
          ~their_start:(their_start + 1) ~their_len ~their
          l1 l2
    | a::l1, b::l2 ->
        aux
          ~mine_start ~mine_len:(mine_len + 1) ~mine:(a :: mine)
          ~their_start ~their_len:(their_len + 1) ~their:(b :: their)
          l1 l2
  in
  aux
    ~mine_start:(if a = "" then 0 else 1) ~mine_len:0 ~mine:[]
    ~their_start:(if b = "" then 0 else 1) ~their_len:0 ~their:[]
    (to_lines a) (to_lines b)

let diff a b = match a, b with
  | None, None -> invalid_arg "no input given"
  | None, Some (filename_b, "") ->
      Some { operation = Git_ext (filename_b, filename_b, Create_only);
             hunks = []; mine_no_nl = true; their_no_nl = true; }
  | Some (filename_a, ""), None ->
      Some { operation = Git_ext (filename_a, filename_a, Delete_only);
             hunks = []; mine_no_nl = true; their_no_nl = true; }
  | None, Some (filename_b, b) -> diff_op (Create filename_b) "" b
  | Some (filename_a, a), None -> diff_op (Delete filename_a) a ""
  | Some (_, a), Some (_, b) when String.equal a b -> None (* NOTE: Optimization *)
  | Some (filename_a, a), Some (filename_b, b) -> diff_op (Edit (filename_a, filename_b)) a b
