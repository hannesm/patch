
let hunk_eq a b =
  let open Patch in
  a.mine_start = b.mine_start &&
  a.mine_len = b.mine_len &&
  a.their_start = b.their_start &&
  a.their_len = b.their_len &&
  List.length a.mine = List.length b.mine &&
  List.length a.their = List.length b.their &&
  List.for_all (fun x -> List.mem x b.mine) a.mine &&
  List.for_all (fun x -> List.mem x b.their) a.their

let patch_eq a b =
  let open Patch in
  operation_eq a.operation b.operation &&
  List.length a.hunks = List.length b.hunks &&
  List.for_all (fun h -> List.exists (fun h' -> hunk_eq h h') b.hunks) a.hunks

let test_t = Alcotest.testable (Patch.pp ~git:false) patch_eq

let basic_files = [
  Some "foo\n" ;
  Some {|foo
bar
baz
boo
foo
bar
baz
boo
|} ;
  Some {|foo
bar
baz
boo
foo
bar
bar
boo
foo
bar
baz
|} ;
  Some {|foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
|} ;
  None ;
  Some "foo\n" ;
  Some "foo\n" ]

let basic_diffs =
  let hdr =
    Printf.sprintf
{|--- a%s2019-03-10 16:48:51.826103000 +0100
+++ b%s2019-03-10 16:48:54.373352000 +0100
|} "\t" "\t"
  in
  [
    hdr ^
{|@@ -1 +1 @@
-foo
+foobar
|} ;
    hdr ^
{|@@ -2,7 +2,7 @@
 bar
 baz
 boo
-foo
+foo2
 bar
 baz
 boo
|} ;
    hdr ^
{|@@ -1,5 +1,5 @@
 foo
-bar
+bar2
 baz
 boo
 foo
@@ -8,4 +8,4 @@
 boo
 foo
 bar
-baz
+baz3
|} ;
    hdr ^
{|@@ -1,6 +1,7 @@
 foo
 foo
 foo
+foo3
 foo
 foo
 foo
@@ -9,6 +10,7 @@
 foo
 foo
 foo
+foo5
 foo
 foo
 foo
@@ -31,6 +33,11 @@
 foo
 foo
 foo
+bar
 foo
 foo
 foo
+foo
+foo
+foo
+bar2
|} ;
{|--- /dev/null
+++ b
@@ -0,0 +1 @@
+foo
|} ;
{|--- a
+++ /dev/null
@@ -1 +0,0 @@
-foo
|} ;
{|--- a
+++ b
@@ -1 +1,2 @@
 foo
+foo
|}
  ]

let basic_hunks =
  let open Patch in
  let hunk1 = [ { mine_start = 0 ; mine_len = 1 ; mine = ["foo"] ;
                  their_start = 0 ; their_len = 1 ; their = ["foobar"] } ]
  in
  let diff = { operation = Edit ("a", "b") ; hunks = hunk1 ; mine_no_nl = false ; their_no_nl = false } in
  let hunk2 =
    [ { mine_start = 1 ; mine_len = 7 ; mine = [ "bar" ; "baz" ; "boo" ; "foo" ; "bar" ; "baz" ; "boo" ] ;
        their_start = 1 ; their_len = 7 ; their = [ "bar" ; "baz" ; "boo" ; "foo2" ; "bar" ; "baz" ; "boo" ] } ]
  in
  let hunk3 = [
    { mine_start = 0 ; mine_len = 5 ; mine = [ "foo" ; "bar" ; "baz" ; "boo" ; "foo" ] ;
      their_start = 0 ; their_len = 5 ; their = [ "foo" ; "bar2" ; "baz" ; "boo" ; "foo" ] } ;
    { mine_start = 7 ; mine_len = 4 ; mine = [ "boo" ; "foo" ; "bar" ; "baz" ] ;
      their_start = 7 ; their_len = 4 ; their = [ "boo" ; "foo" ; "bar" ; "baz3" ] }
  ] in
  let hunk4 = [
    { mine_start = 0 ; mine_len = 6 ; mine = [ "foo" ; "foo" ; "foo" ; "foo" ; "foo" ; "foo" ] ;
      their_start = 0 ; their_len = 7 ; their = [ "foo" ; "foo" ; "foo" ; "foo3" ; "foo" ; "foo" ; "foo" ] } ;
    { mine_start = 8 ; mine_len = 6 ; mine = [ "foo" ; "foo" ; "foo" ; "foo" ; "foo" ; "foo" ] ;
      their_start = 9 ; their_len = 7 ; their = [ "foo" ; "foo" ; "foo" ; "foo5" ; "foo" ; "foo" ; "foo" ] } ;
    { mine_start = 30 ; mine_len = 6 ; mine = [ "foo" ; "foo" ; "foo" ; "foo" ; "foo" ; "foo" ] ;
      their_start = 32 ; their_len = 11 ; their = [ "foo" ; "foo" ; "foo" ; "bar" ; "foo" ; "foo" ; "foo" ; "foo" ; "foo" ; "foo" ; "bar2" ] }
  ] in
  let hunk5= [
    { mine_start = 0 ; mine_len = 0 ; mine = [] ;
      their_start = 0 ; their_len = 1 ; their = [ "foo" ] }
  ] in
  let diff5 = { diff with operation = Create "b" ; hunks = hunk5 } in
  let hunk6 = [
    { mine_start = 0 ; mine_len = 1 ; mine = [ "foo" ] ;
      their_start = 0 ; their_len = 0 ; their = [ ] }
  ] in
  let diff6 = { diff with operation = Delete "a" ; hunks = hunk6 } in
  let hunk7 = [
    { mine_start = 0 ; mine_len = 1 ; mine = [ "foo" ] ;
      their_start = 0 ; their_len = 2 ; their = [ "foo" ; "foo" ] }
  ] in
  let diff7 = { diff with operation = Edit ("a", "b") ; hunks = hunk7 } in
  List.map (fun d -> [ d ])
    [
      diff ;
      { diff with hunks = hunk2 } ;
      { diff with hunks = hunk3 } ;
      { diff with hunks = hunk4 } ;
      diff5 ;
      diff6 ;
      diff7 ;
    ]

let basic_app = [
  Some "foobar\n" ;
  Some {|foo
bar
baz
boo
foo2
bar
baz
boo
|} ;
  Some {|foo
bar2
baz
boo
foo
bar
bar
boo
foo
bar
baz3
|} ;
  Some {|foo
foo
foo
foo3
foo
foo
foo
foo
foo
foo
foo
foo
foo5
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
foo
bar
foo
foo
foo
foo
foo
foo
bar2
|} ;
 Some {|foo
|} ;
  None ;
  Some {|foo
foo
|}
]

let basic_parse diff exp () =
  let diffs = Patch.parse diff in
  Alcotest.(check (list test_t) __LOC__ exp diffs)

let parse_diffs =
  List.mapi (fun idx (diff, exp) ->
      "basic" ^ string_of_int idx, `Quick, basic_parse diff exp)
    (List.combine basic_diffs basic_hunks)

let basic_apply file diff exp () =
  match Patch.parse diff with
  | [ diff ] ->
    let res = Patch.patch file diff in
    Alcotest.(check (option string) __LOC__ exp res)
  | _ -> Alcotest.fail "expected one"

let apply_diffs =
  List.mapi (fun idx (exp, (data, diff)) ->
      "basic" ^ string_of_int idx, `Quick, basic_apply data diff exp)
    (List.combine basic_app (List.combine basic_files basic_diffs))

(* a diff with multiple files to patch, with each of the four kinds:
   rename, delete, create, edit *)
let multi_diff = {|
--- foo
+++ bar
@@ -1 +1 @@
-bar
+foobar
--- foobar
+++ /dev/null
@@ -1 +0,0 @@
-baz
--- /dev/null
+++ baz
@@ -0,0 +1 @@
+baz
\ No newline at end of file
--- foobarbaz
+++ foobarbaz
@@ -1 +1 @@
-foobarbaz
+foobar
|}

let multi_hunks =
  let open Patch in
  let hunk1 = [ { mine_start = 0 ; mine_len = 1 ; mine = ["bar"] ;
                  their_start = 0 ; their_len = 1 ; their = ["foobar"] } ]
  in
  let diff1 = { operation = Edit ("foo", "bar") ; hunks = hunk1 ; mine_no_nl = false ; their_no_nl = false } in
  let hunk2 =
    [ { mine_start = 0 ; mine_len = 1 ; mine = [ "baz" ] ;
        their_start = 0 ; their_len = 0 ; their = [] } ]
  in
  let diff2 = { operation = Delete "foobar" ; hunks = hunk2 ; mine_no_nl = false ; their_no_nl = false } in
  let hunk3 = [
    { mine_start = 0 ; mine_len = 0 ; mine = [ ] ;
      their_start = 0 ; their_len = 1 ; their = [ "baz" ] }
  ] in
  let diff3 = { operation = Create "baz" ; hunks = hunk3 ;  mine_no_nl = false ; their_no_nl = true } in
  let hunk4 = [
    { mine_start = 0 ; mine_len = 1 ; mine = [ "foobarbaz" ] ;
      their_start = 0 ; their_len = 1 ; their = [ "foobar" ] }
  ] in
  let diff4 = { operation = Edit ("foobarbaz", "foobarbaz") ; hunks = hunk4 ;  mine_no_nl = false ; their_no_nl = false } in
  [ diff1 ; diff2 ; diff3 ; diff4 ]

let multi_files = [ Some "bar" ; Some "baz" ; None ; Some "foobarbaz" ]

let multi_exp = [ Some "foobar" ; None ; Some "baz" ; Some "foobar" ]

let multi_apply () =
  let diffs = Patch.parse multi_diff in
  Alcotest.(check int __LOC__ (List.length multi_files) (List.length diffs));
  Alcotest.(check int __LOC__ (List.length multi_exp) (List.length diffs));
  List.iter2 (fun diff (input, expected) ->
      let res = Patch.patch input diff in
      Alcotest.(check (option string) __LOC__ expected res))
    diffs (List.combine multi_files multi_exp)

let multi_diffs = [
  "multi parse", `Quick, basic_parse multi_diff multi_hunks ;
  "multi apply", `Quick, multi_apply ;
]

let regression_diff, regression_hunks =
  let open Patch in
  {|
--- a	2024-03-22 20:38:14.411917871 +0000
+++ b	2024-03-22 20:04:53.409348792 +0000
@@ -1 +1 @@
--- /dev/null
+aaa
|},
  [ { operation = Edit ("a", "b");
      hunks = [ { mine_start = 0; mine_len = 1; mine = ["-- /dev/null"];
                  their_start = 0; their_len = 1; their = ["aaa"]} ];
      mine_no_nl = false; their_no_nl = false} ]

let basic_regression_diffs = [
  "basic regression parse", `Quick, basic_parse regression_diff regression_hunks ;
]

let data = "data/"

let read file =
  let filename = data ^ file in
  let size = Unix.(stat filename).st_size in
  let buf = Bytes.create size in
  let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0 in
  let res = Unix.read fd buf 0 size in
  assert (res = size) ;
  Unix.close fd ;
  Bytes.unsafe_to_string buf

let opt_read file = try Some (read file) with Unix.Unix_error _ -> None

let op_test = Alcotest.testable (Patch.pp_operation ~git:false) Patch.operation_eq

let parse_real_diff_header file hdr () =
  let data = read (file ^ ".diff") in
  let diffs = Patch.parse data in
  Alcotest.(check int __LOC__ 1 (List.length diffs));
  Alcotest.check op_test __LOC__ hdr (List.hd diffs).Patch.operation

let parse_real_diff_headers =
  List.map (fun (file, hdr) ->
      "parsing " ^ file ^ ".diff", `Quick, parse_real_diff_header file hdr)
    [ "first", Patch.Edit ("first.old", "first.new") ;
      "create1", Patch.Create "a/create1" ;
      "git1", Patch.Create "git1.new" ;
      "git2", Patch.Rename_only ("git2.old", "git2.new") ;
      "git3", Patch.Edit ("git3.old", "git3.new") ;
      "git4", Patch.Delete "git4.old"
    ]

let regression_test name () =
  let old = opt_read (name ^ ".old") in
  let diff = read (name ^ ".diff") in
  let exp = opt_read (name ^ ".new") in
  match Patch.parse diff with
  | [ diff ] ->
    let res = Patch.patch old diff in
    Alcotest.(check (option string) __LOC__ exp res)
  | ds -> Alcotest.fail ("expected one, found " ^ string_of_int (List.length ds))

module S = Set.Make(String)

let drop_ext str =
  try
    let idx = String.rindex str '.' in
    String.sub str 0 idx
  with
  | Not_found -> str

let regression_diffs =
  let collect_dir dir =
    let open Unix in
    let dh = opendir dir in
    let next () = try Some (readdir dh) with End_of_file -> None in
    let rec doone acc = function
      | Some "." | Some ".." -> doone acc (next ())
      | Some s -> doone (s :: acc) (next ())
      | None -> acc
    in
    let res = doone [] (next ()) in
    closedir dh ;
    res
  in
  let files = collect_dir data in
  let tests = List.fold_left (fun acc file -> S.add (drop_ext file) acc) S.empty files in
  List.map (fun test -> "regression " ^ test, `Quick, regression_test test) (S.elements tests)

let diff_tests_mine_unavailable_gen ~their_no_nl =
  let a = None
  and b =
{|aaa
bbb
ccc
ddd
eee|}^(if their_no_nl then "" else "\n")
  in
  let diff = Patch.diff (Create "b") a (Some b) in
  let hunk =
    { Patch.operation = Create "b";
      hunks = [ { mine_start = 0; mine_len = 0; mine = [];
                  their_start = 0; their_len = 5; their = ["aaa"; "bbb"; "ccc"; "ddd"; "eee"]} ];
      mine_no_nl = false; their_no_nl}
  in
  diff, Some hunk

let diff_tests_mine_unavailable_their_no_nl, diff_tests_hunk_mine_unavailable_their_no_nl =
  diff_tests_mine_unavailable_gen ~their_no_nl:true
let diff_tests_mine_unavailable_none_no_nl, diff_tests_hunk_mine_unavailable_none_no_nl =
  diff_tests_mine_unavailable_gen ~their_no_nl:false

let diff_tests_their_unavailable_gen ~mine_no_nl =
  let a =
{|aaa
bbb
ccc
ddd
eee|}^(if mine_no_nl then "" else "\n")
  and b = None
  in
  let diff = Patch.diff (Delete "a") (Some a) b in
  let hunk =
    { Patch.operation = Delete "a";
      hunks = [ { mine_start = 0; mine_len = 5; mine = ["aaa"; "bbb"; "ccc"; "ddd"; "eee"];
                  their_start = 0; their_len = 0; their = []} ];
      mine_no_nl; their_no_nl = false}
  in
  diff, Some hunk

let diff_tests_their_unavailable_mine_no_nl, diff_tests_hunk_their_unavailable_mine_no_nl =
  diff_tests_their_unavailable_gen ~mine_no_nl:true
let diff_tests_their_unavailable_none_no_nl, diff_tests_hunk_their_unavailable_none_no_nl =
  diff_tests_their_unavailable_gen ~mine_no_nl:false

let diff_tests_empty_gen ~mine_no_nl ~their_no_nl =
  let a = if mine_no_nl then "" else "\n"
  and b = if their_no_nl then "" else "\n" in
  let diff = Patch.diff (Edit ("a", "b")) (Some a) (Some b) in
  let hunk =
    if (mine_no_nl && their_no_nl) || (not mine_no_nl && not their_no_nl) then
      None
    else
      let mine_len, mine = if mine_no_nl then 0, [] else 1, [""] in
      let their_len, their = if their_no_nl then 0, [] else 1, [""] in
      Some { Patch.operation = Edit ("a", "b");
             hunks = [ { mine_start = 0; mine_len; mine;
                         their_start = 0; their_len; their} ];
             mine_no_nl = false; their_no_nl = false}
  in
  diff, hunk

let diff_tests_empty_both_no_nl, diff_tests_hunk_empty_both_no_nl =
  diff_tests_empty_gen ~mine_no_nl:true ~their_no_nl:true
let diff_tests_empty_mine_no_nl, diff_tests_hunk_empty_mine_no_nl =
  diff_tests_empty_gen ~mine_no_nl:true ~their_no_nl:false
let diff_tests_empty_their_no_nl, diff_tests_hunk_empty_their_no_nl =
  diff_tests_empty_gen ~mine_no_nl:false ~their_no_nl:true
let diff_tests_empty_none_no_nl, diff_tests_hunk_empty_none_no_nl =
  diff_tests_empty_gen ~mine_no_nl:false ~their_no_nl:false

let diff_tests_no_diff_gen ~mine_no_nl ~their_no_nl =
  let a =
{|aaa
bbb
ccc
ddd
eee|}^(if mine_no_nl then "" else "\n")
  and b =
{|aaa
bbb
ccc
ddd
eee|}^(if their_no_nl then "" else "\n")
  in
  let diff = Patch.diff (Edit ("a", "b")) (Some a) (Some b) in
  let hunk =
    if (mine_no_nl && their_no_nl) || (not mine_no_nl && not their_no_nl) then
      None
    else
      Some { Patch.operation = Edit ("a", "b");
             hunks = [ { mine_start = 4; mine_len = 1; mine = ["eee"];
                         their_start = 4; their_len = 1; their = ["eee"]} ];
             mine_no_nl; their_no_nl}
  in
  diff, hunk

let diff_tests_no_diff_both_no_nl, diff_tests_hunk_no_diff_both_no_nl =
  diff_tests_no_diff_gen ~mine_no_nl:true ~their_no_nl:true
let diff_tests_no_diff_mine_no_nl, diff_tests_hunk_no_diff_mine_no_nl =
  diff_tests_no_diff_gen ~mine_no_nl:true ~their_no_nl:false
let diff_tests_no_diff_their_no_nl, diff_tests_hunk_no_diff_their_no_nl =
  diff_tests_no_diff_gen ~mine_no_nl:false ~their_no_nl:true
let diff_tests_no_diff_none_no_nl, diff_tests_hunk_no_diff_none_no_nl =
  diff_tests_no_diff_gen ~mine_no_nl:false ~their_no_nl:false

let diff_tests_middle_same_size_gen ~mine_no_nl ~their_no_nl =
  let a =
{|aaa
bbb
ccc
ddd
eee|}^(if mine_no_nl then "" else "\n")
  and b =
{|aaa
bbb
test1
test2
eee|}^(if their_no_nl then "" else "\n")
  in
  let diff = Patch.diff (Edit ("a", "b")) (Some a) (Some b) in
  let hunk =
    { Patch.operation = Edit ("a", "b");
      hunks = [ { mine_start = 2; mine_len = 3; mine = ["ccc"; "ddd"; "eee"];
                  their_start = 2; their_len = 3; their = ["test1"; "test2"; "eee"]} ];
      mine_no_nl; their_no_nl}
  in
  diff, Some hunk

let diff_tests_middle_same_size_both_no_nl, diff_tests_hunk_middle_same_size_both_no_nl =
  diff_tests_middle_same_size_gen ~mine_no_nl:true ~their_no_nl:true
let diff_tests_middle_same_size_mine_no_nl, diff_tests_hunk_middle_same_size_mine_no_nl =
  diff_tests_middle_same_size_gen ~mine_no_nl:true ~their_no_nl:false
let diff_tests_middle_same_size_their_no_nl, diff_tests_hunk_middle_same_size_their_no_nl =
  diff_tests_middle_same_size_gen ~mine_no_nl:false ~their_no_nl:true
let diff_tests_middle_same_size_none_no_nl, diff_tests_hunk_middle_same_size_none_no_nl =
  diff_tests_middle_same_size_gen ~mine_no_nl:false ~their_no_nl:false

let diff_tests_middle_diff_size_gen ~mine_no_nl ~their_no_nl =
  let a =
{|aaa
bbb
ccc
ddd
eee|}^(if mine_no_nl then "" else "\n")
  and b =
{|aaa
bbb
test1
eee|}^(if their_no_nl then "" else "\n")
  in
  let diff = Patch.diff (Edit ("a", "b")) (Some a) (Some b) in
  let hunk =
    { Patch.operation = Edit ("a", "b");
      hunks = [ { mine_start = 2; mine_len = 3; mine = ["ccc"; "ddd"; "eee"];
                  their_start = 2; their_len = 2; their = ["test1"; "eee"]} ];
      mine_no_nl; their_no_nl}
  in
  diff, Some hunk

let diff_tests_middle_diff_size_both_no_nl, diff_tests_hunk_middle_diff_size_both_no_nl =
  diff_tests_middle_diff_size_gen ~mine_no_nl:true ~their_no_nl:true
let diff_tests_middle_diff_size_mine_no_nl, diff_tests_hunk_middle_diff_size_mine_no_nl =
  diff_tests_middle_diff_size_gen ~mine_no_nl:true ~their_no_nl:false
let diff_tests_middle_diff_size_their_no_nl, diff_tests_hunk_middle_diff_size_their_no_nl =
  diff_tests_middle_diff_size_gen ~mine_no_nl:false ~their_no_nl:true
let diff_tests_middle_diff_size_none_no_nl, diff_tests_hunk_middle_diff_size_none_no_nl =
  diff_tests_middle_diff_size_gen ~mine_no_nl:false ~their_no_nl:false

let diff_tests_beginning_same_size_gen ~mine_no_nl ~their_no_nl =
  let a =
{|aaa
bbb
ccc
ddd
eee|}^(if mine_no_nl then "" else "\n")
  and b =
{|test1
bbb
ccc
ddd
eee|}^(if their_no_nl then "" else "\n")
  in
  let diff = Patch.diff (Edit ("a", "b")) (Some a) (Some b) in
  let hunk =
    { Patch.operation = Edit ("a", "b");
      hunks = [ { mine_start = 0; mine_len = 5; mine = ["aaa"; "bbb"; "ccc"; "ddd"; "eee"];
                  their_start = 0; their_len = 5; their = ["test1"; "bbb"; "ccc"; "ddd"; "eee"]} ];
      mine_no_nl; their_no_nl}
  in
  diff, Some hunk

let diff_tests_beginning_same_size_both_no_nl, diff_tests_hunk_beginning_same_size_both_no_nl =
  diff_tests_beginning_same_size_gen ~mine_no_nl:true ~their_no_nl:true
let diff_tests_beginning_same_size_mine_no_nl, diff_tests_hunk_beginning_same_size_mine_no_nl =
  diff_tests_beginning_same_size_gen ~mine_no_nl:true ~their_no_nl:false
let diff_tests_beginning_same_size_their_no_nl, diff_tests_hunk_beginning_same_size_their_no_nl =
  diff_tests_beginning_same_size_gen ~mine_no_nl:false ~their_no_nl:true
let diff_tests_beginning_same_size_none_no_nl, diff_tests_hunk_beginning_same_size_none_no_nl =
  diff_tests_beginning_same_size_gen ~mine_no_nl:false ~their_no_nl:false

let diff_tests_beginning_diff_size_gen ~mine_no_nl ~their_no_nl =
  let a =
{|aaa
bbb
ccc
ddd
eee|}^(if mine_no_nl then "" else "\n")
  and b =
{|test1
ccc
ddd
eee|}^(if their_no_nl then "" else "\n")
  in
  let diff = Patch.diff (Edit ("a", "b")) (Some a) (Some b) in
  let hunk =
    { Patch.operation = Edit ("a", "b");
      hunks = [ { mine_start = 0; mine_len = 5; mine = ["aaa"; "bbb"; "ccc"; "ddd"; "eee"];
                  their_start = 0; their_len = 4; their = ["test1"; "ccc"; "ddd"; "eee"]} ];
      mine_no_nl; their_no_nl}
  in
  diff, Some hunk

let diff_tests_beginning_diff_size_both_no_nl, diff_tests_hunk_beginning_diff_size_both_no_nl =
  diff_tests_beginning_diff_size_gen ~mine_no_nl:true ~their_no_nl:true
let diff_tests_beginning_diff_size_mine_no_nl, diff_tests_hunk_beginning_diff_size_mine_no_nl =
  diff_tests_beginning_diff_size_gen ~mine_no_nl:true ~their_no_nl:false
let diff_tests_beginning_diff_size_their_no_nl, diff_tests_hunk_beginning_diff_size_their_no_nl =
  diff_tests_beginning_diff_size_gen ~mine_no_nl:false ~their_no_nl:true
let diff_tests_beginning_diff_size_none_no_nl, diff_tests_hunk_beginning_diff_size_none_no_nl =
  diff_tests_beginning_diff_size_gen ~mine_no_nl:false ~their_no_nl:false

let diff_tests_end_same_size_gen ~mine_no_nl ~their_no_nl =
  let a =
{|aaa
bbb
ccc
ddd
eee|}^(if mine_no_nl then "" else "\n")
  and b =
{|aaa
bbb
ccc
ddd
test1|}^(if their_no_nl then "" else "\n")
  in
  let diff = Patch.diff (Edit ("a", "b")) (Some a) (Some b) in
  let hunk =
    { Patch.operation = Edit ("a", "b");
      hunks = [ { mine_start = 4; mine_len = 1; mine = ["eee"];
                  their_start = 4; their_len = 1; their = ["test1"]} ];
      mine_no_nl; their_no_nl}
  in
  diff, Some hunk

let diff_tests_end_same_size_both_no_nl, diff_tests_hunk_end_same_size_both_no_nl =
  diff_tests_end_same_size_gen ~mine_no_nl:true ~their_no_nl:true
let diff_tests_end_same_size_mine_no_nl, diff_tests_hunk_end_same_size_mine_no_nl =
  diff_tests_end_same_size_gen ~mine_no_nl:true ~their_no_nl:false
let diff_tests_end_same_size_their_no_nl, diff_tests_hunk_end_same_size_their_no_nl =
  diff_tests_end_same_size_gen ~mine_no_nl:false ~their_no_nl:true
let diff_tests_end_same_size_none_no_nl, diff_tests_hunk_end_same_size_none_no_nl =
  diff_tests_end_same_size_gen ~mine_no_nl:false ~their_no_nl:false

let diff_tests_end_diff_size_gen ~mine_no_nl ~their_no_nl =
  let a =
{|aaa
bbb
ccc
ddd
eee|}^(if mine_no_nl then "" else "\n")
  and b =
{|aaa
bbb
ccc
test1|}^(if their_no_nl then "" else "\n")
  in
  let diff = Patch.diff (Edit ("a", "b")) (Some a) (Some b) in
  let hunk =
    { Patch.operation = Edit ("a", "b");
      hunks = [ { mine_start = 3; mine_len = 2; mine = ["ddd"; "eee"];
                  their_start = 3; their_len = 1; their = ["test1"]} ];
      mine_no_nl; their_no_nl}
  in
  diff, Some hunk

let diff_tests_end_diff_size_both_no_nl, diff_tests_hunk_end_diff_size_both_no_nl =
  diff_tests_end_diff_size_gen ~mine_no_nl:true ~their_no_nl:true
let diff_tests_end_diff_size_mine_no_nl, diff_tests_hunk_end_diff_size_mine_no_nl =
  diff_tests_end_diff_size_gen ~mine_no_nl:true ~their_no_nl:false
let diff_tests_end_diff_size_their_no_nl, diff_tests_hunk_end_diff_size_their_no_nl =
  diff_tests_end_diff_size_gen ~mine_no_nl:false ~their_no_nl:true
let diff_tests_end_diff_size_none_no_nl, diff_tests_hunk_end_diff_size_none_no_nl =
  diff_tests_end_diff_size_gen ~mine_no_nl:false ~their_no_nl:false

let check_diff diff1 diff2 () =
  Alcotest.(check (option test_t) __LOC__ diff1 diff2)

let unified_diff_creation = [
  "mine unavailable, their no_nl", `Quick, check_diff diff_tests_mine_unavailable_their_no_nl diff_tests_hunk_mine_unavailable_their_no_nl ;
  "mine unavailable, none no_nl", `Quick, check_diff  diff_tests_mine_unavailable_none_no_nl diff_tests_hunk_mine_unavailable_none_no_nl;
  "their unavailable, mine no_nl", `Quick, check_diff diff_tests_their_unavailable_mine_no_nl diff_tests_hunk_their_unavailable_mine_no_nl ;
  "their unavailable, none no_nl", `Quick, check_diff diff_tests_their_unavailable_none_no_nl diff_tests_hunk_their_unavailable_none_no_nl ;
  "empty, both no_nl", `Quick, check_diff diff_tests_empty_both_no_nl diff_tests_hunk_empty_both_no_nl;
  "empty, mine no_nl", `Quick, check_diff diff_tests_empty_mine_no_nl diff_tests_hunk_empty_mine_no_nl;
  "empty, their no_nl", `Quick, check_diff diff_tests_empty_their_no_nl diff_tests_hunk_empty_their_no_nl;
  "empty, none no_nl", `Quick, check_diff diff_tests_empty_none_no_nl diff_tests_hunk_empty_none_no_nl;
  "no diff, both no_nl", `Quick, check_diff diff_tests_no_diff_both_no_nl diff_tests_hunk_no_diff_both_no_nl ;
  "no diff, mine no_nl", `Quick, check_diff diff_tests_no_diff_mine_no_nl diff_tests_hunk_no_diff_mine_no_nl ;
  "no diff, their no_nl", `Quick, check_diff diff_tests_no_diff_their_no_nl diff_tests_hunk_no_diff_their_no_nl ;
  "no diff, none no_nl", `Quick, check_diff diff_tests_no_diff_none_no_nl diff_tests_hunk_no_diff_none_no_nl ;
  "middle, same size, both no_nl", `Quick, check_diff diff_tests_middle_same_size_both_no_nl diff_tests_hunk_middle_same_size_both_no_nl ;
  "middle, same size, mine no_nl", `Quick, check_diff  diff_tests_middle_same_size_mine_no_nl diff_tests_hunk_middle_same_size_mine_no_nl;
  "middle, same size, their no_nl", `Quick, check_diff diff_tests_middle_same_size_their_no_nl diff_tests_hunk_middle_same_size_their_no_nl ;
  "middle, same size, none no_nl", `Quick, check_diff diff_tests_middle_same_size_none_no_nl diff_tests_hunk_middle_same_size_none_no_nl ;
  "middle, diff size, both no_nl", `Quick, check_diff diff_tests_middle_diff_size_both_no_nl diff_tests_hunk_middle_diff_size_both_no_nl ;
  "middle, diff size, mine no_nl", `Quick, check_diff diff_tests_middle_diff_size_mine_no_nl diff_tests_hunk_middle_diff_size_mine_no_nl ;
  "middle, diff size, their no_nl", `Quick, check_diff diff_tests_middle_diff_size_their_no_nl diff_tests_hunk_middle_diff_size_their_no_nl ;
  "middle, diff size, none no_nl", `Quick, check_diff diff_tests_middle_diff_size_none_no_nl diff_tests_hunk_middle_diff_size_none_no_nl ;
  "beginning, same size, both no_nl", `Quick, check_diff diff_tests_beginning_same_size_both_no_nl diff_tests_hunk_beginning_same_size_both_no_nl ;
  "beginning, same size, mine no_nl", `Quick, check_diff diff_tests_beginning_same_size_mine_no_nl diff_tests_hunk_beginning_same_size_mine_no_nl ;
  "beginning, same size, their no_nl", `Quick, check_diff diff_tests_beginning_same_size_their_no_nl diff_tests_hunk_beginning_same_size_their_no_nl ;
  "beginning, same size, none no_nl", `Quick, check_diff diff_tests_beginning_same_size_none_no_nl diff_tests_hunk_beginning_same_size_none_no_nl ;
  "beginning, diff size, both no_nl", `Quick, check_diff diff_tests_beginning_diff_size_both_no_nl diff_tests_hunk_beginning_diff_size_both_no_nl ;
  "beginning, diff size, mine no_nl", `Quick, check_diff diff_tests_beginning_diff_size_mine_no_nl diff_tests_hunk_beginning_diff_size_mine_no_nl ;
  "beginning, diff size, their no_nl", `Quick, check_diff diff_tests_beginning_diff_size_their_no_nl diff_tests_hunk_beginning_diff_size_their_no_nl ;
  "beginning, diff size, none no_nl", `Quick, check_diff diff_tests_beginning_diff_size_none_no_nl diff_tests_hunk_beginning_diff_size_none_no_nl ;
  "end, same size, both no_nl", `Quick, check_diff diff_tests_end_same_size_both_no_nl diff_tests_hunk_end_same_size_both_no_nl ;
  "end, same size, mine no_nl", `Quick, check_diff diff_tests_end_same_size_mine_no_nl diff_tests_hunk_end_same_size_mine_no_nl ;
  "end, same size, their no_nl", `Quick, check_diff diff_tests_end_same_size_their_no_nl diff_tests_hunk_end_same_size_their_no_nl ;
  "end, same size, none no_nl", `Quick, check_diff diff_tests_end_same_size_none_no_nl diff_tests_hunk_end_same_size_none_no_nl ;
  "end, diff size, both no_nl", `Quick, check_diff diff_tests_end_diff_size_both_no_nl diff_tests_hunk_end_diff_size_both_no_nl ;
  "end, diff size, mine no_nl", `Quick, check_diff diff_tests_end_diff_size_mine_no_nl diff_tests_hunk_end_diff_size_mine_no_nl ;
  "end, diff size, their no_nl", `Quick, check_diff diff_tests_end_diff_size_their_no_nl diff_tests_hunk_end_diff_size_their_no_nl ;
  "end, diff size, none no_nl", `Quick, check_diff diff_tests_end_diff_size_none_no_nl diff_tests_hunk_end_diff_size_none_no_nl ;
]

let operations exp diff () =
  let ops = diff |> Patch.parse |> List.map (fun p -> p.Patch.operation) in
  Alcotest.(check (list op_test)) __LOC__ exp ops

let unified_diff_spaces = {|\
--- "a/foo bar"	2024-09-04 10:56:24.139293679 +0200
+++ "b/foo bar"	2024-09-04 10:56:12.519195763 +0200
@@ -1 +1 @@
-This is wrong.
+This is right.
|}

let unified_diff_spaces =
  operations [Patch.Edit ("a/foo bar", "b/foo bar")] unified_diff_spaces

let git_diff_spaces = {|\
diff --git a/foo bar b/foo bar
index ef00db3..88adca3 100644
--- a/foo bar
+++ b/foo bar
@@ -1 +1 @@
-This is wrong.
+This is right.
|}

let git_diff_spaces =
  operations [Patch.Edit ("foo bar", "foo bar")] git_diff_spaces

let busybox_diff_spaces = {|\
--- a/foo bar
+++ b/foo bar
@@ -1 +1 @@
-This is wrong.
+This is right.
|}

let busybox_diff_spaces =
  operations [Patch.Edit ("a/foo bar", "b/foo bar")] busybox_diff_spaces

let unified_diff_quotes = {|\
--- "foo bar \"baz\""	2024-09-27 11:09:48.325541553 +0200
+++ "\"foo\" bar baz"	2024-09-27 11:06:42.612922437 +0200
@@ -1 +1 @@
-This is right.
+This is wrong.
|}

let unified_diff_quotes =
  operations [Patch.Edit ({|foo bar "baz"|}, {|"foo" bar baz|})] unified_diff_quotes

let git_diff_quotes = {|\
diff --git "a/foo bar \"baz\"" "b/\"foo\" bar baz"
index 88adca3..ef00db3 100644
--- "a/foo bar \"baz\""
+++ "b/\"foo\" bar baz"
@@ -1 +1 @@
-This is right.
+This is wrong.
|}

let git_diff_quotes =
  operations [Patch.Edit ({|foo bar "baz"|}, {|"foo" bar baz|})] git_diff_quotes

let busybox_diff_quotes = {|\
--- foo bar "baz"
+++ "foo" bar baz
@@ -1 +1 @@
-This is right.
+This is wrong.
|}

let busybox_diff_quotes =
  operations [Patch.Edit ({|foo bar "baz"|}, {|"foo" bar baz|})] busybox_diff_quotes

let dev_null_like = {|\
--- /dev/null_but_actually_not
+++ b
@@ -0,0 +1 @@
+foo
|}

let dev_null_like =
  operations [Patch.Edit ("/dev/null_but_actually_not", "b")] dev_null_like

let macos_diff_N_deletion = {|\
diff -ruaN a/test b/test
--- a/test	2024-03-21 11:29:11
+++ b/test	1970-01-01 01:00:00
@@ -1 +0,0 @@
-aaa
|}

let macos_diff_N_deletion =
  operations [Patch.Delete "a/test"] macos_diff_N_deletion

let openbsd_diff_N_deletion = {|\
diff -ruaN a/test b/test
--- a/test	Thu Mar 21 12:34:45 2024
+++ b/test	Thu Jan  1 01:00:00 1970
@@ -1 +0,0 @@
-aaa
|}

let openbsd_diff_N_deletion =
  operations [Patch.Delete "a/test"] openbsd_diff_N_deletion

let gnu_diff_N_deletion = {|\
diff -ruaN a/test b/test
--- a/test	2024-03-21 11:35:38.363194916 +0000
+++ b/test	1970-01-01 01:00:00.000000000 +0100
@@ -1 +0,0 @@
-aaa
|}

let gnu_diff_N_deletion =
  operations [Patch.Delete "a/test"] gnu_diff_N_deletion

let busybox_diff_N_deletion = {|\
--- a/test
+++ /dev/null
@@ -1 +0,0 @@
-aaa
|}

let busybox_diff_N_deletion =
  operations [Patch.Delete "a/test"] busybox_diff_N_deletion

let quoted_filename = {|\
--- /dev/null
+++ "\a\b\f\n\r\t\v\\\"\001\177\046"
@@ -0,0 +1 @@
+aaa
|}

let quoted_filename =
  operations [Patch.Create "\007\b\012\n\r\t\011\\\"\001\127&"] quoted_filename

let unquoted_filename = {|\
--- /dev/null
+++ \a\b\f\n\r\t\v\\\"\001\177\046
@@ -0,0 +1 @@
+aaa
|}

let unquoted_filename =
  operations [Patch.Create {|\a\b\f\n\r\t\v\\\"\001\177\046|}] unquoted_filename

let filename_diffs =
  [
    "unified diff with spaces", `Quick, unified_diff_spaces;
    "git diff with spaces", `Quick, git_diff_spaces;
    "busybox diff with spaces", `Quick, busybox_diff_spaces;
    "unified diff with quotes", `Quick, unified_diff_quotes;
    "git diff with quotes", `Quick, git_diff_quotes;
    "busybox diff with quotes", `Quick, busybox_diff_quotes;
    "file that looks like /dev/null", `Quick, dev_null_like;
    "diff -uN with file deletion on macOS", `Quick, macos_diff_N_deletion;
    "diff -uN with file deletion on OpenBSD", `Quick, openbsd_diff_N_deletion;
    "diff -uN with file deletion with GNU diff", `Quick, gnu_diff_N_deletion;
    "diff -uN with file deletion with Busybox", `Quick, busybox_diff_N_deletion;
    "heavily quoted filename", `Quick, quoted_filename;
    "unquoted filename with backslashes", `Quick, unquoted_filename;
  ]

let tests = [
  "parse", parse_diffs ;
  "apply", apply_diffs ;
  "multiple", multi_diffs ;
  "filename", filename_diffs ;
  "regression basic", basic_regression_diffs ;
  "parse real diffs", parse_real_diff_headers ;
  "regression", regression_diffs ;
  "diff", unified_diff_creation ;
]

let () =
  Alcotest.run "Patch tests" tests
