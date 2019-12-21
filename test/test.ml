
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

let test_hunk = Alcotest.testable Patch.pp_hunk hunk_eq

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
  let diff = { operation = Rename ("a", "b") ; hunks = hunk1 ; mine_no_nl = false ; their_no_nl = false } in
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
  let diff7 = { diff with operation = Rename ("a", "b") ; hunks = hunk7 } in
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
  let diffs = Patch.to_diffs diff in
  Alcotest.(check (list test_t) __LOC__ exp diffs)

let parse_diffs =
  List.mapi (fun idx (diff, exp) ->
      "basic" ^ string_of_int idx, `Quick, basic_parse diff exp)
    (List.combine basic_diffs basic_hunks)

let basic_apply file diff exp () =
  match Patch.to_diffs diff with
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
  let diff1 = { operation = Rename ("foo", "bar") ; hunks = hunk1 ; mine_no_nl = false ; their_no_nl = false } in
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
  let diff4 = { operation = Edit "foobarbaz" ; hunks = hunk4 ;  mine_no_nl = false ; their_no_nl = false } in
  [ diff1 ; diff2 ; diff3 ; diff4 ]

let multi_files = [ Some "bar" ; Some "baz" ; None ; Some "foobarbaz" ]

let multi_exp = [ Some "foobar" ; None ; Some "baz" ; Some "foobar" ]

let multi_apply () =
  let diffs = Patch.to_diffs multi_diff in
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
  let diffs = Patch.to_diffs data in
  Alcotest.(check int __LOC__ 1 (List.length diffs));
  Alcotest.check op_test __LOC__ hdr (List.hd diffs).Patch.operation

let parse_real_diff_headers =
  List.map (fun (file, hdr) ->
      "parsing " ^ file ^ ".diff", `Quick, parse_real_diff_header file hdr)
    [ "first", Patch.Rename ("first.old", "first.new") ;
      "create1", Patch.Create "a/create1" ;
      "git1", Patch.Create "git1.new" ;
      "git2", Patch.Rename_only ("git2.old", "git2.new") ;
      "git3", Patch.Rename ("git3.old", "git3.new") ;
      "git4", Patch.Delete "git4.old"
    ]

let regression_test name () =
  let old = opt_read (name ^ ".old") in
  let diff = read (name ^ ".diff") in
  let exp = opt_read (name ^ ".new") in
  match Patch.to_diffs diff with
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

let tests = [
  "parse", parse_diffs ;
  "apply", apply_diffs ;
  "multiple", multi_diffs ;
  "parse real diffs", parse_real_diff_headers ;
  "regression", regression_diffs ;
]

let () =
  Alcotest.run "Patch tests" tests
