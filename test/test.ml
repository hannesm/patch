
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

let hunk =
  let module M = struct
    type t = Patch.hunk
    let pp = Patch.pp_hunk
    let equal = hunk_eq
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let t =
  let module M = struct
    type t = Patch.t
    let pp = Patch.pp
    let equal a b =
      let open Patch in
      String.equal a.mine_name b.mine_name &&
      String.equal a.their_name b.their_name &&
      List.length a.hunks = List.length b.hunks &&
      List.for_all (fun h -> List.exists (fun h' -> hunk_eq h h') b.hunks) a.hunks
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let basic_files = [
  "foo\n" ;
  {|foo
bar
baz
boo
foo
bar
baz
boo
|} ;
  {|foo
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
  {|foo
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
|} ]

let basic_diffs = [
{|--- a   2019-03-10 16:48:51.826103000 +0100
+++ b   2019-03-10 16:48:54.373352000 +0100
@@ -1 +1 @@
-foo
+foobar
|} ;
{|--- a   2019-03-10 17:26:02.773281000 +0100
+++ b   2019-03-10 17:26:11.088352000 +0100
@@ -2,7 +2,7 @@
 bar
 baz
 boo
-foo
+foo2
 bar
 baz
 boo
|} ;
{|--- a   2019-03-10 17:26:02.773281000 +0100
+++ b   2019-03-10 17:35:48.434586000 +0100
@@ -1,5 +1,5 @@
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
{|--- a   2019-03-10 17:39:43.596593000 +0100
+++ b   2019-03-10 17:40:05.894975000 +0100
@@ -1,6 +1,7 @@
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
|} ]

let basic_hunks =
  let open Patch in
  let hunk1 = [ { mine_start = 0 ; mine_len = 1 ; mine = ["foo"] ;
                  their_start = 0 ; their_len = 1 ; their = ["foobar"] } ]
  in
  let diff = { mine_name = "a" ; their_name = "b" ; hunks = hunk1 ; no_nl = false } in
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
  List.map (fun d -> [ d ])
    [
      diff ;
      { diff with hunks = hunk2 } ;
      { diff with hunks = hunk3 } ;
      { diff with hunks = hunk4 }
    ]

let basic_app = [
  "foobar\n" ;
  {|foo
bar
baz
boo
foo2
bar
baz
boo
|} ;
  {|foo
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
  {|foo
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
|} ]

let basic_parse diff exp () =
  let diffs = Patch.to_diffs diff in
  Alcotest.(check (list t) __LOC__ exp diffs)

let parse_diffs =
  List.mapi (fun idx (diff, exp) ->
      "basic" ^ string_of_int idx, `Quick, basic_parse diff exp)
    (List.combine basic_diffs basic_hunks)

let basic_apply file diff exp () =
  match Patch.to_diffs diff with
  | [ diff ] -> begin match Patch.patch (Some file) diff with
    | Ok data -> Alcotest.(check string __LOC__ exp data)
    | Error (`Msg m) -> Alcotest.fail m
    end
  | _ -> Alcotest.fail "expected one"

let apply_diffs =
  List.mapi (fun idx (exp, (data, diff)) ->
      "basic" ^ string_of_int idx, `Quick, basic_apply data diff exp)
    (List.combine basic_app (List.combine basic_files basic_diffs))

let data = "data/"

let regression_test name () =
  let read file =
    let filename = data ^ file in
    let size = Unix.(stat filename).st_size in
    let buf = Bytes.create size in
    let fd = Unix.openfile filename [ Unix.O_RDONLY ] 0 in
    let res = Unix.read fd buf 0 size in
    assert (res = size) ;
    Unix.close fd ;
    Bytes.unsafe_to_string buf
  in
  let old = read (name ^ ".old") in
  let diff = read (name ^ ".diff") in
  let exp = read (name ^ ".new") in
  match Patch.to_diffs diff with
  | [ diff ] -> begin match Patch.patch (Some old) diff with
    | Ok data -> Alcotest.(check string __LOC__ exp data)
    | Error (`Msg m) -> Alcotest.fail m
    end
  | _ -> Alcotest.fail "expected one"

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
  "regression", regression_diffs ;
]

let () =
  Alcotest.run "Patch tests" tests
