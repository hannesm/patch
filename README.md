## Patch - apply your unified diffs in pure OCaml

The loosely specified `diff` file format is widely used for transmitting
differences of line-based information. The motivating example is
[`opam`](https://opam.ocaml.org), which is able to validate updates being
cryptographically signed (e.g. [conex](https://github.com/hannesm/conex)) by
providing a unified diff.

The [test-based infered specification](https://www.artima.com/weblogs/viewpost.jsp?thread=164293)
implemented in this library is the following grammar.

```
decimal := [0-9]+
any := any character except newline

filename := "/dev/null" | any except tab character
file := filename "\t" any "\n"
mine := "--- " file
theirs := "+++ " file

no_newline = "\ No newline at end of file"
hunk_line_prefix := " " | "-" | "+"
hunk_line := hunk_line_prefix any | no_newline
range := decimal "," decimal | decimal
hunk_hdr := "@@ -" range " + " range " @@\n"
hunk := hunk_hdr line+

diff := mine theirs hunk+
```

A `diff` consists of a two-line header containing the filenames (or "/dev/null"
for creation and deletion) followed by the actual changes in hunks. A complete
diff file is represented by a list of `diff` elements. The OCaml types below,
provided by this library, represent mine and theirs as operation (edit, rename,
delete, create). Since a diff is line-based, if the file does not end with a
newline character, the line in the diff always contains a newline, but the
special marker `no_newline` is added to the diff. The `range` information
carries start line and chunk size in the respective file, with two side
conditions: if the chunk size is 0, the start line refers to after which the
chunk should be added or deleted, and if the chunk size is omitted (including
the comma), it is set to 1. NB from practical experiments, only "+1" and "-1"
are supported.

```OCaml
type operation =
  | Edit of string
  | Rename of string * string
  | Delete of string
  | Create of string

type hunk (* positions and contents *)

type t = {
  operation : operation ;
  hunks : hunk list ;
  mine_no_nl : bool ;
  their_no_nl : bool ;
}
```

## Installation

`opam pin add patch https://github.com/hannesm/patch`
