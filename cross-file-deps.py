#!/usr/bin/env python3
"""
Find cross-file symbol dependencies among a set of Common Lisp source files.

Usage
-----
    python3 cross-file-deps.py FILE1.lisp FILE2.lisp [...]
    python3 cross-file-deps.py --glob 'src/numerical-functions/*.lisp'
    python3 cross-file-deps.py @filelist.txt             # one path per line
    python3 cross-file-deps.py --stdin < filelist.txt    # or via stdin

Options
-------
    --show-sites      Print every call site, not just the summary.
    --symbols-only    Print just the set of symbols crossing boundaries.
    --ignore REGEX    Regex of symbol names to ignore (repeatable).
    --min-defs N      Only consider symbols defined in exactly N files (default: 1,
                      i.e. unambiguous).  Set >1 only for diagnostic use.
    --quiet           Only print the cycle summary at the end.

Approach
--------
Purely lexical.  No Lisp image required.

Pass 1: for each file, find every top-level `(def* NAME ...)` form and record
        NAME as "defined in that file".  The set of def-forms covered is broad:
        defun, defmfun, defmacro, defvar, defparameter, defconstant, defstruct,
        defclass, defgeneric, defmethod, defsetf, deftype, defprop, defgrad,
        defmspec, defmvar, def-simplifier, define-symbol-macro, and any other
        top-level token starting with "def".

Pass 2: for each file, scan the full source for any reference to a name defined
        in some OTHER file in the input set.  Uses word-boundary matching that
        respects Lisp symbol-constituent characters:
            $ % * + - / < = > ? ! _ @ & .  plus alphanumerics
        A match only counts if the character immediately before and after the
        hit is NOT a symbol constituent.

Limitations (important to understand)
-------------------------------------
*   False positives in string literals and comments.  A name that appears inside
    `"..."` or after `;` is reported.  For cycle-breaking work this is usually
    fine -- it's rare, and reviewing a handful of false positives is cheaper
    than missing a real edge.  Pass `--strip-strings-and-comments` to remove
    them (best-effort; not a full reader).
*   Local variable shadowing.  If a function binds `dn` locally and never calls
    the `dn` defined in another file, the reference still gets reported.  You
    have to eyeball those.  (Mentioned in the report.)
*   Does not resolve package qualifiers.  `foo:bar` is treated as a reference
    to `bar`; if a different package also defines `bar`, the attribution may
    be wrong.  For single-package codebases (Maxima is mostly :maxima) this
    is fine.
*   Reader macros like `#+feature` branches are not filtered; a name inside
    a `#-ignored` branch still appears in the text.  Rare in practice.

Exit status
-----------
0 if no cross-file dependencies found, 1 otherwise.  Useful for CI.
"""

import argparse
import glob
import os
import re
import sys
from collections import defaultdict


# Characters that can appear inside a Common Lisp symbol token (besides
# alphanumerics).  Conservative but covers everything Maxima uses.
#
# NOTE the inclusion of '^' -- several Maxima macros are named m+, m*, m^
# etc.  Leaving ^ out would cause the def-form regex to see (defmacro m^
# ...) as "defmacro defining a name of 'm'", which is wrong.
SYMBOL_CHARS = r"A-Za-z0-9!?*+\-/<=>%\$_@&.\^~"

# A def-form is any top-level `(def...` where the first token after the
# open-paren starts with "def" (case-insensitive) and is followed by a name.
# We anchor to column 0 so we don't pick up nested forms.  The captured name
# is group 2.
DEF_FORM_RE = re.compile(
    r"^\((def[" + SYMBOL_CHARS + r"]*)\s+([" + SYMBOL_CHARS + r"]+)",
    re.MULTILINE | re.IGNORECASE,
)


def file_basename(path: str) -> str:
    """Return the basename without extension, for display and keying."""
    return os.path.splitext(os.path.basename(path))[0]


def read_file(path: str) -> str:
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.read()


def strip_strings_and_comments(text: str) -> str:
    """
    Remove Common Lisp ";..." line comments, "#| ... |#" block comments, and
    "..." strings.  Replaces them with spaces so line numbers and positions
    are preserved.  Not a full reader; approximate.
    """
    out = []
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        # Line comment: ; to end of line
        if c == ";":
            j = text.find("\n", i)
            if j < 0:
                j = n
            out.append(" " * (j - i))
            i = j
            continue
        # Character literal: #\X or #\Name (e.g. #\Space).  This must
        # come BEFORE the string branch, because #\" is a character
        # literal for the double-quote character and without handling it
        # our "..." scanner would see it as opening a new string and then
        # miscount every following quote.
        if c == "#" and i + 1 < n and text[i + 1] == "\\":
            out.append("##")
            j = i + 2
            if j < n:
                out.append(" ")
                j += 1
                # Multi-char char names like #\Newline, #\Space.
                while j < n and text[j].isalpha():
                    out.append(" ")
                    j += 1
            i = j
            continue
        # Block comment: #| ... |#  (nestable in CL; we handle one level)
        if c == "#" and i + 1 < n and text[i + 1] == "|":
            depth = 1
            j = i + 2
            out.append("  ")
            while j < n and depth > 0:
                if text[j] == "#" and j + 1 < n and text[j + 1] == "|":
                    depth += 1
                    out.append("  ")
                    j += 2
                elif text[j] == "|" and j + 1 < n and text[j + 1] == "#":
                    depth -= 1
                    out.append("  ")
                    j += 2
                else:
                    out.append(" " if text[j] != "\n" else "\n")
                    j += 1
            i = j
            continue
        # String literal: "..." with \" escape
        if c == '"':
            out.append(" ")
            j = i + 1
            while j < n and text[j] != '"':
                if text[j] == "\\" and j + 1 < n:
                    out.append("  ")
                    j += 2
                else:
                    out.append(" " if text[j] != "\n" else "\n")
                    j += 1
            if j < n:
                out.append(" ")
                j += 1
            i = j
            continue
        out.append(c)
        i += 1
    return "".join(out)


def extract_definitions(path: str, text: str) -> dict:
    """
    Return a dict mapping NAME -> list of (line, def-form) for every top-level
    def-form found in this file.  Line is 1-based.
    """
    defs = defaultdict(list)
    for m in DEF_FORM_RE.finditer(text):
        def_form = m.group(1).lower()
        name = m.group(2)
        line = text.count("\n", 0, m.start()) + 1
        defs[name].append((line, def_form))
    return defs


def build_symbol_regex(names):
    """
    Build one big alternation regex matching any of NAMES as a whole Lisp symbol.
    Uses negative look-behind and look-ahead for symbol-constituent characters
    so "foo" doesn't match inside "foo-bar".
    """
    if not names:
        return None
    # Sort longest-first so the alternation prefers the most specific match,
    # though with proper anchoring this shouldn't matter.
    escaped = sorted((re.escape(n) for n in names), key=len, reverse=True)
    pattern = (
        r"(?<![" + SYMBOL_CHARS + r"])"
        r"(" + "|".join(escaped) + r")"
        r"(?![" + SYMBOL_CHARS + r"])"
    )
    return re.compile(pattern)


def find_references(text: str, rx, defined_in_this_file: set):
    """
    Yield (line, name, snippet) for every match of rx in text that is NOT a
    name defined in the current file.  Skips the defining occurrence of a
    name (e.g., the `foo` in `(defun foo ...)`).

    `defined_in_this_file` is the set of names locally defined; those are
    skipped because a file referring to its own definitions isn't an edge.
    """
    for m in rx.finditer(text):
        name = m.group(1)
        if name in defined_in_this_file:
            continue
        line = text.count("\n", 0, m.start()) + 1
        # Extract the enclosing line for context
        line_start = text.rfind("\n", 0, m.start()) + 1
        line_end = text.find("\n", m.end())
        if line_end < 0:
            line_end = len(text)
        snippet = text[line_start:line_end].strip()
        yield line, name, snippet


def analyze(paths, ignore_patterns=None, strip_strings=False,
            show_sites=False, symbols_only=False, quiet=False,
            min_defs=1):
    """Return exit code; 0 = no cross-file deps, 1 = some found."""
    ignore_res = [re.compile(p) for p in (ignore_patterns or [])]

    # Pass 1: load files and extract definitions.
    file_text = {}     # basename -> text
    file_path = {}     # basename -> original path
    defs_by_file = {}  # basename -> {name: [(line, def-form), ...]}
    name_to_files = defaultdict(set)   # name -> set of basenames defining it

    for p in paths:
        bn = file_basename(p)
        if bn in file_text:
            print(f"WARNING: duplicate basename {bn!r} from {p}; skipping",
                  file=sys.stderr)
            continue
        raw = read_file(p)
        text = strip_strings_and_comments(raw) if strip_strings else raw
        file_text[bn] = text
        file_path[bn] = p
        defs = extract_definitions(p, text)
        defs_by_file[bn] = defs
        for name in defs:
            name_to_files[name].add(bn)

    if not file_text:
        print("No files to analyze.", file=sys.stderr)
        return 0

    # Filter symbols:
    #   - skip names matched by --ignore regex
    #   - optionally require exactly min_defs definitions
    module_symbols = {}  # name -> defining-basename (only if unambiguous)
    ambiguous = []       # names defined in more than one file
    for name, files in name_to_files.items():
        if any(r.search(name) for r in ignore_res):
            continue
        if len(files) > 1:
            ambiguous.append((name, sorted(files)))
        if len(files) >= min_defs and len(files) == 1:
            module_symbols[name] = next(iter(files))

    # Build one regex matching any exported module symbol.
    rx = build_symbol_regex(list(module_symbols.keys()))

    # Pass 2: scan each file for references to symbols defined elsewhere.
    edges = defaultdict(lambda: defaultdict(list))
    # edges[caller_bn][callee_bn] -> [(line, name, snippet), ...]

    if rx is not None:
        for caller_bn, text in file_text.items():
            local_defs = set(defs_by_file[caller_bn].keys())
            for line, name, snippet in find_references(text, rx, local_defs):
                callee_bn = module_symbols[name]
                if callee_bn == caller_bn:
                    continue
                edges[caller_bn][callee_bn].append((line, name, snippet))

    # --- Report ---
    if not quiet:
        print("=== FILES ANALYZED ===")
        for bn in sorted(file_text):
            nd = len(defs_by_file[bn])
            print(f"  {bn}.lisp  ({nd} top-level definitions)")
        print()

        if ambiguous:
            print("=== AMBIGUOUS SYMBOLS (defined in multiple files) ===")
            for name, files in sorted(ambiguous):
                print(f"  {name:40}  in {', '.join(files)}")
            print(f"  {len(ambiguous)} ambiguous symbol(s) skipped.\n")

    if symbols_only:
        crossing = set()
        for bn in edges:
            for target in edges[bn]:
                for _, name, _ in edges[bn][target]:
                    crossing.add(name)
        for name in sorted(crossing):
            print(name)
        return 1 if crossing else 0

    if not edges:
        if not quiet:
            print("=== RESULT ===")
            print("No cross-file dependencies found among the input files.")
        return 0

    if not quiet:
        print("=== CROSS-FILE DEPENDENCIES ===")
    total_edges = 0
    for caller in sorted(edges):
        for callee in sorted(edges[caller]):
            hits = edges[caller][callee]
            total_edges += len(hits)
            unique_names = sorted({h[1] for h in hits})
            if quiet:
                print(f"{caller}.lisp -> {callee}.lisp  "
                      f"({len(hits)} site(s), {len(unique_names)} symbol(s)): "
                      f"{', '.join(unique_names)}")
                continue
            print(f"\n{caller}.lisp  ->  {callee}.lisp   "
                  f"({len(hits)} reference(s), {len(unique_names)} symbol(s))")
            for name in unique_names:
                name_hits = [h for h in hits if h[1] == name]
                print(f"  {name}  ({len(name_hits)} site(s))")
                if show_sites:
                    for line, _, snippet in name_hits:
                        print(f"    {caller}.lisp:{line}: {snippet}")

    if not quiet:
        print("\n=== SUMMARY ===")
        print(f"Total cross-file reference sites: {total_edges}")
        pairs = sum(1 for c in edges for _ in edges[c])
        print(f"Distinct (caller, callee) pairs:  {pairs}")

        # Cycle detection.
        cycles = []
        for a in edges:
            for b in edges[a]:
                if a < b and a in edges.get(b, {}):
                    cycles.append((a, b))
        if cycles:
            print(f"\n=== CYCLES ===")
            for a, b in cycles:
                print(f"  {a}.lisp  <->  {b}.lisp")
        else:
            print("\nNo cycles detected.")

    return 1


def expand_inputs(args):
    """Expand --glob, @file, --stdin, and plain paths into a flat path list."""
    paths = []
    for item in args.files:
        if item.startswith("@"):
            with open(item[1:], encoding="utf-8") as f:
                paths.extend(line.strip() for line in f if line.strip())
        else:
            paths.append(item)
    for pattern in args.glob or []:
        paths.extend(sorted(glob.glob(pattern)))
    if args.stdin:
        paths.extend(line.strip() for line in sys.stdin if line.strip())
    # Deduplicate while preserving order.
    seen, uniq = set(), []
    for p in paths:
        if p not in seen:
            seen.add(p)
            uniq.append(p)
    return uniq


def main():
    ap = argparse.ArgumentParser(
        description=(
            "Find cross-file symbol dependencies among a set of Common Lisp "
            "files by lexical scan (no Lisp image required)."
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    ap.add_argument("files", nargs="*",
                    help="Lisp source files to analyze.  Prefix with @ to "
                         "read a list of paths from a file.")
    ap.add_argument("--glob", action="append",
                    help="Glob pattern to add files (can repeat).")
    ap.add_argument("--stdin", action="store_true",
                    help="Also read file paths from stdin, one per line.")
    ap.add_argument("--ignore", action="append", default=[],
                    help="Regex of symbol names to skip (can repeat).")
    ap.add_argument("--show-sites", action="store_true",
                    help="Print every call site, not just counts.")
    ap.add_argument("--symbols-only", action="store_true",
                    help="Print only the set of cross-boundary symbol names.")
    ap.add_argument("--strip-strings-and-comments", dest="strip_strings",
                    action="store_true",
                    help="Best-effort removal of strings and comments before "
                         "scanning (reduces false positives).")
    ap.add_argument("--quiet", action="store_true",
                    help="One-line-per-edge output; skip file inventory.")
    ap.add_argument("--min-defs", type=int, default=1,
                    help="Minimum number of definitions for a name to count "
                         "(default: 1).  Ambiguous names (>1) are always "
                         "skipped and reported separately.")
    args = ap.parse_args()

    paths = expand_inputs(args)
    if not paths:
        ap.error("No input files.  Pass paths, use --glob, --stdin, or @file.")

    missing = [p for p in paths if not os.path.isfile(p)]
    if missing:
        for p in missing:
            print(f"ERROR: not a file: {p}", file=sys.stderr)
        return 2

    return analyze(
        paths,
        ignore_patterns=args.ignore,
        strip_strings=args.strip_strings,
        show_sites=args.show_sites,
        symbols_only=args.symbols_only,
        quiet=args.quiet,
        min_defs=args.min_defs,
    )


if __name__ == "__main__":
    sys.exit(main())
