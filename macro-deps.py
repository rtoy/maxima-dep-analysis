#!/usr/bin/env python3
"""
Lex-based macro dependency analyzer for Maxima.

Approach:
  1. Index every (defmacro NAME ...) form in every .lisp file in src/,
     keyed by macro name. We track the file each macro is defined in.
  2. For every file, scan its source for (NAME ...) usages where NAME
     is a known macro defined in another file -> file-level
     macro-usage edges.
  3. Parse maxima.system to map each file to its module and read each
     module's declared :depends-on.
  4. Aggregate file-level edges to module level. For each module,
     compare its actual macro-usage edges against (declared deps +
     transitive closure of declared deps).
  5. Report missing and unused (with caveats).

What this catches that xref doesn't:
  - Macros referenced inside files that aren't loaded into the live
    image.
  - Macros referenced inside quoted lists that get evaluated later.

What this misses that xref doesn't:
  - Function calls (this is macros only by design).
  - Special-variable references.
  - Package-qualified resolution: lex sees `(incf x)` and assumes it
    binds to whichever package's incf macro is defined; if multiple
    packages define a same-named macro, the lex tool will record edges
    to all of them (a known false positive source).

What neither catches:
  - Read-time effects (e.g., *read-default-float-format*).
  - Property-list dispatch.
  - Init-order dependencies between defvars.
"""

import os
import re
import sys
from collections import defaultdict, deque

SRC = "/home/claude/maxima/src"
SYSTEM_FILE = os.path.join(SRC, "maxima.system")

# ---------------------------------------------------------------------------
# 1. Read source files and strip strings + comments.
# ---------------------------------------------------------------------------

def strip_strings_and_comments(src):
    """Strip Lisp ;-comments and "..." string contents to avoid false
    positives. Keeps file structure intact (newlines preserved)."""
    out = []
    i, n = 0, len(src)
    while i < n:
        c = src[i]
        # Block comment #| ... |#
        if c == '#' and i + 1 < n and src[i+1] == '|':
            depth = 1
            i += 2
            while i < n and depth > 0:
                if src[i] == '#' and i + 1 < n and src[i+1] == '|':
                    depth += 1; i += 2
                elif src[i] == '|' and i + 1 < n and src[i+1] == '#':
                    depth -= 1; i += 2
                else:
                    if src[i] == '\n':
                        out.append('\n')
                    i += 1
            continue
        # Line comment ;... to EOL
        if c == ';':
            while i < n and src[i] != '\n':
                i += 1
            continue
        # String literal "..."
        if c == '"':
            out.append('"')
            i += 1
            while i < n:
                if src[i] == '\\' and i + 1 < n:
                    i += 2
                    continue
                if src[i] == '"':
                    out.append('"')
                    i += 1
                    break
                if src[i] == '\n':
                    out.append('\n')
                i += 1
            continue
        out.append(c)
        i += 1
    return ''.join(out)

def lisp_files_in(directory):
    out = []
    for entry in os.listdir(directory):
        full = os.path.join(directory, entry)
        if os.path.isfile(full) and entry.endswith('.lisp'):
            out.append(entry)
        elif os.path.isdir(full):
            for f in os.listdir(full):
                if f.endswith('.lisp'):
                    out.append(os.path.join(entry, f))
    return sorted(out)

print("Reading sources...", file=sys.stderr)
all_files = lisp_files_in(SRC)
content = {}
for relpath in all_files:
    full = os.path.join(SRC, relpath)
    try:
        with open(full, 'r', errors='replace') as fh:
            content[relpath] = strip_strings_and_comments(fh.read())
    except Exception as e:
        print(f"  warning: couldn't read {relpath}: {e}", file=sys.stderr)

print(f"  {len(content)} files read.", file=sys.stderr)

# ---------------------------------------------------------------------------
# Per-file package detection.
# ---------------------------------------------------------------------------
# A macro defined in package P called in package Q is suspicious unless
# Q :use's P. Most Maxima macros are in package :maxima. Detecting
# which package a file is in lets us filter out pure-name-collision
# false positives like (len ...) in intl.lisp where len happens to be a
# local variable, while the macro `len' lives in :maxima:spgcd.lisp.

PACKAGE_RE = re.compile(
    r'^\s*\(in-package\s+(?:#?\:|\#?\")?(\S+?)[\)\"]',
    re.IGNORECASE | re.MULTILINE
)

file_package = {}
for f, src in content.items():
    m = PACKAGE_RE.search(src)
    if m:
        pkg = m.group(1).lower().strip(':"')
        # Strip surrounding `#:'-style sigils that the regex left.
        pkg = pkg.lstrip('#').lstrip(':')
        file_package[f] = pkg
    else:
        file_package[f] = None  # unknown

# Summary of packages used.
from collections import Counter
pkg_counts = Counter(file_package.values())
print(f"  packages: {dict(pkg_counts)}", file=sys.stderr)

# ---------------------------------------------------------------------------
# 2. Index defmacro definitions.
# ---------------------------------------------------------------------------
# Macro name allowed chars: alphanumerics + - * + ! < > % # @ ? / $ &

NAME_CHARS = r'[\w\-\*\+\!<>%#@?/$&\.=]'
MACRO_DEF_RE = re.compile(
    r'\(defmacro\s+(' + NAME_CHARS + r'+)',
    re.IGNORECASE | re.MULTILINE
)

# A macro can be defined more than once across files (uncommon). Keep
# the first definition site, and warn if there are clashes.
macro_to_files = defaultdict(list)
for f, src in content.items():
    for m in MACRO_DEF_RE.finditer(src):
        name = m.group(1).lower()
        if f not in macro_to_files[name]:
            macro_to_files[name].append(f)

# Filter to single-defining-file: when a name is defined in N>1 files,
# we can't reliably attribute edges to any one of them, so we drop it
# from the analysis.
macro_to_file = {}
ambiguous = []
for name, files in macro_to_files.items():
    if len(files) == 1:
        macro_to_file[name] = files[0]
    else:
        ambiguous.append((name, files))

print(f"  {len(macro_to_file)} unambiguous macros indexed; "
      f"{len(ambiguous)} skipped due to multiple defining files.",
      file=sys.stderr)

# Filter out macros whose names are too short to be safely matched
# (single-char names like `a' would generate huge volumes of false
# positives). We require at least 2 chars OR a name that is unlikely
# to appear in incidental positions.
def safely_matchable(name):
    if len(name) >= 3:
        return True
    # 2-char names are still risky but often legitimate (e.g. `m1', `m2')
    # We allow them but they'll be more noisy.
    if len(name) == 2:
        return True
    return False

# CL standard macros and special operators. If a Maxima file defines a
# macro with one of these names (e.g. `incf' in the bigfloat package),
# every file in :maxima that uses cl:incf will look like it uses the
# bigfloat one. These are systematic false-positive generators, so we
# drop them from the macro index entirely.
CL_RESERVED = {
    'and', 'assert', 'block', 'case', 'catch', 'ccase', 'cerror', 'check-type',
    'cond', 'ctypecase', 'decf', 'declaim', 'declare', 'defclass', 'defconstant',
    'defgeneric', 'define-compiler-macro', 'define-condition',
    'define-method-combination', 'define-modify-macro', 'define-setf-expander',
    'define-symbol-macro', 'defmacro', 'defmethod', 'defpackage', 'defparameter',
    'defsetf', 'defstruct', 'deftype', 'defun', 'defvar', 'destructuring-bind',
    'do', 'do*', 'do-all-symbols', 'do-external-symbols', 'do-symbols', 'dolist',
    'dotimes', 'ecase', 'etypecase', 'eval-when', 'flet', 'formatter', 'function',
    'go', 'handler-bind', 'handler-case', 'if', 'ignore-errors', 'in-package',
    'incf', 'labels', 'lambda', 'let', 'let*', 'locally', 'loop', 'loop-finish',
    'macrolet', 'multiple-value-bind', 'multiple-value-call', 'multiple-value-list',
    'multiple-value-prog1', 'multiple-value-setq', 'nth-value', 'or', 'pop',
    'prog', 'prog*', 'prog1', 'prog2', 'progn', 'progv', 'psetf', 'psetq', 'push',
    'pushnew', 'quote', 'remf', 'restart-bind', 'restart-case', 'return',
    'return-from', 'rotatef', 'setf', 'setq', 'shiftf', 'step', 'symbol-macrolet',
    'tagbody', 'the', 'throw', 'time', 'trace', 'typecase', 'unless', 'untrace',
    'unwind-protect', 'when', 'with-accessors', 'with-compilation-unit',
    'with-condition-restarts', 'with-hash-table-iterator',
    'with-input-from-string', 'with-open-file', 'with-open-stream',
    'with-output-to-string', 'with-package-iterator', 'with-simple-restart',
    'with-slots', 'with-standard-io-syntax',
}

dropped_cl = []
filtered = {}
for name, f in macro_to_file.items():
    if not safely_matchable(name):
        continue
    if name in CL_RESERVED:
        dropped_cl.append((name, f))
        continue
    filtered[name] = f
macro_to_file = filtered

if dropped_cl:
    print(f"  {len(dropped_cl)} macros dropped because they shadow CL "
          f"reserved names:", file=sys.stderr)
    for name, f in dropped_cl:
        print(f"    {name} (defined in {f})", file=sys.stderr)

print(f"  {len(macro_to_file)} after CL-shadow + length filter.",
      file=sys.stderr)

# ---------------------------------------------------------------------------
# 3. Find macro usage edges.
# ---------------------------------------------------------------------------
# A "usage" of macro NAME in file F is a substring `(NAME` followed by
# a non-name character (whitespace, `)`, `(`, `'`, etc.) in F's source.

# Build one big regex per file is too noisy. Instead, build a single
# alternation over all known macro names and scan each file's text once.

# Sort macros longest-first so the alternation prefers longer matches.
sorted_macros = sorted(macro_to_file.keys(), key=lambda x: -len(x))
# Escape and join. Anchor: opening paren, then macro name, then a
# *whitespace* terminator. We deliberately exclude `)` from the
# lookahead set, because patterns like `((mexpt) ...)' are Maxima's
# symbolic-expression notation -- the inner `(mexpt)' is the operator
# symbol, not a macro call. Real macro calls always have whitespace
# (or possibly comments) before their arguments.
USAGE_RE = re.compile(
    r'\((' +
    '|'.join(re.escape(n) for n in sorted_macros) +
    r')(?=\s)',
    re.IGNORECASE
)

print("Scanning for macro usages...", file=sys.stderr)
file_macro_uses = defaultdict(lambda: defaultdict(int))   # file -> {(callee_file, macro_name): count}
total_edges = 0
filtered_by_pkg = 0
for f, src in content.items():
    caller_pkg = file_package.get(f)
    for m in USAGE_RE.finditer(src):
        name = m.group(1).lower()
        deffile = macro_to_file.get(name)
        if not deffile or deffile == f:
            continue
        # Package check: a macro defined in package P is only callable
        # from package Q if Q == P (or Q :uses P, which we don't model
        # here; in Maxima nearly all macro use is intra-:maxima so the
        # simple check suffices).
        callee_pkg = file_package.get(deffile)
        if caller_pkg and callee_pkg and caller_pkg != callee_pkg:
            filtered_by_pkg += 1
            continue
        file_macro_uses[f][(deffile, name)] += 1
        total_edges += 1

print(f"  {total_edges} macro-use sites in {len(file_macro_uses)} files "
      f"({filtered_by_pkg} cross-package matches filtered out).",
      file=sys.stderr)

# ---------------------------------------------------------------------------
# 4. Parse maxima.system: map files -> modules; read declared :depends-on.
# ---------------------------------------------------------------------------

def parse_maxima_system(path):
    """Returns (modules, file_to_module).
    modules: list of dicts with keys 'name', 'depends_on', 'files'.
    file_to_module: dict mapping each component file (with .lisp suffix)
    to its owning module name."""
    with open(path) as f:
        text = f.read()
    # Strip line comments to keep things sane.
    text = re.sub(r';[^\n]*', '', text)
    modules = []
    # Find each (:module NAME ...) form. The `:module` keyword always
    # appears immediately after a `(` (with possible whitespace).
    for m in re.finditer(r':module\s+(\S+)', text):
        name = m.group(1).strip().lower()
        # Walk back from m.start() over whitespace to find the `(` that
        # opens this form.
        k = m.start() - 1
        while k >= 0 and text[k] in ' \t\n\r':
            k -= 1
        if k < 0 or text[k] != '(':
            # Not at the start of an s-expression -- skip; could be a
            # comment fragment we missed.
            continue
        open_idx = k
        # Find matching close paren.
        depth = 1
        j = open_idx + 1
        in_string = False
        while j < len(text) and depth > 0:
            ch = text[j]
            if in_string:
                if ch == '\\' and j + 1 < len(text):
                    j += 2; continue
                if ch == '"':
                    in_string = False
            else:
                if ch == '"':
                    in_string = True
                elif ch == '(':
                    depth += 1
                elif ch == ')':
                    depth -= 1
                    if depth == 0:
                        break
            j += 1
        body = text[open_idx + 1: j]
        # Within body, find :depends-on (...) -- a single-level paren.
        depends_on = []
        deps_match = re.search(r':depends-on\s*\(', body)
        if deps_match:
            ds = deps_match.end() - 1   # the `(`
            d2 = 1
            di = ds + 1
            while di < len(body) and d2 > 0:
                if body[di] == '(':
                    d2 += 1
                elif body[di] == ')':
                    d2 -= 1
                    if d2 == 0: break
                di += 1
            deps_body = body[ds+1:di]
            depends_on = [s.lower() for s in re.findall(r'"([^"]+)"', deps_body)]
        # Within body, find :components (...).
        files = []
        comp_match = re.search(r':components\s*\(', body)
        if comp_match:
            cs = comp_match.end() - 1   # the `(`
            d2 = 1
            ci = cs + 1
            while ci < len(body) and d2 > 0:
                if body[ci] == '(':
                    d2 += 1
                elif body[ci] == ')':
                    d2 -= 1
                    if d2 == 0: break
                ci += 1
            components_body = body[cs+1:ci]
            for fm in re.finditer(r'\((?::file|:private-file)\s+"([^"]+)"',
                                  components_body):
                files.append(fm.group(1) + '.lisp')
        modules.append({'name': name,
                        'depends_on': depends_on,
                        'files': files})
    file_to_module = {}
    for mod in modules:
        for f in mod['files']:
            file_to_module[f] = mod['name']
    return modules, file_to_module

print("Parsing maxima.system...", file=sys.stderr)
modules, file_to_module = parse_maxima_system(SYSTEM_FILE)
print(f"  {len(modules)} modules, {len(file_to_module)} files mapped.",
      file=sys.stderr)

# Some files in macro_to_file might not be in any module (rare; e.g.,
# files with read-time conditionals that excluded them from the system,
# or skipped by ASDF entirely). Identify them.
unmapped = sorted(f for f in content if f not in file_to_module)
if unmapped:
    print(f"  {len(unmapped)} source files not in any module: ",
          file=sys.stderr)
    for f in unmapped[:10]:
        print(f"    {f}", file=sys.stderr)
    if len(unmapped) > 10:
        print(f"    ... ({len(unmapped) - 10} more)", file=sys.stderr)

# ---------------------------------------------------------------------------
# 5. Compute module-level macro edges and diff against declared deps.
# ---------------------------------------------------------------------------

# For each module M, collect: set of foreign modules whose files are
# the targets of M's macro usages.
module_targets = defaultdict(set)   # mod -> set of (target_mod, [(callee_file, macro, count), ...])
module_target_detail = defaultdict(lambda: defaultdict(list))
# detail: mod -> target_mod -> list of (caller_file, callee_file, macro, count)

for caller_file, edges in file_macro_uses.items():
    src_mod = file_to_module.get(caller_file)
    if not src_mod:
        continue
    for (callee_file, macro), count in edges.items():
        tgt_mod = file_to_module.get(callee_file)
        if not tgt_mod or tgt_mod == src_mod:
            continue
        module_target_detail[src_mod][tgt_mod].append(
            (caller_file, callee_file, macro, count)
        )

# Transitive closure of declared deps.
def transitive_deps(mod_name, modules_index):
    seen = set()
    stack = list(modules_index.get(mod_name, {}).get('depends_on', []))
    while stack:
        d = stack.pop()
        if d in seen:
            continue
        seen.add(d)
        stack.extend(modules_index.get(d, {}).get('depends_on', []))
    return seen

modules_index = {m['name']: m for m in modules}

print()
print("=" * 72)
print("MACRO-DEPENDENCY ANALYSIS (lex-based)")
print("=" * 72)
print()
print("KNOWN LIMITATIONS:")
print("  This is a regex scan of (defmacro NAME ...) sites and (NAME ...)")
print("  uses. It cannot tell apart:")
print("    - a macro call (foo x y)")
print("    - a quoted list element '((foo x y) ...)")
print("    - a Maxima symbolic-expression operator like ((foo) x y)")
print("    - a let-binding clause (foo (some-expr))")
print("    - an assoc-list pair (foo bar)")
print("  Common false positives in Maxima:")
print("    - mexpt: defined as a local macro in hyp.lisp, but appears")
print("      everywhere as the Maxima exponentiation operator symbol.")
print("    - num/denom/pow/term: short generic names also used as")
print("      variables and in Maxima expression head positions.")
print("    - simp: a Maxima form marker used in symbolic expressions.")
print("    - table: a 1-line macro in todd-coxeter.lisp; very common")
print("      variable name.")
print()
print("  Confident MISSING signals are those naming distinctive macros")
print("  unlikely to clash, e.g. 'errcatch', 'declare-top', 'signp',")
print("  'destructuring-let*', 'msetq', 'm*', 'm^t'.")
print()

n_clean = 0
n_with_issues = 0
total_missing = 0
total_unused = 0

for mod in modules:
    name = mod['name']
    declared = set(mod['depends_on'])
    transitive = transitive_deps(name, modules_index)
    actual = set(module_target_detail.get(name, {}).keys())
    missing = sorted(actual - transitive)
    unused = sorted(declared - actual)
    if not missing and not unused:
        n_clean += 1
        continue
    n_with_issues += 1
    total_missing += len(missing)
    total_unused += len(unused)
    print(f"--- {name} ---")
    print(f"  declared: {' '.join(sorted(declared)) or '(none)'}")
    print(f"  actual:   {' '.join(sorted(actual)) or '(none)'}")
    if missing:
        print(f"  MISSING (real macro uses, not in declared+transitive):")
        for tgt in missing:
            sites = module_target_detail[name][tgt]
            # Aggregate by (caller_file, macro) for readable display
            by_macro = defaultdict(int)
            for caller_file, callee_file, macro, count in sites:
                by_macro[(caller_file, callee_file, macro)] += count
            example_lines = sorted(by_macro.items(),
                                   key=lambda kv: -kv[1])[:3]
            example_str = ', '.join(
                f"{caller}:{macro}->{callee}" + (f"({n})" if n > 1 else "")
                for ((caller, callee, macro), n) in example_lines
            )
            print(f"    -> {tgt}  [{example_str}]")
    if unused:
        print(f"  UNUSED (declared but no macro usage from this module):")
        print(f"    {' '.join(unused)}")
    print()

print("=" * 72)
print(f"SUMMARY: {len(modules)} modules total, "
      f"{n_clean} clean, {n_with_issues} with discrepancies")
print(f"  total MISSING: {total_missing}")
print(f"  total UNUSED:  {total_unused}")
print("=" * 72)
