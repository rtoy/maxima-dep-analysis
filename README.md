# Maxima dependency-analysis tools

A set of tools developed during a cycle-breaking and module-organization
audit of Maxima's `src/`.  All but `cross-file-deps.py` and
`macro-deps.py` require a CMUCL image with Maxima built and loaded
(CMUCL is the only Lisp here that exposes a programmatically-queryable
xref database).

## Files

### Lex-based (Python — work without a Lisp image)

- **`cross-file-deps.py`** — Lex scanner for cross-file edges.  Indexes
  `(defmfun NAME ...)`, `(defun NAME ...)`, `(defmacro NAME ...)`, etc.,
  and finds calling references.  Works on any `.lisp` file but suffers
  from the usual lex limitations (no package-awareness without
  `--strip-strings-and-comments`, no awareness of binding contexts,
  etc.).  Useful for first-pass spot checks before booting CMUCL.

  Usage:
    python3 cross-file-deps.py --strip-strings-and-comments file1.lisp file2.lisp ...

- **`macro-deps.py`** — Lex scanner specialized for macro-only edges.
  Indexes `(defmacro NAME ...)`, finds `(NAME ` usages.  Filters CL-
  reserved names (incf, decf, when, ...) and cross-package matches
  via per-file `(in-package)` detection.  Output is a per-module
  MISSING/UNUSED report against `maxima.system`.

  Usage (run from src/):
    python3 macro-deps.py > /tmp/report.txt

### xref-based (Common Lisp — require running CMUCL with Maxima loaded)

- **`cross-file-deps-cmucl-v7.lisp`** — The main tool.  Loads CMUCL's
  `xref` package, queries `xref::*who-calls*` and friends, attributes
  callees to defining files via debug-info, and reports per-module
  inter-file edges with cycle detection.  Provides the `report-module`
  function.

  Usage:
    (load "cross-file-deps-cmucl-v7.lisp")
    (report-module '("file1" "file2" "file3")
                   :directory "./src/"
                   :kinds '(:macros :calls :refs :binds :sets))

- **`outbound-deps.lisp`** — Companion to v7.  Reports all outbound
  xref edges from a single file across the entire src/ tree, grouped
  by callee defining-file.  Used to determine what a single file
  actually depends on.

  Usage:
    (load "outbound-deps.lisp")
    (report-file-deps "scs" :src-dir "./src/")

- **`system-deps.lisp`** — Earlier system-wide audit tool.  Walks the
  live `mk:defsystem` component tree and runs xref against the whole
  source tree to produce per-module MISSING/UNUSED reports.  Gave
  noisy results (446 MISSING / 73 UNUSED on a build); superseded
  by `module-deps-audit.lisp`.

  Usage:
    (load "system-deps.lisp")
    (report-system-deps)

- **`module-deps-audit.lisp`** — System-wide audit using the lean
  `xref::find-xrefs-for-pathname` path that v7 and `outbound-deps`
  use.  Parses `maxima.system`, walks every file in every module,
  and reports per-module MISSING and UNUSED deps with edge counts.
  Cleaner than `system-deps.lisp`.

  Usage:
    (load "outbound-deps.lisp")
    (load "module-deps-audit.lisp")
    (audit-module-deps :system-file "./src/maxima.system"
                       :src-dir     "./src/")

## Limitations

These tools see what xref records: function calls, macroexpansions,
special-variable refs/binds/sets.  They do NOT see:

- Read-time effects (e.g. `compatibility-macros1` needing
  `float-format` because numeric literals must be read as
  `double-float`).
- Property-list dispatch (`(get sym 'some-prop)` patterns).
- Init-order dependencies via load-time `setf`-of-property forms
  whose results are consumed by later modules.
- `funcall` of dynamically-built symbol names.

In particular, the UNUSED column in the system-wide audits is NOT a
safe basis for removing declared deps without further investigation.
The MISSING column is more reliable (xref-recorded edges are real)
but can over-report (a runtime-only edge often works via load-order
even without explicit declaration).

For a documented example of an invisible dep that broke the build
when violated, see the `bind-fpprec` macro: defined in `nummod.lisp`,
macroexpanded by `hypergeometric.lisp` and `nfloat.lisp`.  Moving
`nummod` out of `miscellaneous` (which `hypergeometric` declares as
a dep) without also moving the consumer files breaks the bigfloat
hypergeometric path silently.  This was caught only by testsuite
failures, not by xref-based analysis run on master.

## Audit baseline

A run of `audit-module-deps` against `maxima.system` (after the
nummod-to-hypergeometric and rand-mt19937-to-random changes) showed
no module with MISSING xref edges, and only four modules with declared
deps that have no observed xref edges:

- `globals -> intl` -- globals.lisp calls `intl:gettext`.  When running
  the audit on CMUCL, this resolves to CMUCL's bundled intl (outside
  src/), so the src-dir filter drops the edge.  On other Lisps,
  Maxima's own intl.lisp provides the implementation and the dep is
  visibly load-bearing.  Either way the dep is real.

- `compatibility-macros1 -> float-format` -- float-format sets
  `*read-default-float-format*` to double-float; numeric literals read
  in dependent files depend on this state, but no xref edge exists.

- `numerical -> defmfun` -- the f2cl-translated SLATEC files.  Likely
  removable in principle; low-risk to keep.

- `numeric-bigfloat -> package, proclaim` -- bootstrap-order deps for
  the GCL/non-GCL conditional Lisp-package setup; no direct symbolic
  reference from numeric.lisp.

All four are explainable as legitimate non-xref-visible deps.  The
audit's main forward-looking use is as a baseline: if a future change
introduces an xref-visible dep that isn't declared, MISSING will flag
it; if a refactor drops a real edge, the corresponding entry will move
into the no-xref-edges list and warrant investigation before removal.
