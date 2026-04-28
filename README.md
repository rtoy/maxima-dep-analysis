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

- **`compile-maxima-with-xref.lisp`** — Build helper.  Sets
  `c:*record-xref-info*` to T, initializes the xref database, loads
  `maxima.system`, and compiles Maxima via `mk:compile-system` with
  xref tracking enabled.  Run this once at the start to produce a
  CMUCL image whose xref database is populated; the other tools below
  query that database.

  Usage:
    (load "compile-maxima-with-xref.lisp")
    (compile-maxima-with-xref :maxima-src-dir "/path/to/maxima/src/")
    ;; optional: (save-xref-data "maxima-with-xref.core")

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
special-variable refs/binds/sets, and (verified) defconstant
references.  They do NOT see:

- Read-time effects (e.g. `compatibility-macros1` needing
  `float-format` because numeric literals must be read as
  `double-float`).
- **Deftype references.** xref records defconstant uses but does
  not record deftype uses.  A file with `(declare (type flonum
  x))` shows no xref edge to the file defining the `flonum`
  deftype.  Verified empirically: `(xref:who-references
  'maxima::flonum)` returns nothing even though `flonum` is used
  throughout Maxima.
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

## Module-level cycles

A run of `report-module-cycles` against the same tree finds one large
strongly-connected component plus a long list of directly-mutual
pairs.

### Strongly-connected components

The whole computational core forms a single SCC of 51 modules
(every module in the SCC can reach every other through some chain
of edges, not necessarily direct):

  info, utilities, maxima-language-compiler, definite-integration,
  variable-predicates, simp-utilities, solve, i-o, commands, reader,
  evaluator, taylor-series, basic-utilities, nformat, rational-functions,
  simplification, matrix-algebra, translated-packages, graphics-drivers,
  server, globals, compatibility-macros, float-properties,
  numeric-bigfloat, defmfun, trigonometry, limits, polynomial,
  algebraic-database, numerical-utilities, hypergeometric,
  fundamental-macros, integration, gamma-expint, bessel-functions,
  miscellaneous, pattern-matching, random, ifactor, factoring,
  other-macros, gcd, m2-pattern-matcher, display, special-functions,
  maxima-language-compiler-macros, determinants, debugging,
  utility-macros, documentation, poisson-series.

This isn't surprising: Maxima's simplifier, evaluator, type-checker,
and assumption database are mutually recursive by design.

### Directly-mutual pairs

`report-module-cycles` lists 126 mutually-calling module pairs.  The
heavy hitters reflect intentional core couplings; the lighter pairs
are sometimes single-function "leaks" worth investigating.  Top
pairs by total edge count:

| Pair                                          | Edges     |
|-----------------------------------------------|-----------|
| factoring <-> rational-functions              | 454 + 26  |
| simp-utilities <-> trigonometry               |  10 + 425 |
| miscellaneous <-> simp-utilities              | 423 + 5   |
| simp-utilities <-> simplification             |  44 + 289 |
| limits <-> simp-utilities                     | 219 + 1   |
| algebraic-database <-> simp-utilities         | 153 + 54  |
| commands <-> simp-utilities                   | 175 + 6   |
| rational-functions <-> simp-utilities         |  73 + 49  |
| numeric-bigfloat <-> simplification           | 103 + 10  |
| algebraic-database <-> miscellaneous          |  12 + 96  |
| gcd <-> rational-functions                    | 105 + 3   |
| evaluator <-> utilities                       |  91 + 4   |

(Run `report-module-cycles` for the full list of 126 pairs with
example caller/callee functions.)

### Notes on the cycle data

- xref's `:binds` and `:sets` edges include defstruct-slot
  references.  Some pairs that look like "cycles" in the report are
  actually structural — e.g., many pairs show up only because of
  `residu.lisp:RES` (a defstruct slot) being bound across files.
  This is a real xref-recorded edge but doesn't reflect runtime
  call-flow.

- Light pairs (1-3 edges in one direction) are often candidates for
  refactoring.  Examples worth a look:
    factoring -> limits     (1 edge: factor.lisp NIL binds limit::deg)
    factoring -> matrix-algebra  (1 edge: nalgfa BDISCR calls matrix DET)
    simplification -> special-functions  (1 edge: FPCATALAN1)
    display <-> simplification           (1 + 1 edges, bigfloat fmt)

  These could potentially be eliminated by moving small functions
  to a different module.
