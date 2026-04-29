;;; Intra-module (and general cross-file) dependency analysis for CMUCL.
;;;
;;; Version 7.  Special-variable defining-file lookup now uses CMUCL's
;;; native source-location info (which is recorded for defvar,
;;; defparameter, defconstant, defmvar) when available.  Only when
;;; CMUCL's info doesn't have it (typically because the symbol was
;;; proclaimed special via `(proclaim '(special X))' or maxima's
;;; `(declare-top (special X))', neither of which records source
;;; location) do we fall back to lexically scanning files in the
;;; directory.  Files are scanned just-in-time and cached, so most
;;; queries don't trigger any I/O.
;;;
;;; Built entirely on CMUCL's xref database.  Now that
;;; preprocessor-macroexpand registers toplevel macro uses (rtoy's
;;; CMUCL fix), the lexical pass v4 needed for toplevel macros is
;;; gone.  v5 adds three new kinds for special-variable usage:
;;;
;;;   :calls    in-function function calls       (xref :calls)
;;;   :macros   macro expansions                  (xref :macroexpands)
;;;   :refs     special-variable reads            (xref :references)
;;;   :binds    special-variable rebindings (let) (xref :binds)
;;;   :sets     special-variable assignments      (xref :sets)
;;;
;;; The default kind set is (:calls :macros) -- the two compile-time
;;; or function-level dependencies that matter most for breaking
;;; intra-module cycles.  Variable-usage kinds are opt-in because they
;;; tend to add a lot of edges that aren't really compile-time
;;; significant; defvar declares the symbol special globally, and
;;; readers/binders/setters only need the variable bound at runtime,
;;; not the defvar visible at compile time.
;;;
;;; Usage:
;;;   (load "cross-file-deps-cmucl-v5.lisp")
;;;   (report-module '("mat" "linnew" "matrix") :directory "./src/")
;;;
;;;   (report-module ... :kinds '(:calls))
;;;   (report-module ... :kinds '(:macros))
;;;   (report-module ... :kinds '(:refs :binds :sets))
;;;   (report-module ... :kinds '(:calls :macros :refs :binds :sets))

(in-package :cl-user)

;;; ------------------------------------------------------------------
;;; Defining-file lookup
;;;
;;; For functions/macros, we use the function's debug-info source
;;; (authoritative for any compiled function loaded into the image).
;;;
;;; For special variables, we need a separate path because defvar et
;;; al. don't produce an fbound symbol.  We populate a lexical map by
;;; scanning the input files for top-level (def(var|parameter|
;;; constant|mvar) NAME ...) forms.
;;; ------------------------------------------------------------------

(defun function-defining-pathname (sym)
  (let ((fn (and (fboundp sym)
                 (or (macro-function sym) (fdefinition sym)))))
    (unless fn (return-from function-defining-pathname nil))
    (let ((code-obj (ignore-errors (kernel:function-code-header fn))))
      (unless code-obj (return-from function-defining-pathname nil))
      (let ((info (ignore-errors (kernel:%code-debug-info code-obj))))
        (unless (and info (typep info 'c::debug-info))
          (return-from function-defining-pathname nil))
        (dolist (src (c::debug-info-source info))
          (when (eq (c::debug-source-from src) :file)
            (let ((name (c::debug-source-name src)))
              (when name
                (return-from function-defining-pathname
                  (ignore-errors (truename name)))))))))))

(defun xref-defining-pathname (sym)
  "Fallback: scan all xref tables for a context whose name is SYM and
return its file.  Catches symbols whose function-cell isn't loaded in
this image but were registered as definers in some xref entry."
  (dolist (ctx (xref:who-calls sym :reverse t))
    (let ((f (xref::xref-context-file ctx)))
      (when (and f (eq (xref::xref-context-name ctx) sym))
        (return-from xref-defining-pathname
          (ignore-errors (truename f))))))
  (dolist (tbl (list xref::*who-calls*
                     xref::*who-macroexpands*
                     xref::*who-references*
                     xref::*who-binds*
                     xref::*who-sets*))
    (maphash
     (lambda (key contexts)
       (declare (ignore key))
       (dolist (ctx contexts)
         (when (and (eq (xref::xref-context-name ctx) sym)
                    (xref::xref-context-file ctx))
           (return-from xref-defining-pathname
             (ignore-errors (truename (xref::xref-context-file ctx)))))))
     tbl))
  nil)

(defun defining-pathname (sym)
  (or (function-defining-pathname sym)
      (xref-defining-pathname sym)))

(defun normalize-to-truenames (file-refs)
  (let ((items (if (listp file-refs) file-refs (list file-refs))))
    (mapcar (lambda (f)
              (or (ignore-errors (truename f))
                  (error "Cannot resolve ~S to a truename." f)))
            items)))

;;; ------------------------------------------------------------------
;;; Lexical scan for special-variable definitions in the input files.
;;;
;;; xref doesn't record defvar/defparameter/defconstant/defmvar,
;;; nor declare-top special / proclaim special, because none of them
;;; go through the IR1 path that registers xref entries.  To resolve
;;; a special-variable reference back to its defining file we read
;;; each input file with READ and look for:
;;;
;;;   (defvar NAME ...)        ; CL standard
;;;   (defparameter NAME ...)
;;;   (defconstant NAME ...)
;;;   (defmvar NAME ...)       ; Maxima
;;;   (declare-top (special NAME...) ...)   ; Maxima -- proclaim wrapper
;;;   (proclaim '(special NAME...))         ; CL
;;;   (declaim (special NAME...))           ; CL
;;;
;;; The first defining file wins (so if a symbol is defvar'd in file A
;;; and re-declared special in file B, A is recorded).
;;; ------------------------------------------------------------------

(defparameter *def-variable-operators*
  '(defvar defparameter defconstant defmvar)
  "Top-level operators that define a special variable as their second
element.")

(defun special-names-from-decl-spec (spec)
  "If SPEC is `(special name1 name2 ...)' return the list of names.
Otherwise return nil."
  (when (and (consp spec)
             (symbolp (car spec))
             (string= (symbol-name (car spec)) "SPECIAL"))
    (remove-if-not #'symbolp (cdr spec))))

(defun scan-form-for-defined-vars (form)
  "Given a top-level FORM read from a file, return a list of symbols
defined as special variables by that form.  Recognises def(var|...),
declare-top, proclaim, and declaim."
  (cond
    ((not (and (consp form) (symbolp (car form))))
     nil)
    ((member (car form) *def-variable-operators* :test #'eq)
     (and (symbolp (second form)) (list (second form))))
    ;; (declare-top (special ...) (special ...) ...)
    ((string= (symbol-name (car form)) "DECLARE-TOP")
     (loop for spec in (cdr form) nconc (special-names-from-decl-spec spec)))
    ;; (declaim (special ...) (special ...) ...)
    ((eq (car form) 'declaim)
     (loop for spec in (cdr form) nconc (special-names-from-decl-spec spec)))
    ;; (proclaim '(special ...))
    ((eq (car form) 'proclaim)
     (let ((arg (second form)))
       (when (and (consp arg) (eq (car arg) 'quote))
         (special-names-from-decl-spec (second arg)))))
    (t nil)))

(defun scan-file-for-defvars (truename)
  "Return a list of (SYM . TRUENAME) pairs for every special variable
defined at top level in the given file.  Robust against the file's
package; uses *package* :maxima if no in-package found.  Reader
errors on individual forms are skipped.  Stream-of-warnings (CMUCL's
'unmatched close parenthesis' diagnostics, etc.) are suppressed --
they're not actionable for our purpose."
  (let ((result '())
        (*package* (find-package :maxima))
        ;; Silence CMUCL reader warnings (e.g. about extraneous close
        ;; parens).  These go to *error-output*, not the condition
        ;; system.  Redirect both standard streams to a sink during
        ;; the scan.
        (*error-output* (make-broadcast-stream))
        (*standard-output* (make-broadcast-stream)))
    (with-open-file (s truename :direction :input)
      (loop
        (let ((form (handler-case (read s nil :eof)
                      (error () :error))))
          (cond ((eq form :eof) (return))
                ((eq form :error)
                 ;; Skip to the next top-level form.  The reader has
                 ;; consumed up to the offending character.  Just
                 ;; continue; if read keeps failing we'll eventually
                 ;; hit :eof.
                 nil)
                ((and (consp form) (symbolp (car form))
                      (eq (car form) 'in-package))
                 (let ((p (find-package (second form))))
                   (when p (setq *package* p))))
                (t
                 (dolist (sym (scan-form-for-defined-vars form))
                   (push (cons sym truename) result)))))))
    (nreverse result)))

;;; ------------------------------------------------------------------
;;; Lazy defvar-table resolver.
;;;
;;; The eager build-defvar-table from v6 read every .lisp file in the
;;; directory upfront, even when most queries are :calls/:macros which
;;; don't need it at all.  v7 instead uses CMUCL's native source
;;; tracking when available, and falls back to scanning files lazily
;;; on a per-symbol basis -- and only for symbols where it's needed.
;;;
;;; Key idea:
;;;   - CMUCL records source-location for defvar/defparameter/defconstant
;;;     /defmvar at compile time.  `(c::info source-location defvar SYM)'
;;;     returns a `c::file-source-location' whose `pathname' slot is the
;;;     defining file.  Direct query, no I/O.
;;;
;;;   - For symbols proclaimed special some other way (`proclaim',
;;;     `declaim', or maxima's `declare-top special'), CMUCL doesn't
;;;     record source location.  Those need a lexical scan.
;;;
;;;   - We scan files JIT, caching the per-file
;;;     symbol->file map.  Most audits never touch the lexical scan;
;;;     even those that do typically only scan a handful of files.
;;;
;;; The resolver is a closure with state.  Returns a function that
;;; accepts a symbol and returns the symbol's defining truename (or
;;; nil), using the cheapest method that produces an answer.
;;; ------------------------------------------------------------------

(defun cmucl-defvar-source-pathname (sym)
  "Use CMUCL's native source-location tracking to find SYM's defining
file, if it was defined via defvar/defparameter/defconstant/defmvar
(all of which call (c:source-location) at expansion time).  Returns a
truename or nil."
  (let ((loc (ignore-errors (c::info :source-location :defvar sym))))
    (when (and loc (typep loc 'c::file-source-location))
      (let ((path (c::file-source-location-pathname loc)))
        (when path
          (ignore-errors (truename (pathname path))))))))

(defun cmucl-symbol-is-special-p (sym)
  "True if CMUCL globaldb records SYM as :special-kind."
  (eq :special (ignore-errors (c::info :variable :kind sym))))


(defun make-defvar-resolver (&key scan-directory truenames)
  "Return a closure that maps a symbol to its defining-file truename
for special-variable purposes.  SCAN-DIRECTORY (if non-nil) is the
directory whose .lisp files will be lazily scanned when CMUCL's native
source-location info is unavailable.  TRUENAMES is the audit set,
used only to prefer scanning input files first."
  (let ((sym->file (make-hash-table :test 'eq))
        (scanned (make-hash-table :test 'equal))
        (input-set (make-hash-table :test 'equal))
        (scan-dir (cond ((null scan-directory) nil)
                        ((eq scan-directory t)
                         (and (first truenames)
                              (make-pathname :name nil :type nil
                                             :defaults (first truenames))))
                        (t (pathname scan-directory))))
        (scan-queue truenames)
        (extras-queued nil))
    (dolist (tn truenames) (setf (gethash tn input-set) t))
    (labels
        ((record-from-form (sym tn)
           (unless (gethash sym sym->file)
             (setf (gethash sym sym->file) tn)))
         (scan-one-file (tn)
           (unless (gethash tn scanned)
             (setf (gethash tn scanned) t)
             (handler-case
                 (dolist (entry (scan-file-for-defvars tn))
                   (record-from-form (car entry) (cdr entry)))
               (error () nil))))
         (queue-extras ()
           (when (and scan-dir (not extras-queued))
             (setf extras-queued t)
             (dolist (extra (directory
                             (make-pathname :name :wild :type "lisp"
                                            :defaults scan-dir)))
               (let ((etn (ignore-errors (truename extra))))
                 (when (and etn (not (gethash etn scanned)))
                   (setf scan-queue (nconc scan-queue (list etn))))))))
         (scan-next ()
           (loop
             (when (null scan-queue)
               (queue-extras)
               (when (null scan-queue) (return nil)))
             (let ((tn (pop scan-queue)))
               (unless (gethash tn scanned)
                 (scan-one-file tn)
                 (return t))))))
      (lambda (sym)
        (block resolve
          ;; 1. Native CMUCL source-location: fastest, no I/O.
          (let ((tn (cmucl-defvar-source-pathname sym)))
            (when tn (return-from resolve tn)))
          ;; 2. Already cached?
          (multiple-value-bind (tn found) (gethash sym sym->file)
            (when found (return-from resolve tn)))
          ;; 3. Only scan if the symbol is actually special globally.
          (unless (cmucl-symbol-is-special-p sym)
            (return-from resolve nil))
          ;; 4. JIT scan until we find or run out.
          (loop
            (multiple-value-bind (tn found) (gethash sym sym->file)
              (when found (return-from resolve tn)))
            (unless (scan-next)
              (return-from resolve nil))))))))

;;; ------------------------------------------------------------------
;;; Edge collection from xref
;;; ------------------------------------------------------------------

(defstruct edge
  caller-file caller-name callee-file callee-name kind)

(defparameter *kind-to-db-key*
  '((:calls     . :calls)
    (:macros    . :macroexpands)
    (:refs      . :references)
    (:binds     . :binds)
    (:sets      . :sets))
  "Mapping from user-facing :kinds to the underlying xref DB keys.")

(defparameter *kind-to-edge-tag*
  '((:calls  . :call)
    (:macros . :macro)
    (:refs   . :ref)
    (:binds  . :bind)
    (:sets   . :set))
  "Mapping from user-facing :kinds to the per-edge :kind tag we store.")

(defun build-sym->file (truenames resolver kinds)
  "Resolve each symbol that appears as a target in any xref DB to its
defining file, returning a hash table.  Only walks db-keys
corresponding to the requested KINDS, so e.g. a :macros-only query
doesn't trigger queries against :references/:binds/:sets that might
needlessly invoke special-variable resolution.

For each symbol encountered:
  1. Call RESOLVER (a closure from MAKE-DEFVAR-RESOLVER) -- this
     handles special-variable defining files using CMUCL's native
     source-location info or a JIT lexical scan.  If RESOLVER returns
     a truename in the audit set, use that.
  2. Otherwise fall back to FUNCTION-DEFINING-PATHNAME (debug-info)
     and XREF-DEFINING-PATHNAME (xref's own context tables).  These
     cover fbound symbols: functions and macros.

If the resolver returns a truename outside the audit set, the symbol
is suppressed (no edge emitted, function-fallback can't override).
This is critical for the case of symbols that are both fbound (in the
audit set) AND proclaimed special (outside the audit set) -- we must
not attribute the edge to the function-defining file."
  (let ((tbl (make-hash-table :test 'eq))
        (set (make-hash-table :test 'equal))
        (suppressed (make-hash-table :test 'eq))
        (db-keys (loop for k in kinds
                       for db-key = (cdr (assoc k *kind-to-db-key*))
                       when db-key collect db-key)))
    (dolist (tn truenames) (setf (gethash tn set) t))
    (labels
        ((maybe-record (sym)
           (when (and (symbolp sym)
                      (not (gethash sym tbl))
                      (not (gethash sym suppressed)))
             (let ((var-tn (funcall resolver sym)))
               (cond ((null var-tn)
                      (let ((fn-tn (defining-pathname sym)))
                        (when (and fn-tn (gethash fn-tn set))
                          (setf (gethash sym tbl) fn-tn))))
                     ((gethash var-tn set)
                      (setf (gethash sym tbl) var-tn))
                     (t
                      (setf (gethash sym suppressed) t)))))))
      (dolist (tn truenames)
        (dolist (db-key db-keys)
          (dolist (entry (xref:find-xrefs-for-pathname db-key tn))
            (destructuring-bind (target matches) entry
              ;; Skip phantom entries with empty contexts -- see comment
              ;; in module-deps-audit.lisp:FILE-OUTBOUND-EDGES.
              (when matches
                (maybe-record target)
                (dolist (ctx matches)
                  (maybe-record (xref::xref-context-name ctx)))))))))
    tbl))

(defun xref-edges (truenames kinds resolver)
  "Walk each requested kind's xref DB for every input file's truename,
emitting an edge struct for each target whose defining file is also
in the set (and different from the caller-file)."
  (let ((sym->file (build-sym->file truenames resolver kinds))
        (edges '()))
    (dolist (caller-tn truenames)
      (dolist (kind kinds)
        (let ((db-key (cdr (assoc kind *kind-to-db-key*)))
              (edge-tag (cdr (assoc kind *kind-to-edge-tag*))))
          (when (and db-key edge-tag)
            (dolist (entry (xref:find-xrefs-for-pathname db-key caller-tn))
              (destructuring-bind (callee matches) entry
                (let ((callee-tn (gethash callee sym->file)))
                  (when (and callee-tn (not (equal callee-tn caller-tn)))
                    (dolist (ctx matches)
                      (push (make-edge
                             :caller-file caller-tn
                             :caller-name (xref::xref-context-name ctx)
                             :callee-file callee-tn
                             :callee-name callee
                             :kind edge-tag)
                            edges))))))))))
    edges))

(defun intra-set-edges (truenames &key (kinds '(:calls :macros))
                                       (scan-directory nil))
  (let ((resolver (make-defvar-resolver :scan-directory scan-directory
                                        :truenames truenames)))
    (xref-edges truenames kinds resolver)))

;;; ------------------------------------------------------------------
;;; Reporting
;;; ------------------------------------------------------------------

(defun file-label (tn)
  (let ((n (pathname-name tn)) (tp (pathname-type tn)))
    (if tp (format nil "~A.~A" n tp) n)))

(defun group-edges-by-pair (edges)
  (let ((tbl (make-hash-table :test 'equal)) (out '()))
    (dolist (e edges)
      (push e (gethash (list (edge-caller-file e) (edge-callee-file e)) tbl)))
    (maphash (lambda (k v) (push (cons k v) out)) tbl)
    out))

(defun edge-kinds-summary (edges)
  (let ((counts (make-hash-table :test 'eq))
        (parts '()))
    (dolist (e edges)
      (incf (gethash (edge-kind e) counts 0)))
    (dolist (spec '((:call  "call")
                    (:macro "macro (in-body)")
                    (:ref   "var read")
                    (:bind  "var bind")
                    (:set   "var set")))
      (let ((n (gethash (first spec) counts 0)))
        (when (plusp n)
          (push (format nil "~A ~A~:P" n (second spec)) parts))))
    (if parts
        (format nil "~{~A~^, ~}" (nreverse parts))
        "0 sites")))

(defun edge-callee-tag (hits)
  "For a set of hits with the same callee, build a string describing
the per-kind counts."
  (with-output-to-string (s)
    (let ((counts (make-hash-table :test 'eq))
          (sep ""))
      (dolist (e hits) (incf (gethash (edge-kind e) counts 0)))
      (dolist (spec '((:call  "fn call")
                      (:macro "in-body macro")
                      (:ref   "var read")
                      (:bind  "var bind")
                      (:set   "var set")))
        (let ((n (gethash (first spec) counts 0)))
          (when (plusp n)
            (format s "~A~A ~A~:P" sep n (second spec))
            (setq sep ", ")))))))

(defun report-cross-file-deps (file-refs &key show-sites
                                              (kinds '(:calls :macros))
                                              (scan-directory nil)
                                              (stream t))
  (let* ((truenames (normalize-to-truenames file-refs))
         (edges (intra-set-edges truenames :kinds kinds
                                           :scan-directory scan-directory))
         (by-pair (group-edges-by-pair edges)))
    (format stream "~&=== CROSS-FILE DEPENDENCY REPORT ===~%")
    (format stream "Kinds examined: ~{~A~^ ~}~%" kinds)
    (format stream "Files considered (~A):~%" (length truenames))
    (dolist (tn truenames) (format stream "  ~A~%" tn))
    (format stream "~%Total cross-file sites: ~A  (~A)~%"
            (length edges) (edge-kinds-summary edges))
    (format stream "Distinct (caller -> callee) file pairs: ~A~%~%"
            (length by-pair))

    (dolist (pair (sort (copy-list by-pair) #'string<
                        :key (lambda (p)
                               (format nil "~A ~A"
                                       (file-label (first  (car p)))
                                       (file-label (second (car p)))))))
      (destructuring-bind ((caller-tn callee-tn) . items) pair
        (format stream "~A  ->  ~A   (~A)~%"
                (file-label caller-tn) (file-label callee-tn)
                (edge-kinds-summary items))
        (let ((callees (sort (remove-duplicates
                              (mapcar #'edge-callee-name items))
                             #'string<
                             :key (lambda (s)
                                    (if (symbolp s) (symbol-name s)
                                        (princ-to-string s))))))
          (dolist (callee callees)
            (let* ((hits (remove-if-not
                          (lambda (e) (equal (edge-callee-name e) callee))
                          items)))
              (format stream "    ~A  (~A)~%" callee (edge-callee-tag hits))
              (when show-sites
                (dolist (c (sort (remove-duplicates
                                  (mapcar #'edge-caller-name hits))
                                 #'string<
                                 :key (lambda (s)
                                        (if (symbolp s) (symbol-name s) ""))))
                  (format stream "        used by: ~A~%" c))))))
        (format stream "~%")))

    (format stream "=== CYCLES ===~%")
    (let ((seen (make-hash-table :test 'equal)) (cycles '()))
      (dolist (pair by-pair)
        (destructuring-bind (a b) (car pair)
          (let ((key (if (string< (file-label a) (file-label b))
                         (list a b) (list b a))))
            (when (and (not (equal a b))
                       (assoc (list b a) by-pair :test #'equal)
                       (not (gethash key seen)))
              (setf (gethash key seen) t)
              (push key cycles)))))
      (if cycles
          (dolist (c (sort (copy-list cycles) #'string<
                           :key (lambda (p) (file-label (first p)))))
            (format stream "  ~A  <->  ~A~%"
                    (file-label (first c)) (file-label (second c))))
          (format stream "  None.~%")))
    (format stream "~%=== END ===~%")
    edges))

(defun module-files (basenames &key directory (type "lisp"))
  (mapcar (lambda (b)
            (truename (make-pathname :name b :type type :defaults directory)))
          basenames))

(defun report-module (basenames &key directory show-sites
                                     (kinds '(:calls :macros))
                                     (scan-directory t))
  "Run a cross-file dependency report on BASENAMES (without `.lisp'),
located in DIRECTORY.

For special-variable defining-file lookup, v7 first tries CMUCL's
native `(c::info :source-location :defvar SYM)' (works for defvar,
defparameter, defconstant, defmvar -- no I/O).  If CMUCL doesn't have
the info, falls back to lazily reading .lisp files in DIRECTORY,
scanning only as many files as needed to answer outstanding queries.
Pass :scan-directory nil to disable the lexical fallback entirely
(may produce wrong file attribution for symbols proclaimed special
via `(declare-top (special ...))' or `(proclaim ...)')."
  (report-cross-file-deps
   (module-files basenames :directory directory)
   :show-sites show-sites
   :kinds kinds
   :scan-directory (if (eq scan-directory t) directory scan-directory)))

(format t "~%CROSS-FILE DEPENDENCY ANALYZER (CMUCL, v7) LOADED~%")
(format t "Kinds:~%")
(format t "  :calls    in-function function calls         (xref :calls)~%")
(format t "  :macros   macro expansions                    (xref :macroexpands)~%")
(format t "  :refs     special-variable reads              (xref :references)~%")
(format t "  :binds    special-variable rebindings (let)   (xref :binds)~%")
(format t "  :sets     special-variable assignments        (xref :sets)~%")
(format t "Default :kinds = (:calls :macros)~%")
(format t "Special-var defining files:~%")
(format t "  - CMUCL's native source-location info (for defvar etc.)~%")
(format t "  - lazy lexical scan fallback for declare-top/proclaim~%")
(format t "Example:~%")
(format t "  (report-module '(\"residu\" \"defint\" \"hypgeo\" \"laplac\")~%")
(format t "                 :directory \"./src/\")~%")
(format t "  (report-module '(\"residu\" \"defint\" \"hypgeo\" \"laplac\")~%")
(format t "                 :directory \"./src/\" :kinds '(:refs :binds :sets))~%")
