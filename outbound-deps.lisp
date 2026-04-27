;;; outbound-deps.lisp
;;;
;;; Report all outbound xref edges originating in a single file.
;;; Pure xref-database query: doesn't read any source files, doesn't
;;; iterate the source tree.  Just asks CMUCL's xref what it knows.
;;;
;;; Usage (after Maxima is built and loaded into CMUCL):
;;;
;;;   (load "outbound-deps.lisp")
;;;   (report-file-deps "/path/to/maxima/src/scs.lisp")
;;;
;;; Or with just the basename if SRC-DIR is given:
;;;
;;;   (report-file-deps "scs" :src-dir "/path/to/maxima/src/")

(in-package :cl-user)

(defun callee-defining-file (sym)
  "Return the truename of the file SYM is defined in (function or
macro), or NIL if unknown.  Asks CMUCL's debug-info; no source read.

For macros, uses MACRO-FUNCTION rather than FDEFINITION, because
CMUCL's FDEFINITION on a macro returns a generic expander dispatch
function in eval.lisp rather than the user's defining file."
  (handler-case
      (let* ((mf (and (fboundp sym) (macro-function sym)))
             (fn (or mf
                     (and (fboundp sym) (fdefinition sym))))
             (df (and fn (di:function-debug-function fn)))
             (start (and df (di:debug-function-start-location df)))
             (src (and start (di:code-location-debug-source start)))
             (name (and src
                        ;; debug-source-name returns whatever was used
                        ;; to compile (a pathname for :file, otherwise
                        ;; junk).
                        (eq (di:debug-source-from src) :file)
                        (di:debug-source-name src))))
        (and name (probe-file name)))
    (error () nil)))

(defun report-file-deps (file-or-basename
                         &key src-dir
                              (kinds '(:calls :macroexpands :references
                                       :binds :sets))
                              (only-in-src-dir t))
  "Report every outbound xref edge from FILE-OR-BASENAME, grouped by
callee defining-file.

When ONLY-IN-SRC-DIR is true (the default) and SRC-DIR is given, edges
to files outside SRC-DIR are dropped from the report.  This filters
out CMUCL's own runtime sources (eval.lisp, list.lisp, etc.) which
otherwise dominate the output."
  (let* ((target-path
          (cond ((probe-file file-or-basename)
                 (truename file-or-basename))
                (src-dir
                 (truename
                  (merge-pathnames
                   (concatenate 'string file-or-basename ".lisp")
                   (pathname src-dir))))
                (t (error "Can't find ~A" file-or-basename))))
         (src-dir-truename (and src-dir only-in-src-dir
                                (truename src-dir)))
         (src-dir-string (and src-dir-truename
                              (namestring src-dir-truename)))
         (by-callee-file (make-hash-table :test 'equal))
         (filtered-out 0)
         (unknown '())
         (total 0))
    (dolist (kind kinds)
      (dolist (entry (xref::find-xrefs-for-pathname kind target-path))
        (let* ((callee (first entry))
               (contexts (second entry))
               (callee-file (callee-defining-file callee)))
          (incf total (length contexts))
          (cond
            ((null callee-file)
             (push (list kind callee (length contexts)) unknown))
            ((equal callee-file target-path)
             nil)   ; intra-file, skip
            ((and src-dir-string
                  (not (search src-dir-string
                               (namestring callee-file))))
             (incf filtered-out (length contexts)))
            (t
             (dolist (ctx contexts)
               (push (list kind callee (xref::xref-context-name ctx))
                     (gethash callee-file by-callee-file))))))))
    (format t "~%=== OUTBOUND XREF EDGES FROM ~A ===~%"
            (file-namestring target-path))
    (format t "Total: ~D edge~:P across ~D distinct target file~:P~%"
            total (hash-table-count by-callee-file))
    (when (plusp filtered-out)
      (format t "(filtered out ~D edge~:P to files outside ~A)~%"
              filtered-out (namestring src-dir-truename)))
    (terpri)
    (let ((entries '()))
      (maphash (lambda (k v) (push (cons k v) entries)) by-callee-file)
      (setq entries (sort entries
                          (lambda (a b)
                            (> (length (cdr a)) (length (cdr b))))))
      (dolist (e entries)
        (let ((tn (car e)) (edges (cdr e)))
          (format t "  -> ~A  (~D edge~:P)~%"
                  (file-namestring tn) (length edges))
          (let ((counts (make-hash-table :test 'equal)))
            (dolist (edge edges)
              (incf (gethash (cons (first edge) (second edge))
                             counts 0)))
            (let ((rows '()))
              (maphash (lambda (k v) (push (cons k v) rows)) counts)
              (setq rows (sort rows
                               (lambda (a b) (> (cdr a) (cdr b)))))
              (dolist (r (subseq rows 0 (min 5 (length rows))))
                (format t "       ~A ~A (~D)~%"
                        (car (car r)) (cdr (car r)) (cdr r)))
              (when (> (length rows) 5)
                (format t "       ... and ~D more~%"
                        (- (length rows) 5))))))))
    (when unknown
      (format t "~%~D edge~:P to symbols with unknown defining-file ~
(CL/built-ins, gensyms, etc.)~%" (length unknown)))
    (values)))
