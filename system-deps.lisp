;;; System-level dependency analyzer for Maxima's maxima.system.
;;;
;;; Builds on cross-file-deps-cmucl-v7.lisp.  For every :module in
;;; the loaded mk:defsystem, computes the set of foreign modules
;;; whose files are targets of edges originating from this module's
;;; files (across all five xref kinds: :calls, :macros, :refs,
;;; :binds, :sets) and diffs that against the declared :depends-on
;;; list.
;;;
;;; The module structure is read from the live mk:defsystem tree via
;;; (mk::get-system "maxima") -- no source-file parsing needed, no
;;; read-time-conditional issues.  The system must already be loaded
;;; into the running image (which it is in any image built from
;;; maxima.system).
;;;
;;; Reports per module:
;;;   MISSING:  edge target whose module is NOT in the declared
;;;             :depends-on closure (declared deps plus their
;;;             transitive deps).  These are real holes -- the build
;;;             system can't reach the target through any declared
;;;             chain.
;;;   UNUSED:   module declared in :depends-on but no edges go to it.
;;;             We do NOT subtract transitives here; an unused
;;;             direct dep might be deliberately documenting a build
;;;             contract even if no symbols are referenced.
;;;
;;; Caveats:
;;;
;;;   * UNUSED deps are often deliberate.  Boilerplate modules like
;;;     defmfun, compatibility-macros, prerequisites are typically
;;;     declared everywhere as build-discipline.  Eval, funcall of
;;;     dynamically-built symbols, and put/get-property dispatch are
;;;     invisible to xref.  Treat UNUSED as candidates for review,
;;;     not for blind removal.
;;;
;;;   * MISSING deps are real bugs.  The target module is reachable
;;;     by no declared path, yet code does call/refer to it.
;;;
;;; Usage:
;;;   (load "cross-file-deps-cmucl-v7.lisp")
;;;   (load "system-deps.lisp")
;;;   (report-system-deps)
;;;
;;; Optional keyword args:
;;;   :system-name    defaults to "maxima"
;;;   :kinds          defaults to all five (:calls :macros :refs :binds :sets)
;;;   :src-directory  source dir for special-var lex fallback (rare)
;;;   :show-clean     if t, also print modules with no missing/unused

(in-package :cl-user)

;;; ------------------------------------------------------------------
;;; Walking the live mk:defsystem
;;;
;;; mk:defsystem builds a tree of `make::component' structs.  Each
;;; component has a :type slot which is one of :defsystem, :module,
;;; :file, :private-file.  We walk the tree to collect every :module
;;; (with its :depends-on) and the leaf source files under it.
;;; ------------------------------------------------------------------

(defstruct module-info
  name                                  ; string
  depends-on                            ; list of strings
  files)                                ; list of source-file truenames

(defun mk-component-source-truename (comp)
  "Return TRUENAME of the source file for a :file or :private-file
component, or NIL if its file doesn't exist on disk."
  (let ((p (ignore-errors
             (make::component-full-pathname comp :source))))
    (when p (probe-file p))))

(defun mk-collect-leaf-truenames (comp)
  "Walk COMP recursively and return a flat list of source-file
truenames for every :file/:private-file leaf reachable from it.
Files that don't exist on disk are skipped."
  (let ((out '()))
    (labels ((walk (c)
               (case (make::component-type c)
                 ((:file :private-file)
                  (let ((tn (mk-component-source-truename c)))
                    (when tn (push tn out))))
                 ((:defsystem :system :subsystem :module)
                  (dolist (child (make::component-components c))
                    (walk child))))))
      (walk comp))
    (nreverse out)))

(defun mk-component-name-string (comp)
  (let ((n (make::component-name comp)))
    (string-downcase
     (cond ((stringp n) n)
           ((symbolp n) (symbol-name n))
           (t (princ-to-string n))))))

(defun mk-collect-modules (system-name)
  "Return a list of MODULE-INFO structs for every :module under
SYSTEM-NAME's mk:defsystem tree."
  (let ((sys (mk::get-system system-name)))
    (unless sys
      (error "No mk:defsystem named ~S is loaded.  Did you load maxima.system?"
             system-name))
    (let ((mods '()))
      (labels ((walk (c)
                 (case (make::component-type c)
                   (:module
                    (push (make-module-info
                           :name (mk-component-name-string c)
                           :depends-on
                           (mapcar (lambda (d)
                                     (string-downcase
                                      (cond
                                        ;; mk:defsystem resolves
                                        ;; :depends-on entries to the
                                        ;; actual component structs at
                                        ;; load time.  Pull the name
                                        ;; out.
                                        ((and (typep d 'make::component))
                                         (let ((n (make::component-name d)))
                                           (cond ((stringp n) n)
                                                 ((symbolp n) (symbol-name n))
                                                 (t (princ-to-string n)))))
                                        ((stringp d) d)
                                        ((symbolp d) (symbol-name d))
                                        (t (princ-to-string d)))))
                                   (make::component-depends-on c))
                           :files (mk-collect-leaf-truenames c))
                          mods))
                   ((:defsystem :system :subsystem)
                    (dolist (child (make::component-components c))
                      (walk child))))))
        (walk sys))
      (nreverse mods))))

;;; ------------------------------------------------------------------
;;; File -> module mapping
;;; ------------------------------------------------------------------

(defun build-file-truename->module (modules)
  "Build a hash mapping every file's truename to its owning module
name."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (m modules)
      (dolist (tn (module-info-files m))
        (setf (gethash tn tbl) (module-info-name m))))
    tbl))

(defun all-truenames (file->mod)
  (let ((out '()))
    (maphash (lambda (k v) (declare (ignore v)) (push k out)) file->mod)
    out))

(defun edges-by-source-module (edges file->mod)
  "Group EDGES by the module of the caller-file.  Returns a hash of
source-module-name -> list of edges."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (e edges)
      (let ((m (gethash (edge-caller-file e) file->mod)))
        (when m
          (push e (gethash m tbl)))))
    tbl))

(defun foreign-callee-modules (edges source-mod file->mod)
  "Given EDGES that all originate in SOURCE-MOD, return the set of
distinct other modules whose files are callees."
  (let ((seen (make-hash-table :test 'equal))
        (out '()))
    (dolist (e edges)
      (let ((m (gethash (edge-callee-file e) file->mod)))
        (when (and m (not (string= m source-mod))
                   (not (gethash m seen)))
          (setf (gethash m seen) t)
          (push m out))))
    (sort out #'string<)))

(defun module-by-name (name modules)
  (find name modules :key #'module-info-name :test #'string=))

(defun transitive-deps (name modules)
  "Return the set (list) of all modules transitively in NAME's
:depends-on closure (not including NAME itself).  Modules referenced
by name but not defined are silently skipped."
  (let ((seen (make-hash-table :test 'equal))
        (work (list name)))
    (loop while work do
      (let ((cur (pop work)))
        (let ((m (module-by-name cur modules)))
          (when m
            (dolist (d (module-info-depends-on m))
              (unless (gethash d seen)
                (setf (gethash d seen) t)
                (push d work)))))))
    (let ((out '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k out)) seen)
      (sort out #'string<))))

;;; ------------------------------------------------------------------
;;; Fast system-wide xref walker
;;;
;;; cross-file-deps-cmucl-v7's xref-edges calls
;;; xref:find-xrefs-for-pathname once per (file, kind) pair.  Each
;;; call does a full maphash over the corresponding xref hash table.
;;; For 280 files x 5 kinds, that's 1400 full DB walks (or 2800
;;; counting build-sym->file).  Maxima's xref tables have enough
;;; entries that this takes minutes.
;;;
;;; Below is a system-wide variant that walks each DB hash table
;;; *once* per kind (5 walks total), grouping entries by their
;;; caller-pathname as it goes.  Caller files outside our truename
;;; set are skipped on the spot.
;;; ------------------------------------------------------------------

(defun fast-system-xref-edges (truenames kinds resolver)
  "Like xref-edges but walks each xref DB only once per kind.  Vastly
faster for system-wide queries."
  (let* ((set (make-hash-table :test 'equal))
         (sym->file (make-hash-table :test 'eq))
         (suppressed (make-hash-table :test 'eq))
         ;; Memoize (truename PATHNAME) since the same caller-file
         ;; appears in many xref-context structs.
         (path-cache (make-hash-table :test 'equal))
         (edges '()))
    (dolist (tn truenames) (setf (gethash tn set) t))
    (labels
        ((cached-truename (path)
           (multiple-value-bind (val present) (gethash path path-cache)
             (cond (present val)
                   (t
                    (let ((tn (ignore-errors (truename path))))
                      (setf (gethash path path-cache) tn))))))
         (resolve-defining-file (sym)
           "Memoized lookup: SYM -> truename in our audit set, or NIL
if it's defined outside the set or undefined."
           (multiple-value-bind (val present) (gethash sym sym->file)
             (cond (present val)
                   ((gethash sym suppressed) nil)
                   (t
                    (let ((var-tn (funcall resolver sym)))
                      (cond ((null var-tn)
                             (let ((fn-tn (defining-pathname sym)))
                               (cond ((and fn-tn (gethash fn-tn set))
                                      (setf (gethash sym sym->file) fn-tn))
                                     (t
                                      (setf (gethash sym suppressed) t)
                                      nil))))
                            ((gethash var-tn set)
                             (setf (gethash sym sym->file) var-tn))
                            (t
                             (setf (gethash sym suppressed) t)
                             nil))))))))
      (dolist (kind kinds)
        (let ((db-key (cdr (assoc kind *kind-to-db-key*)))
              (edge-tag (cdr (assoc kind *kind-to-edge-tag*))))
          (when (and db-key edge-tag)
            (let ((tbl (ecase db-key
                         (:calls xref::*who-calls*)
                         (:called xref::*who-is-called*)
                         (:macroexpands xref::*who-macroexpands*)
                         (:references xref::*who-references*)
                         (:binds xref::*who-binds*)
                         (:sets xref::*who-sets*))))
              ;; Walk the entire DB once.  Each entry is
              ;;   callee-symbol -> list of context structs
              ;; where each context records (caller-name caller-file).
              (maphash
               (lambda (callee contexts)
                 (let ((callee-tn (resolve-defining-file callee)))
                   (when callee-tn
                     (dolist (ctx contexts)
                       (let ((caller-tn (cached-truename
                                         (xref::xref-context-file ctx))))
                         (when (and caller-tn
                                    (gethash caller-tn set)
                                    (not (equal caller-tn callee-tn)))
                           (push (make-edge
                                  :caller-file caller-tn
                                  :caller-name (xref::xref-context-name ctx)
                                  :callee-file callee-tn
                                  :callee-name callee
                                  :kind edge-tag)
                                 edges)))))))
               tbl))))))
    edges))

;;; ------------------------------------------------------------------
;;; Reporting
;;; ------------------------------------------------------------------

(defun report-system-deps (&key (system-name "maxima")
                                (kinds '(:calls :macros :refs :binds :sets))
                                (src-directory nil)
                                (show-clean nil))
  "Query mk:defsystem for SYSTEM-NAME (default \"maxima\"), run
xref-based analysis on every file, and report per-module
discrepancies between declared :depends-on and the actual
cross-module edges.

The system must already be loaded (i.e., maxima.system must have
been read by mk:defsystem so that mk::get-system finds it).

SRC-DIRECTORY, if provided, is the source directory used as a
fallback for resolving special-variable defining files when CMUCL's
native source-location info doesn't cover them.  Without it, a few
edges to specials defined via `(declare-top (special X))' or
`(proclaim '(special X))' may be missed."
  (let* ((modules (mk-collect-modules system-name))
         (file->mod (build-file-truename->module modules))
         (truenames (all-truenames file->mod))
         (resolver (make-defvar-resolver :scan-directory src-directory
                                         :truenames truenames))
         (edges (fast-system-xref-edges truenames kinds resolver))
         (by-source (edges-by-source-module edges file->mod))
         (n-modules 0)
         (n-clean 0)
         (n-with-issues 0)
         (n-missing 0)
         (n-unused 0))
    (format t "~%=== SYSTEM-LEVEL DEPENDENCY REPORT ===~%")
    (format t "System:      ~A~%" system-name)
    (format t "Modules:     ~D~%" (length modules))
    (format t "Files:       ~D (existing on disk)~%" (length truenames))
    (format t "Kinds:       ~{~A~^ ~}~%" kinds)
    (format t "Total cross-file edges: ~D~%~%" (length edges))
    (dolist (m modules)
      (incf n-modules)
      (let* ((name (module-info-name m))
             (declared (sort (copy-list (module-info-depends-on m)) #'string<))
             (transitive (transitive-deps name modules))
             (actual (foreign-callee-modules
                      (gethash name by-source) name file->mod))
             ;; Missing: edges go there, but neither it nor any of
             ;; its parents-via-deps is declared.  This is the "real"
             ;; missing-dep list -- transitively-satisfied edges
             ;; aren't flagged.
             (missing (set-difference actual transitive :test #'string=))
             ;; Unused: declared but no edges go there.  We can't
             ;; safely subtract transitives here because if A declares
             ;; B and B declares C, removing B from A's deps would
             ;; lose the explicit dependency.
             (unused (set-difference declared actual :test #'string=)))
        (cond ((or missing unused)
               (incf n-with-issues)
               (incf n-missing (length missing))
               (incf n-unused (length unused))
               (format t "--- ~A ---~%" name)
               (format t "  declared :depends-on:  ~{~A~^ ~}~%"
                       (or declared '("(none)")))
               (format t "  actual edges go to:    ~{~A~^ ~}~%"
                       (or actual '("(none)")))
               (when missing
                 (format t "  MISSING (edge target not in declared+transitive deps):~%    ~{~A~^ ~}~%"
                         missing))
               (when unused
                 (format t "  UNUSED (declared but no edges go there):~%    ~{~A~^ ~}~%"
                         unused))
               (format t "~%"))
              (t
               (incf n-clean)
               (when show-clean
                 (format t "--- ~A --- (clean)~%" name)
                 (format t "  ~{~A~^ ~}~%~%" (or declared '("(none)"))))))))
    (format t "=== SUMMARY ===~%")
    (format t "  Modules total:       ~D~%" n-modules)
    (format t "  Clean (no issues):   ~D~%" n-clean)
    (format t "  With discrepancies:  ~D~%" n-with-issues)
    (format t "    total MISSING:     ~D~%" n-missing)
    (format t "    total UNUSED:      ~D~%" n-unused)
    (format t "~%")
    (values)))

(format t "~%SYSTEM-LEVEL DEPENDENCY ANALYZER LOADED~%")
(format t "Usage:~%")
(format t "  (report-system-deps)~%")
(format t "  (report-system-deps :show-clean t)~%")
(format t "  (report-system-deps :kinds '(:calls :macros))~%")
(format t "  (report-system-deps :src-directory \"./src/\")~%")
