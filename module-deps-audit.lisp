;;; module-deps-audit.lisp
;;;
;;; System-wide audit of module-level :depends-on declarations against
;;; actual xref edges.  For each module:
;;;   - the declared :depends-on list,
;;;   - the actual outbound module-edges (computed from per-file xref),
;;;   - suggested adds (actual deps not in declared+transitive),
;;;   - suggested removals (declared deps with no observed edges).
;;;
;;; CAVEATS:
;;;   - "Suggested removals" are NOT safe to act on blindly.  xref
;;;     records function calls, macroexpansions, special-var refs/binds/
;;;     sets.  It does NOT record:
;;;       * read-time effects (e.g. compatibility-macros1 needing
;;;         float-format because numeric literals must read as doubles);
;;;       * init-order dependencies via load-time property registration
;;;         (the conjugate.lisp pattern: nothing macroexpands, but
;;;         (setf (get '$conjugate 'operators) ...) must run before
;;;         downstream code uses $conjugate);
;;;       * funcall of dynamically-built symbol names.
;;;   - "Suggested adds" are more reliable: if xref records an edge,
;;;     it's real.  But a declared transitive ancestor may already
;;;     cover it.  We compute transitive closure to filter those out.
;;;
;;; Usage (after Maxima is built and loaded into CMUCL):
;;;
;;;   (load "outbound-deps.lisp")    ; for callee-defining-file
;;;   (load "module-deps-audit.lisp")
;;;   (audit-module-deps :system-file "./src/maxima.system"
;;;                      :src-dir     "./src/")
;;;
;;; To detect module-level cycles instead:
;;;
;;;   (report-module-cycles :system-file "./src/maxima.system"
;;;                         :src-dir     "./src/")

(in-package :cl-user)

;;; ---------------------------------------------------------------
;;; Parse maxima.system
;;; ---------------------------------------------------------------

(defun strip-line-comments (text)
  (with-output-to-string (out)
    (with-input-from-string (in text)
      (loop for line = (read-line in nil nil) while line do
            (let ((semi (position #\; line)))
              (write-line (if semi (subseq line 0 semi) line) out))))))

(defun match-paren (text open-idx)
  "Return index of the close-paren matching the open-paren at OPEN-IDX."
  (let ((depth 1) (i (1+ open-idx)) (in-str nil) (n (length text)))
    (loop while (and (< i n) (> depth 0)) do
          (let ((c (char text i)))
            (cond
              (in-str
               (case c
                 (#\\ (incf i 2) (return-from nil))
                 (#\" (setf in-str nil) (incf i))
                 (t   (incf i))))
              (t
               (case c
                 (#\" (setf in-str t) (incf i))
                 (#\( (incf depth) (incf i))
                 (#\) (decf depth) (when (zerop depth) (return-from match-paren i)) (incf i))
                 (t   (incf i)))))))
    i))

(defstruct sysmod
  name           ; lowercase string
  depends-on     ; list of lowercase strings
  files)         ; list of lowercase basenames (no .lisp)

(defun parse-maxima-system (path)
  (let* ((text (strip-line-comments
                (with-open-file (s path) (let ((b (make-string (file-length s))))
                                          (read-sequence b s) b))))
         (modules '()))
    (loop with start = 0
          for m = (search ":module" text :start2 start)
          while m do
            ;; Walk back to '(' before :module
            (let ((k (1- m)))
              (loop while (and (>= k 0)
                               (member (char text k) '(#\Space #\Tab #\Newline #\Return)))
                    do (decf k))
              (when (and (>= k 0) (char= (char text k) #\())
                (let* ((open k)
                       (close (match-paren text open))
                       (body (subseq text (1+ open) close))
                       ;; name: token after ":module"
                       (name-start (+ (search ":module" body) (length ":module")))
                       (name-end (position-if
                                  (lambda (c)
                                    (member c '(#\Space #\Tab #\Newline #\Return)))
                                  body :start (1+ name-start)))
                       (name (string-downcase
                              (string-trim " "
                                           (subseq body (1+ name-start) name-end))))
                       (deps '())
                       (files '()))
                  ;; :depends-on (...)
                  (let ((d (search ":depends-on" body)))
                    (when d
                      (let* ((p (position #\( body :start (+ d (length ":depends-on"))))
                             (q (match-paren body p))
                             (depbody (subseq body (1+ p) q))
                             (i 0))
                        (loop
                          (let ((qs (position #\" depbody :start i)))
                            (unless qs (return))
                            (let ((qe (position #\" depbody :start (1+ qs))))
                              (unless qe (return))
                              (push (string-downcase (subseq depbody (1+ qs) qe)) deps)
                              (setf i (1+ qe))))))))
                  ;; :components (...)
                  (let ((c (search ":components" body)))
                    (when c
                      (let* ((p (position #\( body :start (+ c (length ":components"))))
                             (q (match-paren body p))
                             (cbody (subseq body (1+ p) q))
                             (i 0))
                        ;; (:file "name") and (:private-file "name")
                        (loop
                          (let ((fpos (search ":file" cbody :start2 i)))
                            (unless fpos (return))
                            (let ((qs (position #\" cbody :start fpos)))
                              (unless qs (return))
                              (let ((qe (position #\" cbody :start (1+ qs))))
                                (unless qe (return))
                                (push (string-downcase (subseq cbody (1+ qs) qe)) files)
                                (setf i (1+ qe)))))))))
                  (push (make-sysmod :name name
                                     :depends-on (nreverse deps)
                                     :files (nreverse files))
                        modules)
                  (setf start (1+ close)))))
            (incf start))
    (nreverse modules)))

;;; ---------------------------------------------------------------
;;; Per-file outbound edges (xref-only, no source reads)
;;; ---------------------------------------------------------------

(defun file-outbound-edges (file-truename src-dir-string)
  "Return list of (callee-truename . edge-count) for outbound edges
from FILE-TRUENAME, restricted to callees whose defining-file lies
under SRC-DIR-STRING."
  (let ((tab (make-hash-table :test 'equal)))
    (dolist (kind '(:calls :macroexpands :references :binds :sets))
      (dolist (entry (xref::find-xrefs-for-pathname kind file-truename))
        (let* ((callee (first entry))
               (contexts (second entry))
               (cf (callee-defining-file callee)))
          (when (and cf
                     (not (equal cf file-truename))
                     (search src-dir-string (namestring cf)))
            (incf (gethash cf tab 0) (length contexts))))))
    (let ((out '()))
      (maphash (lambda (k v) (push (cons k v) out)) tab)
      out)))

;;; ---------------------------------------------------------------
;;; Audit driver
;;; ---------------------------------------------------------------

(defun transitive-closure (mod-name modules-by-name)
  (let ((seen (make-hash-table :test 'equal)))
    (labels ((walk (n)
               (let ((m (gethash n modules-by-name)))
                 (when m
                   (dolist (d (sysmod-depends-on m))
                     (unless (gethash d seen)
                       (setf (gethash d seen) t)
                       (walk d)))))))
      (walk mod-name))
    (let ((out '()))
      (maphash (lambda (k v) (declare (ignore v)) (push k out)) seen)
      out)))

(defun audit-module-deps (&key (system-file "./src/maxima.system")
                               (src-dir     "./src/")
                               (stream      *standard-output*)
                               verbose)
  "Audit each module's :depends-on against actual xref edges.

By default, prints only the declared deps and the suggested-to-remove
deps for each module.  Pass :VERBOSE T to also print actual outbound
modules with edge counts and MISSING deps (modules used but not
declared).

Suggested removals are NOT safe to act on blindly -- xref doesn't see
read-time effects, init-order property registrations, or
funcall-of-symbol patterns.  Treat the list as candidates for
investigation, not a removal list."
  (let* ((modules (parse-maxima-system system-file))
         (modules-by-name (make-hash-table :test 'equal))
         ;; basename (lowercase, no extension) -> module name
         (file-to-mod (make-hash-table :test 'equal))
         (src-dir-truename (truename src-dir))
         (src-dir-string (namestring src-dir-truename)))
    (dolist (m modules)
      (setf (gethash (sysmod-name m) modules-by-name) m)
      (dolist (f (sysmod-files m))
        (setf (gethash f file-to-mod) (sysmod-name m))))
    (format stream "~%Parsed ~D modules from ~A~%"
            (length modules) system-file)
    ;; For each module, accumulate per-file edges into module-level edges.
    (dolist (m modules)
      (let* ((mod-name (sysmod-name m))
             (declared (sysmod-depends-on m))
             (transitive (transitive-closure mod-name modules-by-name))
             (covered (let ((s (make-hash-table :test 'equal)))
                        (dolist (d declared) (setf (gethash d s) t))
                        (dolist (d transitive) (setf (gethash d s) t))
                        s))
             (target-mods (make-hash-table :test 'equal))
             (target-edge-count (make-hash-table :test 'equal)))
        (dolist (basename (sysmod-files m))
          (let* ((file-path
                  (probe-file
                   (merge-pathnames
                    (concatenate 'string basename ".lisp")
                    src-dir-truename))))
            (when file-path
              (dolist (e (file-outbound-edges file-path src-dir-string))
                (let* ((callee-tn (car e))
                       (count (cdr e))
                       (callee-base (string-downcase
                                     (pathname-name callee-tn)))
                       (callee-mod (gethash callee-base file-to-mod)))
                  (when (and callee-mod (not (equal callee-mod mod-name)))
                    (setf (gethash callee-mod target-mods) t)
                    (incf (gethash callee-mod target-edge-count 0)
                          count)))))))
        ;; Report.
        (let* ((missing (let ((out '()))
                          (maphash (lambda (k v)
                                     (declare (ignore v))
                                     (unless (gethash k covered)
                                       (push k out)))
                                   target-mods)
                          (sort out #'string<)))
               (unused (let ((out '()))
                         (dolist (d declared)
                           (unless (gethash d target-mods)
                             (push d out)))
                         (sort out #'string<))))
          (cond
            (verbose
             (format stream "~%--- ~A ---~%" mod-name)
             (format stream "  declared: ~{~A~^ ~}~%" declared)
             (let ((actuals '()))
               (maphash (lambda (k v)
                          (declare (ignore v))
                          (push (cons k (gethash k target-edge-count 0))
                                actuals))
                        target-mods)
               (setf actuals (sort actuals
                                   (lambda (a b) (> (cdr a) (cdr b)))))
               (format stream "  actual:   ~{~A(~D)~^ ~}~%"
                       (loop for x in actuals
                             nconc (list (car x) (cdr x)))))
             (when missing
               (format stream "  MISSING:~%")
               (dolist (mm missing)
                 (format stream "    ~A  (~D edge~:P)~%"
                         mm (gethash mm target-edge-count))))
             (when unused
               (format stream "  no xref edges to:~%    ~{~A~^ ~}~%"
                       unused)))
            (t
             (format stream "~%~A~%" mod-name)
             (format stream "  depends-on: ~{~A~^ ~}~%" declared)
             (when missing
               (format stream "  missing (xref edges, not declared/transitive): ~{~A~^ ~}~%"
                       missing))
             (when unused
               (format stream "  no xref edges to: ~{~A~^ ~}~%"
                       unused)))))))
    (format stream "~%(End of audit)~%")
    (values)))

;;; ---------------------------------------------------------------
;;; Module-level cycle detection
;;; ---------------------------------------------------------------
;;;
;;; A module-level cycle is a strongly-connected component of more
;;; than one module in the directed graph (caller-module ->
;;; callee-module).  We use Tarjan's algorithm on the module graph
;;; built from per-file xref edges, and report each non-trivial SCC
;;; together with example function-level edges.

(defstruct rich-edge
  caller-file       ; truename
  caller-name       ; symbol or string
  callee-file       ; truename
  callee-name       ; symbol
  kind)             ; :call :macroexpands :reference :bind :set

(defun file-outbound-rich-edges (file-truename src-dir-string)
  "Like FILE-OUTBOUND-EDGES but returns RICH-EDGE structs with caller
context names retained, so we can report 'function X in file A calls
function Y in file B' for cycle explanation."
  (let ((out '()))
    (dolist (kind '(:calls :macroexpands :references :binds :sets))
      (dolist (entry (xref::find-xrefs-for-pathname kind file-truename))
        (let* ((callee (first entry))
               (contexts (second entry))
               (cf (callee-defining-file callee)))
          (when (and cf
                     (not (equal cf file-truename))
                     (search src-dir-string (namestring cf)))
            (dolist (ctx contexts)
              (push (make-rich-edge
                     :caller-file file-truename
                     :caller-name (xref::xref-context-name ctx)
                     :callee-file cf
                     :callee-name callee
                     :kind kind)
                    out))))))
    out))

(defun strongly-connected-components (nodes edges-by-source)
  "Tarjan's algorithm.  NODES is a list, EDGES-BY-SOURCE is a hash of
node -> list of node successors.  Returns list of SCCs, each a list
of nodes; only non-trivial SCCs (size > 1, or self-loop) are returned."
  (let ((index 0)
        (stack '())
        (on-stack (make-hash-table :test 'equal))
        (idx (make-hash-table :test 'equal))
        (low (make-hash-table :test 'equal))
        (sccs '()))
    (labels ((strong (v)
               (setf (gethash v idx) index
                     (gethash v low) index)
               (incf index)
               (push v stack)
               (setf (gethash v on-stack) t)
               (dolist (w (gethash v edges-by-source))
                 (cond ((not (gethash w idx))
                        (strong w)
                        (setf (gethash v low)
                              (min (gethash v low) (gethash w low))))
                       ((gethash w on-stack)
                        (setf (gethash v low)
                              (min (gethash v low) (gethash w idx))))))
               (when (eql (gethash v low) (gethash v idx))
                 (let ((comp '()))
                   (loop
                     (let ((w (pop stack)))
                       (remhash w on-stack)
                       (push w comp)
                       (when (equal w v) (return))))
                   (when (or (> (length comp) 1)
                             (member v (gethash v edges-by-source)
                                     :test #'equal))
                     (push comp sccs))))))
      (dolist (n nodes)
        (unless (gethash n idx)
          (strong n))))
    sccs))

(defun report-module-cycles (&key (system-file "./src/maxima.system")
                                  (src-dir     "./src/")
                                  (stream      *standard-output*)
                                  (max-examples-per-direction 3))
  "Detect module-level cycles in the call/macroexpand/etc. graph and
report each cycle together with example function-level edges."
  (let* ((modules (parse-maxima-system system-file))
         (file-to-mod (make-hash-table :test 'equal))
         (src-dir-truename (truename src-dir))
         (src-dir-string (namestring src-dir-truename))
         ;; (caller-mod . callee-mod) -> list of rich-edges
         (mod-edges (make-hash-table :test 'equal))
         ;; caller-mod -> list of distinct callee-mod
         (succ (make-hash-table :test 'equal))
         (mod-names '()))
    (dolist (m modules)
      (push (sysmod-name m) mod-names)
      (dolist (f (sysmod-files m))
        (setf (gethash f file-to-mod) (sysmod-name m))))
    ;; Walk every file, classify edges into module pairs.
    (dolist (m modules)
      (dolist (basename (sysmod-files m))
        (let ((fp (probe-file
                   (merge-pathnames
                    (concatenate 'string basename ".lisp")
                    src-dir-truename))))
          (when fp
            (dolist (e (file-outbound-rich-edges fp src-dir-string))
              (let* ((callee-base (string-downcase
                                   (pathname-name (rich-edge-callee-file e))))
                     (callee-mod (gethash callee-base file-to-mod))
                     (caller-mod (sysmod-name m)))
                (when (and callee-mod (not (equal callee-mod caller-mod)))
                  (push e (gethash (cons caller-mod callee-mod) mod-edges))
                  (pushnew callee-mod (gethash caller-mod succ)
                           :test #'equal))))))))
    (let ((cycles (strongly-connected-components
                   (nreverse mod-names) succ)))
      (cond
        ((null cycles)
         (format stream "~%No module-level cycles detected.~%"))
        (t
         (format stream "~%~D module-level cycle~:P:~%" (length cycles))
         (dolist (scc cycles)
           (format stream "~%--- cycle: ~{~A~^ <-> ~} ---~%" scc)
           ;; For every ordered pair within the SCC that has edges,
           ;; print example function-level edges.
           (dolist (caller scc)
             (dolist (callee scc)
               (unless (equal caller callee)
                 (let ((edges (gethash (cons caller callee) mod-edges)))
                   (when edges
                     (format stream "  ~A -> ~A  (~D edge~:P)~%"
                             caller callee (length edges))
                     (let ((shown 0))
                       (dolist (e edges)
                         (when (< shown max-examples-per-direction)
                           (format stream
                                   "    ~A:~A -> ~A:~A  [~A]~%"
                                   (file-namestring (rich-edge-caller-file e))
                                   (rich-edge-caller-name e)
                                   (file-namestring (rich-edge-callee-file e))
                                   (rich-edge-callee-name e)
                                   (rich-edge-kind e))
                           (incf shown)))
                       (when (> (length edges) max-examples-per-direction)
                         (format stream "    ... (~D more)~%"
                                 (- (length edges)
                                    max-examples-per-direction))))))))))))
      (format stream "~%(End of cycle report)~%"))
    (values)))
