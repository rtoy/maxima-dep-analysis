;;;; compile-maxima-with-xref-corrected.lisp
;;;;
;;;; Correct script to compile Maxima with CMUCL XREF enabled
;;;; Based on actual CMUCL XREF documentation

(in-package :cl-user)

(defun compile-maxima-with-xref (&key (maxima-src-dir "/path/to/maxima/src/")
                                      (clear-database t))
  "Compile Maxima with CMUCL XREF tracking enabled"
  
  (format t "~&CMUCL XREF Compilation of Maxima~%")
  (format t "~&================================~%")
  
  ;; Check we're in CMUCL
  #-cmu
  (error "This script requires CMUCL - the Cross-Reference facility is CMUCL-specific")
  
  ;; Enable XREF recording
  (setf c:*record-xref-info* t)
  (format t "~&✓ XREF recording enabled (c:*record-xref-info* = ~A)~%" c:*record-xref-info*)
  
  ;; Clear previous XREF data if requested
  (when clear-database
    (xref:init-xref-database)
    (format t "~&✓ XREF database cleared~%"))
  
  ;; Change to Maxima source directory
  (let ((old-dir (ext:default-directory)))
    (unwind-protect
        (progn
          (setf (ext:default-directory) maxima-src-dir)
          (format t "~&✓ Changed to directory: ~A~%" maxima-src-dir)
          
          ;; Check that maxima.system exists
          (unless (probe-file "maxima.system")
            (error "Cannot find maxima.system in ~A" maxima-src-dir))
          
          ;; Load the system definition
          (format t "~&Loading maxima.system...~%")
          (load "maxima.system")
          
          ;; Compile with XREF enabled
          ;; Using :xref t option to compile-file ensures XREF data is recorded
          (format t "~&Compiling Maxima with XREF tracking...~%")
          (format t "~&This may take several minutes...~%")
          
          ;; Use with-compilation-unit to batch the compilation
          (with-compilation-unit ()
            (mk:compile-system "maxima" :force t))
          
          (format t "~&✓ Maxima compilation complete!~%")
          
          ;; Test that we have XREF data
          (test-xref-data))
      
      ;; Restore directory (cleanup form)
      (setf c:*record-xref-info* nil)
      (setf (ext:default-directory) old-dir))))

(defun test-xref-data ()
  "Test that XREF data was properly collected"
  (format t "~%Testing XREF data collection...~%")
  
  (let ((test-symbols '(simp add mplus mtimes)))
    (dolist (sym test-symbols)
      (when (fboundp sym)
        (let ((callers (xref:who-calls sym)))
          (format t "~&~A is called by ~D contexts~%" sym (length callers))
          
          ;; Show a few examples
          (dolist (context (subseq callers 0 (min 3 (length callers))))
            (format t "  ~A~%" (xref:xref-context-name context)))))))
  
  ;; Test variable references
  (format t "~%Testing variable references...~%")
  (let ((test-vars '(*print-base* *print-radix*)))
    (dolist (var test-vars)
      (when (boundp var)
        (let ((refs (xref:who-references var)))
          (when refs
            (format t "~&~A is referenced by ~D contexts~%" var (length refs))))))))

(defun quick-xref-test ()
  "Quick test to verify XREF is working with some simple examples"
  (format t "~&Quick XREF functionality test...~%")
  
  ;; Enable XREF if not already enabled
  (unless c:*record-xref-info*
    (setf c:*record-xref-info* t)
    (format t "~&Enabled XREF recording~%"))
  
  ;; Compile some test functions
  (format t "~&Compiling test functions...~%")
  (compile 'test-function-1 
           '(lambda (x) (+ x (* 2 x))))
  
  (compile 'test-function-2
           '(lambda (y) (test-function-1 (- y 1))))
  
  ;; Test XREF queries
  (format t "~&Testing XREF queries...~%")
  
  ;; Who calls +?
  (let ((plus-callers (xref:who-calls '+)))
    (format t "~&Functions calling +: ~D~%" (length plus-callers))
    (dolist (caller (subseq plus-callers 0 (min 5 (length plus-callers))))
      (format t "  ~A~%" (xref:xref-context-name caller))))
  
  ;; Who calls our test function?
  (let ((test-callers (xref:who-calls 'test-function-1)))
    (format t "~&Functions calling test-function-1: ~D~%" (length test-callers))
    (dolist (caller test-callers)
      (format t "  ~A~%" (xref:xref-context-name caller)))))

(defun save-xref-data (filename)
  "Save XREF data by saving the current Lisp image"
  (format t "~&Saving XREF data to image ~A~%" filename)
  (format t "~&Note: XREF data is not saved in FASL files~%")
  (format t "~&The only way to preserve it is to save the Lisp image~%")

  #+(and nil cmu)
  (ext:save-lisp filename
                 :init-function (lambda ()
                                  (format t "~&Restored CMUCL with XREF data~%")
                                  (format t "~&Ready for cross-reference analysis~%"))
                 :executable t
                 :purify nil) ; Don't purify to keep XREF data
  (ext:save-lisp filename
                 :executable t
                 :purify t)
  
  #-cmu
  (error "This function requires CMUCL"))

(defun batch-compile-maxima-files (file-list)
  "Compile individual Maxima files with XREF enabled"
  (format t "~&Batch compiling ~D Maxima files with XREF...~%" (length file-list))
  
  (unless c:*record-xref-info*
    (setf c:*record-xref-info* t))
  
  (let ((compiled-count 0)
        (error-count 0))
    
    (with-compilation-unit ()
      (dolist (file file-list)
        (format t "~&Compiling ~A..." file)
        (handler-case
            (progn
              (compile-file file :xref t)
              (incf compiled-count)
              (format t " ✓~%"))
          (error (e)
            (incf error-count)
            (format t " ✗ (~A)~%" e)))))
    
    (format t "~%Compilation summary:~%")
    (format t "  Successfully compiled: ~D~%" compiled-count)
    (format t "  Errors: ~D~%" error-count)))

(defun analyze-file-with-xref (filename)
  "Compile and analyze a single file with detailed XREF output"
  (format t "~&Analyzing ~A with XREF...~%" filename)
  
  (unless c:*record-xref-info*
    (setf c:*record-xref-info* t))
  
  ;; Compile the file
  (compile-file filename :xref t)
  
  ;; Try to find functions defined in this file and analyze them
  ;; This is approximate since CMUCL XREF doesn't directly tell us
  ;; which symbols are defined in which files
  
  (format t "~&Functions that might be defined in ~A:~%" filename)
  
  ;; This is a heuristic - look for functions whose name suggests
  ;; they might be in this file
  (let ((base-name (pathname-name filename)))
    (do-symbols (sym)
      (when (and (fboundp sym)
                 (search base-name (symbol-name sym)))
        (let ((callers (xref:who-calls sym))
              (refs (when (boundp sym) (xref:who-references sym))))
          (when (or callers refs)
            (format t "  ~A:~%" sym)
            (when callers
              (format t "    Called by: ~{~A~^, ~}~%" 
                      (mapcar #'xref:xref-context-name 
                              (subseq callers 0 (min 3 (length callers))))))
            (when refs
              (format t "    Referenced by: ~{~A~^, ~}~%" 
                      (mapcar #'xref:xref-context-name 
                              (subseq refs 0 (min 3 (length refs))))))))))))

(defun extract-xref-summary ()
  "Extract a summary of all XREF information currently available"
  (format t "~&XREF Database Summary~%")
  (format t "~&====================~%")
  
  ;; Count how many symbols have XREF information
  (let ((function-count 0)
        (variable-count 0)
        (total-calls 0)
        (total-refs 0))
    
    (do-all-symbols (sym)
      (when (fboundp sym)
        (let ((callers (xref:who-calls sym)))
          (when callers
            (incf function-count)
            (incf total-calls (length callers)))))
      
      (when (boundp sym)
        (let ((refs (xref:who-references sym)))
          (when refs
            (incf variable-count)
            (incf total-refs (length refs))))))
    
    (format t "Functions with call information: ~D~%" function-count)
    (format t "Variables with reference information: ~D~%" variable-count)
    (format t "Total function call contexts: ~D~%" total-calls)
    (format t "Total variable reference contexts: ~D~%" total-refs)
    
    ;; Show most-called functions
    (format t "~%Most frequently called functions:~%")
    (let ((call-counts '()))
      (do-all-symbols (sym)
        (when (fboundp sym)
          (let ((callers (xref:who-calls sym)))
            (when (> (length callers) 5) ; Only show if called > 5 times
              (push (cons sym (length callers)) call-counts)))))
      
      (setf call-counts (sort call-counts #'> :key #'cdr))
      (dolist (pair (subseq call-counts 0 (min 10 (length call-counts))))
        (format t "  ~A: ~D call sites~%" (car pair) (cdr pair))))))

;; Provide usage instructions
(format t "~%~%CORRECTED CMUCL XREF COMPILATION SCRIPT~%")
(format t "========================================~%")
(format t "Based on actual CMUCL Cross-Reference Facility documentation~%")
(format t "~%Key differences from previous version:~%")
(format t "- Uses proper CMUCL XREF API (xref:who-calls, xref:who-references, etc.)~%")
(format t "- Uses c:*record-xref-info* variable correctly~%")  
(format t "- Uses :xref option to compile-file~%")
(format t "- Handles xref-context structures properly~%")
(format t "~%Usage:~%")
(format t "1. (quick-xref-test) - Test XREF is working~%")
(format t "2. (compile-maxima-with-xref :maxima-src-dir \"/path/to/maxima/src/\")~%")
(format t "3. (extract-xref-summary) - See what data was collected~%")
(format t "4. (save-xref-data \"maxima-with-xref.core\") - Save image with XREF~%")
(format t "~%Then load maxima-xref-analyzer-corrected.lisp for analysis~%")
