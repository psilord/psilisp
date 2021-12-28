(in-package #:st)

;; --------
;; Symbol Table
;; Also synonymous with a "frame" of variables at a scope.
;; --------

(defclass symtab ()
  (
   ;; hash table
   ;; Key: NAME (usually a symbol, but could be more complex)
   ;; Value: syment object
   (%syments :accessor syments
             :initarg :syments
             :initform (make-hash-table :test #'equal))
   ;; Depth from the empty environment for this symbol table.
   (%scope-id :accessor scope-id
              :initarg :scope-id
              :initform 0)))

(defun make-symtab (&rest init-args)
  (apply #'make-instance 'symtab init-args))

;; Maybe I should rename this to BIND-SYMBOL ?
(defun insert-symbol (st name syment)
  (setf (gethash name (syments st)) syment))

(defun lookup-symbol (st name)
  (gethash name (syments st)))

;; Maybe I should rename this to UNBIND-SYMBOL?
(defun remove-symbol (st name)
  (a:when-let ((sym (lookup-symbol st name)))
    (remhash name st)))

(defun all-names (st)
  (when st
    (a:hash-table-keys (syments st))))

(defun all-symbols (st)
  (when st
    (a:hash-table-values (syments st))))

(defun dump-symtab (st)
  (format t " Dumping symbol table (scope-id: ~A):~%" (scope-id st))
  (maphash (lambda (name syment)
             (format t "   ~A -> ~A~%" name syment))
           (syments st)))
