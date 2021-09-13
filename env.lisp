(in-package #:base)

;;;; TODO: COnvert this functionality into three layered packages
;;;; on top of each other to separate concerns and API.

;; --------
;; Symbol Table
;; Also synonymous with a "frame" of variables at a scope.
;; --------

(defstruct symtab
  ;; hash table
  ;; Key: var-spec
  ;; Value: syment
  (syments (make-hash-table :test #'equal))
  ;; Depth from the empty environment for this symbol table.
  (scope-id 0)
  )

(defun insert-symbol (st var-spec syment)
  (setf (gethash var-spec (symtab-syments st)) syment))

(defun lookup-symbol (st var-spec)
  (gethash var-spec (symtab-syments st)))

(defun dump-symtab (st)
  (format t " Dumping symbol table (scope-id: ~A):~%" (symtab-scope-id st))
  (maphash (lambda (var-spec syment)
             (format t "   ~A -> ~A~%" var-spec syment))
           (symtab-syments st)))

;; --------
;; Block structured symbol table
;; --------

(defstruct blsymtab
  ;; a stack of frames throughout the program
  (frames nil))

(defun open-scope (bst)
  "Add a new innermost frame to the environment."
  (let* ((curr-inner (first (blsymtab-frames bst)))
         (new-inner (make-symtab :scope-id
                                 (if curr-inner
                                     (1+ (symtab-scope-id curr-inner))
                                     0))))
    (push new-inner (blsymtab-frames bst))))

(defun close-scope (bst)
  "Remove the innermost frame from the environment."
  (pop (blsymtab-frames bst)))

(defun add-definition (bst var-spec sym)
  "Add a definition to the innermost scope."
  (let ((st (first (blsymtab-frames bst))))
    (unless st
      (error "add-definition: There are no open scopes!"))
    (insert-symbol st var-spec sym)))

(defun find-definition (bst var-spec &key (scope :inner))
  "Search the BLSYMTAB for the definition of VAR-SPEC. If SCOPE is :INNER only
to inner most scope is searched, if it is :ANY then all scopes are searched in
order until one is found.  Return two values of the discovered syment, or NIL
of non is found during lookup and the associated symbol table in which it was
found."
  (ecase scope
    (:inner
     (let ((st (first (blsymtab-frames bst))))
       (when st
         (values (lookup-symbol st var-spec) st))))
    (:any
     (loop :for st :in (blsymtab-frames bst)
           :for sym = (lookup-symbol st var-spec)
           :when sym
             :return (values sym st)))))

(defun current-definitions (bst &key (scope :inner))
  "Return a list of the visible lexical variables defined in the specified
scope."
  (case scope
    (:inner
     (let ((inner (first (blsymtab-frames bst))))
       (when inner
         (a:hash-table-keys (symtab-syments inner)))))
    (:any
     (reduce #'union (mapcar (lambda (st)
                               (a:hash-table-keys (symtab-syments st)))
                             (blsymtab-frames bst))))))

(defun dump-blsymtab (bst)
  (format t "Dumping Block Structured Symbol Table.~%")
  (loop :for st :in (blsymtab-frames bst)
        :do (dump-symtab st)))

;; --------
;; Environment table.
;;
;; Any environmental category like :var, :type, but which is tested with
;; EQUAL etc, can have associated with it a block structured symbol
;; table. Definitions are added or looked up in specific categories.
;; Used to manage all symbol table needs for whatever semantic namespace
;; I need.
;; --------
(defstruct env
  (categories (make-hash-table :test #'equal)))

;; a check to ensure I don't have any typos in my environmental categories.
(defun validate-category (category)
  (ecase category
    ;; NOTE: :debug, debug0, debug1 are for hand debugging, they are not part
    ;; of the environmental semantics of the compiler.
    ((:debug :debug0 :debug1) t)
    ;; These are part of the compiler.
    ((:var) t)))

(defun ensure-blsymtab-in-category (env category)
  "Create an empty block structured symbol table in the environment associated
with the category if none exists and return it. If it already exists, just
return it. NOTE: This function does no scope opening/closing."
  (validate-category category)
  (let ((blsymtab (gethash category (env-categories env))))
    (unless blsymtab
      (setf blsymtab (make-blsymtab)
            (gethash category (env-categories env)) blsymtab))
    blsymtab))

(defun env-open-scope (env category)
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (open-scope blsymtab)))

(defun env-close-scope (env category)
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (close-scope blsymtab)))

(defun env-add-definition (env category var-spec sym)
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (add-definition blsymtab var-spec sym)))

(defun env-find-definition (env category var-spec &key (scope :inner))
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (find-definition blsymtab var-spec :scope scope)))

(defun env-current-definitions (env category &key (scope :inner))
  (let ((blsymtab (ensure-blsymtab-in-category env category)))
    (current-definitions blsymtab :scope scope)))

(defun dump-env (env)
  (format t "Dumping Environment with ~A semantic categories:~%"
          (hash-table-count (env-categories env)))
  (maphash (lambda (category blsymtab)
             (format t ">>> Category: ~S <<<~%" category)
             (dump-blsymtab blsymtab))
           (env-categories env))
  (format t "End dumping Environment.~%"))
