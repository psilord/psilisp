(in-package #:bst)

;; --------
;; Block structured symbol table
;; --------

(defclass blsymtab ()
  (
   ;; a stack of symbol tables each representing a scope of some kind.
   (%frames :accessor frames
            :initarg :frames
            :initform nil)))

(defun make-blsymtab (&rest init-args)
  (apply #'make-instance 'blsymtab init-args))

(defun open-scope (bst)
  "Add a new innermost frame to the environment."
  (let* ((curr-inner (first (frames bst)))
         (new-inner (st:make-symtab :scope-id
                                    (if curr-inner
                                        (1+ (st:scope-id curr-inner))
                                        0))))
    (push new-inner (frames bst))))

(defun close-scope (bst)
  "Remove the innermost frame from the environment."
  (pop (frames bst)))

(defun add-definition (bst name sym)
  "Add a definition to the innermost scope."
  (let ((st (first (frames bst))))
    (unless st
      (error "add-definition: There are no open scopes!"))
    (st:insert-symbol st name sym)))

(defun find-definition (bst name &key (scope :inner))
  "Search the BLSYMTAB for the definition of NAME. If SCOPE is :INNER only
to inner most scope is searched, if it is :ANY then all scopes are searched in
order until one is found.  Return two values of the discovered syment, or NIL
of non is found during lookup and the associated symbol table in which it was
found."
  (ecase scope
    (:inner
     (let ((st (first (frames bst))))
       (when st
         (values (st:lookup-symbol st name) st))))
    (:any
     (loop :for st :in (frames bst)
           :for sym = (st:lookup-symbol st name)
           :when sym
             :return (values sym st)))))

(defun current-definitions (bst &key (scope :inner))
  "Return a list of the visible lexical variables defined in the specified
scope."
  (case scope
    (:inner
     (st:all-names (first (frames bst))))
    (:any
     (reduce #'union (mapcar (lambda (st) (st:all-names st))
                             (frames bst))))))

(defun current-scope (bst)
  (car (frames bst)))

(defun dump-blsymtab (bst)
  (format t "Dumping Block Structured Symbol Table.~%")
  (loop :for st :in (frames bst)
        :do (st:dump-symtab st)))
