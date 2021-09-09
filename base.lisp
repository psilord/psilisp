(in-package #:base)

(defparameter *label-serial-number* 0)

(defmacro defconstants (&body body)
  `(progn
     ,@(loop :for (name val) :on body :by #'cddr
             :collect `(defconstant ,name ,val))))

(defun unique-label ()
  (let ((sn *label-serial-number*))
    (incf *label-serial-number*)
    (format nil "L_~D" sn)))

(defun unique-labels (&rest names)
  (loop :for name :in names
        :collect (concatenate 'string (unique-label) "_"
                              (string-downcase (symbol-name name)))))

;; Prolly too much work went into this...
(defmacro emit ((&key (label nil) (cmnt nil))
                &rest insn) ;; Specifically &rest for indention.
  "LABEL is supplied is evaluated.
CNT is a list of a FMT string and the required args for it.
INSN, acting like a &body, is a FMT string and required args, not in list form"
  (let* ((empty-fmt "")
         (label-fmt "~A:")
         (cmnt-label-fmt "~40A")
         (insn-fmt "    ~36A")
         (cmnt-fmt "# ~A")
         (bare-cmnt-fmt "    # ~A"))
    (multiple-value-bind (line-fmt reified-args-list)
        (cond
          ((and (not label) (not cmnt) (not insn))
           (values (concatenate 'string
                                empty-fmt "~%")
                   `()))

          ((and (not label) (not cmnt) insn)
           (values (concatenate 'string
                                insn-fmt "~%")
                   `((format nil ,@insn))))

          ((and (not label) cmnt (not insn))
           (values (concatenate 'string
                                bare-cmnt-fmt "~%")
                   `((format nil ,@cmnt))))

          ((and (not label) cmnt insn)
           (values (concatenate 'string
                                insn-fmt
                                cmnt-fmt "~%")
                   `((format nil ,@insn)
                     (format nil ,@cmnt))))

          ((and label (not cmnt) (not insn))
           (values (concatenate 'string
                                label-fmt "~%")
                   `(,label)))

          ((and label (not cmnt) insn)
           (values (concatenate 'string
                                label-fmt "~%"
                                insn-fmt "~%")
                   `(,label
                     (format nil ,@insn))))

          ((and label cmnt (not insn))
           (values (concatenate 'string
                                cmnt-label-fmt
                                cmnt-fmt "~%")
                   `((format nil ,label-fmt ,label) ;; tricky!
                     (format nil ,@cmnt))))

          ((and label cmnt insn)
           (values (concatenate 'string
                                label-fmt "~%"
                                insn-fmt
                                cmnt-fmt "~%")
                   `(,label
                     (format nil ,@insn)
                     (format nil ,@cmnt)))))

      `(format *standard-output*  ,line-fmt ,@reified-args-list))))


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

(defun find-definition (bst var-spec &key (scope :inner)) ;; :any
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
