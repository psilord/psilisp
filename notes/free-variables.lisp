(in-package #:c1)

;; -----------------------------------------------------------------------
;; FUTURE STUFF TODO
;; -----------------------------------------------------------------------



;; -----------------------------------------------------------------------
;; Free Variable Discovery
;; Recursively examine a sexp form and return the list of free variables.
;; -----------------------------------------------------------------------

;; TODO: Not entirely sure yet of the usefulness of this, it is half-in/out of
;; the block structured symbol table understanding of the environment.
;;
;; Notably, it doesn't insert anything into the environment.... but instead
;; fools around doing the set theory itself.
(defun free-variables (env expr)
  (cond
    ((immediate-p expr)
     '())

    ((var-p expr)
     (let ((sym (base:find-definition env expr :scope :any)))
       (cond
         ;; Not in symbol table, must be free/unbound/non-existent
         ((null sym)
          (list expr))
         ;; globals or primitives cannot be free.
         ((or (prim-p sym) (global-p sym))
          '())
         ;; syment exists and is not global/prim, must be local, so free!
         (t ;; must be local
          (list expr)))))

    ((if-p env expr)
     (reduce #'union (mapcar (lambda (x) (free-variables env x))
                             (list (if-condition expr)
                                   (if-conseq expr)
                                   (if-altern expr)))))
    ((set!-p env expr)
     (reduce #'union
             (list (free-variables env (set!-var expr))
                   (free-variables env (set!-expr expr)))))
    ((begin-p env expr)
     (reduce #'union (mapcar (lambda (x) (free-variables env x))
                             (cdr expr))))
    ((lambda-p env expr)
     (let ((formal-vars (lambda-formals expr))
           (body-vars (reduce #'union (mapcar (lambda (x)
                                                (free-variables env x))
                                              (lambda-body expr)))))
       (set-difference body-vars formal-vars)))

    ((define-p env expr) ;; maybe sketchy?
     (free-variables env (define-expr expr)))

    ((listp expr) ;; an arbitrary combination
     (reduce #'union (mapcar (lambda (x) (free-variables env x))
                             expr)))
    (t
     (error "free-variables: Have no idea how to handle: ~S~%" expr))))

#++(defun test-fv (expr)
     (let ((env (base:make-blsymtab)))
       (base:open-scope env)
       ;; fake up a default global environment with primitives
       (loop :for v :in *primitive-vars*
	     :do (base:add-definition env v
				      (make-syment :global-p t :prim-p t)))

       (free-variables env expr)))
