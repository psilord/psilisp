;; Just an uncompiled pile of vague ideas in some unreadable lisp dialect.


;; Fundamental type specification.

(define-type u8
    (memory-structure
     :bits 8
     :continguous t
     :alignment 1) ;; what does this REALLY mean?
  (value-semantics
   :fundamental-type :integer
   :signed t
   :overflow :wrap
   :underflow :wrap
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fx* ((a :fixnum) (b :fixnum))
  (asm
   (mov %rax a)
   (mov %rbx b)
   (shr %rax 2)
   (mulq %rax %rbx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seeing about CLOS like modeling only with some slight extentions to
;; LAMBDA.

(lambda-e
 type-name ;; type-name if this function is a closure, nil to gensym it.
 (closing scopes) ;; description of what kind of vars to close over/access
 (formal arguments) ;; usual argument list.

 body)

;; Shortct notation:
(lambda (x) x)
->
(lambda-e nil (:lexical) (x) x)

;; When we call this function, it returns a new function which is the
;; closure we want and therefore has a type associated with it.
(define make-bank-account
    (lambda (initial) ;; <- arguments, some of which are closed over later.

      (lambda-e
       bank-account ;; <- type-name of this closure.
       (:lexical) ;; <- What free variables are closed over?
       (cmd &rest args) ;; <- formal arguments

       (ecase cmd ;; <- message disptch idiom
	 (:add (incf initial (first args)))
	 (:sub (decf initial (first args)))))))


(define make-named-bank-account
  (lambda (name)
    (lambda-e
     named-bank-account
     (:lexical bank-account) ;; Automatically have access to INITIAL now.
     (cmd &rest args)

     ;; But how did/does initial get initialized?

     (ecase cmd
       (:name
	;; hrm, seems broken.
	)))))
