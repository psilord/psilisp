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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; On managed memory and garbage collection
;;
;; I am starting to be firmly convinced, unless evidence or math points me
;; in another direction that the type system and garbage collector have to
;; know about each other explicitly.
;;
;; For example, only regions of memory that can be pointed to via tagged
;; pointers are garbage collectable. Memory gotten from 'elsewhere' like
;; another runtime language's library, or via hardware DMA regions, etc, etc,
;; are by definition not collectable things.
;;
;; Thought: There is a diffrence between a sequence that holds exactly and only
;; tagged pointers (or tagged immediates) and a sequence that holds ONLY
;; untagged immediates. You can copy a tagged pointer around and the value
;; it represets won't move in memory. But in the case of an untagged immadiate,
;; you can only copy the entire bit pattern (like a structure copy in C).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Closure conversion example
;;
;; mutable cell transform
;; determine free variables
;; make closures and close all lambdas
;; name & lift closed functions to toplevel
;;
;; If things like + are global variables, then not closing over them is good.

;; original code
(let ((x 10)
      (y 20))
  (list (lambda (v) (+ x y v))
	(lambda (v) (set! x v))))

;; Pass 1: mutable cell transform
(let ((x (make-cell 10))
      (y 20))
  (list (lambda (v) (+ (cell-get x) y v))
	(lambda (v) (cell-set! x v))))

;; Pass 2: determine free variables:
(let ((x (make-cell 10))
      (y 20))
  (list (lambda (v) (+ (cell-get x) y v)) ;; x and y are free
	(lambda (v) (cell-set! x v)))) ;; x is free

;; Pass 3: make closures and produce closed lambdas
;; The x tagged pointer value is copied into two different closures, but both
;; copies of the tagged pointers point to the _same_ tagged array.
(let ((x (make-cell 10)) ;; tagged pointer to unique TA x copied into closures
      (y 20)) ;; immediate value y is copied into closures.
  (list (make-closure :closed-vars (vector x y) ;; variables to close over
		      :func (clambda (c0 v)
				     (+ (cell-get (cref c0 0))
					(cref c0 1)
					v)))
	(make-closure :closed-vars (vector x) ;; variables to close over
		      :func (clambda (c1 v)
				     (cell-set! (cref c1 0) v)))))

;; Pass 4: name and lift clambdas to top level

;; letrec instead of labels for CL indenting...
(labels ((cf0 (clambda (c0 v)
	       (+ (cell-get (cref c0 0))
		(cell-get (cref c0 1))
		v)))
	 (cf1 (clambda (c1 v)
	       (cell-set! (cref c1 0) v))))
  (let ((x (make-cell 10))
	(y 20))
    (list (make-closure :closed-vars (vector x y)
			:func cf0)
	  (make-closure :cloesd-vars (vector x)
			:func cf1))))
