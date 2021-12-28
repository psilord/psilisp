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
;; mutable cell transform (transform assignment to mutation)
;; determine free variables
;; make closures and close all lambdas
;; name & lift closed functions to toplevel
;;
;; If things like + are global variables, then not closing over them is good.
;;
;; NOTE: It is by definition that all variables are tagged pointers. If a
;; variable is mutable it MUST be contained in a cell-like object. A tagged
;; pointer can also be an immediate object too, and this is why it must be
;; contained in a cell-like object. There must _always_ be a reference to the
;; place in question!

;; NOTE: I've left out the fact that all arguments to functions get converted
;; to cells.

;; original code
(let ((x 10)
      (y 20))
  (list (lambda (v) (+ x y v))
        (lambda (v) (set! x v))))

;; Pass 1: mutable cell transform
;; This will make anything that is only a variable into a place. So, for things
;; like (set! (aref some-array index) value), that is already a place and
;; doesn't need to be converted to a specific cell.
(let ((x (make-cell 10))
      (y 20))
  (list (lambda (v) (+ (cell-get x) y v))
        (lambda (v) (cell-set! x v))))

;; Pass 2: determine free variables:
(let ((x (make-cell 10))
      (y 20))
  (list (lambda (v) (+ (cell-get x) y v)) ;; x and y are free
        (lambda (v) (cell-set! x v)))) ;; x is free

;; Pass 3: make (flat) closures and produce closed lambdas
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
          (make-closure :closed-vars (vector x)
                        :func cf1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try again with a more complex closure

;; original code
(let ((x 10)
      (y 20))
  (lambda (z)
    (list (lambda (a) a)
          (lambda (b) (+ x b))
          (lambda (c) (+ y c))
          (lambda (cc) (set! y cc))
          (lambda (d) (+ z d))
          (lambda (e) (+ x y e))
          (lambda (f) (+ x z f))
          (lambda (g) (+ y z g))
          (lambda (h) (+ x y z h)))))

;; Pass 1: mutable cell transform
(let ((x 10)
      (y (make-cell 20)))
  (lambda (z)
    (list (lambda (a) a)
          (lambda (b) (+ x b))
          (lambda (c) (+ (cell-ref y) c))
          (lambda (cc) (cell-set! y cc))
          (lambda (d) (+ z d))
          (lambda (e) (+ x (cell-ref y) e))
          (lambda (f) (+ x z f))
          (lambda (g) (+ (cell-ref y) z g))
          (lambda (h) (+ x (cell-ref y) z h)))))

;; Pass 2: determine free variables
(let ((x 10) ; FV: nil
      (y (make-cell 20)))
  (lambda (z) ; FV: x y
    (list (lambda (a) a) ; FV: nil
          (lambda (b) (+ x b)) ; FV: x
          (lambda (c) (+ (cell-ref y) c)) ; FV: y
          (lambda (cc) (cell-set! y cc)) ; FV: y
          (lambda (d) (+ z d)) ; FV: z
          (lambda (e) (+ x (cell-ref y) e)) ; FV: x y
          (lambda (f) (+ x z f)) ; FV: x z
          (lambda (g) (+ (cell-ref y) z g)) ; FV: y z
          (lambda (h) (+ x (cell-ref y) z h))))) ; FV: x y z

;; Pass 3: Flat Closure Conversion
;; Notice we copy values from one closure into another so all scopes of all
;; variables are cleanly broken at function boundaries.
(let ((x 10)
      (y (make-cell 20)))
  (make-closure
   :closed-vars (vector x y) ; 0 -> x, 1 -> y cell
   ;; NOTE: The LAMBDA below here should be CLAMBDA to represent a lambda
   ;; function with no free variables. but I write LAMBDA here for the nice
   ;; indention.
   :func (lambda (c0 z)
           (list
            (make-closure
             :closed-vars (vector) ; No FV to capture.
             :func (lambda (c1 a) a))

            (make-closure
             :closed-vars (vector (cref c0 0)) ; 0 -> [x copy from c0]
             :func (lambda (c2 b) (+ (cref c2 0) b)))

            (make-closure
             :closed-vars (vector (cref c0 1)) ; 0 -> [y cell copy from c0]
             :func (lambda (c3 c) (+ (cell-ref (cref c3 0)) c)))

            (make-closure
             :closed-vars (vector (cref c0 1)) ; 0 -> [y cell copy from c0]
             :func (lambda (c4 cc) (cell-set! (cref c4 0) cc)))

            (make-closure
             :closed-vars (vector z) ; 0 -> z
             :func (lambda (c5 d) (+ (cref c5 0) d)))

            (make-closure
             :closed-vars (vector (cref c0 0) ; 0 -> [x copy from c0]
                                  (cref c0 1)) ; 1 -> [y cell copy from c0]
             :func (lambda (c6 e) (+ (cref c6 0) (cell-get (cref c6 1)) e)))

            (make-closure
             :closed-vars (vector (cref c0 0) ; 0 -> [x copy from c0]
                                  z) ; 1 -> z
             :func (lambda (c7 f) (+ (cref c7 0) (cref c7 1) f)))

            (make-closure
             :closed-vars (vector (cref c0 1) ; 0 -> [y cell copy from c0]
                                  z) ; 1 -> z
             :func (lambda (c8 g) (+ (cell-ref (cref c7 0)) (cref c7 1) g)))

            (make-closure
             :closed-vars (vector (cref c0 0) ; 0 -> [x copy from c0]
                                  (cref c0 1) ; 1 -> [y cell copy from c0]
                                  z) ; 2 -> z
             :func (lambda (c9 h) (+ (cref c9 0)
                                     (cell-ref (cref c9 1))
                                     (cref c9 2))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some odd thinking. Lower the tagged array representation to the lowest level
;; and implement funcallable instance, objects, structures, closures, etc,
;; using it.

(defun invoke-fun-inst (fun-inst &rest args)
  (apply (aref fun-inst 0) fun-inst args))

;; basically a closure object generator.
(defun make-fun-inst (slot-values fun)
  (let* ((num-slots (length slot-values))
         (fun-inst (make-array (+ num-slots 1))))
    (setf (aref fun-inst 0) fun)
    (loop :for i :from 0 :below num-slots
          :do (setf (aref fun-inst (1+ i)) (aref slot-values i)))
    fun-inst))

(defun iref (fun-inst slot-id)
  (aref fun-inst (1+ slot-id)))

(defun fun-inst-test ()
  (let ((fun-inst
          (make-fun-inst (vector 100)
                         (lambda (c0 &rest args)
                           ;; reference a closed variable at location 0
                           (+ (iref c0 0) (reduce #'+ args))))))
    (invoke-fun-inst fun-inst 1 2 3 4)))
