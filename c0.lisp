;;;; This compiler is from this documentation:
;;;; http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
;;;; https://github.com/namin/inc/blob/master/docs/tutorial.pdf?raw=true

(in-package #:c0)

;; -----------------------------------------
;; Utility stuff
;; -----------------------------------------

;; Help out CL's READ to read schemeish dialect.
;; Prolly should pick a better atomic object...
(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\t
                                #'(lambda (s c n)
                                    (declare (ignore s c n))
                                    ''_true))
  (set-dispatch-macro-character #\# #\f
                                #'(lambda (s c n)
                                    (declare (ignore s c n))
                                    ''_false)))

;; -----------------------------------------
;; Specials & Constants of all kinds.
;; -----------------------------------------

(defparameter *default-file*
  (asdf:system-relative-pathname :psilisp "scheme_entry.S"))


(base:defconstants
  +word-size+ 8 ;; bytes

  +fx-mask+ #x03 ;; the bits that constitute the tag
  +fx-tag+ #x00 ;; lower 2 zero bits are the tag itself.
  +fx-shift+ 2

  +bool-mask+ #xbf ;; the bits that constitute the tag
  +bool-f+ #x2f
  +bool-tag+ +bool-f+ ;; identifies both true and false objects
  +bool-t+ #x6f ;; bit pattern includes +bool-f+ !
  +bool-bit+ 6 ;; bit position of the discriminator bit in bool-t/f

  +char-mask+ #xff
  +char-tag+ #x0f
  +char-shift+ 8 ;; bits

  +null-mask+ #x3f
  +null+ +null-mask+
  )

(defparameter *primitives* (make-hash-table :test #'eq))

(defstruct primitive
  name
  arity
  emitter)

(defmacro define-primitive ((name si env &rest args) &body body)
  (let ((gname (gensym)))
    `(let ((,gname ',name))
       (setf (gethash ,gname *primitives*)
             (make-primitive :name ,gname
                             :arity (length ',args)
                             ;; Add si to the emitter
                             :emitter (lambda ,(cons si (cons env args))
                                        ,@body))))))

;; -----------------------------------------
;; Dealing with emitting of the program.
;; -----------------------------------------



(defun emit-program-to-file (program &key (output-file *default-file*)
                                       (entry "scheme_entry"))
  (with-open-file (strm output-file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (let ((*standard-output* strm)
          (base::*label-serial-number* 0))
      (emit-program program :entry entry))))

(defun emit-scheme-entry (label-name)
  (base:emit (:label label-name)))

(defun emit-scheme-exit ()
  (base:emit () "ret"))

(defun emit-program-header (c-entry-name scheme-toplevel-name)
  (base:emit () ".text")
  (base:emit () ".globl ~A" c-entry-name)

  (base:emit () "#if !__CYGWIN__")
  (base:emit () "# Windows PECOFF doesn't have this directive.")
  (base:emit () ".type ~A, @function" c-entry-name)
  (base:emit () "#endif")

  (base:emit (:label c-entry-name))

  (base:emit (:cmnt ("Save C stack ptr (safe on unix/win."))
             "movq %rsp, %r12")

  (base:emit () "#if __CYGWIN__")
  (base:emit (:cmnt ("Incoming arg0 -> lisp stack pointer."))
             "movq %rcx, %rsp")
  (base:emit () "#else")
  (base:emit (:cmnt ("Incoming arg0 -> lisp stack pointer."))
             "movq %rdi, %rsp")
  (base:emit () "#endif")


  (base:emit () "call ~A" scheme-toplevel-name)


  (base:emit (:cmnt ("Restore C stack pointer."))
             "movq %r12, %rsp")

  (base:emit () "ret"))

;; -----------------------------------------
;; The main compiler codebase.
;; -----------------------------------------

;; ----
;; Fixnums
;; ----

(defun fixnum-bits () (- (* +word-size+ 8) +fx-shift+))
(defun fxlower () (- (expt 2 (- (fixnum-bits) 1))))
(defun fxupper () (1- (expt 2 (- (fixnum-bits) 1))))
(defun fixnump (x) (and (integerp x) (<= (fxlower) x (fxupper))))

;; ----
;; Booleans
;; ----

(defun booleanp (x) (or (eq x #t) (eq x #f)))
(defun boolean-true-p (x) (eq x #t))
(defun boolean-false-p (x) (eq x #f))

;; ----
;; characterp is already in CL.
;; ----

;; ----
;; Immediate representations
;; ----

(defun immediatep (x) (or (fixnump x) (booleanp x) (characterp x) (null x)))
(defun immediate-rep (x)
  (cond
    ((fixnump x)
     (values (ash x +fx-shift+) :fixnum))
    ((booleanp x)
     (values (if (boolean-true-p x)
                 +bool-t+
                 +bool-f+)
             :boolean))
    ((characterp x)
     (values (logior (ash (char-code x) +char-shift+) +char-tag+)
             :character))
    ((null x)
     (values +null+ :null))
    (t
     (error "No immediate rep for '~A' can be determined!" x))))

;; ----
;; Primitives
;; ----

(defun primitivep (name)
  (gethash name *primitives*))

(defun primcallp (x) (and (listp x) (primitivep (car x))))

(defun validate-primcall-args (prim args)
  (unless (= (primitive-arity prim) (length args))
    (error "Primitive ~A is called with ~A arguments, but requires ~A."
           (primitive-name prim)
           (length args)
           (primitive-arity prim)))
  t)

;; ----
;; Conditional Expressions
;; ----

(defun ifp (x)
  (and (listp x) (eq (first x) 'if)))

(defun if-test (x)
  (second x))

(defun if-conseq (x)
  (third x))

(defun if-altern (x)
  (fourth x))

(defun andp (x)
  (and (listp x) (eq (first x) 'and)))

(defun orp (x)
  (and (listp x) (eq (first x) 'or)))

;; ----
;; LET handling
;; ----

(defun letp (x)
  (and (listp x) (eq (first x) 'let)))

(defun let-bindings (x)
  (second x))

(defun let-binding-lhs (x)
  (first x))

(defun let-binding-rhs (x)
  (second x))

(defun let-body (x)
  (cddr x))

;; ----
;; LET* handling
;; ----

(defun let*-p (x)
  (and (listp x) (eq (first x) 'let*)))

(defun let*-bindings (x)
  (second x))

(defun let*-binding-lhs (x)
  (first x))

(defun let*-binding-rhs (x)
  (second x))

(defun let*-body (x)
  (cddr x))

;; ----
;; Variables and environments.
;; ----

;; environments are kept as an a-list.
(defun extend-env (name stack-index current-env)
  (cons (cons name stack-index) current-env))

(defun lookup (var env)
  (find var env :key #'car))

(defun variablep (x)
  (symbolp x))

;; ----
;; LETREC handling
;; ----

(defun letrec-p (x)
  (and (listp x) (eq (first x) 'letrec)))

(defun letrec-bindings (x)
  (second x))

(defun letrec-binding-lhs (x)
  (first x))

(defun letrec-binding-rhs (x)
  (second x))

(defun letrec-body (x)
  (cddr x))

;; ----
;; LAMBDA handling
;; ----

(defun lambda-p (x)
  (and (listp x) (eq (first x) 'lambda)))

(defun lambda-formals (x)
  (second x))

(defun lambda-body (x)
  (cddr x))


;; ----
;; The main translation codes
;; ----

;; Unary Primitives

;; TODO: Check carefully overflow situations.
(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: fxadd1"))
             "addq $0x~X, %rax" (immediate-rep 1))
  #++(base:emit () "and $0x~X, %rax" (* (1- (expt 2 (fixnum-bits))) 4)))

;; TODO: Check carefully underflow situations.
(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: fxsub1"))
             "subq $0x~X, %rax" (immediate-rep 1))
  #++(base:emit () "and $0x~X, %rax" (* (1- (expt 2 (fixnum-bits))) 4)))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: fixnum->char"))
             "shlq $0x~X, %rax" (- +char-shift+ +fx-shift+))
  (base:emit () "or $0x~X, %rax" +char-tag+))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: char->fixnum"))
             "shrq $0x~X, %rax" (- +char-shift+ +fx-shift+)))

(define-primitive (fxzero? si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: fxzero?"))
             "cmpq $0x~X, %rax" 0)
  (emit-gen-bool "sete"))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: fixnum?"))
             "andb $0x~X, %al" +fx-mask+)
  (base:emit () "cmp $0x~X, %al" +fx-tag+)
  (emit-gen-bool "sete"))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: null?"))
             "andb $0x~X, %al" +null-mask+)
  (base:emit () "cmp $0x~X, %al" +null+)
  (emit-gen-bool "sete"))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: char?"))
             "andb $0x~X, %al" +char-mask+)
  (base:emit () "cmp $0x~X, %al" +char-tag+)
  (emit-gen-bool "sete"))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: boolean?"))
             "andb $0x~X, %al" +bool-mask+)
  (base:emit () "cmp $0x~X, %al" +bool-tag+)
  (emit-gen-bool "sete"))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: not"))
             "cmpb $0x~X, %al" +bool-f+)
  (emit-gen-bool "sete"))

(define-primitive (fxlognot si env arg) ;; No error checking
  (emit-expr si env arg)
  (base:emit (:cmnt ("prim: fxlognot"))
             "notq %rax")
  (base:emit () "andq $0x~X, %rax" (* (1- (expt 2 (fixnum-bits))) 4)))

;; Binary primitives

(define-primitive (fx+ si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fx+"))
             "addq ~A(%rsp), %rax" si))

(define-primitive (fx- si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fx-"))
             "subq ~A(%rsp), %rax" si))

(define-primitive (fx* si env arg0 arg1)
  ;; Use 4xy = (4x/4) * 4y to compute the value. If we bare multiplied them,
  ;; then they can overflow before we shift. But shifting one first will
  ;; preserve the maximum possible number of bits that we can use for the
  ;; operation.
  (emit-binop-args si env arg0 arg1)
  (base:emit () "sar $2, %rax")
  (base:emit (:cmnt ("prim: fx*"))
             "mulq ~A(%rsp)" si))

(define-primitive (fxlogand si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fx-"))
             "andq ~A(%rsp), %rax" si))

(define-primitive (fxlogor si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fxlogor"))
             "orq ~A(%rsp), %rax" si))

(define-primitive (fx= si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fx="))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "sete"))

(define-primitive (fx< si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fx<"))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "setl"))

(define-primitive (fx<= si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fx<="))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "setle"))

(define-primitive (fx> si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fx>"))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "setg"))

(define-primitive (fx>= si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: fx>="))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "setge"))

(define-primitive (char= si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: char="))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "sete"))

(define-primitive (char< si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: char<"))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "setl"))

(define-primitive (char<= si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: char<="))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "setle"))

(define-primitive (char> si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: char>"))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "setg"))

(define-primitive (char>= si env arg0 arg1)
  (emit-binop-args si env arg0 arg1)
  (base:emit (:cmnt ("prim: char>="))
             "cmpq ~A(%rsp), %rax" si)
  (emit-gen-bool "setge"))


(defun next-stack-index (si)
  (- si +word-size+))

(defun emit-gen-bool (setcc)
  ;; Right after the CMP instruction, we do this to generate the boolean value
  ;; instruction stream. We do it a lot, so abstract it to this function.
  (base:emit () "~A %al" setcc)
  (base:emit () "movzbq %al, %rax")
  (base:emit () "salb $0x~X, %al" +bool-bit+)
  (base:emit () "orb $0x~X, %al" +bool-tag+))

(defun emit-binop-args (si env arg0 arg1)
  (emit-expr si env arg1)
  (emit-stack-save si)
  (emit-expr (next-stack-index si) env arg0))

(defun emit-stack-save (si)
  (base:emit () "movq %rax, ~A(%rsp)" si))

(defun emit-stack-load (si)
  (base:emit () "movq ~A(%rsp), %rax" si))

(defun emit-immediate (x)
  (unless (immediatep x)
    (error "emit-immediate: Expression ~A is not an immediate!" x))
  (multiple-value-bind (encoded-value desc) (immediate-rep x)
    ;; Trickey handling of negative fixnums due to syntax requirements of gas.
    (if (and (fixnump x) (< x 0))
        (base:emit (:cmnt ("imm: ~(~A~) ~A" desc x))
                   "movq $-0x~X, %rax" (abs encoded-value))
        (base:emit (:cmnt ("imm: ~(~A~) ~A" desc x))
                   "movq $0x~X, %rax" encoded-value))))

(defun emit-primcall (si env x)
  (unless (primcallp x)
    (error "emit-primcall: Expression ~A is not a primcall!" x))
  (let* ((prim-name (car x))
         (prim-args (cdr x))
         (prim (gethash prim-name  *primitives*)))
    (validate-primcall-args prim prim-args)
    (apply (primitive-emitter prim) si env prim-args)))

(defun emit-if (si env x)
  (unless (ifp x)
    (error "emit-if: Expression ~A is not an IF expression!" x))
  (let ((alt-label (base:unique-label))
        (end-label (base:unique-label)))
    (emit-expr si env (if-test x))
    ;; NOTE: Unless %al specifically contains #f, we treat it as true.
    (base:emit (:cmnt ("if cond"))
               "cmp $0x~X, %al" +bool-f+)
    (base:emit () "je ~A" alt-label)
    (base:emit (:cmnt ("consequent")))
    (emit-expr si env (if-conseq x))
    (base:emit () "jmp ~A" end-label)
    (base:emit (:label alt-label))
    ;; TODO: If there is no if-altern, then we emit a null immediate into
    ;; %rax. R5RS says a false condition with no altern has an unspecified
    ;; return value, so good enough for now.
    (base:emit (:cmnt ("alternate")))
    (emit-expr si env (if-altern x))
    (base:emit (:label end-label))))

;; While we could just convert an AND form to a nested IF form (basically a
;; macro expansion then resubmit it back to emit-expr), we'll do something
;; slightly different in order to make the assembly more efficient and smaller.
(defun emit-and (si env x)
  (unless (andp x)
    (error "emit-and: Expression ~A is not an AND expression!" x))
  (if (null (cdr x))
      (emit-expr si env #t)
      (let ((shortcircuit-false (base:unique-label)))
        (base:emit (:cmnt ("AND expr start")))
        (loop :for test-x :in (cdr x)
              :do (base:emit (:cmnt ("Test condition")))
                  (emit-expr si env test-x)
                  (base:emit () "cmpb $0x~X, %al" +bool-f+)
                  (base:emit (:cmnt ("AND: possible short circuit"))
                             "je ~A" shortcircuit-false))
        (base:emit (:cmnt ("%rax now holds whatever the final result was.")))
        (base:emit (:label shortcircuit-false)))))

;; While we could just convert an OR form to a nested IF form (basically a
;; macro expansion then resubmit it back to emit-expr), we'll do something
;; slightly different in order to make the assembly more efficient and smaller.
(defun emit-or (si env x)
  (unless (orp x)
    (error "emit-or: Expression ~A is not an OR expression!" x))
  (if (null (cdr x))
      (emit-expr si env #f)
      (let ((shortcircuit-true (base:unique-label)))
        (base:emit (:cmnt ("OR expr start")))
        (loop :for test-x :in (cdr x)
              :do (base:emit (:cmnt ("Test condition")))
                  (emit-expr si env test-x)
                  (base:emit () "cmpb $0x~X, %al" +bool-f+)
                  (base:emit (:cmnt ("OR: possible short circuit"))
                             "jne ~A" shortcircuit-true))
        (base:emit (:cmnt ("%rax now holds whatever the final result was.")))
        (base:emit (:label shortcircuit-true)))))

;; NOTE: This doesn't allow closures yet. C-like stack use so far.
(defun emit-let (si env x)
  (labels ((process-let (bindings si new-env)
             (cond
               ((null bindings)
                (apply #'emit-expr si new-env (let-body x)))
               (t
                (let ((b (car bindings)))
                  (emit-expr si env (let-binding-rhs b))
                  (base:emit (:cmnt ("LET: Store var ~A at ~A"
                                     (let-binding-lhs b) si)))
                  (emit-stack-save si)
                  (process-let (cdr bindings)
                               (next-stack-index si)
                               (extend-env (let-binding-lhs b)
                                           si
                                           new-env)))))))
    (process-let (let-bindings x) si env)))

;; NOTE: This doesn't allow closures yet. C-like stack use so far.
(defun emit-let* (si env x)
  (labels ((process-let* (bindings si new-env)
             (cond
               ((null bindings)
                (apply #'emit-expr si new-env (let*-body x)))
               (t
                (let ((b (car bindings)))
                  (emit-expr si new-env (let*-binding-rhs b))
                  (base:emit (:cmnt ("LET*: Store var ~A at ~A"
                                     (let-binding-lhs b) si)))
                  (emit-stack-save si)
                  (process-let* (cdr bindings)
                                (next-stack-index si)
                                (extend-env (let*-binding-lhs b)
                                            si
                                            new-env)))))))
    (process-let* (let*-bindings x) si env)))

(defun emit-variable-ref (env var)
  (let ((binding (lookup var env)))
    (if binding
        (progn (base:emit (:cmnt ("Load var ~A at ~A" var (cdr binding))))
               (emit-stack-load (cdr binding)))
        (error "Variable ~A is not present in the environment!" var))))

(defun make-initial-env (var-list val-list)
  (mapcar #'cons var-list val-list))

;; NOTE: A toplevel only letrec for now. BROKEN.
(defun emit-letrec (x)
  (let* ((bindings (letrec-bindings x))
         (lvars (mapcar #'letrec-binding-lhs bindings))
         (lambdas (mapcar #'letrec-binding-rhs bindings))
         (labels (apply #'base:unique-labels lvars))
         (env (make-initial-env lambdas labels)))
    (mapc (emit-lambda env) lambdas labels)
    ;; TODO: Add in toplevel scheme entry here.
    (apply #'emit-expr (= +word-size+) env (letrec-body x))))

;; This is not a closed function.
(defun emit-lambda (env)
  (lambda (x label)
    (emit-scheme-entry label)
    (let ((fmls (lambda-formals x))
          (body (lambda-body x)))
      (labels ((f (fmls si env)
                 (cond
                   ((null fmls)
                    (apply #'emit-expr si env body))
                   (t
                    (f (cdr fmls)
                       (- si +word-size+)
                       (extend-env (first fmls) si env))))))
        (f fmls (- +word-size+) env)))
    (emit-scheme-exit)))

(defun emit-expr (si env x)
  (cond
    ((immediatep x) (emit-immediate x))
    ((variablep x) (emit-variable-ref env x))
    ((primcallp x) (emit-primcall si env x))
    ((ifp x) (emit-if si env x))
    ((letp x) (emit-let si env x))
    ((let*-p x) (emit-let* si env x))
    ((andp x) (emit-and si env x))
    ((orp x) (emit-or si env x))
    (t
     (error "emit-expr: Unknown expression: ~A" x))))

(defun emit-program (x &key (entry "scheme_entry"))
  (emit-program-header entry "L_scheme_entry")
  (emit-scheme-entry "L_scheme_entry")
  (emit-expr (- +word-size+) '() x)
  (emit-scheme-exit))

(defun the-program ()
  `(fx<= (fxadd1 8) (fxadd1 8))
  )

;; TODO: Abstract this stuff into base.

(defun test (&key (program (the-program)) (output *standard-output*))
  (emit-program-to-file program)
  (let* ((here (uiop:getcwd))
         (there (namestring (asdf:system-relative-pathname :psilisp ""))))
    ;; Try, with as little work as possible, to support Windows paths.
    (when (/= (uiop:chdir there) 0)
      (format t "Oops. Cannot chdir to there: ~S~%" there))
    (unwind-protect
         (uiop:run-program "make && ./de" :force-shell t :output output)
      (when (/= (uiop:chdir here) 0)
        (format t "Oops. Cannot chdir back to here: ~S" here)))))


(defun capture-repl-output (output)
  "OUTPUT is a multiline string that is the output of the c0 program.
Return a list of XXX strings whose values were here in a REPL output line:
INC-SCHEME> XXX ;; comments
as found in the output string (which may be a multiline string)."

  (let ((lines (remove-if-not
                (lambda (line)
                  (ppcre:scan "^INC-SCHEME> " line))
                (ppcre:split "(\\r)?\\n" output)))
        (result nil))

    ;; TODO: This is not entirely good for matching...
    (loop :for line :in lines
          :do  (ppcre:register-groups-bind (text)
                   ("^INC-SCHEME> (.*) ;;.*$" line)
                 (push text result)))

    (nreverse result)))
